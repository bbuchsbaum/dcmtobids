run_post_op <- function(cmd, src_file, dst_file, warning_sink = NULL) {
  command <- gsub("src_file", shQuote(src_file), cmd, fixed = TRUE)
  command <- gsub("dst_file", shQuote(dst_file), command, fixed = TRUE)

  out_file <- tempfile("dcmtobids-postop-out-")
  err_file <- tempfile("dcmtobids-postop-err-")
  on.exit(unlink(c(out_file, err_file), force = TRUE), add = TRUE)

  status <- system2(
    command = "sh",
    args = c("-c", command),
    stdout = out_file,
    stderr = err_file
  )
  stdout <- if (file.exists(out_file)) paste(readLines(out_file, warn = FALSE), collapse = "\n") else ""
  stderr <- if (file.exists(err_file)) paste(readLines(err_file, warn = FALSE), collapse = "\n") else ""

  if (!identical(status, 0L)) {
    msg <- paste0("post_op command failed: ", command)
    if (is.function(warning_sink)) {
      warning_sink(
        code = "W_POST_OP_FAILED",
        message = msg,
        severity = "warning",
        context = list(command = command, status = status, stderr = stderr)
      )
    } else {
      cli::cli_alert_warning("post_op command failed: {.code {command}}")
      if (nzchar(stderr)) {
        cli::cli_text("stderr: {stderr}")
      }
    }
  }

  invisible(list(status = status, stdout = stdout, stderr = stderr))
}

build_id_map <- function(plan) {
  out <- list()
  matched <- which(plan$status == "MATCHED" & !is.na(plan$id) & nzchar(plan$id))

  for (idx in matched) {
    src_root <- plan$source_root[[idx]]
    files <- find_associated_files(src_root)
    exts <- unique(vapply(files, infer_file_ext, character(1)))
    ext <- if (".nii.gz" %in% exts) {
      ".nii.gz"
    } else if (".nii" %in% exts) {
      ".nii"
    } else {
      NA_character_
    }

    if (is.na(ext)) {
      next
    }

    rel <- file.path(plan$destination_dir[[idx]], paste0(plan$destination_stem[[idx]], ext))
    id <- plan$id[[idx]]
    out[[id]] <- unique(c(out[[id]], rel))
  }

  out
}

apply_sidecar_changes <- function(
    data,
    sidecar_changes,
    id_map,
    participant_label,
    bids_uri = "URI",
    warning_sink = NULL
) {
  if (!length(sidecar_changes)) {
    return(data)
  }

  for (key in names(sidecar_changes)) {
    vals <- sidecar_changes[[key]]
    if (!is.list(vals)) {
      vals <- as.list(vals)
    }

    assigned <- list()

    for (val in vals) {
      scalar <- val
      if (is.list(scalar)) {
        scalar <- unlist(scalar, recursive = TRUE, use.names = FALSE)
      }
      if (length(scalar) != 1L) {
        next
      }

      scalar <- scalar[[1]]

      if (is.character(scalar) && identical(trimws(scalar), "")) {
        data[[key]] <- NULL
        next
      }

      if (key %in% PATH_SIDECAR_CHANGE_KEYS && !is.null(id_map[[as.character(scalar)]])) {
        mapped <- id_map[[as.character(scalar)]]
        if (identical(bids_uri, "URI")) {
          mapped <- paste0("bids::", mapped)
        } else {
          mapped <- sub(paste0("^", participant_label, "/"), "", mapped)
        }
        assigned <- c(assigned, as.list(mapped))
      } else if (key %in% PATH_SIDECAR_CHANGE_KEYS && is.character(scalar)) {
        msg <- paste0("No ID found for sidecar_changes[", key, "] value '", scalar, "'; skipping this entry.")
        if (is.function(warning_sink)) {
          warning_sink(
            code = "W_MISSING_ID_REFERENCE",
            message = msg,
            severity = "warning",
            context = list(field = key, value = scalar)
          )
        } else {
          cli::cli_alert_warning(
            "No ID found for sidecar_changes[{key}] value {.val {scalar}}; skipping this entry."
          )
        }
      } else {
        assigned <- c(assigned, list(scalar))
      }
    }

    if (length(assigned) == 1L) {
      data[[key]] <- assigned[[1]]
    } else if (length(assigned) > 1L) {
      data[[key]] <- unlist(assigned, recursive = TRUE, use.names = FALSE)
    }
  }

  data
}

#' Convert files from a conversion plan
#'
#' @param plan Plan from [plan_conversion()]
#' @param bids_dir Destination BIDS directory
#' @param clobber Overwrite existing files
#' @param bids_uri IntendedFor path style: URI or relative
#' @param post_op Optional list of post-op command objects
#' @return Conversion summary list
#' @export
convert_plan <- function(
    plan,
    bids_dir,
    clobber = FALSE,
    bids_uri = "URI",
    post_op = list()
) {
  bids_dir <- fs::path_abs(bids_dir)
  fs::dir_create(bids_dir, recurse = TRUE)

  id_map <- build_id_map(plan)

  converted <- 0L
  skipped <- 0L
  errors <- 0L
  warning_events <- list()

  emit_warning <- function(code, message, severity = "warning", context = list()) {
    warning_events[[length(warning_events) + 1L]] <<- list(
      code = code,
      severity = severity,
      message = message,
      context = context
    )
    if (identical(severity, "warning")) {
      cli::cli_alert_warning(message)
    } else if (identical(severity, "error")) {
      cli::cli_alert_warning(message)
    } else {
      cli::cli_alert_info(message)
    }
  }

  for (i in seq_len(nrow(plan))) {
    if (!identical(plan$status[[i]], "MATCHED")) {
      next
    }

    src_root <- plan$source_root[[i]]
    files <- find_associated_files(src_root)
    if (!length(files)) {
      emit_warning(
        code = "W_NO_SOURCE_FILES",
        message = paste0("No files found for source root ", src_root),
        severity = "warning",
        context = list(source_root = src_root)
      )
      skipped <- skipped + 1L
      next
    }

    dst_base <- file.path(bids_dir, plan$destination_dir[[i]], plan$destination_stem[[i]])
    dsts <- paste0(dst_base, vapply(files, infer_file_ext, character(1)))
    existing_dsts <- dsts[fs::file_exists(dsts)]

    if (length(existing_dsts) && !isTRUE(clobber)) {
      emit_warning(
        code = "W_OUTPUT_EXISTS_SKIPPED",
        message = paste0(
          "Skipping existing acquisition (use --clobber): ",
          plan$destination_stem[[i]]
        ),
        severity = "warning",
        context = list(paths = existing_dsts, destination_stem = plan$destination_stem[[i]])
      )
      skipped <- skipped + length(files)
      next
    }

    for (src in files) {
      ext <- infer_file_ext(src)
      dst <- paste0(dst_base, ext)

      ok <- tryCatch({
        if (identical(ext, ".json")) {
          data <- read_json_file(src)
          data$Dcm2bidsVersion <- as.character(utils::packageVersion("dcmtobids"))
          if (!is.na(plan$task_name[[i]]) && nzchar(plan$task_name[[i]])) {
            data$TaskName <- plan$task_name[[i]]
          }
          data <- apply_sidecar_changes(
            data = data,
            sidecar_changes = plan$sidecar_changes[[i]],
            id_map = id_map,
            participant_label = plan$participant_label[[i]],
            bids_uri = bids_uri,
            warning_sink = emit_warning
          )
          write_json_file(dst, data)
          TRUE
        } else {
          safe_copy_file(src, dst, clobber = clobber)
        }
      }, error = function(e) {
        emit_warning(
          code = "E_MOVE_FAILED",
          message = paste0("Failed to move ", src, " -> ", dst, ": ", conditionMessage(e)),
          severity = "error",
          context = list(src = src, dst = dst)
        )
        NA
      })

      if (isTRUE(ok)) {
        converted <- converted + 1L
      } else if (identical(ok, FALSE)) {
        skipped <- skipped + 1L
      } else {
        errors <- errors + 1L
      }
    }

    if (length(post_op)) {
      src_file <- paste0(dst_base, ".nii.gz")
      if (!fs::file_exists(src_file)) {
        alt <- paste0(dst_base, ".nii")
        src_file <- if (fs::file_exists(alt)) alt else src_file
      }

      for (op in post_op) {
        op_datatype <- unlist(op$datatype %||% list("any"), recursive = TRUE, use.names = FALSE)
        op_suffix <- unlist(op$suffix %||% list("_any"), recursive = TRUE, use.names = FALSE)
        if (!(plan$datatype[[i]] %in% op_datatype || "any" %in% op_datatype)) {
          next
        }
        if (!(plan$suffix[[i]] %in% op_suffix || "_any" %in% op_suffix)) {
          next
        }

        dst_file <- src_file
        if (!is.null(op$custom_entities)) {
          extra_entities <- c(normalize_custom_entities(op$custom_entities), plan$custom_entities[[i]])
          extra_stem <- build_bids_stem(
            participant_label = plan$participant_label[[i]],
            session_label = plan$session_label[[i]],
            custom_entities = extra_entities,
            suffix = plan$suffix[[i]],
            do_not_reorder_entities = isTRUE(plan$do_not_reorder_entities[[i]])
          )
          extra_base <- file.path(bids_dir, plan$destination_dir[[i]], extra_stem)
          dst_file <- paste0(extra_base, infer_file_ext(src_file))

          # Parity with Python post_op: create paired JSON for custom_entities variant.
          src_json <- paste0(dst_base, ".json")
          extra_json <- paste0(extra_base, ".json")
          if (fs::file_exists(src_json) && (!fs::file_exists(extra_json) || isTRUE(clobber))) {
            fs::dir_create(dirname(extra_json), recurse = TRUE)
            fs::file_copy(src_json, extra_json, overwrite = isTRUE(clobber))
          }
        }

        if (!is.null(op$cmd) && is.character(op$cmd) && nzchar(op$cmd)) {
          run_post_op(op$cmd, src_file = src_file, dst_file = dst_file, warning_sink = emit_warning)
        }
      }
    }
  }

  list(
    converted = converted,
    skipped = skipped,
    errors = errors,
    warnings = warning_events
  )
}

#' Plan and run conversion directly
#'
#' @param config_path Path to config JSON
#' @param input_dir Directory containing dcm2niix outputs
#' @param bids_dir Destination BIDS directory
#' @param participant_label Participant label
#' @param session_label Session label
#' @param auto_extract_entities Enable automatic extraction of BIDS entities
#'   (e.g. `task`, `dir`, `echo`) from sidecar fields
#' @param do_not_reorder_entities Keep entity order as supplied
#' @param clobber Overwrite existing files
#' @return Summary list
#' @export
run_conversion <- function(
    config_path,
    input_dir,
    bids_dir,
    participant_label,
    session_label = "",
    auto_extract_entities = FALSE,
    do_not_reorder_entities = FALSE,
    clobber = FALSE
) {
  config <- read_config(config_path)
  plan <- plan_conversion(
    config = config,
    input_dir = input_dir,
    participant_label = participant_label,
    session_label = session_label,
    search_method = config$search_method %||% "fnmatch",
    case_sensitive = config$case_sensitive %||% TRUE,
    auto_extract_entities = isTRUE(auto_extract_entities) || (config$auto_extract_entities %||% FALSE),
    do_not_reorder_entities = isTRUE(do_not_reorder_entities) || (config$do_not_reorder_entities %||% FALSE),
    dup_method = config$dup_method %||% "run"
  )

  convert_plan(
    plan = plan,
    bids_dir = bids_dir,
    clobber = clobber,
    bids_uri = config$bids_uri %||% "URI",
    post_op = config$post_op %||% list()
  )
}
