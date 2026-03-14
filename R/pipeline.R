sidecar_lt <- function(a, b, comp_keys = c("AcquisitionTime", "SeriesNumber", "SidecarFilename")) {
  lts <- logical()

  for (key in comp_keys) {
    lt <- NA
    a_has <- key %in% names(a$data)
    b_has <- key %in% names(b$data)

    if (a_has && b_has) {
      aval <- a$data[[key]]
      bval <- b$data[[key]]
      if (!identical(aval, bval)) {
        lt <- tryCatch({
          isTRUE(aval < bval)
        }, error = function(e) NA)
      }
    }
    lts <- c(lts, lt)
  }

  for (lt in lts) {
    if (!is.na(lt)) {
      return(isTRUE(lt))
    }
  }

  FALSE
}

sort_sidecars_like_python <- function(sidecars) {
  if (length(sidecars) <= 1L) {
    return(sidecars)
  }

  idx <- seq_along(sidecars)
  for (i in 2:length(idx)) {
    j <- i
    while (j > 1L) {
      left <- idx[[j - 1L]]
      right <- idx[[j]]
      if (isTRUE(sidecar_lt(sidecars[[right]], sidecars[[left]]))) {
        idx[[j - 1L]] <- right
        idx[[j]] <- left
        j <- j - 1L
      } else {
        break
      }
    }
  }

  sidecars[idx]
}

collect_sidecars <- function(input_dir) {
  input_dir <- fs::path_abs(input_dir)
  if (!fs::dir_exists(input_dir)) {
    cli::cli_abort("Input directory not found: {.file {input_dir}}")
  }

  json_paths <- sort(fs::dir_ls(input_dir, recurse = TRUE, type = "file", regexp = "\\.json$"))
  sidecars <- vector("list", length(json_paths))

  for (i in seq_along(json_paths)) {
    path <- as.character(json_paths[[i]])
    data <- read_json_file(path)
    data$SidecarFilename <- basename(path)
    sidecars[[i]] <- list(
      path = path,
      root = sidecar_root_from_json(path),
      data = data
    )
  }

  sort_sidecars_like_python(sidecars)
}

combine_extractors <- function(user_extractors, auto_extract_entities = FALSE) {
  user_extractors <- user_extractors %||% list()
  if (!is.list(user_extractors)) {
    user_extractors <- list()
  }

  if (!isTRUE(auto_extract_entities)) {
    return(user_extractors)
  }

  tags <- union(names(user_extractors), names(AUTO_EXTRACTORS))
  out <- list()

  for (tag in tags) {
    user_vals <- unlist(user_extractors[[tag]] %||% list(), recursive = TRUE, use.names = FALSE)
    auto_vals <- unlist(AUTO_EXTRACTORS[[tag]] %||% list(), recursive = TRUE, use.names = FALSE)
    out[[tag]] <- unique(as.character(c(user_vals, auto_vals)))
  }

  out
}

extract_entities <- function(sidecar_data, desc, config, warning_sink = NULL) {
  custom_entities <- normalize_custom_entities(desc$custom_entities)
  extractors <- combine_extractors(config$extractors, isTRUE(config$auto_extract_entities))
  extracted <- list()

  if (length(extractors)) {
    for (tag in names(extractors)) {
      if (is.null(sidecar_data[[tag]])) {
        next
      }

      values <- sidecar_data[[tag]]
      values <- as.character(unlist(values, recursive = TRUE, use.names = FALSE))
      patterns <- as.character(unlist(extractors[[tag]], recursive = TRUE, use.names = FALSE))

      for (pattern in patterns) {
        found <- FALSE
        for (value in values) {
          captures <- extract_named_captures(pattern, value)
          if (length(captures)) {
            for (nm in names(captures)) {
              extracted[[nm]] <- captures[[nm]]
            }
            found <- TRUE
            break
          }
        }
        if (found) {
          next
        }
      }
    }
  }

  requested_keys <- sub("-.*$", "", custom_entities)
  entities <- character()

  if (length(custom_entities) && !isTRUE(config$auto_extract_entities)) {
    entities <- custom_entities
  } else if (length(custom_entities)) {
    keep <- intersect(names(extracted), custom_entities)
    complete <- custom_entities[grepl("-", custom_entities, fixed = TRUE)]
    entities <- unique(c(keep, complete))
  }

  if (isTRUE(config$auto_extract_entities)) {
    auto_key <- paste(desc$datatype, desc$suffix, sep = "_")
    wanted <- AUTO_ENTITIES[[auto_key]] %||% character()
    found <- intersect(names(extracted), wanted)
    missing <- setdiff(wanted, c(found, requested_keys))
    if (length(missing)) {
      emit_structured_event(
        code = "W_AUTO_ENTITY_MISSING",
        message = paste0(
          "Entities ", paste(missing, collapse = ", "),
          " were not found for datatype ", desc$datatype,
          " suffix ", desc$suffix, "."
        ),
        warning_sink = warning_sink,
        context = list(
          datatype = desc$datatype,
          suffix = desc$suffix,
          missing = missing
        )
      )
    }
    entities <- unique(c(entities, found))
  }

  task_name <- NULL
  if (isTRUE(config$auto_extract_entities)) {
    replaced <- normalize_custom_entities(entities)
  } else {
    replaced <- normalize_custom_entities(custom_entities)
  }

  for (entity in entities) {
    if (is.null(extracted[[entity]])) {
      next
    }

    value <- extracted[[entity]]
    if (identical(entity, "dir")) {
      value <- ENTITY_DIR_MAP[[value]] %||% value
    }
    if (identical(entity, "task")) {
      task_name <- value
    }

    replaced <- vapply(
      replaced,
      function(x) {
        if (identical(x, entity)) {
          paste(entity, value, sep = "-")
        } else {
          x
        }
      },
      character(1)
    )
  }

  cleaned <- replaced[grepl("-", replaced, fixed = TRUE)]

  list(
    custom_entities = cleaned,
    task_name = task_name
  )
}

#' Build a conversion plan
#'
#' @param config Parsed config list
#' @param input_dir Directory containing dcm2niix sidecars
#' @param participant_label Participant ID (with or without sub- prefix)
#' @param session_label Session ID (with or without ses- prefix)
#' @param search_method Matching method: fnmatch or re
#' @param case_sensitive Whether matching should be case sensitive
#' @param do_not_reorder_entities Keep entity order as given
#' @param dup_method Duplicate strategy: run or dup
#' @return Data frame plan
#' @export
plan_conversion <- function(
    config,
    input_dir,
    participant_label,
    session_label = "",
    search_method = config$search_method %||% "fnmatch",
    case_sensitive = config$case_sensitive %||% TRUE,
    auto_extract_entities = config$auto_extract_entities %||% FALSE,
    do_not_reorder_entities = config$do_not_reorder_entities %||% FALSE,
    dup_method = config$dup_method %||% "run",
    warning_sink = NULL
) {
  warning_events <- list()
  collect_warning <- function(code, message, severity = "warning", context = list()) {
    warning_events[[length(warning_events) + 1L]] <<- list(
      code = code,
      severity = severity,
      message = message,
      context = context
    )
    emit_structured_event(
      code = code,
      message = message,
      warning_sink = warning_sink,
      severity = severity,
      context = context
    )
  }

  validate_config(config, warning_sink = collect_warning)
  config <- config
  config$auto_extract_entities <- isTRUE(auto_extract_entities)

  if (!search_method %in% ALLOWED_SEARCH_METHODS) {
    if (!identical(search_method, config$search_method %||% NULL)) {
      collect_warning(
        code = "W_UNSUPPORTED_SEARCH_METHOD",
        message = paste0(
          "search_method ", search_method,
          " is unsupported; falling back to ", ALLOWED_SEARCH_METHODS[[1]], "."
        ),
        context = list(value = search_method)
      )
    }
    search_method <- ALLOWED_SEARCH_METHODS[[1]]
  }
  if (!isTRUE(case_sensitive %in% c(TRUE, FALSE))) {
    if (!identical(case_sensitive, config$case_sensitive %||% NULL)) {
      collect_warning(
        code = "W_CASE_SENSITIVE_INVALID",
        message = "case_sensitive is not boolean; falling back to TRUE.",
        context = list(value = case_sensitive)
      )
    }
    case_sensitive <- TRUE
  }
  if (!dup_method %in% ALLOWED_DUP_METHODS) {
    if (!identical(dup_method, config$dup_method %||% NULL)) {
      collect_warning(
        code = "W_UNSUPPORTED_DUP_METHOD",
        message = paste0(
          "dup_method ", dup_method,
          " is unsupported; falling back to ", ALLOWED_DUP_METHODS[[1]], "."
        ),
        context = list(value = dup_method)
      )
    }
    dup_method <- ALLOWED_DUP_METHODS[[1]]
  }

  participant_label <- normalize_participant_label(participant_label)
  session_label <- normalize_session_label(session_label)

  sidecars <- collect_sidecars(input_dir)
  descriptions <- config$descriptions

  if (!length(sidecars)) {
    collect_warning(
      code = "W_NO_JSON_SIDECARS",
      message = paste0("No JSON sidecars found in ", input_dir, "."),
      context = list(input_dir = input_dir)
    )
  }

  rows <- vector("list", length(sidecars))

  for (i in seq_along(sidecars)) {
    sidecar <- sidecars[[i]]

    matches <- which(vapply(
      descriptions,
      function(desc) criteria_matches(
        sidecar$data,
        desc$criteria,
        search_method = search_method,
        case_sensitive = case_sensitive
      ),
      logical(1)
    ))

    if (length(matches) == 0L) {
      rows[[i]] <- data.frame(
        source_json = sidecar$path,
        source_root = sidecar$root,
        sort_index = i,
        status = "SKIPPED",
        reason = "No matching description",
        participant_label = participant_label,
        session_label = session_label,
        datatype = NA_character_,
        suffix = NA_character_,
        destination_dir = NA_character_,
        destination_stem = NA_character_,
        id = NA_character_,
        task_name = NA_character_,
        do_not_reorder_entities = do_not_reorder_entities,
        stringsAsFactors = FALSE
      )
      rows[[i]]$custom_entities <- I(list(character()))
      rows[[i]]$sidecar_changes <- I(list(list()))
      next
    }

    if (length(matches) > 1L) {
      choices <- vapply(matches, function(idx) descriptions[[idx]]$suffix %||% "", character(1))
      rows[[i]] <- data.frame(
        source_json = sidecar$path,
        source_root = sidecar$root,
        sort_index = i,
        status = "AMBIGUOUS",
        reason = paste("Multiple matches:", paste(choices, collapse = ", ")),
        participant_label = participant_label,
        session_label = session_label,
        datatype = NA_character_,
        suffix = NA_character_,
        destination_dir = NA_character_,
        destination_stem = NA_character_,
        id = NA_character_,
        task_name = NA_character_,
        do_not_reorder_entities = do_not_reorder_entities,
        stringsAsFactors = FALSE
      )
      rows[[i]]$custom_entities <- I(list(character()))
      rows[[i]]$sidecar_changes <- I(list(list()))
      next
    }

    desc <- descriptions[[matches[[1]]]]
    extracted <- extract_entities(sidecar$data, desc, config, warning_sink = collect_warning)

    custom_entities <- extracted$custom_entities
    stem <- build_bids_stem(
      participant_label = participant_label,
      session_label = session_label,
      custom_entities = custom_entities,
      suffix = desc$suffix,
      do_not_reorder_entities = do_not_reorder_entities
    )

    rows[[i]] <- data.frame(
      source_json = sidecar$path,
      source_root = sidecar$root,
      sort_index = i,
      status = "MATCHED",
      reason = "",
      participant_label = participant_label,
      session_label = session_label,
      datatype = as.character(desc$datatype),
      suffix = as.character(desc$suffix),
      destination_dir = file.path(
        participant_directory(participant_label, session_label),
        as.character(desc$datatype)
      ),
      destination_stem = stem,
      id = as.character(desc$id %||% NA_character_),
      task_name = as.character(extracted$task_name %||% NA_character_),
      do_not_reorder_entities = do_not_reorder_entities,
      stringsAsFactors = FALSE
    )
    rows[[i]]$custom_entities <- I(list(custom_entities))
    rows[[i]]$sidecar_changes <- I(list(desc$sidecar_changes %||% list()))
  }

  plan <- do.call(rbind, rows)
  plan$destination_stem_original <- plan$destination_stem
  plan <- apply_duplicate_entities(plan, dup_method = dup_method)
  rownames(plan) <- NULL
  attr(plan, "warnings") <- warning_events
  plan
}

format_dry_run_table <- function(plan, bids_dir = NULL) {
  out <- data.frame(
    source = basename(plan$source_json),
    destination = rep("(none)", nrow(plan)),
    status = plan$status,
    stringsAsFactors = FALSE
  )

  matched <- which(plan$status == "MATCHED")
  if (length(matched)) {
    dest <- file.path(plan$destination_dir[matched], paste0(plan$destination_stem[matched], ".json"))
    if (!is.null(bids_dir)) {
      dest <- file.path(bids_dir, dest)
    }
    out$destination[matched] <- dest
  }

  out
}

#' Print dry-run plan
#'
#' @param plan Plan from [plan_conversion()]
#' @param bids_dir Optional destination root for rendering destination paths
#' @return Invisibly returns table data frame
#' @export
print_dry_run_plan <- function(plan, bids_dir = NULL) {
  tbl <- format_dry_run_table(plan, bids_dir = bids_dir)

  cli::cli_h1("dcmtobids dry-run")
  for (i in seq_len(nrow(tbl))) {
    cli::cli_text(
      "{.file {tbl$source[[i]]}} -> {.file {tbl$destination[[i]]}} [{.val {tbl$status[[i]]}}]"
    )
  }

  counts <- table(plan$status)
  cli::cli_rule()
  for (name in names(counts)) {
    cli::cli_text("{name}: {counts[[name]]}")
  }

  invisible(tbl)
}
