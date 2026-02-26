build_destination_id_map <- function(plan, bids_dir) {
  out <- list()
  matched <- which(plan$status == "MATCHED" & !is.na(plan$id) & nzchar(plan$id))
  bids_dir <- fs::path_abs(bids_dir)

  for (idx in matched) {
    rel_nii_gz <- file.path(plan$destination_dir[[idx]], paste0(plan$destination_stem[[idx]], ".nii.gz"))
    rel_nii <- file.path(plan$destination_dir[[idx]], paste0(plan$destination_stem[[idx]], ".nii"))

    rel <- if (fs::file_exists(file.path(bids_dir, rel_nii_gz))) {
      rel_nii_gz
    } else if (fs::file_exists(file.path(bids_dir, rel_nii))) {
      rel_nii
    } else {
      NA_character_
    }

    if (is.na(rel)) {
      next
    }
    id <- plan$id[[idx]]
    out[[id]] <- unique(c(out[[id]], rel))
  }

  out
}

filter_path_sidecar_changes <- function(sidecar_changes) {
  if (!is.list(sidecar_changes) || !length(sidecar_changes)) {
    return(list())
  }
  keep <- names(sidecar_changes) %in% PATH_SIDECAR_CHANGE_KEYS
  sidecar_changes[keep]
}

#' Populate IntendedFor/Sources in existing BIDS sidecars from a plan
#'
#' @param plan Plan from [plan_conversion()]
#' @param bids_dir Existing BIDS directory
#' @param bids_uri IntendedFor path style: URI or relative
#' @return Summary list with updated/skipped/errors/warnings
#' @export
populate_intended_for_plan <- function(plan, bids_dir, bids_uri = "URI") {
  bids_dir <- fs::path_abs(bids_dir)
  updated <- 0L
  skipped <- 0L
  errors <- 0L
  warning_events <- list()
  id_map <- build_destination_id_map(plan, bids_dir)

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

    patch_changes <- filter_path_sidecar_changes(plan$sidecar_changes[[i]])
    if (!length(patch_changes)) {
      next
    }

    dst_json_rel <- file.path(plan$destination_dir[[i]], paste0(plan$destination_stem[[i]], ".json"))
    dst_json <- file.path(bids_dir, dst_json_rel)
    if (!fs::file_exists(dst_json)) {
      emit_warning(
        code = "W_TARGET_JSON_MISSING",
        message = paste0("Target JSON missing for populate-intended-for: ", dst_json_rel),
        severity = "warning",
        context = list(path = dst_json_rel)
      )
      skipped <- skipped + 1L
      next
    }

    ok <- tryCatch({
      data <- read_json_file(dst_json)
      updated_data <- apply_sidecar_changes(
        data = data,
        sidecar_changes = patch_changes,
        id_map = id_map,
        participant_label = plan$participant_label[[i]],
        bids_uri = bids_uri,
        warning_sink = emit_warning
      )
      if (!identical(data, updated_data)) {
        write_json_file(dst_json, updated_data)
        TRUE
      } else {
        FALSE
      }
    }, error = function(e) {
      emit_warning(
        code = "E_POPULATE_FAILED",
        message = paste0("Failed to update ", dst_json_rel, ": ", conditionMessage(e)),
        severity = "error",
        context = list(path = dst_json_rel)
      )
      NA
    })

    if (isTRUE(ok)) {
      updated <- updated + 1L
    } else if (identical(ok, FALSE)) {
      skipped <- skipped + 1L
    } else {
      errors <- errors + 1L
    }
  }

  list(
    updated = updated,
    skipped = skipped,
    errors = errors,
    warnings = warning_events
  )
}
