#' Read a dcmtobids config JSON
#'
#' @param config_path Path to JSON config
#' @return Parsed config list
#' @export
read_config <- function(config_path) {
  config_path <- fs::path_abs(config_path)
  if (!fs::file_exists(config_path)) {
    cli::cli_abort("Config file not found: {.file {config_path}}")
  }
  config <- read_json_file(config_path)
  validate_config(config)
  config
}

#' Validate a dcmtobids config object
#'
#' @param config Parsed config list
#' @return Invisibly TRUE on success
#' @export
validate_config <- function(config) {
  if (!is_named_list(config)) {
    cli::cli_abort("Config must be a JSON object.")
  }

  search_method <- config$search_method %||% "fnmatch"
  if (!search_method %in% ALLOWED_SEARCH_METHODS) {
    cli::cli_alert_warning(
      "search_method {.val {search_method}} is unsupported; falling back to {.val {ALLOWED_SEARCH_METHODS[[1]]}}."
    )
  }

  if (!is.null(config$dup_method) && !config$dup_method %in% ALLOWED_DUP_METHODS) {
    cli::cli_alert_warning("dup_method {.val {config$dup_method}} is unsupported; falling back to {.val {ALLOWED_DUP_METHODS[[1]]}}.")
  }

  if (!is.null(config$bids_uri) && !config$bids_uri %in% ALLOWED_BIDS_URI) {
    cli::cli_abort("bids_uri must be one of {.val {ALLOWED_BIDS_URI}}.")
  }

  if (!is.null(config$case_sensitive) && !isTRUE(config$case_sensitive %in% c(TRUE, FALSE))) {
    cli::cli_alert_warning("case_sensitive is not boolean; falling back to TRUE.")
  }

  if (!is.null(config$do_not_reorder_entities) &&
      !isTRUE(config$do_not_reorder_entities %in% c(TRUE, FALSE))) {
    cli::cli_abort("do_not_reorder_entities must be true or false.")
  }

  descriptions <- config$descriptions
  if (!is.list(descriptions) || length(descriptions) == 0L) {
    cli::cli_abort("Config must contain a non-empty {.field descriptions} array.")
  }

  ids <- character()
  serialized <- character(length(descriptions))

  for (i in seq_along(descriptions)) {
    desc <- descriptions[[i]]
    label <- paste0("descriptions[", i, "]")

    if (!is_named_list(desc)) {
      cli::cli_abort("{.field {label}} must be an object.")
    }

    for (required in c("datatype", "suffix", "criteria")) {
      if (is.null(desc[[required]]) || (is.character(desc[[required]]) && trimws(desc[[required]]) == "")) {
        cli::cli_abort("{.field {label}.{required}} is required.")
      }
    }

    if (!is_named_list(desc$criteria)) {
      cli::cli_abort("{.field {label}.criteria} must be an object.")
    }

    if (!is.null(desc$custom_entities)) {
      if (!(is.character(desc$custom_entities) || is.list(desc$custom_entities))) {
        cli::cli_abort("{.field {label}.custom_entities} must be a string or array.")
      }
    }

    if (!is.null(desc$sidecar_changes) && !is_named_list(desc$sidecar_changes)) {
      cli::cli_abort("{.field {label}.sidecar_changes} must be an object.")
    }

    if (!is.null(desc$id)) {
      if (!is.character(desc$id) || length(desc$id) != 1L || trimws(desc$id) == "") {
        cli::cli_abort("{.field {label}.id} must be a non-empty string.")
      }
      ids <- c(ids, desc$id)
    }

    serialized[[i]] <- jsonlite::toJSON(desc$criteria, auto_unbox = TRUE, null = "null")
  }

  dup_ids <- unique(ids[duplicated(ids)])
  if (length(dup_ids)) {
    cli::cli_alert_warning("Duplicate IDs detected: {.val {dup_ids}}")
  }

  for (i in seq_along(descriptions)) {
    for (j in seq_len(i - 1L)) {
      di <- descriptions[[i]]
      dj <- descriptions[[j]]
      if (identical(di$datatype, dj$datatype) &&
          identical(di$suffix, dj$suffix) &&
          identical(serialized[[i]], serialized[[j]])) {
        cli::cli_alert_warning(
          "Potential overlapping criteria between descriptions[{j}] and descriptions[{i}]."
        )
      }
    }
  }

  invisible(TRUE)
}
