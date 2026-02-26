count_named <- function(x) {
  tb <- table(x)
  out <- as.list(as.integer(tb))
  names(out) <- names(tb)
  out
}

aggregate_warning_codes <- function(events) {
  if (!length(events)) {
    return(list())
  }

  by_code <- split(events, vapply(events, function(e) as.character(e$code %||% "UNKNOWN"), character(1)))
  out <- list()
  for (code in names(by_code)) {
    evs <- by_code[[code]]
    severities <- unique(vapply(evs, function(e) as.character(e$severity %||% "warning"), character(1)))
    severity <- if ("error" %in% severities) "error" else if ("warning" %in% severities) "warning" else severities[[1]]
    out[[length(out) + 1L]] <- list(
      code = code,
      severity = severity,
      count = length(evs)
    )
  }
  out
}

#' Build structured summary report from a plan
#'
#' @param plan Plan from [plan_conversion()]
#' @param conversion Optional conversion summary from [convert_plan()]
#' @return Named list report
#' @export
summarize_plan <- function(plan, conversion = NULL, helper = NULL) {
  matched <- plan[plan$status == "MATCHED", , drop = FALSE]
  duplicates <- list()
  if (nrow(matched)) {
    stems <- split(seq_len(nrow(matched)), matched$destination_stem)
    stems <- stems[vapply(stems, length, integer(1)) > 1L]
    duplicates <- lapply(stems, function(idx) {
      as.character(matched$source_json[idx])
    })
  }

  warning_events <- list()

  add_event <- function(code, message, severity = "warning", context = list()) {
    warning_events[[length(warning_events) + 1L]] <<- list(
      code = code,
      severity = severity,
      message = message,
      context = context
    )
  }

  n_skipped <- sum(plan$status == "SKIPPED", na.rm = TRUE)
  if (n_skipped > 0L) {
    add_event(
      code = "W_NO_MATCH",
      message = paste0(n_skipped, " source files had no matching description."),
      severity = "warning",
      context = list(count = n_skipped)
    )
  }

  n_ambiguous <- sum(plan$status == "AMBIGUOUS", na.rm = TRUE)
  if (n_ambiguous > 0L) {
    add_event(
      code = "W_AMBIGUOUS_MATCH",
      message = paste0(n_ambiguous, " source files matched multiple descriptions."),
      severity = "warning",
      context = list(count = n_ambiguous)
    )
  }

  if (length(duplicates) > 0L) {
    add_event(
      code = "W_DUPLICATE_DESTINATION",
      message = "Multiple source files map to at least one destination stem.",
      severity = "warning",
      context = list(count = length(duplicates))
    )
  }

  if (!is.null(conversion)) {
    if (!is.null(conversion$warnings) && length(conversion$warnings)) {
      warning_events <- c(warning_events, conversion$warnings)
    }
    if (!is.null(conversion$errors) && conversion$errors > 0L) {
      add_event(
        code = "E_CONVERSION_ERRORS",
        message = paste0(conversion$errors, " file conversion errors were encountered."),
        severity = "error",
        context = list(count = conversion$errors)
      )
    }
  }

  if (!is.null(helper)) {
    if (!is.null(helper$warnings) && length(helper$warnings)) {
      warning_events <- c(warning_events, helper$warnings)
    }
  }

  qa_codes <- aggregate_warning_codes(warning_events)
  qa_gate <- if (any(vapply(qa_codes, function(x) identical(x$severity, "error"), logical(1)))) {
    "fail"
  } else if (length(qa_codes) > 0L) {
    "warn"
  } else {
    "pass"
  }

  report <- list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    counts = count_named(plan$status),
    datatype_counts = if (nrow(matched)) count_named(matched$datatype) else list(),
    unmatched = as.character(plan$source_json[plan$status == "SKIPPED"]),
    ambiguous = as.character(plan$source_json[plan$status == "AMBIGUOUS"]),
    duplicate_destinations = duplicates,
    warnings = warning_events,
    qa = list(
      gate = qa_gate,
      codes = qa_codes
    )
  )

  if (!is.null(conversion)) {
    report$conversion <- conversion
  }
  if (!is.null(helper)) {
    report$helper <- helper
  }

  report
}

#' Write conversion report as JSON
#'
#' @param report Report object from [summarize_plan()]
#' @param path Output JSON path
#' @return Invisibly returns output path
#' @export
write_conversion_report <- function(report, path) {
  path <- fs::path_abs(path)
  write_json_file(path, report)
  invisible(path)
}

#' Build structured summary report from an IntendedFor population run
#'
#' @param population Summary from [populate_intended_for_plan()]
#' @return Named list report
#' @export
summarize_population <- function(population) {
  warning_events <- population$warnings %||% list()
  if (!is.null(population$errors) && population$errors > 0L) {
    warning_events <- c(
      warning_events,
      list(list(
        code = "E_POPULATE_ERRORS",
        severity = "error",
        message = paste0(population$errors, " populate-intended-for errors were encountered."),
        context = list(count = population$errors)
      ))
    )
  }

  qa_codes <- aggregate_warning_codes(warning_events)
  qa_gate <- if (any(vapply(qa_codes, function(x) identical(x$severity, "error"), logical(1)))) {
    "fail"
  } else if (length(qa_codes) > 0L) {
    "warn"
  } else {
    "pass"
  }

  list(
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    action = "populate-intended-for",
    counts = list(
      updated = population$updated %||% 0L,
      skipped = population$skipped %||% 0L,
      errors = population$errors %||% 0L
    ),
    warnings = warning_events,
    qa = list(
      gate = qa_gate,
      codes = qa_codes
    )
  )
}
