default_inspect_fields <- function() {
  c(
    "SeriesDescription",
    "ProtocolName",
    "SeriesNumber",
    "AcquisitionTime",
    "Modality",
    "ImageType",
    "PhaseEncodingDirection",
    "EchoNumber",
    "EchoTime",
    "RepetitionTime"
  )
}

collapse_sidecar_value <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  if (!length(x)) {
    return(NA_character_)
  }
  x <- as.character(x)
  x <- x[nzchar(x)]
  if (!length(x)) {
    return(NA_character_)
  }
  if (length(x) == 1L) {
    return(x[[1]])
  }
  paste(x, collapse = "|")
}

#' Inspect helper sidecars and return an inventory table
#'
#' @param input_dir Directory containing sidecar JSON files
#' @param fields Sidecar keys to include as columns
#' @return Data frame inventory
#' @export
inspect_sidecars <- function(input_dir, fields = default_inspect_fields()) {
  fields <- unique(as.character(fields))
  sidecars <- collect_sidecars(input_dir)

  if (!length(sidecars)) {
    out <- data.frame(
      source_json = character(),
      source_root = character(),
      sort_index = integer(),
      stringsAsFactors = FALSE
    )
    for (f in fields) {
      out[[f]] <- character()
    }
    return(out)
  }

  rows <- vector("list", length(sidecars))
  for (i in seq_along(sidecars)) {
    sc <- sidecars[[i]]
    row <- data.frame(
      source_json = sc$path,
      source_root = sc$root,
      sort_index = i,
      stringsAsFactors = FALSE
    )
    for (f in fields) {
      row[[f]] <- collapse_sidecar_value(sc$data[[f]])
    }
    rows[[i]] <- row
  }

  do.call(rbind, rows)
}

print_inspect_summary <- function(inventory, fields, max_unique = 20L) {
  cli::cli_alert_info("Sidecars discovered: {nrow(inventory)}")
  if (!nrow(inventory)) {
    return(invisible(NULL))
  }

  fields <- fields[fields %in% names(inventory)]
  for (field in fields) {
    vals <- inventory[[field]]
    vals <- vals[!is.na(vals) & nzchar(vals)]
    cli::cli_rule(left = field)
    if (!length(vals)) {
      cli::cli_text("No non-empty values")
      next
    }
    tab <- sort(table(vals), decreasing = TRUE)
    top <- head(tab, max_unique)
    for (i in seq_along(top)) {
      cli::cli_text("{names(top)[[i]]} ({as.integer(top[[i]])})")
    }
    if (length(tab) > length(top)) {
      cli::cli_text("... and {length(tab) - length(top)} more")
    }
  }
  invisible(NULL)
}

write_sidecar_inventory <- function(inventory, path) {
  path <- fs::path_abs(path)
  fs::dir_create(dirname(path), recurse = TRUE)
  utils::write.table(
    inventory,
    file = path,
    sep = "\t",
    quote = TRUE,
    row.names = FALSE,
    na = ""
  )
  invisible(path)
}
