`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

is_named_list <- function(x) {
  is.list(x) && !is.null(names(x)) && all(nzchar(names(x)))
}

is_empty_value <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (length(x) == 0) {
    return(TRUE)
  }
  if (is.character(x) && length(x) == 1 && trimws(x) == "") {
    return(TRUE)
  }
  FALSE
}

normalize_participant_label <- function(label) {
  if (!is.character(label) || length(label) != 1L || trimws(label) == "") {
    cli::cli_abort("Participant label must be a non-empty string.")
  }
  label <- trimws(label)
  if (!startsWith(label, "sub-")) {
    label <- paste0("sub-", label)
  }
  bare <- sub("^sub-", "", label)
  if (!grepl("^[A-Za-z0-9]+$", bare)) {
    cli::cli_abort("Participant label {.val {bare}} must be alphanumeric.")
  }
  label
}

normalize_session_label <- function(label) {
  if (is.null(label) || (is.character(label) && length(label) == 1L && trimws(label) == "")) {
    return("")
  }
  if (!is.character(label) || length(label) != 1L) {
    cli::cli_abort("Session label must be a single string.")
  }
  label <- trimws(label)
  if (!startsWith(label, "ses-")) {
    label <- paste0("ses-", label)
  }
  bare <- sub("^ses-", "", label)
  if (!grepl("^[A-Za-z0-9]+$", bare)) {
    cli::cli_abort("Session label {.val {bare}} must be alphanumeric.")
  }
  label
}

participant_prefix <- function(participant_label, session_label = "") {
  if (session_label == "") {
    participant_label
  } else {
    paste(participant_label, session_label, sep = "_")
  }
}

participant_directory <- function(participant_label, session_label = "") {
  if (session_label == "") {
    participant_label
  } else {
    file.path(participant_label, session_label)
  }
}

normalize_custom_entities <- function(x) {
  if (is.null(x) || (is.character(x) && length(x) == 1L && trimws(x) == "")) {
    return(character())
  }

  out <- x
  if (is.list(out)) {
    out <- unlist(out, recursive = TRUE, use.names = FALSE)
  }

  out <- as.character(out)
  out <- trimws(out)
  out <- out[nzchar(out)]
  unique(out)
}

read_json_file <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

write_json_file <- function(path, data) {
  fs::dir_create(dirname(path), recurse = TRUE)
  txt <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(txt, path, useBytes = TRUE)
  invisible(path)
}

sidecar_root_from_json <- function(path) {
  sub("\\.json$", "", path, ignore.case = TRUE)
}

infer_file_ext <- function(path) {
  if (grepl("\\.nii\\.gz$", path, ignore.case = TRUE)) {
    return(".nii.gz")
  }
  ext <- tools::file_ext(path)
  if (!nzchar(ext)) {
    return("")
  }
  paste0(".", tolower(ext))
}

safe_move_file <- function(src, dst, clobber = FALSE) {
  fs::dir_create(dirname(dst), recurse = TRUE)

  if (fs::file_exists(dst)) {
    if (!clobber) {
      return(FALSE)
    }
    fs::file_delete(dst)
  }

  moved <- tryCatch({
    fs::file_move(src, dst)
    TRUE
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("cross-device|EXDEV|Invalid cross-device", msg, ignore.case = TRUE)) {
      fs::file_copy(src, dst, overwrite = clobber)
      fs::file_delete(src)
      TRUE
    } else {
      stop(e)
    }
  })

  moved
}

find_associated_files <- function(src_root) {
  paths <- sort(Sys.glob(paste0(src_root, ".*")), decreasing = TRUE)
  keep_ext <- c(".nii", ".nii.gz", ".json", ".bval", ".bvec")
  paths[vapply(paths, function(p) infer_file_ext(p) %in% keep_ext, logical(1))]
}

extract_named_captures <- function(pattern, value) {
  # Accept Python-style named groups (?P<name>...) used by upstream dcm2bids configs.
  pattern <- gsub("\\(\\?P<([A-Za-z][A-Za-z0-9_]*)>", "(?<\\1>", pattern, perl = TRUE)

  m <- regexec(pattern, value, perl = TRUE)
  hit <- regmatches(value, m)[[1]]
  if (length(hit) == 0) {
    return(list())
  }

  out <- list()
  hit_names <- names(hit)
  if (!is.null(hit_names) && any(nzchar(hit_names))) {
    for (i in seq_along(hit)) {
      nm <- hit_names[[i]]
      if (nzchar(nm)) {
        out[[nm]] <- hit[[i]]
      }
    }
    return(out)
  }

  starts <- attr(m[[1]], "capture.start")
  lens <- attr(m[[1]], "capture.length")
  cn <- attr(m[[1]], "capture.names")
  if (is.null(cn) || length(cn) == 0) {
    return(list())
  }

  for (i in seq_along(cn)) {
    if (nzchar(cn[[i]]) && starts[[i]] > 0) {
      out[[cn[[i]]]] <- substr(value, starts[[i]], starts[[i]] + lens[[i]] - 1L)
    }
  }
  out
}

flatten_list <- function(x) {
  out <- list()
  for (item in x) {
    if (is.list(item) && !is_named_list(item)) {
      out <- c(out, flatten_list(item))
    } else {
      out <- c(out, list(item))
    }
  }
  out
}
