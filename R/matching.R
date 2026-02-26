compare_scalar <- function(name, pattern, search_method = "fnmatch", case_sensitive = TRUE) {
  name <- as.character(name)
  pattern <- as.character(pattern)

  if (search_method == "re") {
    return(grepl(pattern, name, perl = TRUE, ignore.case = !isTRUE(case_sensitive)))
  }

  if (!case_sensitive) {
    name <- tolower(name)
    pattern <- tolower(pattern)
  }

  rx <- utils::glob2rx(pattern)
  grepl(rx, name, perl = TRUE)
}

compare_list <- function(name, pattern, search_method = "fnmatch", case_sensitive = TRUE) {
  name <- unlist(name, recursive = TRUE, use.names = FALSE)
  pattern <- unlist(pattern, recursive = TRUE, use.names = FALSE)

  if (length(name) != length(pattern)) {
    return(FALSE)
  }

  all(vapply(
    seq_along(name),
    function(i) compare_scalar(name[[i]], pattern[[i]], search_method, case_sensitive),
    logical(1)
  ))
}

compare_complex <- function(name, pattern, search_method = "fnmatch", case_sensitive = TRUE) {
  mode <- names(pattern)[1]
  patterns <- pattern[[1]]

  if (!is.list(patterns)) {
    patterns <- as.list(patterns)
  }

  results <- vapply(
    patterns,
    function(pat) {
      if (is.list(name) || (is.atomic(name) && length(name) > 1L)) {
        compare_list(name, pat, search_method, case_sensitive)
      } else {
        compare_scalar(name, pat, search_method, case_sensitive)
      }
    },
    logical(1)
  )

  if (identical(mode, "any")) {
    any(results)
  } else {
    FALSE
  }
}

compare_float <- function(name, pattern) {
  if (!is_named_list(pattern) || length(pattern) != 1L) {
    return(FALSE)
  }

  key <- names(pattern)[1]
  value <- pattern[[1]]

  name_float <- suppressWarnings(as.numeric(name))
  if (is.na(name_float)) {
    return(FALSE)
  }

  if (key %in% c("btw", "btwe")) {
    vals <- suppressWarnings(as.numeric(unlist(value, recursive = TRUE, use.names = FALSE)))
    if (length(vals) != 2L || anyNA(vals)) {
      return(FALSE)
    }
    if (key == "btw") {
      return(name_float > vals[[1]] && name_float < vals[[2]])
    }
    return(name_float >= vals[[1]] && name_float <= vals[[2]])
  }

  vals <- suppressWarnings(as.numeric(unlist(value, recursive = TRUE, use.names = FALSE)))
  if (length(vals) != 1L || is.na(vals)) {
    return(FALSE)
  }

  cmp <- vals[[1]]
  switch(
    key,
    gt = cmp < name_float,
    lt = cmp > name_float,
    ge = cmp <= name_float,
    le = cmp >= name_float,
    FALSE
  )
}

criteria_matches <- function(data, criteria, search_method = "fnmatch", case_sensitive = TRUE) {
  if (!is_named_list(criteria)) {
    return(FALSE)
  }

  out <- logical(length(criteria))
  idx <- 1L

  for (tag in names(criteria)) {
    pattern <- criteria[[tag]]
    name <- data[[tag]] %||% ""

    if (is_named_list(pattern)) {
      if (length(pattern) != 1L) {
        stop("Criteria dictionary must use a single key.")
      }
      key <- names(pattern)[1]
      if (identical(key, "any")) {
        out[[idx]] <- compare_complex(name, pattern, search_method, case_sensitive)
      } else if (key %in% c("lt", "gt", "le", "ge", "btw", "btwe")) {
        out[[idx]] <- compare_float(name, pattern)
      } else {
        out[[idx]] <- FALSE
      }
    } else if (is.list(name) || (is.atomic(name) && length(name) > 1L)) {
      out[[idx]] <- compare_list(name, pattern, search_method, case_sensitive)
    } else {
      if (is_empty_value(name) && is_empty_value(pattern)) {
        out[[idx]] <- TRUE
      } else if (!is_empty_value(name) && !is_empty_value(pattern)) {
        out[[idx]] <- compare_scalar(name, pattern, search_method, case_sensitive)
      } else {
        out[[idx]] <- FALSE
      }
    }

    idx <- idx + 1L
  }

  all(out)
}
