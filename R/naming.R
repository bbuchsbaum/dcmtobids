is_entity_token <- function(token) {
  parts <- strsplit(token, "-", fixed = TRUE)[[1]]
  length(parts) == 2L
}

reorder_bids_tokens <- function(tokens, do_not_reorder_entities = FALSE) {
  if (isTRUE(do_not_reorder_entities)) {
    return(tokens)
  }

  entity_tokens <- tokens[vapply(tokens, is_entity_token, logical(1))]
  singleton_tokens <- tokens[!vapply(tokens, is_entity_token, logical(1))]

  if (!length(entity_tokens)) {
    return(tokens)
  }

  entity_keys <- sub("-.*$", "", entity_tokens)
  entity_values <- sub("^[^-]+-", "", entity_tokens)

  current <- stats::setNames(as.list(entity_values), entity_keys)

  out <- character()

  for (key in BIDS_ENTITY_ORDER) {
    if (!is.null(current[[key]])) {
      out <- c(out, paste0(key, "-", current[[key]]))
      current[[key]] <- NULL
    }
  }

  if (length(current)) {
    for (key in names(current)) {
      out <- c(out, paste0(key, "-", current[[key]]))
    }
  }

  c(out, singleton_tokens)
}

build_bids_stem <- function(
    participant_label,
    session_label = "",
    custom_entities = character(),
    suffix,
    do_not_reorder_entities = FALSE
) {
  prefix <- participant_prefix(participant_label, session_label)
  tokens <- c(
    unlist(strsplit(prefix, "_", fixed = TRUE), use.names = FALSE),
    normalize_custom_entities(custom_entities),
    as.character(suffix)
  )

  tokens <- tokens[nzchar(tokens)]
  if (!length(tokens)) {
    cli::cli_abort("Cannot build BIDS stem from empty tokens.")
  }

  reordered <- reorder_bids_tokens(tokens, do_not_reorder_entities)
  paste(reordered, collapse = "_")
}

apply_duplicate_entities <- function(plan, dup_method = "run") {
  matched <- which(plan$status == "MATCHED")
  if (!length(matched)) {
    return(plan)
  }

  method <- if (identical(dup_method, "dup")) "dup" else "run"
  groups <- split(matched, plan$destination_stem[matched])

  for (grp in groups) {
    if (length(grp) <= 1L) {
      next
    }

    if ("sort_index" %in% names(plan)) {
      grp <- grp[order(plan$sort_index[grp])]
    } else {
      grp <- grp[order(plan$source_json[grp])]
    }
    assign_idx <- seq_along(grp)
    if (identical(method, "dup")) {
      assign_idx <- assign_idx[-length(assign_idx)]
    }

    for (i in assign_idx) {
      idx <- grp[[i]]
      ents <- normalize_custom_entities(plan$custom_entities[[idx]])
      ents <- c(ents, sprintf("%s-%02d", method, i))
      plan$custom_entities[[idx]] <- ents
      plan$destination_stem[[idx]] <- build_bids_stem(
        participant_label = plan$participant_label[[idx]],
        session_label = plan$session_label[[idx]],
        custom_entities = ents,
        suffix = plan$suffix[[idx]],
        do_not_reorder_entities = plan$do_not_reorder_entities[[idx]]
      )
    }
  }

  plan
}
