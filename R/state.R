new_run_id <- function(command) {
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  nonce <- paste(sample(c(letters, 0:9), size = 6L, replace = TRUE), collapse = "")
  paste0(stamp, "-", command, "-", nonce)
}

plan_for_tsv <- function(plan) {
  out <- plan
  for (nm in names(out)) {
    col <- out[[nm]]
    if (is.list(col)) {
      out[[nm]] <- vapply(
        col,
        function(x) {
          if (is.null(x)) {
            return("")
          }
          jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
        },
        character(1)
      )
    }
  }
  out
}

record_cli_run <- function(
    command,
    bids_dir,
    argv,
    status = 0L,
    error_message = "",
    config_path = NULL,
    plan = NULL,
    report = NULL,
    helper = NULL,
    conversion = NULL,
    populate = NULL
) {
  if (is.null(bids_dir) || !nzchar(bids_dir)) {
    return(invisible(NULL))
  }

  tryCatch({
    bids_dir <- fs::path_abs(bids_dir)
    run_id <- new_run_id(command)
    run_dir <- file.path(bids_dir, ".dcmtobids", "runs", run_id)
    fs::dir_create(run_dir, recurse = TRUE)

    if (!is.null(config_path) && nzchar(config_path) && fs::file_exists(config_path)) {
      fs::file_copy(fs::path_abs(config_path), file.path(run_dir, "config.json"), overwrite = TRUE)
    }

    if (!is.null(plan)) {
      plan_tsv <- plan_for_tsv(plan)
      utils::write.table(
        plan_tsv,
        file = file.path(run_dir, "plan.tsv"),
        sep = "\t",
        quote = TRUE,
        row.names = FALSE,
        na = ""
      )
    }

    if (!is.null(report)) {
      write_json_file(file.path(run_dir, "report.json"), report)
    }
    if (!is.null(helper)) {
      write_json_file(file.path(run_dir, "helper.json"), helper)
    }
    if (!is.null(conversion)) {
      write_json_file(file.path(run_dir, "conversion.json"), conversion)
    }
    if (!is.null(populate)) {
      write_json_file(file.path(run_dir, "populate.json"), populate)
    }

    meta <- list(
      run_id = run_id,
      command = command,
      argv = argv,
      status = as.integer(status),
      error_message = error_message %||% "",
      generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      package_version = as.character(utils::packageVersion("dcmtobids")),
      bids_dir = bids_dir
    )
    write_json_file(file.path(run_dir, "meta.json"), meta)
    invisible(run_dir)
  }, error = function(e) {
    cli::cli_alert_warning("Failed to record run manifest: {conditionMessage(e)}")
    invisible(NULL)
  })
}
