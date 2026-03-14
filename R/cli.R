cli_usage <- function() {
  paste(
    "dcmtobids <command> [options]",
    "",
    "Commands:",
    "  init-config       Write a starter config JSON",
    "  doctor            Check runtime readiness",
    "  inspect           Inventory helper sidecars (table + unique values)",
    "  populate-intended-for  Recompute IntendedFor/Sources in BIDS JSONs",
    "  validate-config   Validate a config file",
    "  dry-run           Build and print conversion plan",
    "  convert           Execute conversion plan",
    "  run               Run dcm2niix + convert in one command",
    "  version           Print package version",
    "",
    "Common options for dry-run/convert:",
    "  -c, --config PATH",
    "  -i, --input-dir PATH",
    "  -o, --bids-dir PATH",
    "  -p, --participant LABEL",
    "  -s, --session LABEL",
    "      --report PATH",
    "      --auto-extract-entities",
    "      --do-not-reorder-entities",
    "      --case-insensitive",
    "      --clobber (convert only)",
    "      --fail-on-warning (convert/run)",
    "",
    "Inspect options:",
    "  -i, --input-dir PATH",
    "      --out-tsv PATH",
    "      --unique-fields CSV",
    "      --max-unique N",
    "",
    "Populate options:",
    "  -c, --config PATH",
    "  -i, --input-dir PATH",
    "  -o, --bids-dir PATH",
    "  -p, --participant LABEL",
    "  -s, --session LABEL",
    "      --report PATH",
    "      --bids-uri URI|relative",
    "      --case-insensitive",
    "      --auto-extract-entities",
    "      --do-not-reorder-entities",
    "      --fail-on-warning",
    "",
    "Run-specific options:",
    "  -d, --dicom-dir PATHS      Comma-separated DICOM dirs/archives",
    "      --dcm2niix-bin PATH    dcm2niix binary (default: dcm2niix)",
    "      --dcm2niix-options STR dcm2niix option string",
    "      --skip-dcm2niix        Treat input as precomputed NIfTI/JSON",
    "      --force-dcm2bids       Regenerate helper output",
    sep = "\n"
  )
}

parse_init_opts <- function(args) {
  parser <- optparse::OptionParser(option_list = list(
    optparse::make_option(c("-o", "--output"), type = "character", dest = "output"),
    optparse::make_option(c("-f", "--force"), action = "store_true", default = FALSE, dest = "force")
  ))
  opts <- optparse::parse_args(parser, args = args, positional_arguments = FALSE)
  if (is.null(opts$output) || !nzchar(opts$output)) {
    cli::cli_abort("init-config requires --output.")
  }
  opts
}

parse_doctor_opts <- function(args) {
  parser <- optparse::OptionParser(option_list = list(
    optparse::make_option(c("-c", "--config"), type = "character", default = "", dest = "config"),
    optparse::make_option(c("-o", "--bids-dir"), type = "character", default = "", dest = "bids_dir"),
    optparse::make_option("--dcm2niix-bin", type = "character", default = "dcm2niix", dest = "dcm2niix_bin"),
    optparse::make_option("--skip-dcm2niix", action = "store_true", default = FALSE, dest = "skip_dcm2niix")
  ))
  optparse::parse_args(parser, args = args, positional_arguments = FALSE)
}

parse_validate_opts <- function(args) {
  parser <- optparse::OptionParser(option_list = list(
    optparse::make_option(c("-c", "--config"), type = "character", dest = "config")
  ))
  opts <- optparse::parse_args(parser, args = args, positional_arguments = FALSE)
  if (is.null(opts$config) || !nzchar(opts$config)) {
    cli::cli_abort("validate-config requires --config.")
  }
  opts
}

parse_inspect_opts <- function(args) {
  parser <- optparse::OptionParser(option_list = list(
    optparse::make_option(c("-i", "--input-dir"), type = "character", dest = "input_dir"),
    optparse::make_option("--out-tsv", type = "character", default = "", dest = "out_tsv"),
    optparse::make_option("--unique-fields", type = "character", default = "SeriesDescription,ProtocolName", dest = "unique_fields"),
    optparse::make_option("--max-unique", type = "integer", default = 20L, dest = "max_unique")
  ))
  opts <- optparse::parse_args(parser, args = args, positional_arguments = FALSE)
  if (is.null(opts$input_dir) || !nzchar(opts$input_dir)) {
    cli::cli_abort("inspect requires --input-dir.")
  }
  if (is.null(opts$max_unique) || is.na(opts$max_unique) || opts$max_unique < 1L) {
    cli::cli_abort("--max-unique must be >= 1.")
  }
  opts
}

parse_populate_opts <- function(args) {
  parser <- optparse::OptionParser(option_list = list(
    optparse::make_option(c("-c", "--config"), type = "character", dest = "config"),
    optparse::make_option(c("-i", "--input-dir"), type = "character", dest = "input_dir"),
    optparse::make_option(c("-o", "--bids-dir"), type = "character", dest = "bids_dir"),
    optparse::make_option(c("-p", "--participant"), type = "character", dest = "participant"),
    optparse::make_option(c("-s", "--session"), type = "character", default = "", dest = "session"),
    optparse::make_option(c("-r", "--report"), type = "character", default = "", dest = "report"),
    optparse::make_option("--bids-uri", type = "character", default = "", dest = "bids_uri"),
    optparse::make_option("--auto-extract-entities", action = "store_true", default = FALSE, dest = "auto_extract_entities"),
    optparse::make_option("--do-not-reorder-entities", action = "store_true", default = FALSE, dest = "do_not_reorder_entities"),
    optparse::make_option("--case-insensitive", action = "store_true", default = FALSE, dest = "case_insensitive"),
    optparse::make_option("--fail-on-warning", action = "store_true", default = FALSE, dest = "fail_on_warning")
  ))

  opts <- optparse::parse_args(parser, args = args, positional_arguments = FALSE)
  required <- c("config", "input_dir", "bids_dir", "participant")
  missing <- required[vapply(required, function(k) is.null(opts[[k]]) || !nzchar(opts[[k]]), logical(1))]
  if (length(missing)) {
    cli::cli_abort("Missing required options: {.val {missing}}")
  }
  if (isTRUE(opts$auto_extract_entities) && isTRUE(opts$do_not_reorder_entities)) {
    cli::cli_abort("--auto-extract-entities and --do-not-reorder-entities cannot be used together.")
  }
  if (nzchar(opts$bids_uri) && !opts$bids_uri %in% ALLOWED_BIDS_URI) {
    cli::cli_abort("--bids-uri must be one of {.val {ALLOWED_BIDS_URI}}.")
  }

  opts
}

parse_plan_opts <- function(args, include_clobber = FALSE) {
  option_list <- list(
    optparse::make_option(c("-c", "--config"), type = "character", dest = "config"),
    optparse::make_option(c("-i", "--input-dir"), type = "character", dest = "input_dir"),
    optparse::make_option(c("-o", "--bids-dir"), type = "character", dest = "bids_dir"),
    optparse::make_option(c("-p", "--participant"), type = "character", dest = "participant"),
    optparse::make_option(c("-s", "--session"), type = "character", default = "", dest = "session"),
    optparse::make_option(c("-r", "--report"), type = "character", default = "", dest = "report"),
    optparse::make_option("--auto-extract-entities", action = "store_true", default = FALSE, dest = "auto_extract_entities"),
    optparse::make_option("--do-not-reorder-entities", action = "store_true", default = FALSE, dest = "do_not_reorder_entities"),
    optparse::make_option("--case-insensitive", action = "store_true", default = FALSE, dest = "case_insensitive")
  )

  if (isTRUE(include_clobber)) {
    option_list <- c(option_list, list(
      optparse::make_option("--clobber", action = "store_true", default = FALSE, dest = "clobber"),
      optparse::make_option("--fail-on-warning", action = "store_true", default = FALSE, dest = "fail_on_warning")
    ))
  }

  parser <- optparse::OptionParser(option_list = option_list)
  opts <- optparse::parse_args(parser, args = args, positional_arguments = FALSE)

  required <- c("config", "input_dir", "bids_dir", "participant")
  missing <- required[vapply(required, function(k) is.null(opts[[k]]) || !nzchar(opts[[k]]), logical(1))]
  if (length(missing)) {
    cli::cli_abort("Missing required options: {.val {missing}}")
  }
  if (isTRUE(opts$auto_extract_entities) && isTRUE(opts$do_not_reorder_entities)) {
    cli::cli_abort("--auto-extract-entities and --do-not-reorder-entities cannot be used together.")
  }

  opts
}

split_dicom_dirs <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    character()
  } else {
    vals <- strsplit(x, ",", fixed = TRUE)[[1]]
    vals <- trimws(vals)
    vals[nzchar(vals)]
  }
}

parse_run_opts <- function(args) {
  parser <- optparse::OptionParser(option_list = list(
    optparse::make_option(c("-d", "--dicom-dir"), type = "character", dest = "dicom_dir"),
    optparse::make_option(c("-c", "--config"), type = "character", dest = "config"),
    optparse::make_option(c("-o", "--bids-dir"), type = "character", dest = "bids_dir"),
    optparse::make_option(c("-p", "--participant"), type = "character", dest = "participant"),
    optparse::make_option(c("-s", "--session"), type = "character", default = "", dest = "session"),
    optparse::make_option(c("-r", "--report"), type = "character", default = "", dest = "report"),
    optparse::make_option("--auto-extract-entities", action = "store_true", default = FALSE, dest = "auto_extract_entities"),
    optparse::make_option("--do-not-reorder-entities", action = "store_true", default = FALSE, dest = "do_not_reorder_entities"),
    optparse::make_option("--dcm2niix-bin", type = "character", default = "dcm2niix", dest = "dcm2niix_bin"),
    optparse::make_option("--dcm2niix-options", type = "character", default = DEFAULT_DCM2NIIX_OPTIONS, dest = "dcm2niix_options"),
    optparse::make_option("--skip-dcm2niix", action = "store_true", default = FALSE, dest = "skip_dcm2niix"),
    optparse::make_option("--force-dcm2bids", action = "store_true", default = FALSE, dest = "force_dcm2bids"),
    optparse::make_option("--clobber", action = "store_true", default = FALSE, dest = "clobber"),
    optparse::make_option("--fail-on-warning", action = "store_true", default = FALSE, dest = "fail_on_warning"),
    optparse::make_option("--case-insensitive", action = "store_true", default = FALSE, dest = "case_insensitive")
  ))

  opts <- optparse::parse_args(parser, args = args, positional_arguments = FALSE)
  required <- c("dicom_dir", "config", "bids_dir", "participant")
  missing <- required[vapply(required, function(k) is.null(opts[[k]]) || !nzchar(opts[[k]]), logical(1))]
  if (length(missing)) {
    cli::cli_abort("Missing required options: {.val {missing}}")
  }
  if (isTRUE(opts$auto_extract_entities) && isTRUE(opts$do_not_reorder_entities)) {
    cli::cli_abort("--auto-extract-entities and --do-not-reorder-entities cannot be used together.")
  }

  opts$dicom_dirs <- split_dicom_dirs(opts$dicom_dir)
  if (!length(opts$dicom_dirs)) {
    cli::cli_abort("No input paths provided in --dicom-dir.")
  }

  opts
}

default_config_template <- function() {
  list(
    search_method = "fnmatch",
    case_sensitive = TRUE,
    descriptions = list(
      list(
        datatype = "anat",
        suffix = "T1w",
        criteria = list(
          SeriesDescription = "*T1*"
        )
      ),
      list(
        datatype = "func",
        suffix = "bold",
        custom_entities = list("task-rest"),
        criteria = list(
          SeriesDescription = "*rest*"
        )
      )
    )
  )
}

is_writable_dir <- function(path) {
  path <- fs::path_abs(path)
  fs::dir_create(path, recurse = TRUE)
  probe <- file.path(path, paste0(".dcmtobids-write-check-", Sys.getpid()))
  ok <- tryCatch({
    writeLines("ok", probe)
    fs::file_exists(probe)
  }, error = function(e) FALSE)
  if (fs::file_exists(probe)) {
    fs::file_delete(probe)
  }
  ok
}

run_init_config_cli <- function(args) {
  opts <- parse_init_opts(args)
  out <- fs::path_abs(opts$output)
  if (fs::file_exists(out) && !isTRUE(opts$force)) {
    cli::cli_abort(
      c(
        "Config already exists at {.file {out}}.",
        "i" = "Use --force to overwrite."
      )
    )
  }
  write_json_file(out, default_config_template())
  cli::cli_alert_success("Starter config written: {.file {out}}")
  0L
}

run_doctor_cli <- function(args) {
  opts <- parse_doctor_opts(args)
  checks <- list()
  add_check <- function(name, ok, detail, hint = NULL) {
    checks[[length(checks) + 1L]] <<- list(
      name = name,
      ok = isTRUE(ok),
      detail = detail,
      hint = hint
    )
  }

  add_check(
    name = "dcmtobids package",
    ok = TRUE,
    detail = paste0("version ", as.character(utils::packageVersion("dcmtobids")))
  )

  add_check(
    name = "temp directory writable",
    ok = is_writable_dir(tempdir()),
    detail = tempdir(),
    hint = "Set TMPDIR to a writable location."
  )

  if (isTRUE(opts$skip_dcm2niix)) {
    add_check(
      name = "dcm2niix binary",
      ok = TRUE,
      detail = "skipped (--skip-dcm2niix)"
    )
  } else {
    dcmt <- tryCatch(find_dcm2niix(opts$dcm2niix_bin), error = function(e) e)
    if (inherits(dcmt, "error")) {
      add_check(
        name = "dcm2niix binary",
        ok = FALSE,
        detail = conditionMessage(dcmt),
        hint = "Install dcm2niix or pass --dcm2niix-bin with an absolute path."
      )
    } else {
      add_check(name = "dcm2niix binary", ok = TRUE, detail = dcmt)
    }
  }

  if (nzchar(opts$config)) {
    cfg <- tryCatch(read_config(opts$config), error = function(e) e)
    if (inherits(cfg, "error")) {
      add_check(
        name = "config file",
        ok = FALSE,
        detail = conditionMessage(cfg),
        hint = "Run `dcmtobids validate-config --config <path>` to diagnose."
      )
    } else {
      add_check(name = "config file", ok = TRUE, detail = fs::path_abs(opts$config))
    }
  }

  if (nzchar(opts$bids_dir)) {
    ok_write <- tryCatch(is_writable_dir(opts$bids_dir), error = function(e) FALSE)
    add_check(
      name = "BIDS output dir writable",
      ok = ok_write,
      detail = fs::path_abs(opts$bids_dir),
      hint = "Create the directory and ensure your user has write permissions."
    )
  }

  cli::cli_rule()
  for (ch in checks) {
    if (isTRUE(ch$ok)) {
      cli::cli_alert_success("{ch$name}: {ch$detail}")
    } else {
      cli::cli_alert_warning("{ch$name}: {ch$detail}")
      if (!is.null(ch$hint) && nzchar(ch$hint)) {
        cli::cli_text("i {ch$hint}")
      }
    }
  }

  failures <- sum(!vapply(checks, function(x) isTRUE(x$ok), logical(1)))
  if (failures > 0L) {
    cli::cli_alert_warning("Doctor checks failed: {failures}")
    return(1L)
  }

  cli::cli_alert_success("Doctor checks passed.")
  0L
}

run_validate_cli <- function(args) {
  opts <- parse_validate_opts(args)
  config <- read_config(opts$config)
  validate_config(config)
  cli::cli_alert_success("Config is valid: {.file {fs::path_abs(opts$config)}}")
  0L
}

run_inspect_cli <- function(args) {
  opts <- parse_inspect_opts(args)
  fields <- split_dicom_dirs(opts$unique_fields)
  if (!length(fields)) {
    fields <- c("SeriesDescription", "ProtocolName")
  }

  inventory <- inspect_sidecars(opts$input_dir)
  print_inspect_summary(
    inventory = inventory,
    fields = fields,
    max_unique = as.integer(opts$max_unique)
  )

  if (nzchar(opts$out_tsv)) {
    write_sidecar_inventory(inventory, opts$out_tsv)
    cli::cli_alert_success("Inventory written: {.file {fs::path_abs(opts$out_tsv)}}")
  }
  0L
}

plan_from_cli <- function(config, opts, input_dir) {
  plan <- plan_conversion(
    config = config,
    input_dir = input_dir,
    participant_label = opts$participant,
    session_label = opts$session,
    search_method = config$search_method %||% "fnmatch",
    case_sensitive = if (isTRUE(opts$case_insensitive)) FALSE else (config$case_sensitive %||% TRUE),
    auto_extract_entities = isTRUE(opts$auto_extract_entities) || (config$auto_extract_entities %||% FALSE),
    do_not_reorder_entities = isTRUE(opts$do_not_reorder_entities) || (config$do_not_reorder_entities %||% FALSE),
    dup_method = config$dup_method %||% "run"
  )
  plan
}

write_report_if_requested <- function(report_obj, report_path) {
  if (!nzchar(report_path)) {
    return(invisible(FALSE))
  }
  write_conversion_report(report_obj, report_path)
  cli::cli_alert_success("Report written: {.file {fs::path_abs(report_path)}}")
  invisible(TRUE)
}

status_from_report <- function(error_count = 0L, report_obj = NULL, fail_on_warning = FALSE) {
  status_code <- if (isTRUE(error_count > 0L)) 1L else 0L
  if (identical(status_code, 0L) && isTRUE(fail_on_warning) &&
      !is.null(report_obj) && !identical(report_obj$qa$gate, "pass")) {
    cli::cli_alert_warning("Failing because warnings were detected (--fail-on-warning).")
    status_code <- 1L
  }
  status_code
}

emit_operation_summary <- function(summary, labels = c(updated = "Updated", skipped = "Skipped", errors = "Errors")) {
  cli::cli_rule()
  for (nm in names(labels)) {
    cli::cli_alert_info("{labels[[nm]]}: {summary[[nm]] %||% 0L}")
  }
}

run_populate_cli <- function(args) {
  opts <- parse_populate_opts(args)
  config <- read_config(opts$config)
  plan <- plan_from_cli(config, opts, opts$input_dir)

  pop_summary <- populate_intended_for_plan(
    plan = plan,
    bids_dir = opts$bids_dir,
    bids_uri = if (nzchar(opts$bids_uri)) opts$bids_uri else (config$bids_uri %||% "URI")
  )

  emit_operation_summary(pop_summary)
  report_obj <- summarize_population(pop_summary)
  write_report_if_requested(report_obj, opts$report)
  status_code <- status_from_report(
    error_count = pop_summary$errors,
    report_obj = report_obj,
    fail_on_warning = opts$fail_on_warning
  )

  record_cli_run(
    command = "populate-intended-for",
    bids_dir = opts$bids_dir,
    argv = args,
    status = status_code,
    config_path = opts$config,
    plan = plan,
    report = report_obj,
    populate = pop_summary
  )

  status_code
}

run_dry_run_cli <- function(args) {
  opts <- parse_plan_opts(args, include_clobber = FALSE)
  config <- read_config(opts$config)
  plan <- plan_from_cli(config, opts, opts$input_dir)
  print_dry_run_plan(plan, bids_dir = fs::path_abs(opts$bids_dir))
  report_obj <- summarize_plan(plan)
  write_report_if_requested(report_obj, opts$report)

  record_cli_run(
    command = "dry-run",
    bids_dir = opts$bids_dir,
    argv = args,
    status = 0L,
    config_path = opts$config,
    plan = plan,
    report = report_obj
  )
  0L
}

run_convert_cli <- function(args) {
  opts <- parse_plan_opts(args, include_clobber = TRUE)
  config <- read_config(opts$config)
  plan <- plan_from_cli(config, opts, opts$input_dir)
  print_dry_run_plan(plan, bids_dir = fs::path_abs(opts$bids_dir))
  summary <- convert_plan(
    plan = plan,
    bids_dir = opts$bids_dir,
    clobber = isTRUE(opts$clobber),
    bids_uri = config$bids_uri %||% "URI",
    post_op = config$post_op %||% list()
  )

  emit_operation_summary(
    summary,
    labels = c(converted = "Converted", skipped = "Skipped", errors = "Errors")
  )

  report_obj <- NULL
  if (nzchar(opts$report) || isTRUE(opts$fail_on_warning)) {
    report_obj <- summarize_plan(plan, conversion = summary)
  }
  write_report_if_requested(report_obj, opts$report)
  status_code <- status_from_report(
    error_count = summary$errors,
    report_obj = report_obj,
    fail_on_warning = opts$fail_on_warning
  )

  record_cli_run(
    command = "convert",
    bids_dir = opts$bids_dir,
    argv = args,
    status = status_code,
    config_path = opts$config,
    plan = plan,
    report = report_obj,
    conversion = summary
  )

  if (status_code == 0L) {
    cli::cli_alert_success("Conversion complete.")
  }
  status_code
}

run_full_cli <- function(args) {
  opts <- parse_run_opts(args)
  config <- read_config(opts$config)

  if (!isTRUE(opts$skip_dcm2niix)) {
    version <- dcm2niix_version(opts$dcm2niix_bin)
    if (nzchar(version)) {
      cli::cli_alert_info("dcm2niix: {version}")
    }
  }

  participant_label <- normalize_participant_label(opts$participant)
  session_label <- normalize_session_label(opts$session)
  helper_dir <- file.path(
    fs::path_abs(opts$bids_dir),
    TMP_DIR_NAME,
    participant_prefix(participant_label, session_label)
  )

  helper <- run_dcm2niix(
    dicom_dirs = opts$dicom_dirs,
    output_dir = helper_dir,
    binary = opts$dcm2niix_bin,
    options = opts$dcm2niix_options,
    force = isTRUE(opts$force_dcm2bids),
    skip_dcm2niix = isTRUE(opts$skip_dcm2niix)
  )

  plan <- plan_from_cli(config, opts, helper$output_dir)
  print_dry_run_plan(plan, bids_dir = fs::path_abs(opts$bids_dir))
  summary <- convert_plan(
    plan = plan,
    bids_dir = opts$bids_dir,
    clobber = isTRUE(opts$clobber),
    bids_uri = config$bids_uri %||% "URI",
    post_op = config$post_op %||% list()
  )

  emit_operation_summary(
    summary,
    labels = c(converted = "Converted", skipped = "Skipped", errors = "Errors")
  )

  report_obj <- NULL
  if (nzchar(opts$report) || isTRUE(opts$fail_on_warning)) {
    report_obj <- summarize_plan(
      plan,
      conversion = summary,
      helper = list(
        output_dir = helper$output_dir,
        reused = helper$reused,
        warnings = helper$warnings %||% list()
      )
    )
  }
  write_report_if_requested(report_obj, opts$report)
  status_code <- status_from_report(
    error_count = summary$errors,
    report_obj = report_obj,
    fail_on_warning = opts$fail_on_warning
  )

  record_cli_run(
    command = "run",
    bids_dir = opts$bids_dir,
    argv = args,
    status = status_code,
    config_path = opts$config,
    plan = plan,
    report = report_obj,
    helper = list(
      output_dir = helper$output_dir,
      reused = helper$reused,
      warnings = helper$warnings %||% list()
    ),
    conversion = summary
  )

  if (status_code == 0L) {
    cli::cli_alert_success("Conversion complete.")
  }
  status_code
}

#' CLI entrypoint
#'
#' @param args Character vector of CLI args
#' @return Process status code
#' @export
dcmtobids_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) && identical(args[[1]], "--args")) {
    args <- args[-1]
  }

  if (!length(args) || args[[1]] %in% c("-h", "--help", "help")) {
    cat(cli_usage(), "\n")
    return(0L)
  }

  command <- args[[1]]
  rest <- args[-1]

  status <- tryCatch({
    switch(
      command,
      "init-config" = run_init_config_cli(rest),
      "doctor" = run_doctor_cli(rest),
      "inspect" = run_inspect_cli(rest),
      "populate-intended-for" = run_populate_cli(rest),
      "validate-config" = run_validate_cli(rest),
      "dry-run" = run_dry_run_cli(rest),
      "convert" = run_convert_cli(rest),
      "run" = run_full_cli(rest),
      "version" = {
        cat(as.character(utils::packageVersion("dcmtobids")), "\n")
        0L
      },
      {
        cli::cli_abort("Unknown command: {.val {command}}")
      }
    )
  }, error = function(e) {
    cli::cli_alert_warning(conditionMessage(e))
    1L
  })

  as.integer(status)
}
