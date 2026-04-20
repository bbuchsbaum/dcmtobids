parse_shell_args <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(character())
  }
  scan(text = x, what = character(), quiet = TRUE)
}

#' Find dcm2niix binary
#'
#' @param binary Name or path of dcm2niix binary
#' @return Absolute path to executable
#' @export
find_dcm2niix <- function(binary = "dcm2niix") {
  candidate <- Sys.which(binary)
  if (!nzchar(candidate)) {
    cli::cli_abort(
      c(
        "Could not find {.val {binary}} in PATH.",
        "i" = "Install dcm2niix and retry, or pass --dcm2niix-bin with a full path."
      )
    )
  }
  fs::path_abs(candidate)
}

#' Query dcm2niix version
#'
#' @param binary Name or path of dcm2niix binary
#' @return Version text
#' @export
dcm2niix_version <- function(binary = "dcm2niix") {
  bin <- find_dcm2niix(binary)
  out <- suppressWarnings(system2(bin, "--version", stdout = TRUE, stderr = TRUE))
  if (!length(out)) {
    out <- suppressWarnings(system2(bin, "-v", stdout = TRUE, stderr = TRUE))
  }
  paste(out, collapse = "\n")
}

extract_dicom_input <- function(path, work_dir) {
  if (fs::dir_exists(path)) {
    return(path)
  }

  if (!fs::file_exists(path)) {
    cli::cli_abort("DICOM input does not exist: {.file {path}}")
  }

  lower <- tolower(path)
  out_dir <- file.path(work_dir, tools::file_path_sans_ext(basename(path)))
  fs::dir_create(out_dir, recurse = TRUE)

  if (grepl("\\.zip$", lower)) {
    utils::unzip(path, exdir = out_dir)
    return(out_dir)
  }

  if (grepl("\\.(tar|tar\\.gz|tgz|tar\\.bz2)$", lower)) {
    utils::untar(path, exdir = out_dir)
    return(out_dir)
  }

  cli::cli_abort(
    c(
      "Unsupported input file: {.file {path}}",
      "i" = "Use a directory or one of: .zip, .tar, .tar.gz, .tgz, .tar.bz2"
    )
  )
}

run_dcm2niix_once <- function(binary, args) {
  out_file <- tempfile("dcmtobids-dcm2niix-out-")
  err_file <- tempfile("dcmtobids-dcm2niix-err-")
  on.exit(unlink(c(out_file, err_file), force = TRUE), add = TRUE)

  status <- system2(binary, args = args, stdout = out_file, stderr = err_file)
  stdout <- if (file.exists(out_file)) paste(readLines(out_file, warn = FALSE), collapse = "\n") else ""
  stderr <- if (file.exists(err_file)) paste(readLines(err_file, warn = FALSE), collapse = "\n") else ""

  list(status = status, stdout = stdout, stderr = stderr)
}

#' Run dcm2niix to produce helper sidecars
#'
#' @param dicom_dirs Character vector of DICOM directories or archives
#' @param output_dir Helper output directory
#' @param binary Name or path of dcm2niix binary
#' @param options dcm2niix option string
#' @param force Remove previous helper output and rerun
#' @param skip_dcm2niix Skip execution and copy existing NIfTI/JSON files
#' @return List with `output_dir`, `sidecars`, and `reused`
#' @export
run_dcm2niix <- function(
    dicom_dirs,
    output_dir,
    binary = "dcm2niix",
    options = DEFAULT_DCM2NIIX_OPTIONS,
    force = FALSE,
    skip_dcm2niix = FALSE
) {
  if (!length(dicom_dirs)) {
    cli::cli_abort("At least one input path is required in dicom_dirs.")
  }

  output_dir <- fs::path_abs(output_dir)
  fs::dir_create(output_dir, recurse = TRUE)

  existing <- fs::dir_ls(output_dir, recurse = TRUE, type = "file")
  reused <- FALSE
  warning_events <- list()

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

  if (length(existing) && !isTRUE(force)) {
    emit_warning(
      code = "I_HELPER_REUSED",
      message = paste0("Existing helper output found at ", output_dir, "; reusing (use --force-dcm2bids to regenerate)."),
      severity = "info",
      context = list(output_dir = output_dir)
    )
    reused <- TRUE
    sidecars <- sort(fs::dir_ls(output_dir, recurse = TRUE, type = "file", regexp = "\\.json$"))
    return(list(output_dir = output_dir, sidecars = sidecars, reused = reused, warnings = warning_events))
  }

  if (length(existing) && isTRUE(force)) {
    fs::dir_delete(output_dir)
    fs::dir_create(output_dir, recurse = TRUE)
  }

  dicom_dirs <- vapply(dicom_dirs, fs::path_abs, character(1))

  if (isTRUE(skip_dcm2niix)) {
    for (input in dicom_dirs) {
      if (fs::dir_exists(input)) {
        files <- fs::dir_ls(input, recurse = TRUE, type = "file", regexp = "\\.(json|nii|nii\\.gz|bval|bvec)$")
        for (src in files) {
          rel <- fs::path_rel(src, start = input)
          dst <- file.path(output_dir, rel)
          fs::dir_create(dirname(dst), recurse = TRUE)
          fs::file_copy(src, dst, overwrite = TRUE)
        }
      } else if (fs::file_exists(input)) {
        fs::file_copy(input, file.path(output_dir, basename(input)), overwrite = TRUE)
      }
    }

    sidecars <- sort(fs::dir_ls(output_dir, recurse = TRUE, type = "file", regexp = "\\.json$"))
    return(list(output_dir = output_dir, sidecars = sidecars, reused = FALSE, warnings = warning_events))
  }

  bin <- find_dcm2niix(binary)
  option_args <- parse_shell_args(options)
  extraction_root <- tempfile("dcmtobids-archive-")
  fs::dir_create(extraction_root, recurse = TRUE)
  on.exit(fs::dir_delete(extraction_root), add = TRUE)

  for (input in dicom_dirs) {
    source_dir <- extract_dicom_input(input, extraction_root)
    args <- c(option_args, "-o", output_dir, source_dir)
    res <- run_dcm2niix_once(bin, args)

    if (!identical(res$status, 0L)) {
      cli::cli_abort(
        c(
          "dcm2niix failed for input {.file {input}}.",
          "i" = if (nzchar(res$stderr)) res$stderr else "No stderr output captured."
        )
      )
    }

    if (grepl("Warning|Error", res$stdout, ignore.case = TRUE) ||
        grepl("Warning|Error", res$stderr, ignore.case = TRUE)) {
      emit_warning(
        code = "W_DCM2NIIX_OUTPUT",
        message = paste0("dcm2niix emitted warnings for ", input),
        severity = "warning",
        context = list(input = input, stdout = res$stdout, stderr = res$stderr)
      )
    }
  }

  sidecars <- sort(fs::dir_ls(output_dir, recurse = TRUE, type = "file", regexp = "\\.json$"))
  list(output_dir = output_dir, sidecars = sidecars, reused = FALSE, warnings = warning_events)
}

#' Run full DICOM to BIDS pipeline
#'
#' @param config_path Path to dcmtobids config JSON
#' @param dicom_dirs Character vector of DICOM directories or archives
#' @param bids_dir Destination BIDS directory
#' @param participant_label Participant label
#' @param session_label Session label
#' @param auto_extract_entities Enable automatic extraction of BIDS entities
#'   (e.g. `task`, `dir`, `echo`) from sidecar fields
#' @param do_not_reorder_entities Keep entity order as supplied
#' @param dcm2niix_bin Name/path of dcm2niix binary
#' @param dcm2niix_options dcm2niix option string
#' @param force_dcm2bids Recreate helper output before conversion
#' @param skip_dcm2niix Skip dcm2niix and copy NIfTI/JSON inputs
#' @param clobber Overwrite existing destination files
#' @return List containing helper and conversion results
#' @export
run_dicom_conversion <- function(
    config_path,
    dicom_dirs,
    bids_dir,
    participant_label,
    session_label = "",
    auto_extract_entities = FALSE,
    do_not_reorder_entities = FALSE,
    dcm2niix_bin = "dcm2niix",
    dcm2niix_options = DEFAULT_DCM2NIIX_OPTIONS,
    force_dcm2bids = FALSE,
    skip_dcm2niix = FALSE,
    clobber = FALSE
) {
  participant_label <- normalize_participant_label(participant_label)
  session_label <- normalize_session_label(session_label)
  bids_dir <- fs::path_abs(bids_dir)

  helper_name <- participant_prefix(participant_label, session_label)
  helper_dir <- file.path(bids_dir, TMP_DIR_NAME, helper_name)

  helper <- run_dcm2niix(
    dicom_dirs = dicom_dirs,
    output_dir = helper_dir,
    binary = dcm2niix_bin,
    options = dcm2niix_options,
    force = force_dcm2bids,
    skip_dcm2niix = skip_dcm2niix
  )

  conversion <- run_conversion(
    config_path = config_path,
    input_dir = helper$output_dir,
    bids_dir = bids_dir,
    participant_label = participant_label,
    session_label = session_label,
    auto_extract_entities = auto_extract_entities,
    do_not_reorder_entities = do_not_reorder_entities,
    clobber = clobber
  )

  list(helper = helper, conversion = conversion)
}
