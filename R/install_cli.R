launcher_script_text <- function(lib_path = .libPaths()[1]) {
  c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    paste0("DCMTOBIDS_DEFAULT_LIB=", shQuote(lib_path, type = "sh")),
    ": \"${DCMTOBIDS_R_LIBS:=$DCMTOBIDS_DEFAULT_LIB}\"",
    "export DCMTOBIDS_R_LIBS",
    "Rscript --vanilla -e '.libPaths(c(strsplit(Sys.getenv(\"DCMTOBIDS_R_LIBS\"), .Platform$path.sep, fixed = TRUE)[[1]], .libPaths())); status <- dcmtobids::dcmtobids_main(commandArgs(trailingOnly = TRUE)); quit(save=\"no\", status=status, runLast=FALSE)' \"$@\""
  )
}

#' Install dcmtobids CLI launcher in PATH
#'
#' @param install_dir Directory to place launcher script
#' @param overwrite Overwrite existing launcher
#' @return Path to installed launcher
#' @export
install_cli <- function(
    install_dir = fs::path_abs("~/.local/bin"),
    overwrite = TRUE
) {
  install_dir <- fs::path_expand(install_dir)
  fs::dir_create(install_dir, recurse = TRUE)

  launcher_path <- file.path(install_dir, "dcmtobids")
  if (fs::file_exists(launcher_path) && !isTRUE(overwrite)) {
    cli::cli_abort("Launcher already exists: {.file {launcher_path}}")
  }

  package_lib <- dirname(find.package("dcmtobids"))
  writeLines(launcher_script_text(lib_path = package_lib), launcher_path)

  Sys.chmod(launcher_path, mode = "0755")
  cli::cli_alert_success("Installed CLI launcher: {.file {launcher_path}}")
  cli::cli_alert_info("Ensure {.file {install_dir}} is in your PATH.")
  invisible(launcher_path)
}
