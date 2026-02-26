upstream_data_dir <- function() {
  root <- normalizePath(file.path(testthat::test_path("..", ".."), "Dcm2Bids", "tests", "data"), mustWork = TRUE)
  root
}

upstream_sidecars_dir <- function() {
  file.path(upstream_data_dir(), "sidecars")
}

copy_fixture_sidecars <- function(target_dir) {
  src <- upstream_sidecars_dir()
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, full.names = TRUE)
  file.copy(files, file.path(target_dir, basename(files)), overwrite = TRUE)
  invisible(target_dir)
}

fixture_config_path <- function(name) {
  file.path(upstream_data_dir(), name)
}
