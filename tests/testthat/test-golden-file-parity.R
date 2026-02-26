relative_bids_files <- function(root) {
  files <- list.files(root, recursive = TRUE, full.names = FALSE)
  files <- files[file.info(file.path(root, files))$isdir == FALSE]
  files <- files[!startsWith(files, "tmp_dcm2bids/")]
  files <- gsub("\\\\", "/", files)
  sort(files)
}

read_golden <- function(name) {
  path <- file.path(testthat::test_path("..", "fixtures", "python-golden"), paste0(name, ".txt"))
  if (!file.exists(path)) {
    stop("Golden file missing: ", path)
  }
  x <- readLines(path, warn = FALSE)
  x[nzchar(x)]
}

run_case <- function(case) {
  td <- tempfile(paste0("dcmtobids-golden-", case$name, "-"))
  dir.create(td)
  input_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  copy_fixture_sidecars(input_dir)

  run_conversion(
    config_path = fixture_config_path(case$config),
    input_dir = input_dir,
    bids_dir = bids_dir,
    participant_label = "01",
    session_label = case$session,
    auto_extract_entities = case$auto,
    do_not_reorder_entities = case$noreorder
  )

  relative_bids_files(bids_dir)
}

test_that("golden file-path parity with upstream Python outputs", {
  cases <- list(
    list(name = "config_test", config = "config_test.json", session = "", auto = FALSE, noreorder = FALSE),
    list(name = "config_test_auto_extract", config = "config_test_auto_extract.json", session = "", auto = TRUE, noreorder = FALSE),
    list(name = "config_test_complex", config = "config_test_complex.json", session = "", auto = FALSE, noreorder = FALSE),
    list(name = "config_test_dup", config = "config_test_dup.json", session = "", auto = FALSE, noreorder = FALSE),
    list(name = "config_test_float", config = "config_test_float.json", session = "", auto = FALSE, noreorder = FALSE),
    list(name = "config_test_key_absent", config = "config_test_key_absent.json", session = "", auto = FALSE, noreorder = FALSE),
    list(name = "config_test_multiple_intendedfor", config = "config_test_multiple_intendedfor.json", session = "", auto = TRUE, noreorder = FALSE),
    list(name = "config_test_multiple_intendedfor_uri_relative", config = "config_test_multiple_intendedfor_uri_relative.json", session = "", auto = TRUE, noreorder = FALSE),
    list(name = "config_test_no_reorder", config = "config_test_no_reorder.json", session = "", auto = FALSE, noreorder = TRUE),
    list(name = "config_test_not_case_sensitive_option", config = "config_test_not_case_sensitive_option.json", session = "", auto = FALSE, noreorder = FALSE),
    list(name = "config_test_sidecar", config = "config_test_sidecar.json", session = "dev", auto = FALSE, noreorder = FALSE)
  )

  for (case in cases) {
    got <- run_case(case)
    expected <- read_golden(case$name)
    expect_identical(got, expected, info = case$name)
  }
})
