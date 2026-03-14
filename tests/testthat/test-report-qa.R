code_set <- function(report) {
  vapply(report$qa$codes, function(x) x$code, character(1))
}

test_that("report includes machine-readable warning codes for conversion", {
  td <- tempfile("dcmtobids-report-qa-")
  dir.create(td)

  input1 <- file.path(td, "input1")
  input2 <- file.path(td, "input2")
  bids_dir <- file.path(td, "bids")

  copy_fixture_sidecars(input1)

  config <- read_config(fixture_config_path("config_test.json"))
  plan1 <- plan_conversion(config, input1, participant_label = "01")
  conv1 <- convert_plan(plan1, bids_dir = bids_dir, clobber = FALSE, bids_uri = config$bids_uri %||% "URI")
  report1 <- summarize_plan(plan1, conversion = conv1)

  codes1 <- code_set(report1)
  expect_true("W_MISSING_ID_REFERENCE" %in% codes1)
  expect_true(report1$qa$gate %in% c("warn", "fail"))

  copy_fixture_sidecars(input2)
  plan2 <- plan_conversion(config, input2, participant_label = "01")
  conv2 <- convert_plan(plan2, bids_dir = bids_dir, clobber = FALSE, bids_uri = config$bids_uri %||% "URI")
  report2 <- summarize_plan(plan2, conversion = conv2)

  codes2 <- code_set(report2)
  expect_true("W_OUTPUT_EXISTS_SKIPPED" %in% codes2)
  expect_true(report2$conversion$skipped > 0)
})

test_that("helper warnings propagate into report qa codes", {
  td <- tempfile("dcmtobids-report-helper-")
  dir.create(td)
  src <- file.path(td, "src")
  helper_out <- file.path(td, "helper")
  dir.create(src)

  writeLines('{"SeriesDescription":"x"}', file.path(src, "a.json"))
  writeLines("x", file.path(src, "a.nii"))

  h1 <- run_dcm2niix(dicom_dirs = src, output_dir = helper_out, skip_dcm2niix = TRUE)
  expect_false(h1$reused)

  h2 <- run_dcm2niix(dicom_dirs = src, output_dir = helper_out, skip_dcm2niix = TRUE, force = FALSE)
  expect_true(h2$reused)

  plan <- data.frame(
    source_json = "dummy.json",
    source_root = "dummy",
    sort_index = 1,
    status = "MATCHED",
    reason = "",
    participant_label = "sub-01",
    session_label = "",
    datatype = "func",
    suffix = "bold",
    destination_dir = "sub-01/func",
    destination_stem = "sub-01_task-rest_bold",
    id = NA_character_,
    task_name = NA_character_,
    do_not_reorder_entities = FALSE,
    stringsAsFactors = FALSE
  )
  plan$custom_entities <- I(list(character()))
  plan$sidecar_changes <- I(list(list()))

  report <- summarize_plan(plan, conversion = list(converted = 0, skipped = 0, errors = 0, warnings = list()), helper = h2)
  codes <- code_set(report)
  expect_true("W_HELPER_REUSED" %in% codes)
})

test_that("duplicate resolution is preserved in report metadata", {
  td <- tempfile("dcmtobids-report-dup-")
  dir.create(td)
  input_dir <- file.path(td, "input")
  dir.create(input_dir)
  copy_fixture_sidecars(input_dir)

  config <- read_config(fixture_config_path("config_test_dup.json"))
  plan <- plan_conversion(config, input_dir, participant_label = "01")
  report <- summarize_plan(plan)

  expect_true("W_DUPLICATE_DESTINATION" %in% code_set(report))
  expect_true(length(report$duplicate_resolution) >= 1L)
  expect_equal(report$duplicate_resolution[[1]]$original_stem, "sub-01_localizer")
  expect_true(all(grepl("^sub-01(_dup-[0-9]{2})?_localizer$", report$duplicate_resolution[[1]]$resolved_stems)))
})
