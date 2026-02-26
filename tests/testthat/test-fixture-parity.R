read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = TRUE)

test_that("config_test.json parity core behaviors", {
  td <- tempfile("dcmtobids-fixture-")
  dir.create(td)
  input_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  dir.create(bids_dir)

  copy_fixture_sidecars(input_dir)

  summary <- run_conversion(
    config_path = fixture_config_path("config_test.json"),
    input_dir = input_dir,
    bids_dir = bids_dir,
    participant_label = "01"
  )

  expect_true(summary$converted > 0)

  fmap_492 <- file.path(bids_dir, "sub-01", "fmap", "sub-01_echo-492_fmap.json")
  fmap_738 <- file.path(bids_dir, "sub-01", "fmap", "sub-01_echo-738_fmap.json")
  expect_true(file.exists(fmap_492))
  expect_true(file.exists(fmap_738))

  data_492 <- read_json(fmap_492)
  expect_equal(
    data_492$IntendedFor,
    c(
      "bids::sub-01/dwi/sub-01_dwi.nii.gz",
      "bids::sub-01/anat/sub-01_T1w.nii"
    )
  )

  data_738 <- read_json(fmap_738)
  expect_equal(data_738$IntendedFor, "bids::sub-01/dwi/sub-01_dwi.nii.gz")

  loc_01 <- file.path(bids_dir, "sub-01", "localizer", "sub-01_run-01_localizer.json")
  loc_02 <- file.path(bids_dir, "sub-01", "localizer", "sub-01_run-02_localizer.json")
  loc_03 <- file.path(bids_dir, "sub-01", "localizer", "sub-01_run-03_localizer.json")
  expect_true(file.exists(loc_01))
  expect_true(file.exists(loc_02))
  expect_true(file.exists(loc_03))
  expect_equal(read_json(loc_01)$ProcedureStepDescription, "Modify by dcm2bids")
  expect_equal(read_json(loc_02)$AcquisitionTime, "13:00:41.142500")
  expect_equal(read_json(loc_03)$AcquisitionTime, "13:00:45.130000")

  # Rerun without clobber should preserve existing file mtime.
  mtime_before <- file.info(fmap_738)$mtime
  unlink(input_dir, recursive = TRUE, force = TRUE)
  copy_fixture_sidecars(input_dir)
  run_conversion(
    config_path = fixture_config_path("config_test.json"),
    input_dir = input_dir,
    bids_dir = bids_dir,
    participant_label = "01"
  )
  mtime_after <- file.info(fmap_738)$mtime
  expect_equal(mtime_before, mtime_after)
})

test_that("do_not_reorder_entities preserves custom entity order", {
  td <- tempfile("dcmtobids-fixture-noreorder-")
  dir.create(td)
  input_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  copy_fixture_sidecars(input_dir)

  run_conversion(
    config_path = fixture_config_path("config_test_no_reorder.json"),
    input_dir = input_dir,
    bids_dir = bids_dir,
    participant_label = "01",
    do_not_reorder_entities = TRUE
  )

  expect_true(file.exists(file.path(
    bids_dir,
    "sub-01",
    "func",
    "sub-01_acq-highres_task-rest_bold.json"
  )))
})

test_that("relative bids_uri writes relative IntendedFor paths", {
  td <- tempfile("dcmtobids-fixture-uri-")
  dir.create(td)
  input_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  copy_fixture_sidecars(input_dir)

  run_conversion(
    config_path = fixture_config_path("config_test_multiple_intendedfor_uri_relative.json"),
    input_dir = input_dir,
    bids_dir = bids_dir,
    participant_label = "01"
  )

  fmap <- file.path(bids_dir, "sub-01", "fmap", "sub-01_fmap.json")
  expect_true(file.exists(fmap))
  data <- read_json(fmap)
  expect_equal(
    data$IntendedFor,
    c(
      "localizer/sub-01_run-01_localizer.nii",
      "localizer/sub-01_run-02_localizer.nii",
      "localizer/sub-01_run-03_localizer.nii",
      "anat/sub-01_T1w.nii"
    )
  )
})

test_that("auto extract entities adds task and dir-derived entities", {
  td <- tempfile("dcmtobids-fixture-auto-")
  dir.create(td)
  input_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  copy_fixture_sidecars(input_dir)

  run_conversion(
    config_path = fixture_config_path("config_test_auto_extract.json"),
    input_dir = input_dir,
    bids_dir = bids_dir,
    participant_label = "01",
    auto_extract_entities = TRUE
  )

  func_task <- file.path(
    bids_dir,
    "sub-01",
    "func",
    "sub-01_task-rest_acq-highres_bold.json"
  )
  epi <- file.path(bids_dir, "sub-01", "fmap", "sub-01_dir-AP_epi.json")

  expect_true(file.exists(func_task))
  expect_true(file.exists(epi))
  expect_equal(read_json(func_task)$TaskName, "rest")
})
