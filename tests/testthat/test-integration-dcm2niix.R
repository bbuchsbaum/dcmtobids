test_that("real dcm2niix integration path is available and guarded", {
  testthat::skip_if_not(identical(Sys.getenv("DCMTOBIDS_RUN_DCM2NIIX"), "1"),
                        "Set DCMTOBIDS_RUN_DCM2NIIX=1 to enable real integration test")

  bin <- Sys.which("dcm2niix")
  testthat::skip_if_not(nzchar(bin), "dcm2niix not found in PATH")

  sourcedata <- file.path(upstream_data_dir(), "sourcedata", "sub-01")
  testthat::skip_if_not(dir.exists(sourcedata), "No real sourcedata fixture available")

  td <- tempfile("dcmtobids-real-dcm2niix-")
  dir.create(td)
  bids_dir <- file.path(td, "bids")

  res <- run_dicom_conversion(
    config_path = fixture_config_path("config_test.json"),
    dicom_dirs = sourcedata,
    bids_dir = bids_dir,
    participant_label = "01",
    dcm2niix_bin = bin,
    force_dcm2bids = TRUE,
    skip_dcm2niix = FALSE,
    clobber = FALSE
  )

  expect_true(dir.exists(file.path(bids_dir, "tmp_dcm2bids", "sub-01")))
  expect_true(file.exists(file.path(bids_dir, "sub-01", "anat", "sub-01_T1w.json")))
  expect_true(length(res$helper$sidecars) > 0)
  expect_true(res$conversion$converted > 0)
})
