make_sidecar <- function(path, obj) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE), path)
}

test_that("plan_conversion and convert_plan run end-to-end", {
  td <- tempfile("dcmtobids-test-")
  dir.create(td)

  input_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  dir.create(input_dir)

  src_root <- file.path(input_dir, "001_task-rest")
  make_sidecar(paste0(src_root, ".json"), list(SeriesDescription = "task-rest_bold"))
  writeLines("dummy", paste0(src_root, ".nii.gz"))

  config <- list(
    search_method = "fnmatch",
    descriptions = list(
      list(
        id = "func_task-rest",
        datatype = "func",
        suffix = "bold",
        custom_entities = list("task-rest"),
        criteria = list(SeriesDescription = "*bold*")
      )
    )
  )

  plan <- plan_conversion(
    config = config,
    input_dir = input_dir,
    participant_label = "01"
  )

  expect_equal(nrow(plan), 1)
  expect_equal(plan$status[[1]], "MATCHED")
  expect_match(plan$destination_stem[[1]], "sub-01_task-rest_bold")

  summary <- convert_plan(plan, bids_dir = bids_dir, clobber = FALSE)
  expect_true(summary$converted >= 2)

  out_json <- file.path(bids_dir, plan$destination_dir[[1]], paste0(plan$destination_stem[[1]], ".json"))
  out_nii <- file.path(bids_dir, plan$destination_dir[[1]], paste0(plan$destination_stem[[1]], ".nii.gz"))

  expect_true(file.exists(out_json))
  expect_true(file.exists(out_nii))
  expect_true(file.exists(paste0(src_root, ".json")))
  expect_true(file.exists(paste0(src_root, ".nii.gz")))
})

test_that("convert_plan skips an entire acquisition when any destination file exists", {
  td <- tempfile("dcmtobids-partial-skip-")
  dir.create(td)

  input_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  dir.create(input_dir)
  dir.create(file.path(bids_dir, "sub-01", "func"), recursive = TRUE)

  src_root <- file.path(input_dir, "001_task-rest")
  make_sidecar(
    paste0(src_root, ".json"),
    list(SeriesDescription = "task-rest_bold", SourceVersion = "new")
  )
  writeLines("dummy", paste0(src_root, ".nii.gz"))

  writeLines("{}", file.path(bids_dir, "sub-01", "func", "sub-01_task-rest_bold.json"))

  config <- list(
    descriptions = list(
      list(
        datatype = "func",
        suffix = "bold",
        custom_entities = list("task-rest"),
        criteria = list(SeriesDescription = "*bold*")
      )
    )
  )

  plan <- plan_conversion(
    config = config,
    input_dir = input_dir,
    participant_label = "01"
  )

  summary <- convert_plan(plan, bids_dir = bids_dir, clobber = FALSE)

  expect_equal(summary$converted, 0L)
  expect_equal(summary$errors, 0L)
  expect_true(summary$skipped >= 2L)
  expect_false(file.exists(file.path(bids_dir, "sub-01", "func", "sub-01_task-rest_bold.nii.gz")))
  expect_true(file.exists(paste0(src_root, ".json")))
  expect_true(file.exists(paste0(src_root, ".nii.gz")))
})
