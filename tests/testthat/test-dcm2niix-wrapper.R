make_fake_dcm2niix <- function(path) {
  script <- c(
    "#!/usr/bin/env bash",
    "set -euo pipefail",
    "if [[ \"${1:-}\" == \"--version\" ]]; then",
    "  echo 'dcm2niix version v1.0.0-fake'",
    "  exit 0",
    "fi",
    "out=''",
    "while [[ $# -gt 0 ]]; do",
    "  case \"$1\" in",
    "    -o)",
    "      out=\"$2\"",
    "      shift 2",
    "      ;;",
    "    *)",
    "      shift",
    "      ;;",
    "  esac",
    "done",
    "mkdir -p \"$out\"",
    "echo '{\"SeriesDescription\":\"task-rest_bold\"}' > \"$out/001_fake.json\"",
    "echo \"$(date +%s)\" > \"$out/001_fake.nii.gz\""
  )
  writeLines(script, path)
  Sys.chmod(path, mode = "0755")
}

test_that("run_dcm2niix supports skip mode", {
  td <- tempfile("dcmtobids-dcm2niix-skip-")
  dir.create(td)
  src <- file.path(td, "src")
  out <- file.path(td, "out")
  dir.create(src)
  writeLines('{"SeriesDescription":"x"}', file.path(src, "a.json"))
  writeLines("x", file.path(src, "a.nii"))

  res <- run_dcm2niix(
    dicom_dirs = src,
    output_dir = out,
    skip_dcm2niix = TRUE
  )

  expect_false(res$reused)
  expect_true(file.exists(file.path(out, "a.json")))
  expect_true(file.exists(file.path(out, "a.nii")))
})

test_that("run_dcm2niix reuse and force semantics", {
  td <- tempfile("dcmtobids-dcm2niix-run-")
  dir.create(td)
  src <- file.path(td, "dicom")
  out <- file.path(td, "helper")
  fake <- file.path(td, "dcm2niix-fake")
  dir.create(src)
  make_fake_dcm2niix(fake)

  first <- run_dcm2niix(
    dicom_dirs = src,
    output_dir = out,
    binary = fake,
    options = "-b y"
  )
  expect_false(first$reused)
  expect_true(file.exists(file.path(out, "001_fake.json")))

  m1 <- file.info(file.path(out, "001_fake.nii.gz"))$mtime
  Sys.sleep(1)

  second <- run_dcm2niix(
    dicom_dirs = src,
    output_dir = out,
    binary = fake,
    options = "-b y",
    force = FALSE
  )
  m2 <- file.info(file.path(out, "001_fake.nii.gz"))$mtime
  expect_true(second$reused)
  expect_equal(m1, m2)

  Sys.sleep(1)
  third <- run_dcm2niix(
    dicom_dirs = src,
    output_dir = out,
    binary = fake,
    options = "-b y",
    force = TRUE
  )
  m3 <- file.info(file.path(out, "001_fake.nii.gz"))$mtime
  expect_false(third$reused)
  expect_true(m3 > m2)
})
