test_that("dcmtobids_main handles help and version", {
  expect_equal(dcmtobids_main(character()), 0L)
  expect_equal(dcmtobids_main(c("help")), 0L)
  expect_true(dcmtobids_main(c("version")) %in% c(0L, 1L))
})

test_that("run command supports skip-dcm2niix and report output", {
  td <- tempfile("dcmtobids-cli-run-")
  dir.create(td)
  dicom_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  report <- file.path(td, "report.json")
  dir.create(dicom_dir)

  writeLines('{"SeriesDescription":"task-rest_bold"}', file.path(dicom_dir, "001.json"))
  writeLines("dummy", file.path(dicom_dir, "001.nii.gz"))

  config <- file.path(td, "config.json")
  writeLines(
    jsonlite::toJSON(
      list(
        descriptions = list(
          list(
            datatype = "func",
            suffix = "bold",
            custom_entities = list("task-rest"),
            criteria = list(SeriesDescription = "*bold*")
          )
        )
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ),
    config
  )

  status <- dcmtobids_main(c(
    "run",
    "--skip-dcm2niix",
    "--dicom-dir", dicom_dir,
    "--config", config,
    "--bids-dir", bids_dir,
    "--participant", "01",
    "--report", report
  ))

  expect_equal(status, 0L)
  expect_true(file.exists(report))
  expect_true(file.exists(file.path(bids_dir, "sub-01", "func", "sub-01_task-rest_bold.json")))

  runs_dir <- file.path(bids_dir, ".dcmtobids", "runs")
  expect_true(dir.exists(runs_dir))
  expect_true(length(list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)) >= 1L)
})

test_that("init-config writes a starter config", {
  td <- tempfile("dcmtobids-cli-init-")
  dir.create(td)
  cfg <- file.path(td, "config.json")

  status <- dcmtobids_main(c("init-config", "--output", cfg))
  expect_equal(status, 0L)
  expect_true(file.exists(cfg))

  parsed <- read_config(cfg)
  expect_true(is.list(parsed$descriptions))
  expect_true(length(parsed$descriptions) >= 1L)
})

test_that("doctor passes for skip-dcm2niix helper flow", {
  td <- tempfile("dcmtobids-cli-doctor-")
  dir.create(td)
  cfg <- file.path(td, "config.json")
  writeLines(
    jsonlite::toJSON(
      list(
        descriptions = list(
          list(
            datatype = "anat",
            suffix = "T1w",
            criteria = list(SeriesDescription = "*T1*")
          )
        )
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ),
    cfg
  )

  status <- dcmtobids_main(c(
    "doctor",
    "--skip-dcm2niix",
    "--config", cfg,
    "--bids-dir", file.path(td, "bids")
  ))
  expect_equal(status, 0L)
})

test_that("convert can fail on warnings for CI gating", {
  td <- tempfile("dcmtobids-cli-failwarn-")
  dir.create(td)
  input_dir <- file.path(td, "input")
  bids_dir <- file.path(td, "bids")
  dir.create(input_dir)

  writeLines('{"SeriesDescription":"task-rest_bold"}', file.path(input_dir, "001.json"))
  writeLines("dummy", file.path(input_dir, "001.nii.gz"))

  config <- file.path(td, "config.json")
  writeLines(
    jsonlite::toJSON(
      list(
        descriptions = list(
          list(
            datatype = "func",
            suffix = "bold",
            custom_entities = list("task-rest"),
            criteria = list(SeriesDescription = "*does-not-match*")
          )
        )
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ),
    config
  )

  status <- dcmtobids_main(c(
    "convert",
    "--config", config,
    "--input-dir", input_dir,
    "--bids-dir", bids_dir,
    "--participant", "01",
    "--fail-on-warning"
  ))

  expect_equal(status, 1L)
})

test_that("inspect command reports inventory and writes TSV", {
  td <- tempfile("dcmtobids-cli-inspect-")
  dir.create(td)
  input_dir <- file.path(td, "input")
  out_tsv <- file.path(td, "sidecarinfo.tsv")
  dir.create(input_dir)

  writeLines(
    '{"SeriesDescription":"task-rest_bold","ProtocolName":"func_rest","SeriesNumber":3}',
    file.path(input_dir, "001.json")
  )
  writeLines(
    '{"SeriesDescription":"T1_MPRAGE","ProtocolName":"anat_t1","SeriesNumber":1}',
    file.path(input_dir, "002.json")
  )

  status <- dcmtobids_main(c(
    "inspect",
    "--input-dir", input_dir,
    "--out-tsv", out_tsv,
    "--unique-fields", "SeriesDescription,ProtocolName",
    "--max-unique", "10"
  ))

  expect_equal(status, 0L)
  expect_true(file.exists(out_tsv))

  inv <- read.delim(out_tsv, sep = "\t", stringsAsFactors = FALSE)
  expect_equal(nrow(inv), 2L)
  expect_true(all(c("SeriesDescription", "ProtocolName") %in% names(inv)))
})

test_that("populate-intended-for updates target json from config references", {
  td <- tempfile("dcmtobids-cli-popif-")
  dir.create(td)
  input_dir <- file.path(td, "helper")
  bids_dir <- file.path(td, "bids")
  dir.create(input_dir)
  dir.create(bids_dir)

  writeLines('{"SeriesDescription":"task-rest_bold"}', file.path(input_dir, "001.json"))
  writeLines("dummy", file.path(input_dir, "001.nii.gz"))
  writeLines('{"SeriesDescription":"fmap_pa"}', file.path(input_dir, "002.json"))
  writeLines("dummy", file.path(input_dir, "002.nii.gz"))

  func_dir <- file.path(bids_dir, "sub-01", "func")
  fmap_dir <- file.path(bids_dir, "sub-01", "fmap")
  dir.create(func_dir, recursive = TRUE)
  dir.create(fmap_dir, recursive = TRUE)
  writeLines("dummy", file.path(func_dir, "sub-01_task-rest_bold.nii.gz"))
  writeLines("{}", file.path(func_dir, "sub-01_task-rest_bold.json"))
  writeLines("dummy", file.path(fmap_dir, "sub-01_dir-AP_epi.nii.gz"))
  writeLines("{}", file.path(fmap_dir, "sub-01_dir-AP_epi.json"))

  config <- file.path(td, "config.json")
  writeLines(
    jsonlite::toJSON(
      list(
        descriptions = list(
          list(
            id = "restbold",
            datatype = "func",
            suffix = "bold",
            custom_entities = list("task-rest"),
            criteria = list(SeriesDescription = "*rest*")
          ),
          list(
            datatype = "fmap",
            suffix = "epi",
            custom_entities = list("dir-AP"),
            criteria = list(SeriesDescription = "*fmap*"),
            sidecar_changes = list(IntendedFor = list("restbold"))
          )
        )
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ),
    config
  )

  status <- dcmtobids_main(c(
    "populate-intended-for",
    "--config", config,
    "--input-dir", input_dir,
    "--bids-dir", bids_dir,
    "--participant", "01"
  ))
  expect_equal(status, 0L)

  fmap_json <- jsonlite::fromJSON(file.path(fmap_dir, "sub-01_dir-AP_epi.json"))
  expect_equal(
    fmap_json$IntendedFor,
    "bids::sub-01/func/sub-01_task-rest_bold.nii.gz"
  )

  runs_dir <- file.path(bids_dir, ".dcmtobids", "runs")
  expect_true(dir.exists(runs_dir))
  expect_true(length(list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)) >= 1L)
})

test_that("populate-intended-for supports fail-on-warning", {
  td <- tempfile("dcmtobids-cli-popif-warn-")
  dir.create(td)
  input_dir <- file.path(td, "helper")
  bids_dir <- file.path(td, "bids")
  dir.create(input_dir)
  dir.create(bids_dir)

  writeLines('{"SeriesDescription":"fmap_pa"}', file.path(input_dir, "002.json"))
  writeLines("dummy", file.path(input_dir, "002.nii.gz"))

  fmap_dir <- file.path(bids_dir, "sub-01", "fmap")
  dir.create(fmap_dir, recursive = TRUE)
  writeLines("dummy", file.path(fmap_dir, "sub-01_dir-AP_epi.nii.gz"))
  writeLines("{}", file.path(fmap_dir, "sub-01_dir-AP_epi.json"))

  config <- file.path(td, "config.json")
  writeLines(
    jsonlite::toJSON(
      list(
        descriptions = list(
          list(
            datatype = "fmap",
            suffix = "epi",
            custom_entities = list("dir-AP"),
            criteria = list(SeriesDescription = "*fmap*"),
            sidecar_changes = list(IntendedFor = list("missing-id"))
          )
        )
      ),
      auto_unbox = TRUE,
      pretty = TRUE
    ),
    config
  )

  status <- dcmtobids_main(c(
    "populate-intended-for",
    "--config", config,
    "--input-dir", input_dir,
    "--bids-dir", bids_dir,
    "--participant", "01",
    "--fail-on-warning"
  ))
  expect_equal(status, 1L)
})
