test_that("criteria_matches supports glob and float comparisons", {
  data <- list(
    SeriesDescription = "task-rest_bold",
    EchoTime = 0.00738,
    ImageType = list("ORIGINAL", "PRIMARY", "M", "ND")
  )

  expect_true(dcmtobids:::criteria_matches(data, list(SeriesDescription = "*bold"), "fnmatch", TRUE))
  expect_true(dcmtobids:::criteria_matches(data, list(EchoTime = list(btwe = list(0.007, 0.008))), "fnmatch", TRUE))
  expect_true(dcmtobids:::criteria_matches(
    data,
    list(ImageType = list("ORIGINAL", "PRIMARY", "M", "ND")),
    "fnmatch",
    TRUE
  ))
  expect_false(dcmtobids:::criteria_matches(data, list(SeriesDescription = "*dwi*"), "fnmatch", TRUE))
})
