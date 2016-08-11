context("parse log")

test_that("`parse_log` parses data", {

  logfile <-
      gzfile(system.file("extdata", "Plate_160519200001_Well_G07_000006.gz",
                         package = "cplog")
             )

  logfile <- parse_log(logfile)
  expect_equal(1, 1)

})
