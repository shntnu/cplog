context("parse log")

test_that("`parse_log` parses data", {

  log_dir_root <- system.file("extdata",
                              "051816_3661_Q3Q4_subset/68584110-07ce-41d5-a6d6-058d7a0c8065/",
                              package = "cplog")

  logfile <- parse_log(logfile)
  aws_logs <-
    lapply(
      list.dirs(log_dir_root, recursive = F, full.names = F),
      function(log_dir) {
        logdf <- parse_log_dir(paste0(log_dir_root, log_dir, sep = "/"))
        logdf %>%
          mutate(log_dir = log_dir)
      }
    ) %>%
    bind_rows()

  aws_logs_expected <-
  tibble::frame_data(
    ~log_dir, ~n,
    "Plate_160519200001_Well_A01", 810,
    "Plate_160519200001_Well_A02", 810,
    "Plate_160519200001_Well_A03", 810,
    "Plate_160519200001_Well_A04", 810
  )

  testthat::expect_equal(aws_logs %>% count(log_dir), aws_logs_expected %>% mutate(n = as.integer(n)))

})
