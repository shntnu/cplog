context("parse log")

test_that("`parse_log_dir` parses data", {

  log_dir_root <- system.file("extdata",
                              "051816_3661_Q3Q4_subset/",
                              package = "cplog")

  aws_logs <-
    lapply(
      list.dirs(log_dir_root, recursive = F, full.names = F),
      function(log_dir) {
        parse_log_dir(paste(log_dir_root, log_dir, sep = "/")) %>%
          dplyr::mutate(log_dir = log_dir)
      }
    ) %>%
    dplyr::bind_rows()

  expect_equal(nrow(aws_logs), 3240)

  aws_logs_expected <-
  tibble::frame_data(
    ~log_dir, ~n,
    "Plate_160519200001_Well_A01", 810,
    "Plate_160519200001_Well_A02", 810,
    "Plate_160519200001_Well_A03", 810,
    "Plate_160519200001_Well_A04", 810
  )

  expect_equal(aws_logs %>% dplyr::count(log_dir),
               aws_logs_expected %>% dplyr::mutate(n = as.integer(n)))

})
