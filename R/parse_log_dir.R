#' Parse a directory of log files
#'
#' @param parse_log_dir ...
#' @param ... arguments passed to parse operation
#'
#' @return parsed log
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
parse_log_dir <- function(log_dir, ...) {

  file_list <- list.files(log_dir, full.names = T, pattern = "*.gz")

  if (length(file_list) == 0) {
    return(data.frame())
  }

  tmp_file <- tempfile()
  sapply(
    file_list,
    function(f) {
      fp <- gzfile(f)
      rl <- readLines(fp)
      close(fp)
      rl
    }
    ) %>%
    unlist() %>%
    writeLines(tmp_file)

  parse_log(tmp_file)
}
