#' Parse log file
#'
#' @param logfile ...
#' @param ... arguments passed to parse operation
#'
#' @return parsed log
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
parse_log <- function(logfile, ...) {

  tmp_file <- tempfile()

  readLines(logfile) %>%
    stringr::str_subset("module") %>%
    stringr::str_subset("Image #") %>%
    sapply(function(ln)
        paste0(substr(ln, 1, 36),
               substr(ln, 46,49),
               substr(ln, 36, 44), ":",
               substr(ln, 45, nchar(ln))
               )
    ) %>%
    writeLines(tmp_file)

  logdf <- readr::read_log(tmp_file)

  logdf %<>%
    dplyr::mutate(runtime = as.numeric(X15)) %>%
    dplyr::rename(module = X12) %>%
    dplyr::mutate(image_number = sub(",", "", X10)) %>%
    tidyr::unite(col = date, X3, X4, X5, X6, sep = " ") %>%
    dplyr::mutate(date = lubridate::parse_date_time(date, "%b %d %Y %h:%m:%s")) %>%
    dplyr::rename(module_number = X14) %>%
    dplyr::select(date, image_number, module, module_number, runtime) %>%
    dplyr::mutate(runtime_calc = lead(date)-date) %>%
    dplyr::arrange()

  file.remove(tmp_file)

  logdf
}
