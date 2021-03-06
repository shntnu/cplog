## ----load, warning=FALSE, message=FALSE----------------------------------
library(cplog)
library(dplyr)
library(ggplot2)
library(magrittr)
library(stringr)
library(tidyr)

  
log_dir_root <- system.file("extdata", 
                            "051816_3661_Q3Q4_subset/", 
                            package = "cplog")

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

aws_logs %<>% rename(group = log_dir)

## ----plot, fig.width=5---------------------------------------------------
plot_runtimes <- function(df) {
  df %<>%
    group_by(group) %>%     
    summarize(calculated = sum(runtime_calc, na.rm = T), 
              reported = sum(runtime),
              date = min(date)) %>%
    select(date, calculated, reported) %>%
    gather(estimate_type, runtime, -date)

  p <-  
    ggplot(df, aes(date, runtime, color = estimate_type)) + 
      geom_line(alpha = 0.9) +
      geom_point(alpha = 0.3) +
      xlab("time") +
      ggtitle("Runtimes for each group of images")

  print(p)
}

aws_logs %>% plot_runtimes()

aws_logs  %>% filter(module != "NamesAndTypes") %>% plot_runtimes()


