library(dplyr)
library(tidyr)
library(janitor)

#' Read work diary
#'
#' @param path_to_work_diary 
#' @param format one of c("tasks", "hours")
#'
#' @return
#' @export
#'
#' @examples
read_work_diary_hours <- function(path_to_work_diary = readLines("data-raw/path_to_work_diary.txt")) {
 work_diary = read.csv(path_to_work_diary, skip=1) %>% 
   as_tibble() %>% 
   pivot_longer(-1, names_to = "date") %>% 
   mutate(date = as.Date(paste0(2020, date), format = "%Y%b.%d")) %>% 
   select(date, everything()) %>% 
   arrange(date)
 
 work_hours = work_diary %>% 
   filter(!str_detect(X, "^\\d\\d?:\\d\\d"), 
          !str_detect(X, "^Extra hours"), 
          X != "") %>% 
   pivot_wider(date, X) %>% 
   janitor::clean_names()
   
work_hours
}

work_diary_hours = read_work_diary_hours()

save(work_diary_hours, file="data/work_diary_hours.rda")
