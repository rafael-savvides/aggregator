library(dplyr)
library(tidyr)
library(stringr)

#' Read work diary
#'
#' @param path_to_work_diary 
#' @param format one of c("tasks", "hours")
#'
#' @return
#' @export
#'
#' @examples
read_work_diary_tasks <- function(path_to_work_diary = readLines("data-raw/path_to_work_diary.txt"), which=c("tasks", "hours")) {
 work_diary = read.csv(path_to_work_diary, skip=1) %>% 
   as_tibble() %>% 
   pivot_longer(-1, names_to = "date") %>% 
   mutate(date = as.Date(paste0(2020, date), format = "%Y%b.%d")) %>% 
   select(date, everything()) %>% 
   arrange(date)
   
 work_tasks = work_diary %>% 
   filter(str_detect(X, "^\\d\\d?:\\d\\d") | str_detect(X, "^Extra hours")) %>% 
   rename(time = X)
 
 work_tasks
}

work_diary_tasks = read_work_diary_tasks()

save(work_diary_tasks, file="data/work_diary_tasks.rda")
