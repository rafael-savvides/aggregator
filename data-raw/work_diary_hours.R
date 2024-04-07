library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

#' Read multiple work diaries and join them
#' 
#' Note: This exists in both work_diary_hours.R and work_diary_tasks.R.
#' 
#' @param paths txt file where each line
#' @param read function that reads a work diary
#'
#' @return
#' @export
#'
#' @examples
read_work_diaries = function(paths = readLines("data-raw/paths_to_work_diaries.txt"), read) {
  regex_4digit_number = function(x) regmatches(x, regexpr("\\d\\d\\d\\d", x))
  years = sapply(basename(paths), regex_4digit_number)
  
  diaries = lapply(1:length(paths), function(i) read(paths[i], years[i]))
  diaries_df = do.call(rbind, diaries)
  arrange(diaries_df, date)
}
    

#' Read work diary
#'
#' @param path_to_work_diary .csv downloaded from google sheets
#' @param format one of c("tasks", "hours")
#'
#' @return
#' @export
#'
#' @examples
read_work_diary_hours <- function(path_to_work_diary, year=2020) {
 work_diary = read.csv(path_to_work_diary, skip=1, colClasses = "character") |> 
   as_tibble() |> 
   pivot_longer(-1, names_to = "date") |> 
   mutate(date = as.Date(paste0(year, date), format = "%Y%b.%d")) |> 
   select(date, everything()) |> 
   arrange(date)
 
 work_hours = work_diary |> 
   filter(!str_detect(X, "^\\d\\d?:\\d\\d"), 
          !str_detect(X, "^Extra hours"), 
          X != "") |> 
   pivot_wider(date, X) |> 
   janitor::clean_names() 
 
 cols = c("date", "start", "finish", "extra", "daily_hrs", "hrs_total", "is_workday")
 if (!"is_workday" %in% colnames(work_hours))
   work_hours$is_workday = NA
 
 work_hours[,cols]
}

work_diary_hours = read_work_diaries(read=read_work_diary_hours)

save(work_diary_hours, file="data/work_diary_hours.rda")
