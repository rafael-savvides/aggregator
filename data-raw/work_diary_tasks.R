library(dplyr)
library(tidyr)
library(stringr)

#' Read multiple work diaries and join them 
#' 
#' Note: This exists in both work_diary_hours.R and work_diary_tasks.R.
#'
#' @param paths txt file where each line
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
#' @param year year, because it is not in the file.
#'
#' @return
#' @export
#'
#' @examples
read_work_diary_tasks <- function(path_to_work_diary, year = 2020) {
 work_diary = read.csv(path_to_work_diary, skip=1, colClasses="character") |> 
   as_tibble() |> 
   pivot_longer(-1, names_to = "date") |> 
   mutate(date = as.Date(paste0(year, date), format = "%Y%b.%d")) |> 
   select(date, everything()) |> 
   arrange(date)
   
 work_tasks = work_diary |> 
   filter(str_detect(X, "^\\d\\d?:\\d\\d") | str_detect(X, "^Extra hours")) |> 
   rename(time = X) |>
   mutate(time = process_time(time))
 
 arrange(work_tasks, date, time)
}

#' Process time
#' Add leading zero, convert to ordered factor.
#' 
#' @param time character vector of times in 24h HH:MM format.
process_time = function(time) {
  if (any(sapply(time, function(t) grepl("AM|PM", t)))) warning("Time should be in 24h HH:MM, without AM/PM.")
  time = ifelse(nchar(time) == 4, paste0("0", time), time) # Add leading zero, so 7:00 -> 07:00.
  time = factor(time, ordered = TRUE)
  time
}

work_diary_tasks = read_work_diaries(read=read_work_diary_tasks)

save(work_diary_tasks, file="data/work_diary_tasks.rda")
