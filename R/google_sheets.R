library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readxl)

#' Read multiple work diaries and join them
#' 
#' @param paths txt file where each line
#' @param read function that reads a work diary
#'
#' @return
#' @export
#'
#' @examples
read_work_diaries = function(paths, read) {
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

#' Read xlsx with recording notes
#' 
#'
#' @param path_to_recording_notes 
#' @param sheet 
#'
#' @return
#' @export
#' @examples
read_recordings_notes <- function(path_to_recording_notes, sheet=1) {
  col_types = NULL
  if (sheet == "phone")
    col_types = c("text", "numeric", "text", "text", "text", "text", "numeric")
  
  read_excel(path_to_recording_notes, sheet=sheet, col_types = col_types)
}

#' Read donelist
#'
#' @param path_to_donelist 
#'
#' @return
#' @export
#'
#' @examples
read_donelist <- function(path_to_donelist) {
  donelist = read.csv(path_to_donelist, stringsAsFactors = FALSE) |> 
    mutate(date = as.Date(paste0(2020, Date), format="%Y%b%d")) |> 
    select(-Week, -Day, -Date) |> 
    select(date, everything())
  
  donelist
}
