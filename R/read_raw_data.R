library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(lubridate)
library(googlesheets4)
library(rvest)
library(readxl)

#' Read all datasets
#' 
#' Reads all datasets listed in [load_data_dictionary()].
#'
#' @return list of data frames
#' @export
#'
#' @examples
read_all <- function() {
  data_dict = load_data_dictionary()
  data_list = vector("list", nrow(data_dict))
  names(data_list) = data_dict$name
  for (i in 1:nrow(data_dict)) {
    cat(paste0("Reading ", data_dict$name[i], "...\n"))
    data_list[[i]] = load_(data_dict$name[i])
  }
  cat(paste0("Total size: ", format(object.size(data_all), units="Mb")), "\n")
  data_list
}

#' Read and preprocess S-pankki csv to data frame
#'
#' @param path_to_csv location of csv file
#'
#' @return data frame
#' @export
read_spankki <- function(path_to_csv) {
  make_numeric <- function(s) {
    x <- gsub(",", ".", s) # Commas to periods
    x <- gsub(" ", "", x) # Remove spaces (thousands delimiter). Not present in csv anymore.
    as.numeric(x)
  }
  day_month_year2date <- function(x) as.Date(as.character(x), tryFormats = c("%d.%m.%Y"))
  book <- read.csv(file = path_to_csv,
                   sep=";",
                   encoding = "UTF-8",
                   col.names = c("date_reported", "date_paid", "amount", "type", "payer", "receiver", "receiver_iban", "receiver_bic",
                                 "viite_number", "message", "archive_number"),
                   colClasses = c("character", "character", "character", "character", "character", "character", "character", "character",
                                  "character", "character", "character"))
  
  book %>%
    mutate(amount = make_numeric(amount),
           date_reported = day_month_year2date(date_reported),
           date = day_month_year2date(date_paid)) %>%
    dplyr::select(date, amount, receiver, payer, message, type, receiver_iban)
}

#' Read Danske bank data
#'
#' @param path_to_csv
#'
#' @return
#' @export
#'
#' @examples
read_danske <- function(path_to_csv) {
  day_month_year2date <- function(x) as.Date(as.character(x), tryFormats = c("%d.%m.%Y"))
  read.csv2(path_to_csv,
            col.names = c("date", "receiver_payer", "amount", "balance", "status", "check"),
            stringsAsFactors = FALSE) %>%
    mutate(payer = if_else(amount > 0, as.character(receiver_payer), "Rafael Savvides"),
           receiver = if_else(amount < 0, as.character(receiver_payer), "Rafael Savvides"),
           date = day_month_year2date(date), 
           amount = as.numeric(amount)) %>%
    select(date,  amount, receiver, payer) %>%
    mutate(receiver = str_replace(receiver, " +\\)\\)\\)\\)", ""))
}


#' Read youtube watch history
#'
#' @param path_to_youtube 
#'
#' @return
#' @export
#'
#' @examples
read_youtube <- function(path_to_youtube) {
  fromJSON(path_to_youtube) %>% 
    as_tibble() %>% 
    mutate(title = str_remove(title, "^Watched ")) %>% 
    mutate(timestamp = strptime(time, format = "%Y-%m-%dT%H:%M:%S")) %>% 
    select(title, url=titleUrl, timestamp) 
}

# TODO ####

read_google_calendar <- function(path_to_google_calendar) {
  # How to read .ics files?  
  # See https://stackoverflow.com/questions/43573982/how-to-import-ical-ics-file-in-r
  path_to_google_calendar = readLines("data/paths_to_raw_data/path_to_google_calendar.txt")
  x = readLines(path_to_google_calendar, warn = FALSE)
  
  # Lines that start with space are continuations from previous line.
  for (i in seq.int(length(x), 2)) {
    if (grepl("^\\s", x[i])) {
      x[i-1] = paste0(x[i-1], substring(x[i], 2)) # Remove indentation
      x[i] = NA
    }
  }
  x = x[!is.na(x)]
  field_regex = "^[\\w-;=/]+:" # ATTENDEE and ORGANIZER fields are not parsed
  tibble(x=x, 
         is_begin_event = str_detect(x, "^BEGIN:VEVENT"), 
         is_end_event = str_detect(x, "^END:VEVENT"), 
         id = cumsum(is_begin_event), 
         has_colon = str_detect(x, field_regex)) %>% 
    mutate(field = ifelse(has_colon, str_extract(x, field_regex), NA), 
           content = ifelse(has_colon, str_extract(x, ":.+"), NA)) %>% 
    filter(!is.na(field)) %>% 
    select(field, content, id) %>% 
    group_by(id) %>% 
    pivot_wider(names_from=field, values_from=content)
    summarize(text = list(x)) #TODO
  parse_entry = function(x) {
    #TODO
  }
  
  df
}


read_phone_activity <- function(path_to_phone_activity) {
  # I dont know where this came from but it is as detailed as I wanted.
  phone = read.csv(path_to_phone_activity, stringsAsFactors = FALSE) %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(duration = (end_time - start_time) / 1e3, 
           display_name = as.factor(display_name), 
           start_time = as.POSIXct(formatted_start_time, format="%H:%M:%S (%d-%B-%Y)"), 
           end_time = as.POSIXct(formatted_end_time, format="%H:%M:%S (%d-%B-%Y)")) %>% 
    select(-package_name, -usage, -formatted_start_time, -formatted_end_time)
  
  phone 
  
}

