library(dplyr)
library(purrr)
library(tidyr)

#' Read bank data
#'
#' @param path_to_bank_dir path to directory that contains csv files starting 
#' with "danske" and "spankki"
#'
#' @return data frame
#' @export
#'
#' @examples
read_bank <- function(path_to_bank_dir = readLines("data-raw/path_to_bank_dir.txt")) {
  danske_files = list.files(path_to_bank_dir, "danske.+\\.csv", full.names = TRUE)
  spankki_files = list.files(path_to_bank_dir, "spankki.+\\.csv", full.names = TRUE)
  danske = map_df(danske_files, read_danske) %>% 
    mutate(bank = "Danske")
  spankki = map_df(spankki_files, read_spankki)%>% 
    mutate(bank = "S-Bank")
  bank = full_join(danske, spankki) %>% 
    select(-bank, everything(), bank) %>% 
    arrange(desc(date))
  bank
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


bank = read_bank()

save(bank, file="data/bank.rda")
