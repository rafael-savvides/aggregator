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
read_bank <- function(path_to_bank_dir = readlines("data-raw/path_to_bank_dir.txt")) {
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

bank = read_bank()

save(bank, "data/bank.rda")
