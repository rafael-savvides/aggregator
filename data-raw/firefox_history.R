library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)

#' Read Mozilla Firefox history
#'
#' @param path_to_firefox_history_db path to places.sqlite
#'
#' @return
#' @export
#'
#' @examples
read_firefox_history <- function(path_to_firefox_history_db = readlines("data-raw/path_to_firefox_history.txt")) {
  con = DBI::dbConnect(RSQLite::SQLite(), path_to_firefox_history_db)
  history = dbReadTable(con, "moz_historyvisits") %>% 
    as_tibble()
  
  places = dbReadTable(con, "moz_places") %>% 
    as_tibble()
  
  df = left_join(history, places, by=c("place_id"="id")) %>% 
    select(place_id, visit_date, url, title, origin_id, visit_count) %>% 
    # UNIX timestamp is in microseconds
    mutate(visit_date = as.POSIXct(visit_date / 1e6, origin="1970-01-01"), 
           domain = urltools::domain(url), 
           title = ifelse(is.na(title), "", title)) 
  dbDisconnect(con)
  df
}

firefox_history = read_firefox_history()

save(firefox_history, "data/firefox_history.rda")
