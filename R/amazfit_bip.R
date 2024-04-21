library(DBI)
library(RSQLite)
library(dplyr)

#' Read data from Xiaomi Amazfit Bip smartwatch
#' 
#'
#' @param path_to_db path to SQLite database
#'
#' @return
#' @export
#'
#' @examples
read_amazfit_bip <- function(path_to_db) {
  con = dbConnect(SQLite(), path_to_db)
  tbl = dbReadTable(con, "MI_BAND_ACTIVITY_SAMPLE") |> 
    as_tibble() |> 
    mutate(TIMESTAMP = lubridate::as_datetime(TIMESTAMP), 
           HEART_RATE = if_else(HEART_RATE > 250 | HEART_RATE == -1 | HEART_RATE < 10, 
                                NA_integer_, HEART_RATE))|> 
    select(TIMESTAMP, RAW_INTENSITY, STEPS, RAW_KIND, HEART_RATE) |> 
    rename(timestamp = TIMESTAMP, raw_intensity = RAW_INTENSITY, steps = STEPS, 
           raw_kind = RAW_KIND, heart_rate = HEART_RATE)
  dbDisconnect(con)
  tbl
}
