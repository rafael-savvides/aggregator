library(dplyr)
library(tidyr)

#' Read Daylio data
#'
#' @param path_to_daylio 
#' @param format "long" (default) or wide. Long is needed for plotting.
#'
#' @return data frame
#' @export
#'
#' @examples
read_daylio <- function(path_to_daylio = readLines("data-raw/path_to_daylio.txt"), format = c("long", "wide")) {
  
  daylio = read.csv(path_to_daylio, stringsAsFactors = FALSE, 
                    fileEncoding="UTF-8-BOM") # Needed else first column has `ï..` prefix. 
  # See https://stackoverflow.com/questions/24568056/rs-read-csv-prepending-1st-column-name-with-junk-text/24568505
  
  daylio_long = daylio %>% 
    as_tibble() %>% 
    separate_rows(activities, sep = " \\| ") %>% 
    mutate(timestamp = as.POSIXct(paste(full_date, time))) %>% 
    select(timestamp, mood, activities, note) 
  
  if (format == "wide") daylio else daylio_long
}

daylio = read_daylio()

save(daylio, file="data/daylio.rda")
