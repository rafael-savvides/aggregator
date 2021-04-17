library(jsonlite)
library(dplyr)
library(lubridate)

#' Read raw spotify data
#'
#' @param path_to_json 
#'
#' @return data frame
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
read_spotify = function(path_to_json = readLines("data-raw/path_to_spotify.txt")) {
  read_json(path_to_json, simplifyVector = T) %>% 
    mutate(endTime = ymd_hm(endTime)) %>% 
    rename(timestamp = endTime, artist = artistName, song = trackName, ms_played = msPlayed)
}

spotify = read_spotify()

save(spotify, file="data/spotify.rda")
