library(jsonlite)
library(dplyr)
library(lubridate)

#' Read raw spotify data
#'
#' @param path_to_json 
#'
#' @return data frame
#' @export
#'
#' @examples
read_spotify = function(path_to_json) {
  read_json(path_to_json, simplifyVector = TRUE) |> 
    mutate(endTime = ymd_hm(endTime)) |> 
    rename(timestamp = endTime, artist = artistName, song = trackName, ms_played = msPlayed)
}
