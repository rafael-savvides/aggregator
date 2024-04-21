library(dplyr)
library(lubridate)

#' Read raw lastfm data
#'
#' @param path_to_csv 
#'
#' @return data frame
#' @export
#'
#' @examples
read_lastfm = function(path_to_csv) {
  read.csv(path_to_csv, header = FALSE, col.names = c("artist", "album", "song", "timestamp"), stringsAsFactors = F) |> 
    mutate(timestamp = dmy_hm(timestamp)) |> 
    filter(year(timestamp) > 1990) |> 
    select(timestamp, artist, song, album)
}
