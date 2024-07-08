#' Read raw lastfm data
#'
#' @param path_to_csv
#'
#' @return data frame
#' @export
#' @import dplyr
read_lastfm = function(path_to_csv) {
  read.csv(
    path_to_csv,
    header = FALSE,
    col.names = c("artist", "album", "song", "timestamp"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(timestamp = lubridate::dmy_hm(timestamp)) |>
    dplyr::filter(lubridate::year(timestamp) > 1990) |>
    dplyr::select(timestamp, artist, song, album)
}
