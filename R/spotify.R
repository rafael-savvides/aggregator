#' Read raw spotify data
#'
#' @param path_to_json
#'
#' @return data frame
#' @export
read_spotify = function(path_to_json) {
  jsonlite::read_json(path_to_json, simplifyVector = TRUE) |>
    dplyr::mutate(endTime = lubridate::ymd_hm(endTime)) |>
    dplyr::rename(timestamp = endTime, artist = artistName, song = trackName, ms_played = msPlayed)
}
