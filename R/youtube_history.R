#' Read youtube history
#' @export
#' @import dplyr
read_youtube_history = function(path_to_youtube_history) {
  youtube_raw = jsonlite::fromJSON(readLines(path_to_youtube_history))
  youtube_raw |>
    tidyr::unnest(subtitles, keep_empty = TRUE) |>
    mutate(
      title = stringr::str_remove(title, "^Watched "),
      time = lubridate::with_tz(lubridate::as_datetime(time), tzone = "Europe/Helsinki")
    ) |>
    select(time, title, channel = name) |>
    add_count(channel, name = "channel_views") |>
    group_by(channel) |>
    mutate(channel_unique_videos = length(unique(title))) |>
    ungroup()
}
