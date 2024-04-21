library(tidyr)
library(dplyr)
library(lubridate)
library(jsonlite)

read_youtube_history = function(path_to_youtube_history) {
  youtube_raw = fromJSON(readLines(path_to_youtube_history))
  youtube_raw |> 
    unnest(subtitles, keep_empty=T) |> 
    mutate(title = str_remove(title, "^Watched "), 
           time = with_tz(as_datetime(time), tzone = "Europe/Helsinki")) |> 
    select(time, title, channel=name) |> 
    add_count(channel, name = "channel_views") |> 
    group_by(channel) |> 
    mutate(channel_unique_videos = length(unique(title))) |> 
    ungroup()
}
