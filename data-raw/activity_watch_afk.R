library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)

#' Read ActivityWatch data
#' 
#' Exported data from ActivityWatch are in json format.
#'
#' @param path_to_json 
#'
#' @return
#' @export
#'
#' @examples
read_activity_watch_afk <- function(path_to_json = readLines("data-raw/path_to_activity_watch.txt")) {
    buckets = jsonlite::read_json(path_to_json)[[1]] |> 
    tibble(watchers = .) |> 
    unnest_wider(watchers)
    afk = buckets |> 
      filter(str_detect(id, "afk")) |> 
      select(events) |> 
      unnest_longer(events) |> 
      unnest_wider(events) |> 
      unnest_wider(data)
    afk
}

activity_watch_afk = read_activity_watch_afk()
save(activity_watch_afk, file="data/activity_watch_afk.rda")


activity_watch_work_afk = read_activity_watch_afk(path_to_json=readLines("data-raw/path_to_activity_watch_work.txt"))
save(activity_watch_work_afk, file="data/activity_watch_work_afk.rda")
