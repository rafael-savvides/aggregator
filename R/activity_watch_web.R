library(jsonlite)
library(dplyr)
library(tidyr)

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
read_activity_watch_web <- function(path_to_json) {
    buckets = jsonlite::read_json(path_to_json)[[1]] |> 
    tibble(watchers = .) |> 
    unnest_wider(watchers)
    web = buckets |> 
      filter(str_detect(id, "web")) |> 
      select(events) |> 
      unnest_longer(events) |> 
      unnest_wider(events) |> 
      unnest_wider(data)
    web
}

