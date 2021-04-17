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
read_activity_watch_window <- function(path_to_json = readlines("data-raw/path_to_activity_watch.txt")) {
    buckets = jsonlite::read_json(path_to_json)[[1]] %>% 
    tibble(watchers = .) %>% 
    unnest_wider(watchers)
    window = buckets %>% 
      filter(str_detect(id, "window")) %>% 
      select(events) %>% 
      unnest_longer(events) %>% 
      unnest_wider(events) %>% 
      unnest_wider(data)
    window
}

activity_watch_window = read_activity_watch_window()

save(activity_watch_window, "data/activity_watch_window.rda")
