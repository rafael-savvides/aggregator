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
read_activity_watch_vscode <- function(path_to_json = readLines("data-raw/path_to_activity_watch.txt")) {
    buckets = jsonlite::read_json(path_to_json)[[1]] %>% 
    tibble(watchers = .) %>% 
    unnest_wider(watchers)
    vscode = buckets %>% 
      filter(str_detect(id, "vscode")) %>% 
      select(events) %>% 
      unnest_longer(events) %>% 
      unnest_wider(events) %>% 
      unnest_wider(data)
    vscode
}

activity_watch_vscode = read_activity_watch_vscode()

save(activity_watch_vscode, file="data/activity_watch_vscode.rda")
