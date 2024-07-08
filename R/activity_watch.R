#' Read ActivityWatch data
#'
#' Exported data from ActivityWatch are in json format.
#'
#' @param path_to_json
#'
#' @return
#' @import dplyr
#' @import tidyr
#' @export
read_activity_watch_window <- function(path_to_json) {
  buckets = jsonlite::read_json(path_to_json)[[1]] |>
    tibble(watchers = .) |>
    unnest_wider(watchers)
  window = buckets |>
    filter(stringr::str_detect(id, "window")) |>
    select(events) |>
    unnest_longer(events) |>
    unnest_wider(events) |>
    unnest_wider(data)
  window
}

#' Read ActivityWatch data
#'
#' Exported data from ActivityWatch are in json format.
#'
#' @param path_to_json
#'
#' @return
#' @import dplyr
#' @import tidyr
#' @export
read_activity_watch_web <- function(path_to_json) {
  buckets = jsonlite::read_json(path_to_json)[[1]] |>
    tibble(watchers = .) |>
    unnest_wider(watchers)
  web = buckets |>
    filter(stringr::str_detect(id, "web")) |>
    select(events) |>
    unnest_longer(events) |>
    unnest_wider(events) |>
    unnest_wider(data)
  web
}



#' Read ActivityWatch data
#'
#' Exported data from ActivityWatch are in json format.
#'
#' @param path_to_json
#'
#' @return
#' @import dplyr
#' @import tidyr
#' @export
read_activity_watch_afk <- function(path_to_json) {
  buckets = jsonlite::read_json(path_to_json)[[1]] |>
    tibble(watchers = .) |>
    unnest_wider(watchers)
  afk = buckets |>
    filter(stringr::str_detect(id, "afk")) |>
    select(events) |>
    unnest_longer(events) |>
    unnest_wider(events) |>
    unnest_wider(data)
  afk
}

#' Read ActivityWatch data
#'
#' Exported data from ActivityWatch are in json format.
#'
#' @param path_to_json
#'
#' @return
#' @import dplyr
#' @import tidyr
#' @export
read_activity_watch_vscode <- function(path_to_json) {
  buckets = jsonlite::read_json(path_to_json)[[1]] |>
    tibble(watchers = .) |>
    unnest_wider(watchers)
  vscode = buckets |>
    filter(stringr::str_detect(id, "vscode")) |>
    select(events) |>
    unnest_longer(events) |>
    unnest_wider(events) |>
    unnest_wider(data)
  vscode
}
