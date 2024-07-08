#' Initialize config file with example structure.
#' @export
init_config = function(config_path = get_config_path()) {
  if (is.null(config_path)) {
    config_path = file.path(tools::R_user_dir("aggregator", which = "config"), "config.json")
    message("Copy the config path below to paste it to .Renviron.")
    message(sprintf("R_AGGREGATOR_CONFIG_PATH=%s", config_path))
    message("Open .Renviron? [any key]/n")
    answer = readline()
    if (answer != "n") {
      usethis::edit_r_environ()
    }
    # Alternatively, set option in .Rprofile
    # options(aggregator.config_path = config_path)
  }

  if (file.exists(config_path)) {
    message(sprintf("Config file exists at %s.", config_path))
  } else {
    cache_path = get_cache_path()
    config_example = list(
      datasets = list(example = list(
        path = "",
        type = "telegram"
      )),
      cache_path = cache_path
    )
    dir.create(dirname(config_path), recursive = TRUE)
    jsonlite::write_json(config_example, config_path, pretty = TRUE, auto_unbox = TRUE)
    message(sprintf("Created config file: %s.", config_path))
  }
}

#' Load config file
#'
#' @param path path to config file
load_config = function(config_path = get_config_path()) {
  if (is.null(config_path)) {
    stop("config_path is NULL. Run init_config().")
  }
  jsonlite::read_json(config_path)
}

#' Open config file in RStudio.
#'
#' @param path path to config file
#' @export
edit_config = function(path = get_config_path()) {
  usethis::edit_file(path)
}

#' Get path to config file.
#' Checks in order: environment variable (`R_AGGREGATOR_CONFIG_PATH`), R option `aggregator.config_path`.
#' Returns NULL if config path is not set.
get_config_path = function() {
  path = Sys.getenv("R_AGGREGATOR_CONFIG_PATH")
  if (is.null(path) | path == "") {
    path = getOption("aggregator.config_path")
  }
  if (!is.null(path) && path == "") {
    path = NULL
  }
  path
}

#' Get path to cached data files
get_cache_path = function() {
  tools::R_user_dir("aggregator", which = "data")
}
