#' Load data frame with data information
#' 
#' Keep consistent names for data sets. For every X in `data_names`:
#' - path to raw data is named `path_to_X.txt`
#' - clean data is named `X.csv`
#' - functions are named `read_X`
#'
#' @param path_to_paths_to_raw path to directory that contains paths to raw data in the form `path_to_*.txt`
#' @param path_to_clean path to directory that contains clean data csv files
#'
#' @return
#' @export
#'
#' @examples
load_data_dictionary <- function(path_to_paths_to_raw = "data/paths_to_raw_data", 
                                 path_to_clean = "data/clean", 
                                 fail_silently = FALSE) {
  data_names = c("activity_watch_afk", 
                 "activity_watch_web", 
                 "activity_watch_window", 
                 "activity_watch_vscode", 
                 "amazfit_bip", 
                 "bank",
                 "daylio",
                 "donelist",
                 "facebook_chat", 
                 "google_keep",
                 "kindle_clippings", 
                 "lastfm", 
                 "msn",
                 "phone_recordings", 
                 #"recordings_notes", 
                 "spotify", 
                 "telegram",
                 "whatsapp", 
                 "work_diary_hours", 
                 "work_diary_tasks") 
  path_to_raw = character(length(data_names))
  for (i in seq_along(data_names)) {
    name = data_names[i]
    #functions[[i]] = get(paste0("read_", name)) #TODO Would this work?
    
    if (grepl("activity_watch", name))
      name = "activity_watch"
    if (grepl("bank", name))
      name = "bank_dir"
    if (grepl("msn", name))
      name = "msn_dir"
    if (grepl("google_keep", name))
      name = "google_keep_dir"
    if (grepl("work_diary", name))
      name = "work_diary"
    if (grepl("facebook_chat", name))
      name = "facebook_dir"
    path = file.path(path_to_paths_to_raw, paste0("path_to_", name, ".txt"))
    if (file.exists(path)) {
      path_to_raw[i] = readLines(path)
    } else {
      if (!fail_silently) {
        stop(path, " does not exist.")
      } else {
        #TODO clean this up. I want to be able to silently ignore missing files, and return only the ones that exist.
        path_to_raw[i] = NA
      }
    } 
  }
  
  functions = vector("list", length(data_names)) 
  for (i in seq_along(data_names)) {
    name = data_names[i]
    functions[[i]] = get(paste0("read_", name))
  }
 
  tibble::tibble(name = data_names, 
                 path_to_clean = file.path(path_to_clean, paste0(data_names, ".csv")),
                 path_to_raw = path_to_raw,
                 prepare_raw = functions)  
}

#' Load clean data
#' 
#' If clean data does not exist, then opens raw data and saves clean data to `data/`.
#'
#' @param path_to_clean 
#' @param path_to_raw 
#' @param prepare_raw_data 
#'
#' @return
#' @export
#'
#' @examples
load_clean_data <- function(path_to_clean, path_to_raw, prepare_raw_data, 
                            force_overwrite=FALSE) {
  has_changed = file.info(path_to_raw)$mtime > file.info(path_to_clean)$mtime
  if (!file.exists(path_to_clean) | has_changed | force_overwrite) {
    cat(paste0("Preparing ", path_to_raw, " ...\n"))
    data = prepare_raw_data(path_to_raw)
    cat(paste0("Writing ", path_to_clean, " ...\n"))
    write.csv(data, path_to_clean, row.names = FALSE)
  } else {
    data = read.csv(path_to_clean, stringsAsFactors = FALSE)
  }
  data
}

#' Wrapper for load_clean_data
#'
#' @param name one of `data_names` in [load_data_dictionary]
#'
#' @return
#' @export
#'
#' @examples
load_ <- function(name, data_dict = load_data_dictionary(), force_overwrite=FALSE) {
  d = data_dict[data_dict$name == name, ]
  load_clean_data(d$path_to_clean, d$path_to_raw, d$prepare_raw[[1]], force_overwrite) %>% 
    as_tibble()
}
