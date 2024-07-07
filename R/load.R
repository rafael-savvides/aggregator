
#' List of available functions for reading data
#' @export
available_fns = function() {
  list(
    activity_watch_afk = read_activity_watch_afk,
    activity_watch_vscode = read_activity_watch_vscode,
    activity_watch_web = read_activity_watch_web,
    activity_watch_window = read_activity_watch_window,
    amazfit_bip = read_amazfit_bip,
    app_usage = read_app_usage,
    bank = read_bank,
    daylio = read_daylio,
    donelist = read_donelist,
    dota_matches = read_dota_matches,
    facebook_chat = read_facebook_chat,
    firefox_history = read_firefox_history,
    google_keep = read_google_keep,
    kindle_clippings = read_kindle_clippings,
    lastfm = read_lastfm,
    msn = read_msn,
    note_network = read_note_network,
    nutrilio = read_nutrilio,
    phone_recordings = read_phone_recordings,
    recordings_notes_phone = read_recordings_notes,
    spotify = read_spotify,
    telegram = read_telegram,
    whatsapp = read_whatsapp,
    work_diary_hours = read_work_diary_hours,
    work_diary_tasks = read_work_diary_tasks,
    youtube_history = read_youtube_history
  )
}

#' List of datasets defined in the config file
#' @export
available_data = function() {
  names(load_config()$datasets)
}

#' Load cached dataset
#'
#' @param name 
#'
#' @export
load_data = function(name = "") {
  config = load_config()
  cache_path = config$cache_path
  if (name == "") {
    cached_files = list.files(cache_path)
    if (length(cached_files) == 0 ) {
      print("Cache is empty.")
    } else {
      print("Cached datasets: ")
      print(cached_files)
    }
  } else {
    path = file.path(cache_path, paste0(name, ".rds"))
    if (file.exists(path)) {
      return(readRDS(path))
    } else {
      warning(sprintf("File doesn't exist: %s", path))
      return(invisible(NULL))
    }
  }
}

#' Update cache for datasets
#'
#' @param datasets_to_update 
#'
#' @export
update_cache = function(datasets_to_update = "", force_overwrite=FALSE) {
  update_dataset = function(path_raw, read_fn, path_cached) {
    not_cached = !file.exists(path_cached)
    raw_updated = file.mtime(path_raw) > file.mtime(path_cached)
    if (not_cached || raw_updated || force_overwrite) {
      #TODO Check what should happen when path is not a file.
      df = read_fn(path_raw)
      if (!dir.exists(dirname(path_cached))) {
        dir.create(dirname(path_cached), recursive=TRUE)
      }
      saveRDS(df, file=path_cached)
      cat(sprintf("Cached to %s.", path_cached))
    } else {
      cat(sprintf("Already cached at %s.", path_cached))
    }
  }
  config = load_config()
  if (datasets_to_update == "") {
    datasets_to_update = names(config$datasets)
  }
  n = length(datasets_to_update)
  for (i in seq_len(n)) {
    name = datasets_to_update[i]
    cat(sprintf("%d/%d. %s. ", i, n, name))
    update_dataset(
      path_raw = config$datasets[[name]][["path"]],
      read_fn = available_fns()[[config$datasets[[name]][["type"]]]],
      path_cached = file.path(config$cache_path, paste0(name, ".rds"))
    )
    cat("\n")
  }
}
