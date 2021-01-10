library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(lubridate)
library(googlesheets4)
library(rvest)


#' Read all datasets
#' 
#' Reads all datasets listed in [load_data_dictionary()].
#'
#' @return list of data frames
#' @export
#'
#' @examples
read_all <- function() {
  data_dict = load_data_dictionary()
  data_list = vector("list", nrow(data_dict))
  names(data_list) = data_dict$name
  for (i in 1:nrow(data_dict)) {
    cat(paste0("Reading ", data_dict$name[i], "...\n"))
    data_list[[i]] = load_(data_dict$name[i])
  }
  cat(paste0("Total size: ", format(object.size(data_all), units="Mb")), "\n")
  data_list
}

#' Read ActivityWatch data
#' 
#' Exported data from ActivityWatch are in json format, and contain all "buckets". 
#'
#' @param path_to_json 
#' @param which one of "all", "afk", "window", "web", "vscode"
#'
#' @return
#' @export
#'
#' @examples
read_activity_watch <- function(path_to_json, 
                                which="all") {
  
  buckets = jsonlite::read_json(path_to_json)[[1]] %>% 
    tibble(watchers = .) %>% 
    unnest_wider(watchers)
  
  if (which == "all") {
    buckets
  } else if (which == "afk") {
    afk = buckets %>% 
      filter(str_detect(id, "afk")) %>% 
      select(events) %>% 
      unnest_longer(events) %>% 
      unnest_wider(events) %>% 
      unnest_wider(data)
    afk
  } else if (which == "window") {
    window = buckets %>% 
      filter(str_detect(id, "window")) %>% 
      select(events) %>% 
      unnest_longer(events) %>% 
      unnest_wider(events) %>% 
      unnest_wider(data) 
    window
  } else if (which == "web") {
    web = buckets %>% 
      filter(str_detect(id, "web"))%>% 
      select(events) %>% 
      unnest_longer(events) %>% 
      unnest_wider(events) %>% 
      unnest_wider(data) 
    web
  } else if (which == "vscode") {
    vscode = buckets %>% 
      filter(str_detect(id, "vscode"))%>% 
      select(events) %>% 
      unnest_longer(events) %>% 
      unnest_wider(events) %>% 
      unnest_wider(data) 
    vscode
  }
}

read_activity_watch_afk = function(path_to_json) read_activity_watch(path_to_json=path_to_json, which="afk")
read_activity_watch_window = function(path_to_json) read_activity_watch(path_to_json=path_to_json, which="window")
read_activity_watch_web = function(path_to_json) read_activity_watch(path_to_json=path_to_json, which="web")
read_activity_watch_vscode = function(path_to_json) read_activity_watch(path_to_json=path_to_json, which="vscode")

#' Read data from Xiaomi Amazfit Bip smartwatch
#' 
#'
#' @param path_to_db path to SQLite database
#'
#' @return
#' @export
#'
#' @examples
read_amazfit_bip <- function(path_to_db) {
  con = dbConnect(SQLite(), path_to_db)
  tbl = dbReadTable(con, "MI_BAND_ACTIVITY_SAMPLE") %>% 
    as_tibble() %>% 
    mutate(TIMESTAMP = lubridate::as_datetime(TIMESTAMP), 
           HEART_RATE = if_else(HEART_RATE > 250 | HEART_RATE == -1 | HEART_RATE < 10, 
                                NA_integer_, HEART_RATE))%>% 
    select(TIMESTAMP, RAW_INTENSITY, STEPS, RAW_KIND, HEART_RATE) %>% 
    rename(timestamp = TIMESTAMP, raw_intensity = RAW_INTENSITY, steps = STEPS, 
           raw_kind = RAW_KIND, heart_rate = HEART_RATE)
  dbDisconnect(con)
  tbl
}

#' Read Kindle clippings into data frame
#' 
#' Reads Kindle clippings into tidy data frame. 
#' * Removes duplicates and almost duplicates (from re-highlighting a passage).
#' * Takes author name from content of parentheses in the title or filename. May be incorrect.
#'
#' @param path_to_txt filename. Location of "My Clippings.txt" file from Kindle
#'
#' @return data frame of title, author, body, location, date
#' @export
#' @md
#'
#' @examples
#' clippings = read_kindle_clippings("Clippings.txt")
#' print_clippings(clippings, "Thinking, Fast and Slow")
read_kindle_clippings <- function(path_to_txt) {
  clippings_raw = readLines(path_to_txt, encoding = "UTF-8")
  
  clippings = data.frame(raw = clippings_raw, stringsAsFactors = F) %>% 
    mutate(is_sep = str_detect(raw, "=========="), 
           is_title_author = lag(is_sep, default=T), 
           is_location_date = lag(is_title_author, default=F), 
           is_body = !is_sep & !is_title_author & !is_location_date, 
           id = cumsum(is_title_author)) %>% 
    mutate(title_author = map2(raw, is_title_author, function(x, y) x[y]), 
           location_date = map2(raw, is_location_date, function(x, y) x[y]), 
           body = map2(raw, is_body, function(x, y) x[y])) %>% 
    select(id, title_author, location_date, body) %>% 
    group_by(id) %>% 
    summarise(title_author = paste0(unlist(title_author), collapse=""), 
              location_date = paste0(unlist(location_date), collapse=""), 
              body = paste0(unlist(body), collapse="")) %>% 
    filter(!str_detect(location_date, "- Your Bookmark on ")) %>%
    mutate(location_date = str_remove(location_date, "- Your Highlight on "),
           location = str_remove(location_date, " \\|.*"),
           date = str_extract(location_date, "\\| Added on.*"),
           date = str_remove(date, "\\| Added on "),
           date = lubridate::parse_date_time(date, "A, b! d!, Y! I!:M!:S! p!"),
           author = str_extract(title_author, "\\(\\D*\\)"),
           author = str_remove_all(author, "\\(|\\)"),
           title = str_remove(title_author, " \\(.*")
    ) %>%
    select(title, author, body, location, date) %>% 
    filter(!duplicated(body)) %>% 
    filter(!lead(str_detect(body, fixed(lag(body))))) # Almost duplicates from when a passage is re-highlighted.
  clippings
}

#' Read bank data
#'
#' @param path_to_bank_dir path to directory that contains csv files starting 
#' with "danske" and "spankki"
#'
#' @return data frame
#' @export
#'
#' @examples
read_bank <- function(path_to_bank_dir) {
  danske_files = list.files(path_to_bank_dir, "danske.+\\.csv", full.names = TRUE)
  spankki_files = list.files(path_to_bank_dir, "spankki.+\\.csv", full.names = TRUE)
  danske = purrr::map_df(danske_files, read_danske) %>% 
    dplyr::mutate(bank = "Danske")
  spankki = purrr::map_df(spankki_files, read_spankki)%>% 
    dplyr::mutate(bank = "S-Bank")
  bank = dplyr::full_join(danske, spankki) %>% 
    dplyr::select(-bank, dplyr::everything(), bank) %>% 
    dplyr::arrange(dplyr::desc(date))
  bank
}

#' Read and preprocess S-pankki csv to data frame
#'
#' @param path_to_csv location of csv file
#'
#' @return data frame
#' @export
read_spankki <- function(path_to_csv) {
  make_numeric <- function(s) {
    x <- gsub(",", ".", s) # Commas to periods
    x <- gsub(" ", "", x) # Remove spaces (thousands delimiter). Not present in csv anymore.
    as.numeric(x)
  }
  day_month_year2date <- function(x) as.Date(as.character(x), tryFormats = c("%d.%m.%Y"))
  book <- read.csv(file = path_to_csv,
                   sep=";",
                   encoding = "UTF-8",
                   col.names = c("date_reported", "date_paid", "amount", "type", "payer", "receiver", "receiver_iban", "receiver_bic",
                                 "viite_number", "message", "archive_number"),
                   colClasses = c("character", "character", "character", "character", "character", "character", "character", "character",
                                  "character", "character", "character"))
  
  book %>%
    mutate(amount = make_numeric(amount),
           date_reported = day_month_year2date(date_reported),
           date = day_month_year2date(date_paid)) %>%
    dplyr::select(date, amount, receiver, payer, message, type, receiver_iban)
}

#' Read Danske bank data
#'
#' @param path_to_csv
#'
#' @return
#' @export
#'
#' @examples
read_danske <- function(path_to_csv) {
  day_month_year2date <- function(x) as.Date(as.character(x), tryFormats = c("%d.%m.%Y"))
  read.csv2(path_to_csv,
            col.names = c("date", "receiver_payer", "amount", "balance", "status", "check"),
            stringsAsFactors = FALSE) %>%
    mutate(payer = if_else(amount > 0, as.character(receiver_payer), "Rafael Savvides"),
           receiver = if_else(amount < 0, as.character(receiver_payer), "Rafael Savvides"),
           date = day_month_year2date(date)) %>%
    select(date,  amount, receiver, payer) %>%
    mutate(receiver = str_replace(receiver, " +\\)\\)\\)\\)", ""))
}

#' Read phone recordings into tidy data frame
#'
#' @param path_to_dir 
#'
#' @return data frame (tibble)
#' @export
#'
#' @examples
read_phone_recordings = function(path_to_dir) {
  get_audio_filenames = function() {list.files(path_to_dir, recursive=TRUE, pattern="\\.mp3$|\\.wav$|\\.m4a$", ignore.case = T)}
  get_audio_filenames() %>% 
    tibble(filename=.) %>% 
    mutate(phone = map_chr(filename, function(s) str_split(s, "/")[[1]][1])) %>% 
    filter(str_detect(phone, "nexus|oneplus")) %>% 
    mutate(file = basename(filename),
           file_no_ext = str_remove(file, ".mp3|.m4a|.wav"),
           timestamp = lubridate::parse_date_time(file_no_ext, c("%Y-%m-%d_%H-%M-%S", "%Y%m%d_%H%M%S"), tz = "Europe/Helsinki"), 
           abs_filename = file.path(path_to_dir, filename)) %>% # 1 fails to parse, it has some text
    select(file, phone, timestamp, abs_filename)
}

#' Read Google Sheet with recording notes
#' 
#' Wrapper for using read_sheet without authorization. 
#'
#' @param sheet_url URL to a non-private Google Sheet
#' @param tab worksheet in sheet (as string or as integer position)
#'
#' @return
#' @export
#' @seealso  https://github.com/mbannert/boar-2018/issues/1
#' @examples
read_recording_notes <- function(sheet_url, tab=1) {
  googlesheets4::sheets_deauth()
  googlesheets4::read_sheet(ss = sheet_url, sheet=tab)
}



#' Read Whatsapp chat
#'
#' @param path_to_text 
#'
#' @return
#' @export
#'
#' @examples
read_whatsapp <- function(path_to_text) {
  raw <- readLines(path_to_text, skip = 3)
  chat <- strsplit(raw, split=" - ") 
  i_remove <- c()
  for (i in seq_along(chat)) {
    s <- chat[[i]]
    if (length(s) < 2) {
      chat[[i-1]][2] <- paste(chat[[i-1]][2], s)
      i_remove <- c(i_remove, i)
    } else if (length(s) > 2) { #FIX what about length(s) == 2?
      chat[[i]] <- c(s[1], paste(s[-1], collapse = ""))
    }
  }
  
  chat <- chat[-i_remove] %>% 
    unlist %>% 
    matrix(ncol=2, byrow = T, dimnames = list(NULL, c("timestamp", "message"))) %>% 
    data.frame(stringsAsFactors = F) %>% 
    tbl_df()
  chat$timestamp <- as.POSIXlt(chat$timestamp, format="%m/%d/%Y, %H:%M")
  chat$timestamp$year <- chat$timestamp$year + 2000
  chat$timestamp <- as.POSIXct(chat$timestamp)
  
  # Split message into sender and message.
  # Rows that do not have ": "
  rm.rows <- !grepl(":", chat$message)
  
  chat <- chat[!rm.rows, ]
  chat$sender <- sapply(str_split(chat$message, ":", n=2), function(x) x[1])
  chat$message <- sapply(str_split(chat$message, ":", n=2), function(x) x[2])
  chat
}

#' Read raw spotify data
#'
#' @param path_to_json 
#'
#' @return data frame
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
read_spotify = function(path_to_json) {
  jsonlite::read_json(path_to_json, simplifyVector = T) %>% 
    dplyr::mutate(endTime = lubridate::ymd_hm(endTime)) %>% 
    dplyr::rename(timestamp = endTime, artist = artistName, song = trackName, ms_played = msPlayed)
}

#' Read raw lastfm data
#'
#' @param path_to_csv 
#'
#' @return data frame
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
read_lastfm = function(path_to_csv) {
  read.csv(path_to_csv, header = FALSE, col.names = c("artist", "album", "song", "timestamp"), stringsAsFactors = F) %>% 
    dplyr::mutate(timestamp = lubridate::dmy_hm(timestamp)) %>% 
    dplyr::filter(lubridate::year(timestamp) > 1990) %>% 
    dplyr::select(timestamp, artist, song, album)
}


#' Title
#'
#' @param dir_telegram 
#'
#' @return
#' @export
#'
#' @examples
read_telegram <- function(dir_telegram) {
  json = read_json(dir_telegram)
  chats = json$chats$list
  df = tibble(chat = sapply(chats, function(x) {if (is.null(x$name)) NA_character_ else x$name}), 
              messages = sapply(chats, function(x) x$messages))
  
  parse_chat = function(x) {
    parse_message = function(x) {
      # Telegram seems to have some automatic entity identification (e.g. if 
      # message contains link, phone number, etc). This is stored as a nested 
      # list which complicates parsing.
      if (!is.list(x)) 
        return(x)
      if (length(x) == 1 && !is.null(x[[1]]$text))
        return(x[[1]]$text)
      if (!is.null(x$text))
        return(x$text)
      if (length(x) > 1) {
        return(paste0(sapply(x, parse_message), collapse=" "))
        
        return("")
      }
    }
    x = tibble(chat = df$messages[[2]])
    x_df = x %>% 
      unnest_wider(chat) 
    x_df %>% 
      filter(type == "message") %>% 
      select(-c(type, from_id, file, thumbnail, sticker_emoji, width, 
                height, photo, actor, actor_id, action, discard_reason, 
                location_information, live_location_period_seconds, 
                duration_seconds, via_bot, media_type)) %>% 
      select(from, text, date, edited, id, everything()) %>% 
      mutate(text = map_chr(text, parse_message), 
             content = ifelse(is.na(mime_type), 
                              text, 
                              paste0(ifelse(text=="", text, paste0(text, " \n")), 
                                     "Media: ", 
                                     mime_type))) 
    
    
  }
  
}

#' Title
#'
#' @param path_to_msn_dir 
#'
#' @return
#' @export
#'
#' @examples
read_msn <- function(path_to_msn_dir) {
  parse_chat_html <- function(chat_html) {
    parse_chat_text <- function(chat_text) {
      chat_text %>% 
        mutate(time = str_extract(X1, "\\(.+\\)"), 
               time = str_remove_all(time, "\\(|\\)"),
               sender = str_remove_all(X1, "\\(.+\\) ?|:$"), 
               message = X2) %>% 
        select(-X1, -X2)
    }
    session_start = html_text(html_nodes(chat_html, xpath="/html/body/div/h2"))
    n_sessions = length(session_start)
    if (n_sessions == 0)
      return(data.frame())
    participants = lapply(1:n_sessions, 
                          function(i) html_text(html_nodes(chat_html, xpath=paste0("/html/body/div[",i,"]/ul/li"))))
    chat_text = sapply(1:n_sessions, 
                       function(i) html_table(html_nodes(chat_html, xpath=paste0("/html/body/div[", i, "]/table"))))
    df = data.frame(session_start = session_start, 
                    participants = I(participants), 
                    chat_text = I(chat_text)) %>% 
      mutate(chat_text = I(map(chat_text, parse_chat_text)))
    df
  }
  
  html_files = data.frame(full_path = list.files(path_to_msn_dir, "*.html", 
                                                 full.names = TRUE, recursive = TRUE), 
                          stringsAsFactors = FALSE) %>% 
    mutate(account = gsub(".html$| \\(\\d\\).html$", "", basename(full_path))) %>% 
    arrange(account)
  
  n = nrow(html_files)
  list_of_chats = vector("list", n)
  for (i in 1:n) {
    cat(paste0(i, "/", n, " \t", html_files$account[i], "\n"))
    chat_html = read_html(html_files$full_path[i])
    list_of_chats[[i]] = parse_chat_html(chat_html)
  }
  
  msn = data.frame(account = html_files$account[1:n], 
                   chat = I(list_of_chats), 
                   stringsAsFactors = FALSE) %>%
    tidyr::unnest(chat) %>% 
    mutate(participants = map_chr(participants, paste0, collapse="|"), 
           session_start = str_remove_all(session_start, "^Session Start: |,"), 
           has_weekday_name = str_detect(session_start, "^[a-zA-Z]+ "), 
           session_start = ifelse(has_weekday_name, 
                                  format(strptime(session_start, format="%A %B %d %Y"), "%Y-%m-%d"), 
                                  format(strptime(session_start, format="%d %B %Y"), "%Y-%m-%d"))) %>% 
    unnest(chat_text) %>% 
    mutate(timestamp = ifelse(has_weekday_name, 
                              format(as.POSIXct(paste(session_start, time), format = "%Y-%m-%d %I:%M %p")),
                              format(as.POSIXct(paste(session_start, time), format = "%Y-%m-%d %H:%M")))) %>% 
    select(account, timestamp, participants, sender, message) 
  msn
}

#' Read Google Keep
#'
#' @param dir_google_keep 
#'
#' @return
#' @export
#'
#' @examples
read_google_keep <- function(dir_google_keep) {
  parse_checklist = function(x) {
    if (is.null(x) || is.na(x))
      return("")
    paste0(
      paste0(
        ifelse(sapply(x, function(x) x$isChecked), "[x] ", "[ ] "), 
        sapply(x, function(x) x$text)), 
      collapse="\n"
    )
  }
  
  parse_labels = function(x) {
    if (is.null(x) || is.na(x))
      return("")
    paste0(unlist(x), collapse=",")
  }
  
  parse_annotations = function(x) {
    if (is.null(x) || is.na(x))
      return("")
    x = unlist(x)
    paste0(paste0(names(x), ": ", x), collapse=",\n")
  }
  df = tibble(files = list.files(dir_google_keep, pattern = "*.json", full.names = TRUE)) %>% 
    filter(!str_detect(files, "\\?")) %>% #TODO Fix Greek encoding, ignoring for now.
    mutate(json = map(files, read_json)) %>% 
    unnest_wider(json) %>% 
    mutate(filename = basename(files), 
           timestamp_edited = as.POSIXct(userEditedTimestampUsec / 1e6, origin="1970-01-01"),
           checklist = map_chr(listContent, parse_checklist), 
           labels = map_chr(labels, parse_labels), 
           annotations = map_chr(annotations, parse_annotations)) %>% 
    rename(content = textContent, 
           is_trashed = isTrashed, 
           is_archived = isArchived) %>% 
    select(-attachments, -sharees, -isPinned, -color, -files, -userEditedTimestampUsec, -listContent) %>% 
    select(title, content, checklist, labels, annotations, timestamp_edited, everything())
  
  df
}

#' Read Daylio data
#'
#' @param path_to_daylio 
#' @param format "long" (default) or wide. Long is needed for plotting.
#'
#' @return data frame
#' @export
#'
#' @examples
read_daylio <- function(path_to_daylio, format = c("long", "wide")) {
  
  daylio = read.csv(path_to_daylio, stringsAsFactors = FALSE, 
                    fileEncoding="UTF-8-BOM") # Needed else first column has `ï..` prefix. 
  # See https://stackoverflow.com/questions/24568056/rs-read-csv-prepending-1st-column-name-with-junk-text/24568505
  
  daylio_long = daylio %>% 
    as_tibble() %>% 
    separate_rows(activities, sep = " \\| ") %>% 
    mutate(timestamp = as.POSIXct(paste(full_date, time))) %>% 
    select(timestamp, mood, activities, note) 
  
  if (format == "wide") daylio else daylio_long
}



#' Read donelist
#'
#' @param path_to_donelist 
#'
#' @return
#' @export
#'
#' @examples
read_donelist <- function(path_to_donelist) {
  donelist = read.csv(path_to_donelist, stringsAsFactors = FALSE) %>% 
    mutate(date = as.Date(paste0(2020, Date), format="%Y%b%d")) %>% 
    select(-Week, -Day, -Date) %>% 
    select(date, everything())
  
  donelist
}

#' Read work diary
#'
#' @param path_to_work_diary 
#' @param format one of c("tasks", "hours")
#'
#' @return
#' @export
#'
#' @examples
read_work_diary <- function(path_to_work_diary, which=c("tasks", "hours")) {
 work_diary = read.csv(path_to_work_diary, skip=1) %>% 
   as_tibble() %>% 
   pivot_longer(-1, names_to = "date") %>% 
   mutate(date = as.Date(paste0(2020, date), format = "%Y%b.%d")) %>% 
   select(date, everything()) %>% 
   arrange(date)
 
 work_hours = work_diary %>% 
   filter(!str_detect(X, "^\\d\\d?:\\d\\d"), 
          !str_detect(X, "^Extra hours"), 
          X != "") %>% 
   pivot_wider(date, X) %>% 
   janitor::clean_names()
   
 work_tasks = work_diary %>% 
   filter(str_detect(X, "^\\d\\d?:\\d\\d") | str_detect(X, "^Extra hours")) %>% 
   rename(time = X)
 
 if (match.arg(which) == "tasks") work_tasks else work_hours
}

read_work_diary_hours = function(path_to_work_diary) read_work_diary(path_to_work_diary=path_to_work_diary, which="hours")
read_work_diary_tasks = function(path_to_work_diary) read_work_diary(path_to_work_diary=path_to_work_diary, which="tasks")

# TODO ####

read_youtube <- function() {
  
}

read_facebook_chat <- function() {
  
}

read_google_calendar <- function() {
  # How to read .ics files?  
  # See https://stackoverflow.com/questions/43573982/how-to-import-ical-ics-file-in-r
}

read_google_chrome_history <- function() {
  
}

read_app_usage <- function() {
  
}

read_phone_activity <- function() {
  
}

