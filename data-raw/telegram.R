library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

#' Read Telegram chats
#'
#' @param dir_telegram 
#'
#' @return
#' @export
#'
#' @examples
read_telegram <- function(dir_telegram = readLines("data-raw/path_to_telegram.txt")) {
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
    x = tibble(chat = x)
    x_df = x %>% 
      unnest_wider(chat) 
    x_df = x_df %>% 
      filter(type == "message") %>% 
      select(-intersect(colnames(x_df), 
                        c("type","from_id","file","thumbnail","sticker_emoji",
                          "width","height","photo","actor","actor_id","action",
                          "discard_reason","location_information",
                          "live_location_period_seconds","duration_seconds",
                          "via_bot","media_type", "members", "title", "inviter", 
                          "place_name", "address", "message_id"))) %>% 
      select(from, text, date, edited, id, everything()) %>% 
      mutate(text = map_chr(text, parse_message)) %>% 
      replace_na(list(mime_type="", 
                      forwarded_from=""))
    if ("mime_type" %in% colnames(x_df)) {
      x_df = x_df %>% 
        mutate(text = ifelse(mime_type=="", # may need to be mutate(content = ...)
                             text, 
                             paste0(ifelse(text=="", text, paste0(text, " \n")), 
                                    "Media: ", 
                                    mime_type)))
    } 
    x_df
  }
  df %>% 
    mutate(messages = map(messages, parse_chat)) %>% 
    unnest(messages)
}

telegram = read_telegram()

save(telegram, file="data/telegram.rda")
