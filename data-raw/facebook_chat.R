library(dplyr)
library(purrr)
library(tidyr)

#' Read all Facebook chats
#'
#' @param path_to_fb_dir 
#'
#' @return
#' @export
#'
#' @examples
read_facebook_chat <- function(path_to_fb_dir = readlines("data-raw/path_to_facebook_dir.txt")) {
  parse_messages = function(messages) {
    # messages = fromJSON(fbchat)$messages
    m = messages %>% 
      as_tibble() 
    
    if ("call_duration" %in% colnames(m)) {
      m = mutate(m, 
                 content = if_else(!is.na(call_duration) & 
                                     call_duration != 0, 
                                   str_c(content, " ", call_duration, "sec"), 
                                   content))
    }
    
    m %>% 
      mutate(has_photos = "photos" %in% names(messages) && !map_lgl(photos, is.null), 
             has_link = "link" %in% names(messages) &&  !is.na(share$link), 
             has_reactions = "reactions" %in% names(messages) && 
               !map_lgl(reactions, is.null), 
             has_gifs = "gifs" %in% names(messages) && !map_lgl(gifs, is.null), 
             has_sticker = "sticker" %in% names(messages) &&  !is.na(sticker$uri), 
             has_files = "files" %in% names(messages) &&  !map_lgl(files, is.null), 
             has_audio_files = "audio_files" %in% names(messages) && 
               !map_lgl(audio_files, is.null), 
             has_videos = "videos" %in% names(messages) && !map_lgl(videos, is.null)) %>% 
      mutate(media = case_when(has_photos ~ "photo", 
                               has_link ~ "link",
                               has_gifs ~ "gif",
                               has_sticker ~ "sticker", 
                               has_files ~ "file", 
                               has_audio_files ~ "audio", 
                               has_videos ~ "video", 
                               has_reactions ~ "reactions",
                               TRUE ~ "")) %>% 
      mutate(timestamp = as.POSIXct(timestamp_ms / 1000, origin="1970-01-01")) %>%  
      # About /1000, see comments in https://stackoverflow.com/questions/13456241/convert-unix-epoch-to-date-object
      #TODO timezone may be off
      select(sender=sender_name, timestamp, content, media) 
  }
  
  file_list = list.files(path_to_fb_dir, "message.+\\.json", 
                         recursive=TRUE, full.names = TRUE)
  fb_chats = map(file_list, fromJSON)
  fb = tibble(chat = sapply(fb_chats, function(x) x$title), 
              chat_path = sapply(fb_chats, function(x) x$thread_path), 
              messages = sapply(fb_chats, function(x) x$messages))
  
  fb_parsed = fb %>% 
    mutate(messages = map(messages, parse_messages))
  
  fb_parsed %>% 
    unnest(messages)
}

facebook_chat = read_facebook_chat()

save(facebook_chat, "data/facebook_chat.rda")
