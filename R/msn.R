library(dplyr)
library(tidyr)
library(purrr)
library(rvest)

#' Read MSN chats
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
      chat_text |> 
        mutate(time = str_extract(X1, "\\(.+\\)"), 
               time = str_remove_all(time, "\\(|\\)"),
               sender = str_remove_all(X1, "\\(.+\\) ?|:$"), 
               message = X2) |> 
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
                    chat_text = I(chat_text)) |> 
      mutate(chat_text = I(map(chat_text, parse_chat_text)))
    df
  }
  
  html_files = data.frame(full_path = list.files(path_to_msn_dir, "*.html", 
                                                 full.names = TRUE, recursive = TRUE), 
                          stringsAsFactors = FALSE) |> 
    mutate(account = gsub(".html$| \\(\\d\\).html$", "", basename(full_path))) |> 
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
                   stringsAsFactors = FALSE) |>
    tidyr::unnest(chat) |> 
    mutate(participants = map_chr(participants, paste0, collapse="|"), 
           session_start = str_remove_all(session_start, "^Session Start: |,"), 
           has_weekday_name = str_detect(session_start, "^[a-zA-Z]+ "), 
           session_start = ifelse(has_weekday_name, 
                                  format(strptime(session_start, format="%A %B %d %Y"), "%Y-%m-%d"), 
                                  format(strptime(session_start, format="%d %B %Y"), "%Y-%m-%d"))) |> 
    unnest(chat_text) |> 
    mutate(timestamp = ifelse(has_weekday_name, 
                              format(as.POSIXct(paste(session_start, time), format = "%Y-%m-%d %I:%M %p")),
                              format(as.POSIXct(paste(session_start, time), format = "%Y-%m-%d %H:%M")))) |> 
    select(account, timestamp, participants, sender, message) 
  msn
}
