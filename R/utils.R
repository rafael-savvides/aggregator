library(magrittr) # Needed only for %$%, consider removing.

#' Make a string a valid filename
#'
#' @param x string
#'
#' @return string
#' @export
#'
#' @examples
#' make_valid_filename("this!+is?a'_____valid##filename!")
make_valid_filename <- function(x) {
  gsub("_+", "_", gsub("[ .'\"#/\\?><!,=+:;\\|“…]", "_", x))
}

#' Create a title from a string
#'
#' @param x string
#'
#' @return
#' @export
#'
#' @examples
#' make_title("This is a string. This part should not be in the title.")
make_title <- function(x) {
  if (is.na(x) || x == "") 
    return("")
  if (startsWith(x, "http"))
    return(substr(strsplit(x, "://")[[1]][2], 1, 100))
  substr(strsplit(x, "[\\.\\\n:]|(https://)")[[1]][1], 1, 100)
}

#' Convert Google keep entry into Markdown note
#'
#' @param title 
#' @param content 
#' @param timestamp 
#'
#' @return
#' @export
#'
#' @examples
gkeep_to_md <- function(title, content, timestamp) {
  if (is.na(title) || title == "")
    title = make_title(content)
  if (title == "")
    title = timestamp
  paste0("# ", title, "\n\n", timestamp, "\n\n", content, "\n")
}

#' Saves data frame of google keep into Markdown files
#'
#' Appends notes with same filename.
#'
#' @param gkeep 
#' @param folder 
#'
#' @return
#' @export
#' @import magrittr
#' @examples
#' save_gkeep_to_md(filter(gkeep, !is_archived))
save_gkeep_to_md <- function(gkeep, folder = "mdnotes") {
  gkeep %>% 
    filter(!(title == "" & (is.na(content) || content == ""))) %>% 
    mutate(md = pmap_chr(list(title=title, content=content, timestamp=timestamp_edited), gkeep_to_md), 
           title = ifelse(title == "", map_chr(content, make_title), title), 
           title = ifelse(title == "", timestamp_edited, title), 
           filename = paste0(folder, "/", map_chr(title, make_valid_filename), ".md")) %>% 
    select(md, filename) %$% 
    walk2(.x=md, .y=filename, .f = write_file, append = TRUE)
}
