library(dplyr)
library(jsonlite)
library(purrr)

#' Read Google Keep
#'
#' @param dir_google_keep 
#'
#' @return
#' @export
#'
#' @examples
read_google_keep <- function(dir_google_keep = readLines("data-raw/path_to_google_keep_dir.txt")) {
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

google_keep = read_google_keep()

save(google_keep, file="data/google_keep.rda")
