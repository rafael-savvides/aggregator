library(dplyr)
library(lubridate)

#' Read phone recordings into tidy data frame
#'
#' @param path_to_dir 
#'
#' @return data frame (tibble)
#' @export
#'
#' @examples
read_phone_recordings = function(path_to_dir = readLines("data-raw/path_to_phone_recordings.txt")) {
  get_audio_filenames = function() {list.files(path_to_dir, recursive=TRUE, pattern="\\.mp3$|\\.wav$|\\.m4a$", ignore.case = T)}
  get_audio_filenames() %>% 
    tibble(filename=.) %>% 
    mutate(phone = map_chr(filename, function(s) str_split(s, "/")[[1]][1])) %>% 
    filter(str_detect(phone, "nexus|oneplus")) %>% 
    mutate(file = basename(filename),
           file_no_ext = str_remove(file, ".mp3|.m4a|.wav"),
           timestamp = parse_date_time(file_no_ext, c("%Y-%m-%d_%H-%M-%S", "%Y%m%d_%H%M%S"), tz = "Europe/Helsinki"), 
           abs_filename = file.path(path_to_dir, filename)) %>% # 1 fails to parse, it has some text
    select(file, phone, timestamp, abs_filename)
}

phone_recordings = read_phone_recordings()

save(phone_recordings, file="data/phone_recordings.rda")
