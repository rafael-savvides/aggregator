library(dplyr)
library(purrr)

#' Read smartphone app usage
#'
#' @param path_to_app_usage_dir 
#'
#' @return
#' @export
#'
#' @examples
read_app_usage = function(path_to_app_usage_dir = readlines("data-raw/path_to_app_usage.txt")) {
  make_secs = function(s) { 
    # Converts '0:01:44' to 104 seconds.
    s = as.numeric(strsplit(s, ":")[[1]])
    s[1]*3600 + s[2]*60 + s[3]
  }
  read_one_app_usage_file = function(file) {
    read.csv(file) %>% 
      as_tibble() %>% 
      mutate(timestamp = strptime(paste(Date, Time), format="%m/%d/%y %H:%M:%S"), 
             duration = map_dbl(Duration, make_secs)) %>% 
      select(app=App.name, timestamp, duration)
  }

  files = list.files(path_to_app_usage_dir, full.names = TRUE)
  map_df(files, read_one_app_usage_file)
}

app_usage = read_app_usage()

save(app_usage, "data/app_usage.rda")
