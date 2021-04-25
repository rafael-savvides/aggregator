library(dplyr)

read_dota_matches = function(path_to_csv = readLines("data-raw/path_to_dota_matches.txt")) {
  duration_to_sec = function(x) {
    xs = as.numeric(strsplit(x, ":")[[1]])
    if (length(xs) == 3)
      return(xs[1]*3600 + xs[2]*60 + xs[3])
    if (length(xs) == 2)
      return(xs[1]*60 + xs[2])
    if (length(xs) == 1)
      return(xs)
  }
  dota_matches = read.csv(path_to_csv)
  dota_matches
}

dota_matches = read_dota_matches()

save(dota_matches, file="data/dota_matches.rda")

