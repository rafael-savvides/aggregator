library(dplyr)
library(tidyr)

read_nutrilio = function(path_to_nutrilio = readLines("data-raw/path_to_nutrilio_dir.txt")) {
  latest_nutrilio = tail(sort(list.files(path_to_nutrilio, pattern="^nutrilio_export", full.names = TRUE)), 1)
  nutrilio = read.csv(latest_nutrilio, stringsAsFactors = FALSE, 
                      fileEncoding="UTF-8-BOM") # Needed else first column has `ï..` prefix. 
  nutrilio %>% 
    as_tibble() %>% 
    select(-Date, -Weekday, -Daily.Summary, -`Water..ml.`, -`Weight..kg.`, -Amount, -Health) %>% 
    separate_rows(Food, sep = " \\| ") %>% 
    separate(Food, c("Food", "Portion"), sep=" \\(") %>% 
    mutate(Portion = gsub("×\\)", "", Portion)) %>% 
    mutate(timestamp = as.POSIXct(paste(Full.Date, Time))) %>% 
    select(timestamp, food=Food, portion=Portion, note=Note, amount=Amount_text, health=Health_text) 
}

nutrilio = read_nutrilio()

save(nutrilio, file="data/nutrilio.rda")
