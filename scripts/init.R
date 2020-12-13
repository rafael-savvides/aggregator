## If called with source().
# source("../R/load_data.R")
# source("../R/read_raw_data.R")
# source("../R/utils.R")
# data_dict = load_data_dictionary("../data/paths_to_raw_data/", "e:/Users/savvi/Documents/repos/aggregator/data/clean/") 

source("R/load_data.R")
source("R/read_raw_data.R")
source("R/utils.R")
data_dict = load_data_dictionary()
data_dict

cat("Write `data_dict` to see available data. \n", 
    "To load a dataset, use `load_(name)`. ")

