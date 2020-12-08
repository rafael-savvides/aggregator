source("R/load_data.R")
source("R/read_raw_data.R")
source("R/utils.R")

data_dict = load_data_dictionary()
data_dict

cat("Write `data_dict` to see available data. \n", 
    "To load a dataset, use `load_(name)`. ")
