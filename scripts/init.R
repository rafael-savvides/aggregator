suppressPackageStartupMessages(source("../R/load_data.R"))
suppressPackageStartupMessages(source("../R/read_raw_data.R"))
suppressPackageStartupMessages(source("../R/utils.R"))
data_dict = load_data_dictionary("../data/paths_to_raw_data/", 
                                 readLines("../data/path_to_clean_dir.txt")) 
data_dict 

cat("Write `data_dict` to see available data. \n", 
    "To load a dataset, use `load_(name)`. ")
