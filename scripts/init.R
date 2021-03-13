suppressPackageStartupMessages(source("../R/load_data.R"))
suppressPackageStartupMessages(source("../R/read_raw_data.R"))
suppressPackageStartupMessages(source("../R/utils.R"))
data_dict = load_data_dictionary("../data/paths_to_raw_data/", 
                                 readLines("../data/path_to_clean_dir.txt")) 
data_dict 

message("* `data_dict` lists available data in a data frame . \n", 
        "* `load_(name)` loads a dataset. ")
