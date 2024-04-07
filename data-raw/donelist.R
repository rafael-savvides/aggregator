library(dplyr)

#' Read donelist
#'
#' @param path_to_donelist 
#'
#' @return
#' @export
#'
#' @examples
read_donelist <- function(path_to_donelist = readLines("data-raw/path_to_donelist.txt")) {
  donelist = read.csv(path_to_donelist, stringsAsFactors = FALSE) |> 
    mutate(date = as.Date(paste0(2020, Date), format="%Y%b%d")) |> 
    select(-Week, -Day, -Date) |> 
    select(date, everything())
  
  donelist
}

donelist = read_donelist()

save(donelist, file="data/donelist.rda")
