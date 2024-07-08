#' Read Nutrilio
#' @export
#' @import dplyr
#' @import tidyr
read_nutrilio = function(
    path_to_nutrilio,
    entry_columns = c("Where", "Other", "Plants", "Fruit", "Dairy.Eggs", "Carbs", "Meat", "Snacks", "Fast.food", "Type", "Processed", "Note"),
    numeric_columns = c("Tastiness", "Amount", "Price....")) {
  non_empty = function(x) !all(is.na(x)) & !all(x == "")
  if (dir.exists(path_to_nutrilio)) {
    latest_nutrilio = tail(sort(list.files(path_to_nutrilio, pattern = "^nutrilio_export", full.names = TRUE)), 1)
  } else if (file.exists(path_to_nutrilio)) {
    latest_nutrilio = path_to_nutrilio
  }
  nutrilio = read.csv(
    latest_nutrilio,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  ) # Needed else first column has `ï..` prefix.

  nutrilio |>
    as_tibble() |>
    pivot_longer(entry_columns, names_to = "category", values_to = "entry") |>
    filter(entry != "") |>
    separate_rows(entry, sep = " \\| ") |>
    separate(entry, c("entry", "portion"), sep = " \\(", fill = "right") |>
    mutate(portion = gsub("×\\)", "", portion)) |>
    mutate(timestamp = as.POSIXct(paste(Full.Date, Time))) |>
    select(timestamp, category, entry, portion, all_of(numeric_columns))
}
