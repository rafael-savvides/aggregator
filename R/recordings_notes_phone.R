library(readxl)

#' Read xlsx with recording notes
#' 
#'
#' @param path_to_recording_notes 
#' @param sheet 
#'
#' @return
#' @export
#' @examples
read_recordings_notes <- function(path_to_recording_notes, sheet=1) {
  col_types = NULL
  if (sheet == "phone")
    col_types = c("text", "numeric", "text", "text", "text", "text", "numeric")
  
  read_excel(path_to_recording_notes, sheet=sheet, col_types = col_types)
}
