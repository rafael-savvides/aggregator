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
read_recordings_notes <- function(path_to_recording_notes = readlines("data-raw/path_to_recordings_notes.txt"), sheet=1) {
  col_types = NULL
  if (sheet == "phone")
    col_types = c("text", "numeric", "text", "text", "text", "text", "numeric")
  
  read_excel(path_to_recording_notes, sheet=sheet, col_types = col_types)
}

read_recordings_notes_phone = function(path) read_recording_notes(path, sheet="phone")

recordings_notes_phone = read_recordings_notes_phone()

save(recordings_notes_phone, "data/recordings_notes_phone.rda")
