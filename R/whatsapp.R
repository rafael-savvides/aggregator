#' Read Whatsapp chat
#'
#' @param path_to_text
#'
#' @return
#' @export
read_whatsapp <- function(path_to_text) {
  raw <- readLines(path_to_text, skip = 3)
  chat <- strsplit(raw, split = " - ")
  i_remove <- c()
  for (i in seq_along(chat)) {
    s <- chat[[i]]
    if (length(s) < 2) {
      chat[[i - 1]][2] <- paste(chat[[i - 1]][2], s)
      i_remove <- c(i_remove, i)
    } else if (length(s) > 2) { # FIX what about length(s) == 2?
      chat[[i]] <- c(s[1], paste(s[-1], collapse = ""))
    }
  }

  chat <- chat[-i_remove] |>
    unlist() |>
    matrix(ncol = 2, byrow = TRUE, dimnames = list(NULL, c("timestamp", "message"))) |>
    data.frame(stringsAsFactors = FALSE) |>
    tbl_df()
  chat$timestamp <- as.POSIXlt(chat$timestamp, format = "%m/%d/%Y, %H:%M")
  chat$timestamp$year <- chat$timestamp$year + 2000
  chat$timestamp <- as.POSIXct(chat$timestamp)

  # Split message into sender and message.
  # Rows that do not have ": "
  rm.rows <- !grepl(":", chat$message)

  chat <- chat[!rm.rows, ]
  chat$sender <- sapply(str_split(chat$message, ":", n = 2), function(x) x[1])
  chat$message <- sapply(str_split(chat$message, ":", n = 2), function(x) x[2])
  chat
}
