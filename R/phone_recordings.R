#' Read phone recordings
#'
#' @param path_to_dir
#'
#' @return data frame (tibble)
#' @export
#' @import dplyr
read_phone_recordings = function(path_to_dir) {
  list.files(path_to_dir, recursive = TRUE, pattern = "\\.mp3$|\\.wav$|\\.m4a$", ignore.case = TRUE) |>
    tibble(filename = .) |>
    mutate(phone = purrr::map_chr(filename, function(s) stringr::str_split(s, "/")[[1]][1])) |>
    filter(stringr::str_detect(phone, "nexus|oneplus")) |>
    mutate(
      file = basename(filename),
      file_no_ext = stringr::str_remove(file, ".mp3|.m4a|.wav"),
      timestamp = lubridate::parse_date_time(file_no_ext, c("%Y-%m-%d_%H-%M-%S", "%Y%m%d_%H%M%S"), tz = "Europe/Helsinki"),
      abs_filename = file.path(path_to_dir, filename)
    ) |> # 1 fails to parse, it has some text
    select(file, phone, timestamp, abs_filename)
}
