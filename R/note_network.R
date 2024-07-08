#' Read adjacency matrix of wiki links between markdown notes
#' @export
read_note_network = function(path_to_notes) {
  abs_paths = list.files(path_to_notes, pattern = "*.md", full.names = TRUE)
  filenames = gsub(".md", "", basename(abs_paths))
  adj_mat = matrix(0, nrow = length(filenames), ncol = length(filenames), dimnames = list(filenames, filenames))
  for (i in seq_along(abs_paths)) {
    f = readLines(abs_paths[i])
    for (j in seq_along(filenames)) {
      if (any(grepl(sprintf("\\[\\[%s(\\.md)?\\]\\]", filenames[j]), f))) {
        adj_mat[i, j] = 1
      }
    }
  }
  adj_mat
}
