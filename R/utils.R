split_rows <- function(x) {
  split(x, seq_len(nrow(x)))
}

merge_rows <- function(x) {
  tibble::as_tibble(do.call(rbind, x))
}
