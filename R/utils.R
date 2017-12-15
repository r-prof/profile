split_rows <- function(x) {
  split(x, seq_len(nrow(x)))
}
