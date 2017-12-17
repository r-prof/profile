#' Read profiler data from an R profiler file
#'
#' @param path A file name
#' @export
read_rprof <- function(path) {
  rprof_ll <- read_rprof_ll(path)
  rprof_to_ds(rprof_ll)
}

read_rprof_ll <- function(path) {
  lines <- readLines(path)

  header <- 1L
  files <- grep("^#File ", lines)
  traces <- setdiff(seq_along(lines), c(header, files))
  list(
    header = lines[header],
    files = lines[files],
    traces = lines[traces]
  )
}
