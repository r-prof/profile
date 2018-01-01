#' Read profiler data from an R profiler file
#'
#' @inheritParams read_pprof
#' @export
read_rprof <- function(path, ..., version = "1.0") {
  stopifnot(version == get_default_meta()$value)
  rprof_ll <- read_rprof_ll(path)
  ds <- rprof_to_ds(rprof_ll)
  validate_profile(ds)
  ds
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
