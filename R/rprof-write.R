#' Write profiler data to an R profiler file
#'
#' Use the profvis or proftools R packages to further analyze files created by
#' this function.
#'
#' @inheritParams write_pprof
#' @export
write_rprof <- function(ds, path) {
  rprof <- ds_to_rprof(ds)
  writeLines(unlist(rprof, use.names = FALSE), path)
}
