#' Write profiler data to an R profiler file
#'
#' @param path A file name
#' @param ds Profiler data
#' @export
write_rprof <- function(path, ds) {
  rprof <- ds_to_rprof(ds)
  writeLines(unlist(rprof, use.names = FALSE), path)
}
