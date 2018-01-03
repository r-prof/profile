#' @rdname read_rprof
#' @export
write_rprof <- function(ds, path) {
  rprof <- ds_to_rprof(ds)
  writeLines(unlist(rprof, use.names = FALSE), path)
  invisible(ds)
}
