#' @rdname read_rprof
#' @export
write_rprof <- function(x, path) {
  rprof <- ds_to_rprof(x)
  writeLines(unlist(rprof, use.names = FALSE), path)
  invisible(x)
}
