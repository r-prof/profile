#' @rdname read_rprof
#' @export
write_rprof <- function(x, path) {
  rprof <- ds_to_rprof(x)
  # Add memory prefix to traces when writing to file
  if (!is.null(rprof$memory)) {
    rprof$traces <- paste0(
      ":", rprof$memory$small_v,
      ":", rprof$memory$big_v,
      ":", rprof$memory$nodes,
      ":", rprof$memory$dup_count, ":",
      rprof$traces
    )
  }
  rprof$memory <- NULL
  writeLines(unlist(rprof, use.names = FALSE), path)
  invisible(x)
}
