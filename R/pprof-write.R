#' @details
#' Use the [`pprof` tool](https://github.com/google/pprof) in conjunction with
#' the `_pprof()` functions. The tool is available in the \pkg{pprof} R package,
#' or (in newer versions) via `go get github.com/google/pprof`.
#'
#' @param ds Profiler data, see [validate_profile()]
#' @export
#' @rdname read_rprof
#' @include rprof-write.R
write_pprof <- function(ds, path) {
  msg <- ds_to_msg(ds)
  write_msg(msg, path)
}

write_msg <- function(msg, path) {
  con <- gzfile(path, "wb")
  defer(close(con))
  RProtoBuf::serialize(msg, con)
}
