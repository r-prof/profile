#' Read profiler data from a proto file
#'
#' @param path A file name
#' @param ... Ignored
#' @param version Version of the data, currently only `"1.0"` is supported.
#'   Pass an explicit value to this argument if your code depends on the data
#'   format.
#' @export
read_pprof <- function(path, ..., version = "1.0") {
  stopifnot(version == get_default_meta()$value)
  msg <- read_msg(path)
  ds <- msg_to_ds(msg)
  validate_profile(ds)
  ds
}

read_msg <- function(path) {
  provide_proto()

  con <- gzfile(path, "rb")
  defer(close(con))
  RProtoBuf::read(perftools.profiles.Profile, con)
}
