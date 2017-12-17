#' Read profiler data from a proto file
#'
#' @param path A file name
#' @export
read_pprof <- function(path) {
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
