#' Write profiler data to a proto file
#'
#' Use the [`pprof` program](https://github.com/google/pprof) to read and
#' analyze the file created by this function.
#'
#' @param ds Profiler data, see [validate_profile()]
#' @param path Target file name
#' @export
write_pprof <- function(ds, path) {
  msg <- ds_to_msg(ds)
  write_msg(msg, path)
}

write_msg <- function(msg, path) {
  con <- gzfile(path, "wb")
  defer(close(con))
  RProtoBuf::serialize(msg, con)
}
