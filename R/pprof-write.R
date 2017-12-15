#' Write profiler data to a proto file
#'
#' @param path A file name
#' @param ds Profiler data
#' @export
write_pprof <- function(path, ds) {
  msg <- ds_to_msg(ds)
  write_msg(msg, path)
}

write_msg <- function(msg, path) {
  con <- gzfile(path, "wb")
  defer(close(con))
  RProtoBuf::serialize(msg, con)
}
