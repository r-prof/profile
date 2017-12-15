#' Read a pprof file from a proto file
#'
#' @param path A file name
#' @export
read_pprof <- function(path) {
  msg <- read_msg(path)
  msg_to_ds(msg)
}

read_msg <- function(path) {
  if (!is_installed("RProtoBuf")) {
    abort("Package RProtoBuf is required to read pprof files.")
  }

  install_proto_files()

  con <- gzfile(path, "rb")
  defer(close(con))
  RProtoBuf::read(perftools.profiles.Profile, con)
}


install_proto_files <- function() {
  RProtoBuf::readProtoFiles(system.file("proto", "profile.proto", package = utils::packageName()))
}
