provide_proto <- function() {
  if (!is_installed("RProtoBuf")) {
    abort("Package RProtoBuf is required to read pprof files.")
  }

  install_proto_files()
}

install_proto_files <- function() {
  RProtoBuf::readProtoFiles(system.file("proto", "profile.proto", package = utils::packageName()))
}
