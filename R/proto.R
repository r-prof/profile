provide_proto <- function() {
  check_installed("RProtoBuf")
  install_proto_files()
}

install_proto_files <- function() {
  RProtoBuf::readProtoFiles(system.file("proto", "profile.proto", package = utils::packageName()))
}
