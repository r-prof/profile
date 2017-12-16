context("pprof-write")

test_that("roundtrip", {
  skip_if_not_installed("RProtoBuf")

  ds <- read_pprof("proto/1.out.prof.pb.gz")

  path <- tempfile("profiler", fileext = ".pb.gz")
  write_pprof(path, ds)
  ds1 <- read_pprof(path)

  expect_identical(strip_msg(ds), strip_msg(ds1))
})
