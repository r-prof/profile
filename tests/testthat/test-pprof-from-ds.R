context("pprof-from-ds")

test_that("roundtrip works", {
  skip_if_not_installed("RProtoBuf")

  ds <- read_pprof("proto/1.out.prof.pb.gz")

  ds1 <- msg_to_ds(ds_to_msg(ds))
  ds2 <- msg_to_ds(ds_to_msg(ds1))

  expect_identical(strip_msg(ds1), strip_msg(ds2))
})
