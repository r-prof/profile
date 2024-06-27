test_that("can load dm", {
  skip_if_not_installed("dm")

  ds <- read_inst_rprof("rprof/1.out")
  dm <- dm_from_profile(ds)

  expect_snapshot({
    dm::dm_examine_constraints(dm)
  })

  expect_equal(unique(dm::dm_examine_constraints(dm)$is_key), TRUE)
})
