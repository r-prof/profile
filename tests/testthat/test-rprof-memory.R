test_that("read memory profiling data", {
  ds <- read_inst_rprof("rprof/memory.out")
  expect_error(validate_profile(ds), NA)

  # sample_types always has 5 rows
  expect_equal(nrow(ds$sample_types), 5)
  expect_equal(ds$sample_types$type, c("samples", "small_v", "big_v", "nodes", "dup_count"))
  expect_equal(ds$sample_types$unit, c("count", "cells", "cells", "bytes", "count"))

  # samples always have memory columns
  expect_true("small_v" %in% names(ds$samples))
  expect_true("big_v" %in% names(ds$samples))
  expect_true("nodes" %in% names(ds$samples))
  expect_true("dup_count" %in% names(ds$samples))

  # memory values should be nonnegative integers (not NA for memory profiles)
  expect_true(is.integer(ds$samples$small_v))
  expect_true(is.integer(ds$samples$big_v))
  expect_true(is.integer(ds$samples$nodes))
  expect_true(is.integer(ds$samples$dup_count))
  expect_true(all(ds$samples$small_v >= 0))
  expect_true(all(ds$samples$big_v >= 0))
  expect_true(all(ds$samples$nodes >= 0))
  expect_true(all(ds$samples$dup_count >= 0))
})

test_that("non-memory profiles have NA memory columns", {
  ds <- read_inst_rprof("rprof/1.out")
  expect_error(validate_profile(ds), NA)

  # sample_types always has 5 rows
  expect_equal(nrow(ds$sample_types), 5)

  # memory columns are always present but all NA
  expect_true("small_v" %in% names(ds$samples))
  expect_true("big_v" %in% names(ds$samples))
  expect_true("nodes" %in% names(ds$samples))
  expect_true("dup_count" %in% names(ds$samples))
  expect_true(all(is.na(ds$samples$small_v)))
  expect_true(all(is.na(ds$samples$big_v)))
  expect_true(all(is.na(ds$samples$nodes)))
  expect_true(all(is.na(ds$samples$dup_count)))
})

test_that("memory profiling roundtrip via rprof", {
  ds <- read_inst_rprof("rprof/memory.out")

  # Convert to rprof and back
  ds1 <- rprof_to_ds(ds_to_rprof(ds))
  expect_error(validate_profile(ds1), NA)

  # roundtrip again
  ds2 <- rprof_to_ds(ds_to_rprof(ds1))
  expect_error(validate_profile(ds2), NA)

  expect_identical(strip_dots(ds1), strip_dots(ds2))
})

test_that("memory profiling write and read back", {
  ds <- read_inst_rprof("rprof/memory.out")

  path <- tempfile("profiler_mem", fileext = ".out")
  write_rprof(ds, path)

  ds1 <- read_rprof(path)
  expect_error(validate_profile(ds1), NA)

  expect_identical(strip_dots(ds), strip_dots(ds1))
})
