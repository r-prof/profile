ds_to_rprof <- function(ds) {
  validate_profile(ds)

  has_memory <- nrow(ds$sample_types) > 1

  . <- ds$locations
  . <- merge(., ds$functions[c("function_id", "system_name", "filename")], by = "function_id", sort = FALSE, all.x = TRUE)
  . <- .[-1L]
  . <- tibble::as_tibble(.)
  location_files <- .

  . <- tibble::tibble(filename = unique(location_files$filename[location_files$line != 0]))
  . <- tibble::rowid_to_column(., "file_id")
  . <- tibble::as_tibble(.)
  unique_files <- .

  . <- merge(location_files, unique_files, by = "filename", all.x = TRUE)
  . <- .[-1L]
  . <- tibble::as_tibble(.)
  flat_locations <- .

  files <- paste0("#File ", unique_files$file_id, ": ", unique_files$filename)

  # Expand samples by value (repeat count)
  sample_idx <- rep(seq_len(nrow(ds$samples)), ds$samples$value)

  traces <- map_chr(
    seq_along(sample_idx),
    function(i) {
      si <- sample_idx[[i]]
      loc <- ds$samples$locations[[si]]
      . <- flat_locations[match(loc$location_id, flat_locations$location_id), ]
      stopifnot(.$location_id == loc$location_id)
      funs <- paste0(
        ifelse(is.na(.$file_id), "", paste0(.$file_id, "#", .$line, " ")),
        '"', .$system_name, '"'
      )
      paste(c(funs, ""), collapse = " ")
    }
  )

  header <- if (has_memory) {
    "memory profiling: line profiling: sample.interval=20000"
  } else {
    "line profiling: sample.interval=20000"
  }

  # Build memory data for roundtrip compatibility
  memory <- NULL
  if (has_memory) {
    memory <- tibble::tibble(
      small_v = ds$samples$small_v[sample_idx],
      big_v = ds$samples$big_v[sample_idx],
      nodes = ds$samples$nodes[sample_idx],
      dup_count = ds$samples$dup_count[sample_idx]
    )
  }

  tibble::lst(
    header,
    files,
    traces,
    memory
  )
}
