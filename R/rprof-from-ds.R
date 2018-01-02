ds_to_rprof <- function(ds) {
  validate_profile(ds)

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
  traces <- map_chr(
    rep(ds$samples$locations, ds$samples$value),
    function(loc) {
      . <- flat_locations[match(loc$location_id, flat_locations$location_id), ]
      stopifnot(.$location_id == loc$location_id)
      funs <- paste0(
        ifelse(is.na(.$file_id), "", paste0(.$file_id, "#", .$line, " ")),
        '"', .$system_name, '"'
      )
      paste(c(funs, ""), collapse = " ")
    }
  )

  tibble::lst(
    header = "line profiling: sample.interval=20000",
    files,
    traces
  )
}
