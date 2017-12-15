msg_to_ds <- function(msg) {
  samples <- get_samples_from_msg(msg)
  mappings <- get_mappings_from_msg(msg)
  locations <- get_locations_from_msg(msg)

  tibble::lst(
    samples,
    mappings,
    locations,
    msg = as.list(msg)
  )
}

get_samples_from_msg <- function(msg) {
  sample_locs <- map(msg$sample, function(s) {
    tibble::tibble(location_id = s$location_id)
  })
  tibble::tibble(
    sample_id = seq_along(sample_locs),
    loc = sample_locs
  )
}

get_mappings_from_msg <- function(msg) {
  mappings <- map(msg$mapping, function(m) {
    tibble::as_tibble(as.list(m))
  })
  mappings <- tibble::as_tibble(do.call(rbind, mappings))
  mappings$mapping_id <- as.integer(mappings$id)
  mappings$id <- NULL
  mappings$build_id <- as.integer(mappings$build_id)
  mappings$filename <- msg$string_table[mappings$filename + 1]

  names <- c("mapping_id", "filename")
  mappings[union(names, names(mappings))]
}

get_locations_from_msg <- function(msg) {
  locations <- map(msg$location, function(loc) {
    loc_list <- as.list(loc)
    # Using the last entry for inlined functions, see profile.proto
    if (length(loc_list$line) > 0) {
      loc_line <- as.list(loc_list$line[[length(loc_list$line)]])
    } else {
      loc_line <- list(function_id = NA_real_, line = NA_real_)
    }
    loc_list$line <- NULL
    tibble::as_tibble(c(loc_list, loc_line))
  })
  locations <- tibble::as_tibble(do.call(rbind, locations))
  locations$location_id <- as.integer(locations$id)
  locations$id <- NULL
  locations$mapping_id <- as.integer(locations$mapping_id)
  locations$function_id <- as.integer(locations$function_id)
  locations$line <- as.integer(locations$line)

  names <- c("location_id", "mapping_id", "function_id", "line")
  locations[union(names, names(locations))]
}
