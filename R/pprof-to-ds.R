msg_to_ds <- function(msg) {
  samples <- get_samples_from_msg(msg)
  mappings <- get_mappings_from_msg(msg)
  locations <- get_locations_from_msg(msg)
  functions <- get_functions_from_msg(msg)

  tibble::lst(
    samples,
    mappings,
    locations,
    functions,
    .msg = as.list(msg)
  )
}

get_samples_from_msg <- function(msg) {
  samples <- map(msg$sample, function(s) {
    tibble::tibble(
      value = as.integer(s$value[[1]]),
      locations = list(tibble::tibble(location_id = as.integer(s$location_id)))
    )
  })
  samples <- tibble::as_tibble(do.call(rbind, samples))
  samples <- tibble::rowid_to_column(samples, "sample_id")
  samples
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

get_functions_from_msg <- function(msg) {
  functions <- map(msg$`function`, function(f) {
    tibble::as_tibble(as.list(f))
  })
  functions <- tibble::as_tibble(do.call(rbind, functions))
  functions$function_id <- as.integer(functions$id)
  functions$id <- NULL

  functions$name <- msg$string_table[functions$name + 1]
  functions$system_name <- msg$string_table[functions$system_name + 1]
  functions$filename <- msg$string_table[functions$filename + 1]

  functions$start_line <- as.integer(functions$start_line)

  names <- c("function_id")
  functions[union(names, names(functions))]
}
