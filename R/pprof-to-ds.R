msg_to_ds <- function(msg) {
  sample_types <- get_sample_types_from_msg(msg)
  samples <- get_samples_from_msg(msg)
  locations <- get_locations_from_msg(msg)
  functions <- get_functions_from_msg(msg)

  ret <- tibble::lst(
    meta = get_default_meta(),
    sample_types,
    samples,
    locations,
    functions,
    .msg = as.list(msg)
  )
  new_profile_data(ret)
}

get_sample_types_from_msg <- function(msg) {
  sample_types <- map(msg$sample_type, function(st) {
    tibble::tibble(
      type = as.integer(st$type),
      unit = as.integer(st$unit)
    )
  })
  sample_types <- merge_rows(sample_types)

  sample_types$type <- msg$string_table[sample_types$type + 1]
  sample_types$unit <- msg$string_table[sample_types$unit + 1]

  # Check if memory profiling types are present
  mem_types <- c("small_v", "big_v", "nodes", "dup_count")
  has_memory <- all(mem_types %in% sample_types$type)

  if (has_memory) {
    # Keep first row (samples/count) and the memory rows
    keep <- sample_types$type %in% c("samples", mem_types)
    sample_types[keep, ]
  } else {
    sample_types[1, ]
  }
}

get_samples_from_msg <- function(msg) {
  # Determine which value indices to keep
  n_types <- length(msg$sample_type)
  all_types <- map(msg$sample_type, function(st) {
    msg$string_table[as.integer(st$type) + 1]
  })
  all_types <- unlist(all_types)

  mem_types <- c("small_v", "big_v", "nodes", "dup_count")
  has_memory <- all(mem_types %in% all_types)

  if (has_memory) {
    mem_indices <- match(mem_types, all_types)
  }

  samples <- map(msg$sample, function(s) {
    values <- as.integer(s$value)
    row <- tibble::tibble(
      value = values[[1]],
      locations = list(tibble::tibble(location_id = as.integer(s$location_id)))
    )
    if (has_memory) {
      row$small_v <- values[[mem_indices[[1]]]]
      row$big_v <- values[[mem_indices[[2]]]]
      row$nodes <- values[[mem_indices[[3]]]]
      row$dup_count <- values[[mem_indices[[4]]]]
    }
    row
  })
  samples <- tibble::as_tibble(do.call(rbind, samples))
  samples
}

get_locations_from_msg <- function(msg) {
  locations <- map(msg$location, function(loc) {
    loc_list <- as.list(loc)
    loc_list$mapping_id <- NULL
    loc_list$address <- NULL

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
  locations$function_id <- as.integer(locations$function_id)
  locations$line <- as.integer(locations$line)

  names <- c("location_id", "function_id", "line")
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
  functions$name[functions$name == ""] <- "<?>"
  functions$system_name <- msg$string_table[functions$system_name + 1]
  functions$system_name[functions$system_name == ""] <- "<?>"
  functions$filename <- msg$string_table[functions$filename + 1]

  functions$start_line <- as.integer(functions$start_line)

  names <- c("function_id")
  functions[union(names, names(functions))]
}
