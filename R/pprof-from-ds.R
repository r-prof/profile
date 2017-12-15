ds_to_msg <- function(ds) {
  msg <- RProtoBuf::new(perftools.profiles.Profile)

  msg$string_table <- unique(c(
    "",
    ds$mappings$filename,
    ds$functions$name,
    ds$functions$system_name,
    ds$functions$filename
  ))

  add_samples_to_msg(ds$samples, msg)
  add_mappings_to_msg(ds$mappings, msg)
  add_locations_to_msg(ds$locations, msg)
  add_functions_to_msg(ds$functions, msg)
  msg
}

add_samples_to_msg <- function(samples, msg) {
  stopifnot(diff(samples$sample_id) == 1)

  msg$sample <- lapply(split_rows(samples), function(s) {
    s_msg <- RProtoBuf::new(perftools.profiles.Sample)
    s_msg$value <- s$value
    s_msg$location_id <- s$locations[[1]]$location_id
    s_msg
  })
}

add_mappings_to_msg <- function(mappings, msg) {
  mappings$filename <- match(mappings$filename, msg$string_table) - 1L

  msg$mapping <- lapply(split_rows(mappings), function(m) {
    m_msg <- RProtoBuf::new(perftools.profiles.Mapping)
    m_msg$id <- m$mapping_id
    m_msg$filename <- m$filename
    m_msg
  })
}

add_locations_to_msg <- function(locations, msg) {
  msg$location <- lapply(split_rows(locations), function(loc) {
    loc_msg <- RProtoBuf::new(perftools.profiles.Location)
    loc_msg$id <- loc$location_id
    loc_msg$mapping_id <- loc$mapping_id
    if (!is.na(loc$function_id)) {
      loc_msg$line <- list(RProtoBuf::new(perftools.profiles.Line))
      loc_msg$line[[1]]$function_id <- loc$function_id
      loc_msg$line[[1]]$line <- loc$line
    }
    loc_msg
  })
}

add_functions_to_msg <- function(functions, msg) {
  functions$name <- match(functions$name, msg$string_table) - 1L
  functions$system_name <- match(functions$system_name, msg$string_table) - 1L
  functions$filename <- match(functions$filename, msg$string_table) - 1L

  msg$`function` <- lapply(split_rows(functions), function(f) {
    f_msg <- RProtoBuf::new(perftools.profiles.Function)
    f_msg$id <- f$function_id
    f_msg$name <- f$name
    f_msg$system_name <- f$system_name
    f_msg$filename <- f$filename
    f_msg
  })
}
