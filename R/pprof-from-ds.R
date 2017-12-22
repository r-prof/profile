ds_to_msg <- function(ds) {
  validate_profile(ds)
  provide_proto()

  msg <- RProtoBuf::new(perftools.profiles.Profile)

  msg$string_table <- unique(c(
    "",
    ds$sample_types$type,
    ds$sample_types$unit,
    ds$functions$name,
    ds$functions$system_name,
    ds$functions$filename
  ))

  add_sample_types_to_msg(ds$sample_types, msg)
  add_samples_to_msg(ds$samples, msg)
  add_locations_to_msg(ds$locations, msg)
  add_functions_to_msg(ds$functions, msg)
  msg
}

add_sample_types_to_msg <- function(sample_types, msg) {
  sample_types$type <- match(sample_types$type, msg$string_table) - 1L
  sample_types$unit <- match(sample_types$unit, msg$string_table) - 1L

  msg$sample_type <- lapply(split_rows(sample_types), function(st) {
    st_msg <- RProtoBuf::new(perftools.profiles.ValueType)
    st_msg$type <- st$type
    st_msg$unit <- st$unit
    st_msg
  })
}

add_samples_to_msg <- function(samples, msg) {
  msg$sample <- lapply(split_rows(samples), function(s) {
    s_msg <- RProtoBuf::new(perftools.profiles.Sample)
    s_msg$value <- s$value
    s_msg$location_id <- s$locations[[1]]$location_id
    s_msg
  })
}

add_locations_to_msg <- function(locations, msg) {
  msg$location <- lapply(split_rows(locations), function(loc) {
    loc_msg <- RProtoBuf::new(perftools.profiles.Location)
    loc_msg$id <- loc$location_id
    if (!is.na(loc$function_id)) {
      loc_msg$line <- list(RProtoBuf::new(perftools.profiles.Line))
      loc_msg$line[[1]]$function_id <- loc$function_id
      loc_msg$line[[1]]$line <- loc$line
    }
    loc_msg
  })
}

add_functions_to_msg <- function(functions, msg) {
  functions$name[functions$name == "<?>"] <- ""
  functions$system_name[functions$system_name == "<?>"] <- ""

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

utils::globalVariables("perftools.profiles.Function")
utils::globalVariables("perftools.profiles.Label")
utils::globalVariables("perftools.profiles.Line")
utils::globalVariables("perftools.profiles.Location")
utils::globalVariables("perftools.profiles.Mapping")
utils::globalVariables("perftools.profiles.Profile")
utils::globalVariables("perftools.profiles.Sample")
utils::globalVariables("perftools.profiles.ValueType")
