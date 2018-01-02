rprof_to_ds <- function(rprof) {
  sample_types <- get_sample_types_from_rprof(rprof)

  flat_rprof <- get_flat_rprof_from_rprof(rprof)

  flat_rprof <- add_functions_to_flat_rprof(flat_rprof)
  flat_rprof <- add_locations_to_flat_rprof(flat_rprof)
  flat_rprof <- add_samples_to_flat_rprof(flat_rprof)

  ret <- tibble::lst(
    meta = get_default_meta(),
    sample_types,
    samples = flat_rprof$samples,
    locations = flat_rprof$locations,
    functions = flat_rprof$functions,
    .rprof = rprof
  )
  new_profile_data(ret)
}

get_sample_types_from_rprof <- function(rprof) {
  tibble::tibble(
    type = "samples",
    unit = "count"
  )
}

get_flat_rprof_from_rprof <- function(rprof) {
  traces_split <- strsplit(gsub('$', "", rprof$traces), '" ', fixed = TRUE)
  samples <- tibble::tibble(
    sample_id = seq_along(traces_split),
    location = map(traces_split, tibble::enframe, name = "sample_seq", value = "loc")
  )
  samples_flat <- tibble::as_tibble(cbind(
    tibble::tibble(
      sample_id = rep(samples$sample_id, map_int(samples$location, nrow))
    ),
    merge_rows(samples$location)
  ))

  rx <- '^(?:|([0-9]+)#([0-9]+) )"([^"]*)$'
  samples_flat$file_id <- empty_to_zero(gsub(rx, "\\1", samples_flat$loc))
  samples_flat$line <- empty_to_zero(gsub(rx, "\\2", samples_flat$loc))
  samples_flat$system_name <- gsub(rx, "\\3", samples_flat$loc)
  samples_flat$loc <- NULL

  list(
    flat = samples_flat,
    rprof = rprof
  )
}

empty_to_zero <- function(x) {
  x[x == ""] <- "0"
  as.integer(x)
}

add_functions_to_flat_rprof <- function(flat_rprof) {
  . <- flat_rprof$flat[c("system_name", "file_id", "line")]
  . <- .[invoke(order, .), ]
  . <- .[!duplicated(.[c("system_name", "file_id")]), ]
  functions <- .

  files <- gsub("^#File [0-9]+: ", "", flat_rprof$rprof$files)
  functions$filename <- na_to_empty(files[zero_to_na(functions$file_id)])

  functions$function_id <- seq_len(nrow(functions))

  flat_rprof$functions <- tibble::tibble(
    function_id = functions$function_id,
    name = rprof_system_name_to_name(functions$system_name),
    system_name = functions$system_name,
    filename = functions$filename,
    start_line = functions$line,
    .file_id = functions$file_id
  )

  flat_rprof
}

zero_to_na <- function(x) {
  x[x == 0] <- NA
  x
}

na_to_empty <- function(x) {
  x[is.na(x)] <- ""
  x
}

add_locations_to_flat_rprof <- function(flat_rprof) {
  . <- merge(
    flat_rprof$flat[c("system_name", "file_id", "line")],
    flat_rprof$functions[c("system_name", ".file_id", "function_id")],
    by = 1:2
  )
  . <- .[-1:-2]
  . <- unique(.)
  .$location_id <- seq_len(nrow(.))
  . <- .[c("location_id", "function_id", "line")]
  . <- tibble::as_tibble(.)
  locations <- .

  flat_rprof$locations <- locations

  flat_rprof
}

add_samples_to_flat_rprof <- function(flat_rprof) {
  . <- merge(
    flat_rprof$functions[c("function_id", "system_name", ".file_id")],
    flat_rprof$locations[c("function_id", "location_id", "line")],
    by = 1L
  )
  . <- merge(
    flat_rprof$flat[c("file_id", "line", "system_name", "sample_id", "sample_seq")],
    .[c(".file_id", "line", "system_name", "location_id")],
    by = 1:3
  )
  . <- .[-1:-3]
  . <- .[invoke(order, .[c("sample_id", "sample_seq")]), ]
  . <- .[c("sample_id", "location_id")]

  . <- tibble::tibble(
    sample_id = unique(.$sample_id),
    locations = split(., .$sample_id)
  )

  .$locations <- map(.$locations, "[", "location_id")
  .$locations <- map(.$locations, tibble::as_tibble, rownames = NULL)

  .$value <- 1L
  . <- .[c("value", "locations")]

  flat_rprof$samples <- .

  flat_rprof
}

rprof_system_name_to_name <- function(x) {
  . <- x
  . <- gsub(".", "_", ., fixed = TRUE)
  . <- gsub("<GC>", "gc", ., fixed = TRUE)
  .
}
