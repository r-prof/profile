#' Definition of the profile data format
#'
#' @description
#' The data format is stable between major releases.
#' In case of major updates, compatibility functions will be provided.
#'
#' The `validate_profile()` function checks a profile data object
#' for compatibility with the specification.
#' Versioning information embedded in the data is considered.
#'
#' @section Data model:
#' \figure{dm.png}
#' @name validate_profile
#' @param x Profile data, e.g., as returned by [read_pprof()] or [read_rprof()].
#' @export
#' @examples
#' rprof_file <- system.file("samples/rprof/1.out", package = "profile")
#' ds <- read_rprof(rprof_file)
#' validate_profile(ds)
#' ds$samples <- NULL
#' try(validate_profile(ds))
validate_profile <- function(x) {
  #' @details
  #' The profile data is stored in an object of class `"profile_data"`,
  #' which is a named list of [tibble]s.
  stopifnot(is.list(x))
  stopifnot(inherits(x, "profile_data"))
  components <- undotted(names(x))
  stopifnot(map_lgl(x[components], tibble::is_tibble))

  #' This named list has the following components, subsequently referred to as
  #' *tables*:
  #' - `meta`
  #' - `sample_types`
  #' - `samples`
  #' - `locations`
  #' - `functions`
  stopifnot(undotted(names(x)) == c("meta", "sample_types", "samples", "locations", "functions"))
  #' (Components with names starting with a dot are permitted
  #' after the required components, but will be ignored.)

  #'
  #' The `meta` table has two character columns, `key` and `value`.
  #' Additional columns with a leading dot in the name are allowed
  #' after the required columns.
  stopifnot(undotted(names(x$meta)) == c("key", "value"))
  stopifnot(is.character(x$meta$key))
  stopifnot(is.character(x$meta$value))
  #' It is currently restricted to one row with key `"version"` and a value
  #' that is accepted by [package_version()].
  stopifnot(nrow(x$meta) == 1)
  stopifnot(x$meta$key == "version")
  package_version(x$meta$value)

  #'
  #' The `sample_types` table has two character columns, `type` and `unit`.
  #' Additional columns with a leading dot in the name are allowed
  #' after the required columns.
  stopifnot(undotted(names(x$sample_types)) == c("type", "unit"))
  stopifnot(is.character(x$sample_types$type))
  stopifnot(is.character(x$sample_types$unit))
  #' It is currently restricted to one row with values `"samples"` and `"count"`,
  #' respectively.
  stopifnot(nrow(x$sample_types) == 1)
  stopifnot(x$sample_types$type == "samples")
  stopifnot(x$sample_types$unit == "count")

  #'
  #' The `samples` table has two columns, `value` (integer) and `locations`
  #' (list).
  #' Additional columns with a leading dot in the name are allowed
  #' after the required columns.
  stopifnot(undotted(names(x$samples)) == c("value", "locations"))
  stopifnot(is.integer(x$samples$value))
  stopifnot(is.list(x$samples$locations))
  #' The `value` column describes the number of consecutive samples for the
  #' given location, and must be greater than zero.
  stopifnot(x$samples$value > 0)
  #' Each element of the `locations` column is a tibble with one integer column,
  #' `location_id`.
  stopifnot(map_lgl(x$samples$locations, tibble::is_tibble))
  stopifnot(map_chr(x$samples$locations, map_chr, class) == "integer")
  stopifnot(map_chr(x$samples$locations, names) == "location_id")
  #' For each `location_id` value a corresponding observation in the `locations`
  #' table must exist.
  stopifnot(unlist(map(x$samples$locations, "[[", "location_id")) %in% x$locations$location_id)
  #' The locations are listed in inner-first order, i.e., the first location
  #' corresponds to the innermost entry of the stack trace.

  #'
  #' The `locations` table has three integer columns, `location_id`,
  #' `function_id`, and `line`.
  #' Additional columns with a leading dot in the name are allowed
  #' after the required columns.
  stopifnot(undotted(names(x$locations)) == c("location_id", "function_id", "line"))
  stopifnot(is.integer(x$locations$location_id))
  stopifnot(is.integer(x$locations$function_id))
  stopifnot(is.integer(x$locations$line))
  #' All `location_id` values are unique.
  stopifnot(!anyDuplicated(x$locations$location_id))
  #' For each `function_id` value a corresponding observation in the `functions`
  #' table must exist. `NA` values are permitted.
  stopifnot(is.na(x$locations$function_id) | x$locations$function_id %in% x$functions$function_id)
  #' The `line` column describes the line in the source code this location
  #' corresponds to, zero if unknown. All values must be nonnegative.
  #' `NA` values are permitted.
  stopifnot(is.na(x$locations$line) | x$locations$line >= 0)

  #'
  #' The `functions` table has five columns, `function_id` (integer),
  #' `name`, `system_name` and `file_name` (character), and `start_line` (integer).
  #' Additional columns with a leading dot in the name are allowed
  #' after the required columns.
  stopifnot(undotted(names(x$functions)) == c("function_id", "name", "system_name", "filename", "start_line"))
  stopifnot(is.integer(x$functions$function_id))
  stopifnot(is.character(x$functions$name))
  stopifnot(is.character(x$functions$system_name))
  stopifnot(is.character(x$functions$filename))
  stopifnot(is.integer(x$functions$start_line))
  #' All `function_id` values are unique.
  stopifnot(!anyDuplicated(x$functions$function_id))
  #' The `name`, `system_name` and `filename` columns describe function names
  #' (demangled and mangled), and source file names for a function.
  #' Both `name` and `system_name` must not contain empty strings.
  stopifnot(x$functions$name != "")
  stopifnot(x$functions$system_name != "")
  #' The `start_line` column describes the start line of a function in its
  #' source file, zero if unknown. All values must be nonnegative.
  stopifnot(x$functions$start_line >= 0)

}

undotted <- function(x) {
  x[seq_len(max(grep("^[^.]", x)))]
}

get_default_meta <- function() {
  tibble::tibble(
    key = "version",
    value = "1.0"
  )
}

new_profile_data <- function(x) {
  structure(x, class = "profile_data")
}

#' @export
print.profile_data <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.profile_data <- function(x, ...) {
  paste0("Profile data: ", nrow(x$samples), " samples")
}
