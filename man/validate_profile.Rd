% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{validate_profile}
\alias{validate_profile}
\alias{dm_from_profile}
\title{Definition of the profile data format}
\usage{
validate_profile(x)

dm_from_profile(x)
}
\arguments{
\item{x}{Profile data, e.g., as returned by \code{\link[=read_pprof]{read_pprof()}} or \code{\link[=read_rprof]{read_rprof()}}.}
}
\description{
The data format is stable between major releases.
In case of major updates, compatibility functions will be provided.

The \code{validate_profile()} function checks a profile data object
for compatibility with the specification.
Versioning information embedded in the data is considered.

The \code{dm_from_profile()} function converts a profile to a dm object.
The \pkg{dm} package must be installed.
See \code{\link[dm:dm]{dm::dm()}} for more information.
}
\details{
The profile data is stored in an object of class \code{"profile_data"},
which is a named list of \link[tibble:tibble]{tibble::tibble}s.
This named list has the following components, subsequently referred to as
\emph{tables}:
\itemize{
\item \code{meta}
\item \code{sample_types}
\item \code{samples}
\item \code{locations}
\item \code{functions}
(Components with names starting with a dot are permitted
after the required components, but will be ignored.)
}

The \code{meta} table has two character columns, \code{key} and \code{value}.
Additional columns with a leading dot in the name are allowed
after the required columns.
It is currently restricted to one row with key \code{"version"} and a value
that is accepted by \code{\link[=package_version]{package_version()}}.

The \code{sample_types} table has two character columns, \code{type} and \code{unit}.
Additional columns with a leading dot in the name are allowed
after the required columns.
It is currently restricted to one row with values \code{"samples"} and \code{"count"},
respectively.

The \code{samples} table has two columns, \code{value} (integer) and \code{locations}
(list).
Additional columns with a leading dot in the name are allowed
after the required columns.
The \code{value} column describes the number of consecutive samples for the
given location, and must be greater than zero.
Each element of the \code{locations} column is a tibble with one integer column,
\code{location_id}.
For each \code{location_id} value a corresponding observation in the \code{locations}
table must exist.
The locations are listed in inner-first order, i.e., the first location
corresponds to the innermost entry of the stack trace.

The \code{locations} table has three integer columns, \code{location_id},
\code{function_id}, and \code{line}.
Additional columns with a leading dot in the name are allowed
after the required columns.
All \code{location_id} values are unique.
For each \code{function_id} value a corresponding observation in the \code{functions}
table must exist. \code{NA} values are permitted.
The \code{line} column describes the line in the source code this location
corresponds to, zero if unknown. All values must be nonnegative.
\code{NA} values are permitted.

The \code{functions} table has five columns, \code{function_id} (integer),
\code{name}, \code{system_name} and \code{file_name} (character), and \code{start_line} (integer).
Additional columns with a leading dot in the name are allowed
after the required columns.
All \code{function_id} values are unique.
The \code{name}, \code{system_name} and \code{filename} columns describe function names
(demangled and mangled), and source file names for a function.
Both \code{name} and \code{system_name} must not contain empty strings.
The \code{start_line} column describes the start line of a function in its
source file, zero if unknown. All values must be nonnegative.
}
\section{Data model}{

\figure{dm.png}
}

\examples{
rprof_file <- system.file("samples/rprof/1.out", package = "profile")
ds <- read_rprof(rprof_file)
validate_profile(ds)

bad_ds <- ds
bad_ds$samples <- NULL
try(validate_profile(bad_ds))
\dontshow{if (rlang::is_installed("dm")) withAutoprint(\{ # examplesIf}

dm <- dm_from_profile(ds)
print(dm)
\dontshow{\}) # examplesIf}
\dontshow{if (rlang::is_installed(c("dm", "DiagrammeR"))) withAutoprint(\{ # examplesIf}

dm::dm_draw(dm)
\dontshow{\}) # examplesIf}
}
