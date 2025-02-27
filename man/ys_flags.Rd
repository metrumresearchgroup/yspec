% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flags.R
\name{ys_flags}
\alias{ys_flags}
\alias{ys_flags_chr}
\title{Extract flags from yspec meta data}
\usage{
ys_flags(x, ...)

ys_flags_chr(x, ...)
}
\arguments{
\item{x}{a yspec object.}

\item{...}{tidy-select specification of flag names to select.}
}
\value{
\code{ys_flags()} returns a named list of flag column names. \code{ys_flags_chr()}
returns a character vector of all flag columns returned by \code{ys_flags()}.
}
\description{
Use \code{ys_flags_chr()} to get a character vector of all columns returned by
\code{ys_flags()}.
}
\examples{

spec <- ys_help$spec()

ys_flags(spec, nm)

ys_flags_chr(spec, nm) 

}
\seealso{
\code{\link[=ys_select_fl]{ys_select_fl()}}, \code{\link[=ys_factors_fl]{ys_factors_fl()}}
}
