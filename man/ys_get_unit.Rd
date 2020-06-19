% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ys-get.R
\name{ys_get_unit}
\alias{ys_get_unit}
\title{Get unit}
\usage{
ys_get_unit(x, parens = FALSE, default = "", ...)
}
\arguments{
\item{x}{ycol or yspec object}

\item{parens}{if `TRUE`, then parens will be added around any unit with one
or more character}

\item{default}{passed to unit}

\item{...}{passed to unit methods}
}
\description{
Forms unit for a column (`ycol` method) or columns (`yspec` method). Use
[purrr::map] or [purrr::map_chr] to simply extract the unit field.
}
\examples{
spec <- ys_help$spec()

ys_get_unit(spec)

ys_get_unit(spec$WT)

\dontrun{
purrr:::map(spec,"unit")
}
}
\seealso{
[ys_get_short], [ys_get_label], [ys_get_short_unit]
}