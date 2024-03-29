% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ys-tidy.R
\name{ys_mutate}
\alias{ys_mutate}
\title{Mutate yspec item data}
\usage{
ys_mutate(x, ...)
}
\arguments{
\item{x}{A yspec object.}

\item{...}{Named lists of column data to update; only specific items can be 
mutated at this time; see details.}
}
\value{
A `yspec` object
}
\description{
Mutate yspec item data
}
\details{
Items that can be mutated: 
- `short`
- `unit`
- `label`
}
\examples{

spec <- ys_help$spec()
xpec <- yspec:::ys_mutate(spec, TIME = list(unit = "d"), TAD = list(unit = 'w'))
spec$TIME
spec$TAD

}
\seealso{
[update_short()]
}
