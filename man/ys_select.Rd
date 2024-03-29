% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ys-tidy.R
\name{ys_select}
\alias{ys_select}
\title{Select a subset of items from a yspec object}
\usage{
ys_select(.x, ...)
}
\arguments{
\item{.x}{A yspec object.}

\item{...}{Unquoted columns to select.}
}
\value{
A \code{yspec} object that may be length zero if no columns were selected.
}
\description{
Select a subset of items from a yspec object
}
\details{
If no columns are selected, then an empty \code{yspec} object is returned.
}
\examples{

spec <- ys_help$spec()

ys_select(spec, WT, AGE, ALB)

ys_select(spec, Wt = WT, AGE)
 
length(ys_select(spec))
 
}
