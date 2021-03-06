% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nm_input.R
\name{nm_input}
\alias{nm_input}
\title{Write columns and column information for NONMEM $INPUT block}
\usage{
nm_input(spec, width = 60, cat = TRUE)
}
\arguments{
\item{spec}{a yspec object}

\item{width}{number of characters}

\item{cat}{if \code{TRUE}, the text is sent to the console with \code{\link[=cat]{cat()}}}
}
\value{
A character vector of text forming the \verb{$INPUT} block (including
the \verb{$INPUT} part).
}
\description{
Column names are written to the console one-per-line on the
left hand side of a NONMEM comment character (\verb{;}) and
a description of the column is written on the right hand
side.  The description includes the short name and values
with decodes for non-character columns.
}
\examples{
spec <- ys_help$spec()
nm_input(spec)
}
