% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-yspec.R
\name{ys_prune}
\alias{ys_prune}
\title{Prune a data frame, keeping columns in a yspec object}
\usage{
ys_prune(data, spec, ..., .report = FALSE)
}
\arguments{
\item{data}{A data frame with at least one column that is found in \code{spec}.}

\item{spec}{A \code{yspec} object.}

\item{...}{Additional columns carry into the output, specified using
tidy-select syntax.}

\item{.report}{If \code{TRUE}, report missing columns.}
}
\value{
A data frame with common columns with \code{spec} and \code{...}, in the order they
appear in \code{spec}.
}
\description{
Use this to scavenge a data frame for columns that you want to keep. Select
additional columns through \code{...}. Do not use this for final column selection;
use \code{\link[dplyr:select]{dplyr::select()}} instead.
}
\details{
An error is generated if there are no columns in common between \code{data} and
\code{spec}.
}
\examples{
data <- ys_help$data()
spec <- ys_help$spec()
data$STUDY <- NULL

head(ys_prune(data, spec))
head(ys_prune(data, spec, .report = TRUE))

data$FOO <- 1
data$BAR <- 2
data$YAK <- 3

head(ys_prune(data, spec, YAK, FOO))

# Use this for final subsetting
# It will fail if all the columns aren't there
data <- ys_help$data()
head(dplyr::select(data, names(spec)))
 
}
