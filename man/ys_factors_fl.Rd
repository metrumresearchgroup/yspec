% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flags.R
\name{ys_factors_fl}
\alias{ys_factors_fl}
\title{Add factors based on flags}
\usage{
ys_factors_fl(data, spec, ...)
}
\arguments{
\item{data}{the data set to modify.}

\item{spec}{a yspec object.}

\item{...}{tidy-select specification of flag names to select.}
}
\description{
The user passes a data set to modify, a \code{yspec} object, and the names of
flags in the yspec object to create factors in the data set using
\code{\link[=ys_factors]{ys_factors()}}.
}
\details{
Only eligible columns will be considered for factor creation (either
a \code{values} field is present or the \code{make_factor} flag is set).
Therefore, a flag containing a general list of data columns
(potentially continuous and discrete) could be passed without
generating an error.
}
\examples{
data <- ys_help$data()
spec <- ys_help$spec()

names(ys_flags(spec))

data <- ys_factors_fl(data, spec, cat)

head(data, 2)

data <- ys_factors_fl(data, spec, covariate, nm)

head(data, 2)

}
\seealso{
\code{\link[=ys_factors]{ys_factors()}}, \code{\link[=ys_add_factors]{ys_add_factors()}}
}
