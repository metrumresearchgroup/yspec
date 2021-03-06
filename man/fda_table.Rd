% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fda_define.R
\name{fda_table}
\alias{fda_table}
\alias{fda_table_file}
\title{Generate a table for FDA define.pdf document}
\usage{
fda_table(x)

fda_table_file(file)
}
\arguments{
\item{x}{a yspec object}

\item{file}{the full path to yaml specification file}
}
\value{
Character vector of latex code for the content of an FDA
\code{define.pdf} document.  It includes a table of contents as well as data spec
tables for each dataset for a project.
}
\description{
Generate a table for FDA define.pdf document
}
\examples{
spec <- load_spec_ex()
spec
fda_table(spec)

}
