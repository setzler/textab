% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concatenation.R
\name{+.tt_}
\alias{+.tt_}
\title{Concatenate textab blocks vertically.}
\usage{
\method{+}{tt_}(upper_block, lower_block)
}
\arguments{
\item{upper_block}{The upper block of the tabular row.}

\item{lower_block}{The lower block of the tabular row.}
}
\value{
The output is a textab block, formed by vertically concatenating the two provided textab blocks.
}
\description{
Concatenate textab blocks vertically.
}
\examples{
# define some textab blocks
first_block = TexRow(c(1,2))
first_block

second_block = TexRow(3)
second_block

third_block = TexRow(4)
third_block

# concatenate two blocks vertically
first_block + second_block

# concatenate three blocks vertically
first_block + second_block + third_block

# concatenate both horizontally and vertically
# note: horizontal concatenation takes precedence over vertical concatenation
first_block + second_block / third_block

}
