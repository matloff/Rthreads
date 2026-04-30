
\name{shortFactor}
\alias{shortFactor}
\title{Drop unused factor levels}
\description{Convert a factor to a new factor with only the used levels retained.}
\usage{shortFactor(f)}
\arguments{\item{f}{A factor or factor-like object.}}
\value{A factor with unused levels removed.}
\examples{\dontrun{shortFactor(factor(c("a","a","b")))}}

