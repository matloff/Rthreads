
\name{rthSplit}
\alias{rthSplit}
\title{Split a shared matrix by factor levels}
\description{Split the rows of a shared matrix into a list of shared matrices according to a factor.}
\usage{rthSplit(M, splitFactor, prefix = "split")}
\arguments{
  \item{M}{Quoted name of a shared matrix in \code{sharedGlobals}.}
  \item{splitFactor}{A factor with one level per row of \code{M}.}
  \item{prefix}{Prefix for names of the resulting shared matrices.}
}
\value{A list of shared matrix references, one per factor level.}
\examples{\dontrun{rthSplit("X", factor(c(1,1,2,2)))}}

