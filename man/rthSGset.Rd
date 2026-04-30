
\name{rthSGset}
\alias{rthSGset}
\title{Set a shared global element}
\description{Assign values into a shared variable stored in \code{sharedGlobals}.}
\usage{rthSGset(sharedVarName, rows, cols, value)}
\arguments{
  \item{sharedVarName}{Name of the shared variable.}
  \item{rows}{Row indices.}
  \item{cols}{Column indices.}
  \item{value}{Replacement value.}
}
\value{Nothing.}
\examples{\dontrun{rthSGset("nThreads", 1, 1, 2)}}

