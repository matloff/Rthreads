
\name{rthSGget}
\alias{rthSGget}
\title{Get a shared global element}
\description{Retrieve values from a shared variable stored in \code{sharedGlobals}.}
\usage{rthSGget(sharedVarName, rows = "all", cols = "all", pointerOnly = FALSE)}
\arguments{
  \item{sharedVarName}{Name of the shared variable.}
  \item{rows}{Row indices or \code{"all"}.}
  \item{cols}{Column indices or \code{"all"}.}
  \item{pointerOnly}{If \code{TRUE}, return the shared object itself.}
}
\value{The requested entries or the shared object.}
\examples{\dontrun{rthSGget("nThreads")}}

