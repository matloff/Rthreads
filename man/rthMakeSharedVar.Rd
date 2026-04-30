
\name{rthMakeSharedVar}
\alias{rthMakeSharedVar}
\title{Create a shared matrix variable}
\description{Create a bigmemory-backed matrix and register it in the shared environment.}
\usage{rthMakeSharedVar(varName, nr, nc, initVal = NULL)}
\arguments{
  \item{varName}{Name of the shared variable.}
  \item{nr}{Number of rows.}
  \item{nc}{Number of columns.}
  \item{initVal}{Optional initial values.}
}
\value{Nothing.}
\examples{\dontrun{rthMakeSharedVar("x", 10, 3, 0)}}

