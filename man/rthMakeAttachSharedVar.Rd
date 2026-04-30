
\name{rthMakeAttachSharedVar}
\alias{rthMakeAttachSharedVar}
\title{Create or attach a shared variable}
\description{Create a shared variable in thread 0 and attach it in other threads.}
\usage{rthMakeAttachSharedVar(varName, nr, nc, initVal = NULL)}
\arguments{
  \item{varName}{Name of the shared variable.}
  \item{nr}{Number of rows.}
  \item{nc}{Number of columns.}
  \item{initVal}{Optional initial values.}
}
\value{Nothing.}
\examples{\dontrun{rthMakeAttachSharedVar("x", 10, 3)}}

