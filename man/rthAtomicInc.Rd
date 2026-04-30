
\name{rthAtomicInc}
\alias{rthAtomicInc}
\title{Atomically increment a shared variable}
\description{Increase the first row of a shared variable under mutex protection.}
\usage{rthAtomicInc(sharedV, mtx = "mutex0", increm = 1)}
\arguments{
  \item{sharedV}{Quoted name of a shared variable.}
  \item{mtx}{Quoted name of the mutex to use.}
  \item{increm}{Increment amount.}
}
\value{The previous value of the variable.}
\examples{\dontrun{rthAtomicInc("nJoined")}}

