
\name{rthSetup}
\alias{rthSetup}
\title{Initialize shared thread state}
\description{Create the shared variables and synchronization objects needed by \code{Rthreads}.}
\usage{rthSetup(nThreads)}
\arguments{\item{nThreads}{Number of threads to initialize.}}
\value{Nothing.}
\details{Should be run by thread 0.}
\examples{\dontrun{rthSetup(2)}}

