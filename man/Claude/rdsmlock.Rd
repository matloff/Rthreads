\name{rdsmlock}
\alias{rdsmlock}
\alias{rdsmunlock}

\title{
Lock/unlock operations.
}

\description{
Lock/unlock operations to avoid race conditions among the threads.  
}

\usage{
rdsmlock(lck)
rdsmunlock(lck)
}

\arguments{
  \item{lck}{ Lock name, quoted.}
}

\details{
Standard lock/unlock operations from the threaded coding world.  When
one thread executes \code{rdsmlock()}, any other thread attempting to do so
will block until the first thread executes \code{rdsmunlock()}.  If a thread
does \code{rdsmlock()} on an unlocked lock, the thread call immediately
returns and the thread continues.

These functions are set in the call to \code{mgrinit()} via the 
argument \code{boost} to either \code{boostlock} and
\code{boostunlock()} or \code{backlock} and \code{backunlock()},
depending on whether you set \code{boost} to TRUE or FALSE.
respectively.  

Code should be written so that locks are used as sparingly as possible,
since they detract from performance.
}

\author{
Norm Matloff
}

\examples{

\dontrun{
# unreliable function
s <- function(n) {
   for (i in 1:n) {
      tot[1,1] <- tot[1,1] + 1
   }
}

library(parallel)
c2 <- makeCluster(2)
clusterExport(c2,"s")
mgrinit(c2)
mgrmakevar(c2,"tot",1,1)
tot[1,1] <- 0
clusterEvalQ(c2,s(1000))
tot[1,1]  # should be 2000, but likely far from it

s1 <- function(n) {
   require(Rdsm)
   for (i in 1:n) {
      rdsmlock("totlock")
      tot[1,1] <- tot[1,1] + 1
      rdsmunlock("totlock")
   }
}

mgrmakelock(c2,"totlock")
tot[1,1] <- 0
clusterExport(c2,"s1")
clusterEvalQ(c2,s1(1000))
tot[1,1]  # will print out 2000, the correct number
}

}


