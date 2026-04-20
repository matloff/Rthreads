\name{readsync}
\alias{readsync}
\alias{writesync}

\title{
Syncing file-backed variables.
}

\description{
Actions to propagate changes to file-backed \pkg{Rdsm} variables across
a shared file system.
}

\usage{
readsync(varname)
writesync(varname)
}

\arguments{
  \item{varname}{
  Name of the \pkg{Rdsm} variable, quoted.
  }
}

\details{ 

This feature should be considered experimental, with poor performance
and portability.  

Suppose we have an \pkg{Rdsm} variable \code{x} which is in
backing store (\code{fs = TRUE} in call to \code{mgrmakevar()}), and
that we on a shared file system.  (These functions are not needed if all
threads are on the same machine.) When one \pkg{Rdsm} thread writes to
\code{x}, the question here is when the updated value for \code{x} is
visible to other \pkg{Rdsm} threads.

The answer may depend on the underlying file system.  The functions
\code{readsync()} and \code{writesync()} force the updates across the
network.  Normally one would call \code{readsync()} following
\code{rdsmlock()} and call \code{writesync()} just before calling
\code{rdsmunlock()}.

These should work on systems with "close-to-open cache coherency," as
with the Network File System (NFS).  On some systems, these functions
should be unnecessary.  
}

\value{
None.
}

\examples{
library(parallel)
c2 <- makeCluster(2)
mgrinit(c2)
mgrmakevar(c2,"x",1,1,fs=TRUE)

clusterEvalQ(c2,me <- myinfo$id)
clusterEvalQ(c2,if (me==1) x[1,1] <- 3)
# force update on network
clusterEvalQ(c2,if (me==1) writesync("x"))  
clusterEvalQ(c2,if (me==2) readsync("x"))  
clusterEvalQ(c2,if (me==2) x[1,1])  # should be 3
clusterEvalQ(c2,if (me==2) x[1,1] <- 8)
clusterEvalQ(c2,if (me==2) writesync("x"))  
clusterEvalQ(c2,if (me==1) readsync("x"))  
clusterEvalQ(c2,x[1,1])  # both should yield 8
}

\author{
Norm Matloff
}

