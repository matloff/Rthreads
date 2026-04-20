\name{Rdsm-package}
\alias{Rdsm-package}
\alias{Rdsm}
\docType{package}

\title{
Adds a threaded parallel programming paradigm to R.
}

\description{

This package provides a parallel shared-memory programming paradigm for
R, very similar to threaded programming in C/C++. This enables the
programmer to write simpler, clearer code.  Furthermore, in some
applications this package produces significantly faster code, compared
to versions written for other parallel R libraries.  It also allows
placing very large matrices in secondary storage, while treating them as
being in shared memory.  

}

\details{
\tabular{ll}{
Package: \tab Rdsm\cr
Type: \tab Package\cr
Version: \tab 2.1.1\cr
Date: \tab 2014-02-16\cr
License: \tab GPL (>= 2)\cr
}

List of functions:

   \preformatted{
   initialization, run at manager:  

      mgrinit():  initialize system
      mgrmakevar():  create a shared variable 
      mgrmakelock():  create a lock
      makebarr():  create a barrier

   called by applications:  

      barr():  barrier call
      rdsmlock():  lock operation (via realrdsmlock())
      rdsmunlock():  unlock operation (via realrdsmunlock())

   application utilities:  

      getidxs():  partition a set of indices for work assignment
      getmatrix():  allow a matrix to be referenced regardless of 
                    whether it is specified as a bigmemory object, 
                    a bigmemory descriptor, or via a quoted name
      stoprdsm()  shut down cluster and clean up files          
   }

Built-in variables accessible by the threads, at the worker nodes:

   \preformatted{
   myinfo$nwrkrs:  total number of threads
   myinfo$id:  this thread's ID number
   }


To run, set up a cluster via the \pkg{parallel} package); we'll refer to
the R process from which this is done as the manager; the processes
running in the cluster will be called workers.  Create the application's
shared variables from the manager, using \code{mgrmakevar()}.  Launch
the worker threads, again from the manager, by the \pkg{parallel} call
\code{clusterEvalQ()} or \code{clusterCall()}.  One typically codes so
that the results are in shared variables.  See examples below, and more
in the \code{examples/} directory in this distribution.

The shared variables are read to/written by any of the workers and the
manager.  In fact, while an \pkg{Rdsm} application is running, other R
processes on the same machine (or a different machine sharing the same
file system, if the variables are filebacked) can access the shared
variables.  See the file \code{ExternalAccess.txt} in the \code{doc/}
directory.

\pkg{Rdsm} uses the \pkg{bigmemory} library to store its shared
variables.  Though the latter can work on a (physical) cluster of
several machines sharing a file system, \pkg{Rdsm} does not run on such
systems at this time.

Further documentation in the \code{doc/} directory.

}

\author{
Norm Matloff <matloff@cs.ucdavis.edu>
}

\keyword{
parallel computation
shared memory 
threads
}

\seealso{
   \code{\link{mgrinit}}, 
   \code{\link{mgrmakevar}}, 
   \code{\link{mgrmakelock}},
   \code{\link{barr}},
   \code{\link{rdsmlock}},
   \code{\link{rdsmunlock}},
   \code{\link{getidxs}},
   \code{\link{getmatrix}}
}

\examples{
library(parallel)
c2 <- makeCluster(2)  # form 2-thread Snow cluster
mgrinit(c2)  # initialize Rdsm
mgrmakevar(c2,"m",2,2)  # make a 2x2 shared matrix
m[,] <- 3  # 2x2 matrix of all 3s
# example of shared memory:
# at each thread, set id to Rdsm built-in ID variable for that thread
clusterEvalQ(c2,id <- myinfo$id)
clusterEvalQ(c2,m[1,id] <- id^2)  # assignment executed by each thread
m[,]  # top row of m should now be (1,4)

# matrix multiplication; the product u %*% v is computed, product
# placed in w

# note again:  mmul() call will be executed by each thread

mmul <- function(u,v,w) {
   require(parallel)
   # decide which rows of u this thread will work on
   myidxs <- splitIndices(nrow(u),myinfo$nwrkrs)[[myinfo$id]]
   # multiply this thread's part of u with v, placing the product in the
   # corresponding part of w
   w[myidxs,] <- u[myidxs,] \%*\% v[,]
   invisible(0)  
}

# create test matrices
mgrmakevar(c2,"a",6,2)
mgrmakevar(c2,"b",2,6)
mgrmakevar(c2,"c",6,6)
# give them values
a[,] <- 1:12
b[,] <- 1  # all 1s
clusterExport(c2,"mmul")  # send mmul() to the threads
clusterEvalQ(c2,mmul(a,b,c)) # run the threads
c[,]  # check results
}


