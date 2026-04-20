\name{mgrinit}
\alias{mgrinit}

\title{
Initialize \pkg{Rdsm}
}

\description{
Initializes \pkg{Rdsm} on the given \pkg{parallel} cluster.
}

\usage{
mgrinit(cls,boost=F,barrback=F)
}

\arguments{
  \item{cls}{
  The \pkg{parallel} cluster.
  }
  \item{boost}{
  Lock functions.  If TRUE, \code{boostlock()} and \code{boostunlock()}
  are used; otherwise \code{backlock()} and \code{backunlock()}.
  }
  \item{barrback}{
  If TRUE, the count/sense variables associated with the barrier will be
  placed in backing store.
  }
}

\details{
Run this from the manager (the R process from which you create the
cluster), before creating the shared variables with \code{mgrmakevar}.
The initialization need be done only once for the life of the cluster.

If you put shared variables in backing store (\code{barrback = TRUE} in
\code{mgrmakevar()}), or if you are on a Windows platform, you must 
have \code{boost = FALSE}.  
}

% \value{
% }

\author{
Norm Matloff
}

