\name{makebarr}
\alias{makebarr}

\title{
Create an \pkg{Rdsm} barrier. 
}

\description{
Creates an \pkg{Rdsm} barrier. 
}

\usage{
makebarr(cls,boost=F,barrback=F)
}

\arguments{
  \item{cls}{
  The \pkg{snow} cluster.
  }
  \item{boost}{
  Locks type.  See \code{mgrinit}.
  }
  \item{barrback}{
  If TRUE, the count/sense variables related to the barrier will be
  placed in backing store.
  }
}

\details{
Run this from the manager (the R process from which you create the 
cluster) if you need a barrier.  Only one barrier is allowed in an
\pkg{Rdsm} program (but multiple calls are allowed).  It is 
accessible from application code only through \code{barr()}.
}

% \value{
% }

\author{
Norm Matloff
}

