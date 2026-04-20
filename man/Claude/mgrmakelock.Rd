\name{mgrmakelock}
\alias{mgrmakelock}

\title{
Create an \pkg{Rdsm} lock. 
}

\description{
Creates an \pkg{Rdsm} lock. 
}

\usage{
mgrmakelock(cls,lockname,boost=F)
}

\arguments{
  \item{cls}{
  The \pkg{parallel} cluster.
  }
  \item{lockname}{
  Name of the lock, quoted.  
  }
  \item{boost}{
  If TRUE, boost locks will be used.
  }
}

\details{
Run this from the manager (the R process from which you create the 
cluster) if you need a lock.  The lock is created, lockable/unlockable 
by all threads.  If boost is TRUE, The variable will be of class 
\code{boost.mutex}; see the library \pkg{synchronicity} for details.
}

% \value{
% }

\author{
Norm Matloff
}

