\name{getidxs}
\alias{getidxs}

\title{
Parallelizing work assignment.
}

\description{
Assigns to an \pkg{Rdsm} thread its portion of a set of indices,
for the purpose of partitioning work to the threads.
}

\usage{
getidxs(m)
}

\arguments{
  \item{m}{
  The sequence 1:m will be partitioned, and one portion will be assigned
  to the calling thread.
  }
}

\details{
The range 1:m will be partitioned into r subranges, the i-th of which
will be used by thread i to determine which work that thread has been
assigned.
}

\value{
The subrange assigned to the invoking thread.
}

\author{
Norm Matloff
}

