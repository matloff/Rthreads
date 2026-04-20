\name{getmatrix}
\alias{getmatrix}

\title{
Referencing a matrix via different forms.
}

\description{
Returns a matrix, whether requested via an R matrix, \code{bigmemory}
object, \code{bigmemory} descriptor or quoted name form.
}

\usage{
getmatrix(m)
}

\arguments{
  \item{m}{
  Specification of the matrix, as either an R matrix, \code{bigmemory} 
  object, \code{bigmemory} descriptor or quoted name.
  }
}

\details{
This utility function enables writing general \pkg{Rdsm} code,
specifying a matrix via different forms.
}

\value{
The requested matrix.
}

\examples{
library(parallel)
c2 <- makeCluster(2)
mgrmakevar(c2,"u",2,2)  # u is a 2x2 bigmemory matrix
u[] <- 8  # fill u with 8s
u[]  # prints a 2x2 matrix of 8s
v <- getmatrix(u)  # get u and assign it to v
# u and v are both addresses, pointing to the same memory location
v[]  # prints all 8s
v[2,1] <- 3
v[]  # prints three 8s and a 3
u[]  # prints three 8s and a 3
w <- getmatrix("u")  # w will also be a copy of u
w[]  # same as u
}

\author{
Norm Matloff
}

