\name{stoprdsm}
\alias{stoprdsm}

\title{
Barrier operation.
}

\description{
Standard barrier operation.
}

\usage{
stoprdsm(cls)
}

\arguments{
   \item{cls}{
   Cluster from the \pkg{parallel} package.
   }
}

\details{
Shuts down the given \pkg{parallel} cluster, and removes any
\code{.desc} files that had been created.
}

% \value{
% }

\author{
Norm Matloff
}

% \keyword{models}
