\name{barr}
\alias{barr}

\title{
Barrier operation.
}

\description{
Standard barrier operation.
}

\usage{
barr()
}

% \arguments{
% }

\details{
Standard barrier operation, to ensure that work done by one thread is 
ready before other threads make use of it.  When a thread executes 
\code{barr()}, it will block until all threads have executed it.
}

% \value{
% }

\author{
Norm Matloff
}

% \keyword{models}
