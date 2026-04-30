
\name{tmReduceNThreads}
\alias{tmReduceNThreads}
\title{Reduce the number of threads in a tmux session}
\description{Update the shared thread count and exit selected tmux windows.}
\usage{tmReduceNThreads(toRemove, tmName = "abc")}
\arguments{
  \item{toRemove}{Integer indices of threads to remove.}
  \item{tmName}{tmux session name.}
}
\value{Nothing.}
\details{Assumes no active computation is currently in progress.}
\examples{\dontrun{tmReduceNThreads(4:5, "abc")}}

