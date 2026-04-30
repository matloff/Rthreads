
\name{tmRthreadsInit}
\alias{tmRthreadsInit}
\title{Initialize Rthreads in tmux windows}
\description{Set up multiple tmux windows and start Rthreads across them.}
\usage{tmRthreadsInit(nWindows, tmName = "abc", vert = FALSE)}
\arguments{
  \item{nWindows}{Number of tmux windows.}
  \item{tmName}{tmux session name.}
  \item{vert}{Logical; use vertical splits when \code{TRUE}.}
}
\value{Nothing.}
\examples{\dontrun{tmRthreadsInit(2, "abc")}}

