
\name{tmSendKeys}
\alias{tmSendKeys}
\title{Send commands to tmux windows}
\description{Send a message to one or more tmux windows and execute it.}
\usage{tmSendKeys(tmName, msg, recip = "all")}
\arguments{
  \item{tmName}{tmux session name.}
  \item{msg}{Command string to send.}
  \item{recip}{Recipients, either \code{"all"} or a vector of window indices.}
}
\value{Nothing.}
\examples{\dontrun{tmSendKeys("abc", "sessionInfo()")}}

