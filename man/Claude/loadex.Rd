\name{loadex}
\alias{loadex}

\title{
Sources example files
}

\description{
Facilitates learning a new package, by sourcing any specified example
file for an installed package.
}

\usage{
loadex(pkg,exfile=NA,subdir="examples")
}

\arguments{
   \item{pkg}{Package name, quoted.}
   \item{exfile}{Desired example file name, if any, quoted.}
   \item{subdir}{Subdirectory name of the examples directory in the
   package.}
}

\details{Loads the example file \code{exfile}, from the \code{subdir}
directory within the tree where\code{pkg} is installed.
If \code{exfile = NA}, the names of the files are
returned, without any loading.
}

\value{See the case \code{exfile = NA} above.
}

\author{
Norm Matloff
}

% \keyword{models}
