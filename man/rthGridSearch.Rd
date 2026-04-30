
\name{rthGridSearch}
\alias{rthGridSearch}
\title{Threaded parallel grid search}
\description{Search over a grid of tuning parameter values using threaded cross-validation.}
\usage{rthGridSearch(basicCall, dta, yName, pars, nXval, nTest, classif = FALSE, predFtnReg = NULL, predFtnClassif = NULL)}
\arguments{
  \item{basicCall}{Character string for the model call, without the closing parenthesis.}
  \item{dta}{Data frame containing the response and predictors.}
  \item{yName}{Name of the response column in \code{dta}.}
  \item{pars}{Named list of tuning parameters to expand over.}
  \item{nXval}{Number of cross-validation repetitions per parameter combination.}
  \item{nTest}{Number of rows to use in each test split.}
  \item{classif}{Logical; if \code{TRUE}, use classification error, otherwise use mean absolute error.}
  \item{predFtnReg}{Optional function applied to regression predictions returned by \code{predict()}.}
  \item{predFtnClassif}{Optional function applied to classification predictions returned by \code{predict()}.}
}
\value{A data frame of tuning combinations with columns for the parameters and summary loss statistics.}
\details{The function evaluates each parameter combination over repeated random train/test splits. It assumes that \code{Rthreads} has already been initialized.}
\seealso{\code{splitTrainTest}, \code{rthSetup}, \code{rthJoin}}
\examples{\dontrun{\n# See package documentation and the bundled examples.\n}}

