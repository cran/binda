\name{is.binaryMatrix}
\alias{is.binaryMatrix}

\title{Check For Binary Matrix}
\usage{
is.binaryMatrix(m)
}
\arguments{
  \item{m}{a matrix.}
}


\description{
  \code{is.binaryMatrix}  tests whether \code{m} is a matrix and whether it contains only 0s and 1s.

  Note that functions like \code{\link{binda.ranking}} and \code{\link{binda}} require a binary matrix as input.
}

\value{
  \code{is.binaryMatrix} returns either \code{TRUE} or \code{FALSE}.
}


\author{
  Sebastian Gibb and Korbinian Strimmer (\url{https://strimmerlab.github.io}).
}

\examples{
# load binda library
library("binda")

# test matrix
m = matrix(c(1, 1, 0, 1, 0, 0,
             1, 1, 1, 1, 0, 0,
             1, 0, 0, 0, 1, 1,
             1, 0, 0, 0, 1, 1), nrow=4, byrow=TRUE)

# Test for binary matrix
is.binaryMatrix(m) # TRUE
}
\keyword{univar}
