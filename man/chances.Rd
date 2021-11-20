\name{chances}
\alias{chances}

\title{Estimate Bernoulli Parameters from Binary Matrix with Class Labels}

\usage{
chances(X, L, lambda.freqs, verbose=TRUE)
}

\description{
  \code{chances} estimates Bernoulli parameters (=chances) from a binary matrix and associated class labels.
}

\arguments{
  \item{X}{data matrix (columns correspond to variables, rows to samples).}
  \item{L}{factor containing the class labels, one for each sample (row).}
  \item{lambda.freqs}{shrinkage parameter for class frequencies (if not specified it is estimated).}
  \item{verbose}{report shrinkage intensity and other information.}
}
\details{
   The class-specific chances are estimated using the empirical means over the 0s and 1s in each class. For estimating the pooled mean the class-specific means are weighted using the
   estimated class frequencies. Class frequencies are estimated using \code{\link[entropy]{freqs.shrink}}.
}
\value{
  \code{chances} returns a list with the following components:

  \code{samples}: the samples in each class,

  \code{regularization}: the shrinkage intensity used to estimate the class frequencies,

  \code{freqs}: the estimated class frequencies,

  \code{means}:  the estimated chances (parameters of Bernoulli distribution, expectations of 1s) for each variable conditional on class, as well as the marginal changes (pooled means).
}


\author{
  Sebastian Gibb and Korbinian Strimmer (\url{https://strimmerlab.github.io}).
}
\seealso{ \code{\link{is.binaryMatrix}}.}

\examples{
# load binda library
library("binda")

# example binary matrix with 6 variables (in columns) and 4 samples (in rows)
Xb = matrix(c(1, 1, 0, 1, 0, 0,
             1, 1, 1, 1, 0, 0,
             1, 0, 0, 0, 1, 1,
             1, 0, 0, 0, 1, 1), nrow=4, byrow=TRUE)
colnames(Xb) = paste0("V", 1:ncol(Xb))

# Test for binary matrix
is.binaryMatrix(Xb) # TRUE

L = factor(c("Treatment", "Treatment", "Control", "Control") )

chances(Xb, L)
}
\keyword{multivariate}


