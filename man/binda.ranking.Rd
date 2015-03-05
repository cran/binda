\name{binda.ranking}
\alias{binda.ranking}
\alias{plot.binda.ranking}

\title{Binary Discriminant Analysis: Variable Ranking}

\description{
  \code{binda.ranking} determines a ranking of predictors by computing corresponding t-scores
   between the group means and the pooled mean.

  \code{plot.binda.ranking} provides a graphical visualization of the top ranking variables
}

\usage{
binda.ranking(Xtrain, L, lambda.freqs, verbose=TRUE)
\method{plot}{binda.ranking}(x, top=40, arrow.col="blue", zeroaxis.col="red", ylab="Variables", main, ...)
}

\arguments{
  \item{Xtrain}{A matrix  containing the training data set. Note that 
                the rows correspond to observations and the columns
                to variables.}
  \item{L}{A factor with the class labels of the training samples. }
   \item{lambda.freqs}{Shrinkage intensity for the class frequencies. If not specified it is 
    estimated from the data. \code{lambda.freqs=0} implies no shrinkage (i.e. empirical frequencies)
    and \code{lambda.freqs=1} complete shrinkage (i.e. uniform frequencies). }
   \item{verbose}{Print out some info while computing.}
  \item{x}{A "binda.ranking" object -- this is produced by the binda.ranking() function.}
  \item{top}{The number of top-ranking variables shown in the plot (default: 40).}
  \item{arrow.col}{Color of the arrows in the plot (default is \code{"blue"}).}
  \item{zeroaxis.col}{Color for the center zero axis (default is \code{"red"}).}
  \item{ylab}{Label written next to feature list (default is \code{"Variables"}).}
  \item{main}{Main title (if missing, \code{"The", top, "Top Ranking Variables"} is used).}
  \item{...}{Other options passed on to generic plot().}
}
\details{
The overall ranking of a feature is determined by computing a weighted sum of
the squared t-scores. This is approximately equivalent to the mutual information between the response and each variable.  The same criterion is used in \code{\link{dichotomize}}.  For precise details see Gibb and Strimmer (2015).
}

\references{
Gibb, S., and K. Strimmer. 2015.  Differential protein expression and peak selection in mass spectrometry data by binary discriminant analysis.  Submitted.
\url{http://arxiv.org/abs/1502.07959}
}

\value{

  \code{binda.ranking} returns a matrix with the following columns:

  \item{idx}{original feature number}
   \item{score}{the score determining the overall ranking of a variable}
   \item{t}{for each group and feature the t-score of the class mean versus the pooled mean}
}

\author{
  Sebastian Gibb and Korbinian Strimmer (\url{http://strimmerlab.org}).
}
\seealso{\code{\link{binda}}, \code{\link{predict.binda}}, \code{\link{dichotomize}}.}

\examples{
# load "binda" library
library("binda")

# training data set with labels
Xtrain = matrix(c(1, 1, 0, 1, 0, 0,
             1, 1, 1, 1, 0, 0,
             1, 0, 0, 0, 1, 1,
             1, 0, 0, 0, 1, 1), nrow=4, byrow=TRUE)
colnames(Xtrain) = paste0("V", 1:ncol(Xtrain))
is.binaryMatrix(Xtrain) # TRUE
L = factor(c("Treatment", "Treatment", "Control", "Control") )

# ranking variables
br = binda.ranking(Xtrain, L)
br
#   idx    score t.Control t.Treatment
#V2   2 4.000000 -2.000000    2.000000
#V4   4 4.000000 -2.000000    2.000000
#V5   5 4.000000  2.000000   -2.000000
#V6   6 4.000000  2.000000   -2.000000
#V3   3 1.333333 -1.154701    1.154701
#V1   1 0.000000  0.000000    0.000000
#attr(,"class")
#[1] "binda.ranking"
#attr(,"cl.count")
#[1] 2

# show plot
plot(br)

# result: variable V1 is irrelevant for distinguishing the two groups


}
\keyword{multivariate}
