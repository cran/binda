\name{binda}
\alias{binda}
\alias{predict.binda}

\title{Binary Discriminant Analysis: Model Fit and Class Prediction}

\description{
  \code{binda} trains a diagonal multivariate Bernoulli model.
  \code{predict.binda} performs corresponding class prediction.
}

\usage{
binda(Xtrain, L, lambda.freqs, verbose=TRUE)
\method{predict}{binda}(object, Xtest, verbose=TRUE, ...)
}

\arguments{
  \item{Xtrain}{A matrix  containing the training data set. Note that 
                the rows correspond to observations and the columns
                to variables.}
  \item{L}{A factor with the class labels of the training samples. }
  \item{lambda.freqs}{Shrinkage intensity for the frequencies. If not specified it is 
    estimated from the data. \code{lambda.freqs=0} implies no shrinkage (i.e. empirical frequencies)
    and \code{lambda.freqs=1} complete shrinkage (i.e. uniform frequencies). }
  \item{verbose}{Report shrinkage intensities (binda) and number of used features (predict.binda).}
  \item{object}{An \code{binda} fit object obtained from the function \code{binda}.}
  \item{Xtest}{A matrix containing the test data set. Note that 
                the rows correspond to observations and the columns
                to variables.}
   \item{...}{Additional arguments for generic predict.}
}

\value{
   \code{predict.binda} predicts class probabilities for each test sample and returns
   a list with two components:

  \item{class}{a factor with the most probable class assignment for each test sample, and}
   \item{posterior}{a matrix containing the respective class posterior probabilities.}
  
}


\author{
  Sebastian Gibb and Korbinian Strimmer (\url{http://strimmerlab.org}).
}
\seealso{\code{\link{binda.ranking}}.}

\details{
 For detailed description of binary discriminant analysis as implented in \code{binda} see Gibb and Strimmer (2015).
}


\references{
Gibb, S., and K. Strimmer. 2015.  Differential protein expression and peak selection in mass spectrometry data by binary discriminant analysis.  Submitted.
\url{http://arxiv.org/abs/1502.07959}
}


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

# learn predictor
binda.fit = binda(Xtrain, L)

# predict classes using new test data
Xtest = matrix(c(1, 1, 0, 1, 1, 1,
                 1, 0, 0, 0, 1, 1), nrow=2, byrow=TRUE)
colnames(Xtest) = paste0("V", 1:ncol(Xtest))

predict(binda.fit, Xtest)

}
\keyword{multivariate}
