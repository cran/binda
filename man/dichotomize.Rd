\name{dichotomize}
\alias{dichotomize}
\alias{optimizeThreshold}

\title{Dichotomize Continuous Data Set With Labels}

\usage{
dichotomize(X, thresh)
optimizeThreshold(X, L, lambda.freqs, verbose=FALSE)
}

\description{
  \code{dichotomize} converts a matrix containing continous measurements into a binary matrix.
 
\code{optimizeThreshold} determines optimal thresholds for dichotomization.
}

\arguments{
  \item{X}{data matrix (columns correspond to variables, rows to samples).}
  \item{thresh}{vector of thresholds, one for each variable (column).}
  \item{L}{factor containing the class labels, one for each sample (row).}
  \item{lambda.freqs}{shrinkage parameter for class frequencies (if not specified it is estimated).}
  \item{verbose}{report shrinkage intensity and other information.}
}
\details{
   \code{dichotomize} assigns 0 if a matrix entry is lower than given column-specific threshold, otherwise it assigns 1.

  \code{optimizeThreshold} uses (approximate) mutual information to 
  determine the optimal thresholds.  Specifically, the thresholds are chosen to maximize the
  mutual information between response and each variable.  The same criterion is also used in
   \code{\link{binda.ranking}}.  For detailed description of the dichotomization procedure see Gibb and Strimmer (2015).

   Class frequencies are estimated using \code{\link[entropy]{freqs.shrink}}.
}
\value{
  \code{dichotomize} returns a binary matrix.

  \code{optimizeThreshold} returns a vector containing the variable thresholds.
}



\references{
Gibb, S., and K. Strimmer. 2015.  Differential protein expression and peak selection in mass spectrometry data by binary discriminant analysis. Bioinformatics 31:3156-3162.
<DOI:10.1093/bioinformatics/btv334>
}

\author{
  Sebastian Gibb and Korbinian Strimmer (\url{https://strimmerlab.github.io}).
}
\seealso{ \code{\link{binda.ranking}}, \code{\link[entropy]{freqs.shrink}}, \code{\link[entropy]{mi.plugin}},  \code{\link{is.binaryMatrix}}.}

\examples{
# load binda library
library("binda")

# example data with 6 variables (in columns) and 4 samples (in rows)
X = matrix(c(1, 1, 1, 1.75, 0.4,    0,
             1, 1, 2,    2, 0.4, 0.09,
             1, 0, 1,    1, 0.5,  0.1,
             1, 0, 1,  0.5, 0.6,  0.1), nrow=4, byrow=TRUE)
colnames(X) = paste0("V", 1:ncol(X))

# class labels
L = factor(c("Treatment", "Treatment", "Control", "Control") )
rownames(X) = paste0(L, rep(1:2, times=2))

X
#          V1 V2 V3   V4  V5   V6
#Treatment1  1  1  1 1.75 0.4 0.00
#Treatment2  1  1  2 2.00 0.4 0.09
#Control1    1  0  1 1.00 0.5 0.10
#Control2    1  0  1 0.50 0.6 0.10

# find optimal thresholds (one for each variable)
thr = optimizeThreshold(X, L)
thr
#  V1   V2   V3   V4   V5   V6 
#1.00 1.00 2.00 1.75 0.50 0.10

# convert into binary matrix
# if value is lower than threshold -> 0 otherwise -> 1
Xb = dichotomize(X, thr)
is.binaryMatrix(Xb) # TRUE
Xb
#          V1 V2 V3 V4 V5 V6
#Treatment1  1  1  0  1  0  0
#Treatment2  1  1  1  1  0  0
#Control1    1  0  0  0  1  1
#Control2    1  0  0  0  1  1
#attr(,"thresh")
#  V1   V2   V3   V4   V5   V6 
#1.00 1.00 2.00 1.75 0.50 0.10
}
\keyword{univar}


