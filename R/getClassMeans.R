### getClassMeans.R  (2014-04-07)
###
###    Get Class Means, Class Frequencies, and Class Samples
###
### Copyright 2013-14  Sebastian Gibb and Korbinian Strimmer
###
###
### This file is part of the `binda' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA

## private helper functions

getClassMeans = function(Xtrain, L)
{
  if (!is.factor(L)) stop("Labels must be given as factor!")

  classes = levels(L)
  samples = getClassSamples(L)
  
  mu.count = matrix(0, nrow=length(classes), ncol=ncol(Xtrain) )
  for (k in seq(along=classes))
  {
     mu.count[k,] = colSums( Xtrain[L == classes[k], , drop=FALSE])
  }
  mu = t(mu.count/samples)
  rownames(mu) = colnames(Xtrain)
  colnames(mu) = classes

  return(mu)
}


getClassFreqs = function(L, lambda.freqs, verbose)
{
  if (!is.factor(L)) stop("Labels must be given as factor!")

  classes = levels(L)
  samples = getClassSamples(L)

  # estimate class frequencies
  freqs = entropy::freqs.shrink( samples, lambda.freqs=lambda.freqs, verbose=verbose )

  return(freqs)
}

getClassSamples = function(L)
{
  if (!is.factor(L)) stop("Labels must be given as factor!")

  classes = levels(L)
  samples = numeric(length(classes))
  names(samples) = classes

  for (k in seq(along=classes))
  {
     samples[k] = sum(L == classes[k])
  }

  return(samples)
}


reportDetails = function(X, L)
{
  if (!is.factor(L)) stop("Labels must be given as factor!")

  cat("Number of variables:", ncol(X), "\n")
  cat("Number of observations:", nrow(X), "\n")
  cat("Number of classes:", length(levels(L)), "\n\n")
}


