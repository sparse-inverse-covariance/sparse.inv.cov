\name{corr.thresh}
\alias{corr.thresh}
\title{
Computes a specified number of the largest absolute correlation values and their associated row and column indices given
a n by p data matrix x with p>>n.  Also computes a histogram of the correlation values.
}
\description{
Computes the largest absolute correlations by sequentially doing the calculations in blocks to avoid storing the entire correlation matrix.
Potentially useful in determining the non-zero elements in the inverse covariance matrix.
}
\usage{
corr.thresh(x, nlarge, block.size, nbin2 = 501, use.ginverse = F)
}
\arguments{
  \item{x}{
 n by p data matrix which will be mean centred and scaled by the function to compute correlations
}
  \item{nlarge}{
scalar - the number of largest absolute correlation values to store
}
  \item{block.size}{
scalar - the number of rows of the correlation matrix to compute and store at any one time
}
  \item{nbin2}{
scalar- the numnber of bins to use in the histogram of the correlations. Will be incremented by one if even.
}
  \item{use.ginverse}{
logical - if true compute the correlations in the More-Penrose inverse of the correlation matrix
}
}
\details{
Computes x'x using a scaled x sequentially using block.size rows. The largest absolute correlations and the histogram are updated
for each new block calculated.
}
\value{
A list with components
  \item{itop}{row indices of the largest absolute correlations}
  \item{jtop}{column indices of the largest absolute correlations}
  \item{rtop}{values of the largest absolute correlations}
  \item{hx}{bin centres for the histogram of correlation values}
  \item{hy}{counts for the bins in the histogram}
}
\references{
Friedman,J., Hastie, T., andTibshirani, R. (2010) Applications of the lasso and grouped lasso to the estimation of sparse graphical models
http://www-stat.stanford.edu/~tibs/ftp/ggraph.pdf
}
\author{
Harri Kiiveri
}
\note{
nlarge must be less than r*p-r*(r+1)/2 where r=block.size
}
\seealso{
\code{\link{get.neighbs}}
}
\examples{
x<-matrix(rnorm(50*100),nrow=50)
res<-corr.thresh(x,nlarge=10,block.size=10)
cbind(res$itop,res$jtop,res$rtop)
plot(res$hx,res$hy,ty="h",col=2)
}

\keyword{multivariate }

