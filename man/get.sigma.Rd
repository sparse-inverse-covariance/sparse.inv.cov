\name{get.sigma}
\alias{get.sigma}
\title{
Given a sparse matrix sigma inverse, computes the elements of sigma corresponding
to the non zero elements of the cholesky factor of sigma inverse.
}
\description{
Uses the sparse Takahashi algorithm to efficiently compute a subset of the elements of sigma
}
\usage{
get.sigma(A)
}
\arguments{
  \item{A}{
 - a sparse inverse covariance matrix (dgCMatrix or symmetricMatrix)
}
}
\details{
Only the specified elements of sigma inverse are computed or stored , making this calculation feasible for very large matrices.
Elements of sigma inverse which form the fitted values of the sufficient tatistics for a covariance selection model are a subset of
the elements computed by this function.
}
\value{
A list with components
\item{sigma}{A sparse symmetric matrix (of class symmetrixMatrix) containing the specified elements of sigma }
\item{n }{The number of unique non zero entries in sigma inverse}
\item{naf }{The number of unique non zero entries in the sparse cholesky factor of sigma inverse}
\item{iflag}{zero if inversion successful, 1 otherwise.}
}
\references{
ERISMAN, A. M., AND TINNEY, W. F.(1975). On Computing certain elements of the inverse of a sparse matrix. Communications of the ACM 18, 177-179.
}
\author{
Harri Kiiveri
}
\note{
The function will exit with an error condition if the inverse covariance matrix is not positive definite.
}
\seealso{
\code{\link{hd.covsel}}
}
\examples{
# generate matrix
library(Matrix)
# generate zero pattern
a<-rep(0,25)
ind<-sample(1:25,7)
a[ind]<-1
a<-matrix(a,ncol=5)*.1
a<-a+t(a)
diag(a)<-1
a<-as(a,"dsCMatrix")
# do usual inversion
solve(a)
# compute subset of elements of sigma
b<-get.sigma(a)
b[[1]]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
