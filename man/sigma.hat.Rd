\name{sigma.hat}
\alias{sigma.hat}
\title{
Computes specified rows or block diagonals of the covariance matrix corresponding to a given sparse inverse covariance matrix.
}
\description{
Uses sparse matrix calculations to calculate specified elements of the covariance matrix for a sparse covariance selection model 
}
\usage{
sigma.hat(si, rows, entire.rows = FALSE, col.names = NULL)
}

\arguments{
  \item{si}{
sparseSymmetrix matrix - the inverse covariance matrix
}
  \item{rows}{
numeric vector - identifiers of the rows of the covariance matrix required. 
}
  \item{entire.rows}{
logical - if TRUE compute specified rows of the covariance matrix. If FALSE compute the specified block diagonal entries of the covariance matrix.
}
  \item{col.names}{
character vector of length ncol(si) - optional column names of si
}
}
\details{
Uses standard formulae for the inverse of a partitoned matrix and a sparse cholesky factorisation to compute the required elements of sigma inverse.
}
\value{
If entire.rows=FALSE a dgeMatrix of dimension q by q where q=length(rows) otherwise a q by p dgeMatrix where p=ncol(si).
}
\section{Warning}{
For very high dimensional matrices computations will be slow and the memory limit may be reached if length(rows) is very large.
}
\author{
Harri Kiiveri
}
\note{
Positive definiteness of si is NOT checked.
}
\seealso{
\code{\link{hd.covsel}}
}
\examples{
library("Matrix")
#generate sigma.inv 
sinv<-diag(5)/2
sinv[1,2]=sinv[2,3]=sinv[3,4]=sinv[4,5]=0.1
sinv<-sinv+t(sinv)
sinv<-as(sinv,"dsCMatrix")
# look at covariance matrix
solve(sinv)
# get rows 2,3,and 4 od sigma
sigma.hat(sinv,rows=c(2,3,4),entire.rows=TRUE)
# get only block diagonal
sigma.hat(sinv,rows=c(2,3,4),entire.rows=FALSE)
}
\keyword{ multivariate }

