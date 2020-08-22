\name{get.thresh.adj}
\alias{get.thresh.adj}
\title{
Computes the sparse adjacency matrix from a corr.thresh object. Can be used to define the zero pattern
for function hd.covsel
}
\description{
A convenience function for creating a sparse adjaceny matrix by correlation thresholding.
}
\usage{
get.thresh.adj(obj, topno, p, include.diagonal = T)
}
\arguments{
  \item{obj}{
list - the object produced by the function corr.thresh
}
  \item{topno}{
scalar - the number of largest absolute correlations to use to form the adjacency matrix.
}
  \item{p}{
scalar - the number of columns of the data matrix used  to produce obj
}
  \item{include.diagonal}{
logical - if true include the diagonals in the adjacency matrix
}
}
\details{
Uses the sparse matrix library "Matrix"
}
\value{
A sparse symmetric adjacency matric of class "dsCMatrix"
}
\references{

}
\author{
Harri Kiiveri
}
\note{
}
\seealso{
\code{\link{corr.thresh}}
}
\examples{
x<-matrix(rnorm(50*5),nrow=50)
res<-corr.thresh(x,nlarge=10,block.size=10)
cbind(res$itop,res$jtop,res$rtop)
a<-get.thresh.adj(res,topno=5,p=5)
a
}
\keyword{multivariate }

