\name{mvn.transf}
\alias{mvn.transf}
\title{
Transformation to multivariate normality
}
\description{
Computes transformations for each column of a matrix to improve multivariate normality.
}
\usage{
mvn.transf(X)
}

\arguments{
  \item{X}{
 An n by p data matrix
}
}
\details{
A transformation is computed for each column of X. It involves Winsorising the empirical distribution function
and transforming the result with the inverse cdf of the normal distribtion. Finally a location and scale shift is applied.
}
\value{
An n by p matrix of transformed values.
}
\references{
Liu, H., Lafferty, J., and Wasserman, L.(2009). The Nonparanormal: Semiparametric Estimation of High Dimensional Undirected graphs.
Journal of machine Learning research 10, 1-37.
}
\author{
Harri Kiiveri
}
\note{
There are no checks but n should be at least 2 and preferably much larger.
}
\seealso{
\code{\link{hd.covsel}}
}
\examples{
x<-matrix(rgamma(1000,shape=1),ncol=2)
y<-mvn.transf(x)
par(mfrow=c(2,2))
hist(x,breaks=30)
hist(y,breaks=30)
plot(x[,1],y[,1])
par(mfrow=c(1,1))
}
\keyword{ multivariate }
