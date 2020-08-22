\name{hd.covsel}
\alias{hd.covsel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
hd.covsel
}
\description{
Fits high dimensional covariance selection nmodels using sparse matrices and the LBFGS algorithm
}
\usage{
hd.covsel(X, A, nsamp, eps = 1e-04, corr = FALSE, method = "lbfgs", m = 20, verbose=-1, init.A=FALSE)
}
\arguments{
  \item{X}{
Real matrix of mean corrected data matrix (nsamp by p )
}
  \item{A}{
Sparse symmetric indicator Matrix (p by p) denoting zero pattern to be fit or a sparse symmetric positive definite matrix 
providing user specified initial values for the optimisation, see also the argument init.A. 
}
  \item{nsamp}{
Real scalar denotng the number of samples - rows of X
}
  \item{eps}{
Real scalar defining the convergence criterion. Iterations are terminated when the gradients are all less than this value
}
  \item{corr}{
Logical - TRUE denotes use correlation matrix for fitting instead of covariance matrix
}
  \item{method}{
Character defining optimisation method to be used. Currently limited to "lbfgs"
}
  \item{m}{
Real scalar denoting the number of past iterations stored for computing the second derivative approximation
}
 \item{verbose}{
 integer - print results every "verbose" iterations. No printing if negative. 
 }
 \item{init.A}{Logical - True denotes use elements of A as initial values in the optimisation.}
}
\details{
The model is fitted by maximum likelihood using sparse matrices and efficient calculations to
compute likelihood and gradient values. Requires the Matrix package. Uses the L-BFGS code avialable from
\link{http://users.eecs.northwestern.edu/~nocedal/lbfgs.html}.
}
\value{
A list with the following components
\item{sigma}{sparse symmetricMatrix containing covariances corresponding to the non zero elements of sigma inverse}
\item{sigma.inv }{sparse symmetricMatrix containing the elemenns of the fitted inverse covariance matrix }
\item{iflag}{convergence indicator - iflag=0 means succesfull termination}
\item{niter}{number of iterations}
\item{nfunc}{number of function and gradient evaluations}
\item{cputime}{cputime in seconds to compute the solution}
\item{gnorm}{maximum abslute gradient value at termination point}
\item{stp}{step length at final iteration}
\item{F}{the value of -(log(det(sigma inverse))-trace(sigma inverse * s)) at the solution i.e. -(2/n)*Log L }
}
\references{
D. C. Liu and J. Nocedal. On the Limited Memory Method for Large Scale Optimization (1989), Mathematical Programming B, 45, 3, pp. 503-528.
H. T. Kiiveri. Fitting very large sparse Gaussian graphical models (2010),submitted for publication.
}
\author{
Harri Kiiveri, R port by Rob Dunne
}
\note{
For long runs setting verbose to say 100 or more  can  help show that progress is actually being made.
The init.A=TRUE option is useful for restarting from a previous fit, or from some better initial value than 
the default starting value which is a diagonal matrix with diagonals the inverse of the diagonals of the sample covariance matrix. For high dimensional models
with flat likelihoods an initial fit with larger eps, say.01, and larger m may be required to avoid very long compute times. 
}
\section{Warning }{
Some models can take a very long time to fit.
}
\seealso{
\code{\link{get.neighbs}}
}
\examples{
library(Matrix)
# generate data
X<-matrix(rnorm(50*5),ncol=5)
X<-scale(X,center=TRUE,sc=FALSE)
# generate zero pattern
a<-rep(0,25)
ind<-sample(1:25,5)
a[ind]<-1
a<-matrix(a,ncol=5)
a<-a+t(a)
diag(a)<-1
a<-as(a,"dsCMatrix")
# fit model
res<-hd.covsel(X,a,nsamp=50,corr=FALSE)
print(res$sigma.inv)
}
\keyword{multivariate}
