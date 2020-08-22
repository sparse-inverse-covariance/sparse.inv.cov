\name{reg.select}
\alias{reg.select}
\title{
reg.select
}
\description{
Finds predictors of y from among the columns of X using forward stepwise regression and 5 different BIC criteria
}
\usage{
reg.select(X, y, kmax = 5, ff = 1, N = nrow(X),lars.type="stepwise",lars.normalise=TRUE, lars.intercept = TRUE)
}

\arguments{
  \item{X}{
Real n by p mean corrected data matrix
}
  \item{y}{
Real n by 1 response vector
}
  \item{kmax}{
real scalar - maximum number of predictors allowed in model, including the mean.
}
  \item{ff}{
Real scalar constsant to be added to mean residual summ of squares fro one version of BIC
}
  \item{N}{
Real scalar - the value of N to use in the usual BIC
}
\item{lars.type}{character -type of variable selection used in lars - "stepwise" or "lasso" or "lar"}
\item{lars.normalise}{logical - controls normalise option in lars}
\item{lars.intercept}{logical - whether to include intercept in model or not}
}
\details{
For the given response y, use forward selection on the columns of X to kmax terms 
to define a sequence of models. Chose the model in this sequence by using a minimum BIC. 
The follwing BICs are used:

BICg of Chen and Chen(2008) for g=0, 0.5 and g=1(g=0 is the usual BIC)
BIC2p - the usual BIC with log(n) replaced by log(2p), see An et al.
BICff - the usual BIC with constant ff added to the mean residual sum of squares, see An et Al.
}
\value{
A list with components
\item{selected }{A list with 5 components. The variables selected by BICg for g=0,0.5,1.0, BIC2p and BICff. The length of the component is zero.
if no variables selected}
\item{bics}{A kmax by 5 matrix with colums the BIC values for the 5 BICs in the order described above.}
}
\references{
An H, Huang D,  Yao Q, Zhang C: Stepwise Searching for Feature Variables in High-Dimensional Linear Regression. Available at 
\url{http://stats.lse.ac.uk/q.yao/qyao.links/paper/ahyz08.pdf} ,(2008).

Chen J, Chen Z: Extended Bayesian information criteria for model selection with large model spaces. Biometrika (2008), 95 ,3:759-771.

Efron B, Hastie T, Johnston I, Tibshirani, R: Least Angle Regression (with discussion). Ann. Math. Stat (2004), 32, 2:407-499
}
\author{
Harri Kiiveri
}
\note{
The columns of X should be mean centred, i.e it is the residual matrix after any mean structure has been removed.
}
\seealso{
\code{\link{get.neighbs}}
}
\examples{
# not run
library(lars)
X<-matrix(rnorm(50*6),ncol=6)
X<-scale(X,center=TRUE,sc=FALSE)
y<-X[,1]+2*X[,2]+3*X[,3]+rnorm(50)*.1
res<-reg.select(X,y,kmax=4)
}
\keyword{Regression}

