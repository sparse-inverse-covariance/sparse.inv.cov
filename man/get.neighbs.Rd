\name{get.neighbs}
\alias{get.neighbs}
\title{
get.neighbs
}
\description{
Computes zero pattern in sigma inverse from the data matrix by using regression of each variable or gene on the remaining variables or genes
and various BIC criteria.
}
\usage{
get.neighbs(X, kmax = trunc(nrow(X)/10), ff = 0, cols=1:ncol(X),lars.type="stepwise",BIC01.only=FALSE)
}
\arguments{
\item{X}{
Real n by P data matrix - mean corrected by appropriate mean model
}
\item{kmax}{
Real scalar denoting maximum number of neighbours in the regression for each variable/gene (including the mean).
}
\item{ff}{
Real scalar to add to residual sum of squares fro modified BIC, see below
}
\item{cols}{Real vector of column indices for which to do the regressions. Useful to split the job over several processors
}
\item{lars.type}{character -type of variable selection used in lars - "stepwise" or "lasso" or "lar"}
\item{BIC01.only}{logical - only compute model using BIC of Chen and Chen(2008) with g i.e gamma=1}
}
\details{
For each column in cols, considering the remaining variables as potential predictors, use "stepwise", "lasso" or "lar" selection to kmax terms
to define a sequence of models. Chose the model in this sequence by using a minimum BIC. An indicator matrix A is formed where
for each row i in cols the  entry A[i,j] is 0 unless column j is a predictor of variable i in which case it is one. This
matrix is then symmetrised by computing a=a+t(a) with diagonals defined by cols reset to 1. The indicator matrix is returned 
for 5 versions of BIC, namely:

BICg of Chen and Chen(2008) for g=0, 0.5 and g=1(g=0 is the usual BIC)
BIC2p - the usual BIC with log(n) replaced by log(2p), see An et al.
BICff - the usual BIC with constant ff added to the mean residual sum of squares, see An et Al.
}
\value{
If BIC01.only=FALSE, a list with the following components
\item{a00 }{sparse dsCMatrix indicating zero pattern according to BICg with g=0}
\item{a05 }{sparse dsCMatrix indicating zero pattern according to BICg with g=0.5}
\item{a01 }{sparse dsCMatrix indicating zero pattern according to BICg with g=1.0}
\item{a2p }{sparse dsCMatrix indicating zero pattern according to BIC2p}
\item{aff }{sparse dsCMatrix indicating zero pattern according to BICff}
otherwise a list with single component a01. The nonzero elements of these matrices can be 1 or 2. The i,j th element is 2
if the regression of variable i on the rest includes variable j and the regression of variable j on the rest includes variable i.
}
\note{uses the lars package to do the regressions}
\references{
An H, Huang D,  Yao Q, Zhang C: Stepwise Searching for Feature Variables in High-Dimensional Linear Regression. Available at 
\url{http://stats.lse.ac.uk/q.yao/qyao.links/paper/ahyz08.pdf} , 2008.
Chen J, Chen Z: Extended Bayesian information criteria for model selection with large model spaces. Biometrika 2008, 95 ,3:759-771.
Efron, Bradley; Hastie, Trevor; Johnstone, Iain and Tibshirani, Robert (2004). "Least Angle Regression". Annals of Statistics 32 (2): pp. 407–499.
}
\author{
Harri Kiiveri
}
\section{Warning}{
Note the data matrix X  must be mean corrected
}
\seealso{
\code{\link{reg.select}}
}
\examples{
# not very sensible
X<-matrix(rnorm(25),ncol=5)
X<-scale(X,center=TRUE,sc=FALSE)
res<-get.neighbs(X,kmax=2)
}
\keyword{ Regression }
