# 
#

library(doParallel)  
library(foreach)
no_cores <- 16
registerDoParallel(cores = no_cores)  

library(sparse.inv.cov)
packageDescription("sparse.inv.cov")
data(smoking)

table(smoking[[1]])
dim(smoking[[2]])
X<-smoking[[2]]

D<-matrix(0,nrow=57,ncol=2)
D[1:34,1]<-1
D[35:57,2]<-1

C<-matrix(c(1,-1)/2^0.5,ncol=1)

R<-lm(X~D-1)$residuals
kmax<-3
X <-R


sf <- colSums(X * X)
sf <- sf^0.5
X <- sweep(X, 2, sf, "/")




##############################################

filename<-"/scratch1/dun280/butyrate2_temp/butyrate_residuals1.Rdata" 
kmax<-2
# get only input for this script
args=(commandArgs(TRUE))
# for each arguement evaluate the statement
# e.g. "filename=/"some_file.Rdata/""
for ( i in args ) {
print(i)
  eval(parse(text=i))
}
print(filename)

  
load(filename)
#X<-scale(X,center=T,scale=F)
print(X[1:5,1:4])
##################################################



library(sparse.inv.cov)
library(Matrix)
library(MASS)
library(lars)
    
cols  <-  1:ncol(X)
ff  <-  0
lars.type <- "stepwise"
lars.normalise <- FALSE
lars.intercept  <-  TRUE
BIC01.only <- TRUE
p <- ncol(X)
N <- nrow(X)
pen <- log(N)
if (ff == 0)
    indx <- 1:p
pen <- 2 * log(ncol(X))

system.time(
regsel <- foreach (i=cols) %dopar% {
    
    if (i%%1000 == 0) write(i, file='progress.log')
    
    res <- reg.select(X[, -i], X[, i], kmax = kmax, ff = ff,
                      N = N, lars.type = lars.type, lars.normalise = lars.normalise,
                      lars.intercept = lars.intercept)
    ii00 <- NULL
    ii05 <- NULL
    ii01 <- NULL
    ii2p <- NULL
    iiff <- NULL
    jj00 <- NULL
    jj05 <- NULL
    jj01 <- NULL
    jj2p <- NULL
    jjff <- NULL
    
    if (BIC01.only == TRUE) {
        vars <- (indx[-i])[res$selected[[3]]]
        q <- length(vars)
        if (q > 0) {
            ii01 <- rep(i, q)
            jj01 <- vars
        }
        list(ii01 = ii01, jj01 = jj01)
        
    } else {
        vars <- (indx[-i])[res$selected[[1]]]
        q <- length(vars)
        if (q > 0) {
            ii00 <- rep(i, q)
            jj00 <- vars
        }
        vars <- (indx[-i])[res$selected[[2]]]
        q <- length(vars)
        if (q > 0) {
            ii05 <- rep(i, q)
            jj05 <- vars
        }
        vars <- (indx[-i])[res$selected[[3]]]
        q <- length(vars)
        if (q > 0) {
            ii01 <- rep(i, q)
            jj01 <- vars
        }
        vars <- (indx[-i])[res$selected[[4]]]
        q <- length(vars)
        if (q > 0) {
            ii2p <- rep(i, q)
            jj2p <- vars
        }
        vars <- (indx[-i])[res$selected[[5]]]
        q <- length(vars)
        if (q > 0) {
            iiff <- rep(i, q)
            jjff <- vars
        }
        list(ii00 = ii00, jj00 = jj00,
             ii05 = ii05, jj05 = jj05,
             ii01 = ii01, jj01 = jj01,
             ii2p = ii2p, jj2p = jj2p,
             iiff = iiff, jjff = jjff)
    }
} # end  "for (i in cols)"
)

save(regsel,file='regsel.Rdata')
q()
regsel  <-  unlist(regsel, recursive=F)
# unlist() gets rid of NULLs
ii00  <-  unlist(regsel[names(regsel) == 'ii00'])
jj00  <-  unlist(regsel[names(regsel) == 'jj00'])
ii05  <-  unlist(regsel[names(regsel) == 'ii05'])
jj05  <-  unlist(regsel[names(regsel) == 'jj05'])
ii01  <-  unlist(regsel[names(regsel) == 'ii01'])
jj01  <-  unlist(regsel[names(regsel) == 'jj01'])
ii2p  <-  unlist(regsel[names(regsel) == 'ii2p'])
jj2p  <-  unlist(regsel[names(regsel) == 'jj2p'])
iiff  <-  unlist(regsel[names(regsel) == 'iiff'])
jjff  <-  unlist(regsel[names(regsel) == 'jjff'])

if (BIC01.only == TRUE) {
    a01 <- new("dgTMatrix", i = as.integer(ii01 - 1),
               j = as.integer(jj01 - 1), x = rep(1, length(ii01)), Dim = as.integer(c(p,p)))
    a01 <- as(a01, "dgCMatrix")
    a01 <- a01 + t(a01)
    diag(a01) <- 1
    result <- list(a01 = a01)
} else {
    
    a00 <- new("dgTMatrix", i = as.integer(ii00 - 1), j = as.integer(jj00 - 1),
               x = rep(1, length(ii00)), Dim = as.integer(c(p,p)))
    
    a05 <- new("dgTMatrix", i = as.integer(ii05 - 1), j = as.integer(jj05 -1),
               x = rep(1, length(ii05)), Dim = as.integer(c(p, p)))
    
    a01 <- new("dgTMatrix", i = as.integer(ii01 - 1), j = as.integer(jj01 - 1),
               x = rep(1, length(ii01)), Dim = as.integer(c(p,p)))
    
    a2p <- new("dgTMatrix", i = as.integer(ii2p - 1), j = as.integer(jj2p - 1),
               x = rep(1, length(ii2p)), Dim = as.integer(c(p, p)))
    
    aff <- new("dgTMatrix", i = as.integer(iiff - 1), j = as.integer(jjff - 1),
               x = rep(1, length(iiff)), Dim = as.integer(c(p, p)))
    
    a00 <- as(a00, "dgCMatrix")
    a05 <- as(a05, "dgCMatrix")
    a01 <- as(a01, "dgCMatrix")
    a2p <- as(a2p, "dgCMatrix")
    aff <- as(aff, "dgCMatrix")
    a00 <- a00 + t(a00)
    a05 <- a05 + t(a05)
    a01 <- a01 + t(a01)
    a2p <- a2p + t(a2p)
    aff <- aff + t(aff)
    diag(a00) <- 1
    diag(a05) <- 1
    diag(a01) <- 1
    diag(a2p) <- 1
    diag(aff) <- 1
    result <- list(a00 = a00, a05 = a05, a01 = a01, a2p = a2p, aff = aff)
}


a01<-result[[1]]
diag(a01)<-0
temp1 <- colSums(a01)
temp2<- rowSums(a01)





print("saving result file!")
save(a01,file=paste("result",filename, sep="_"))
#  	temp3 <- Reduce("+", result) #commented out. I think the Reduce may be having problems with
#  	temp3[temp3!=0]<-1           # the size of the matrix
#  
#  	if (mpi.comm.rank() == 0) {
#    		save(temp3,file=paste("result",filename, sep="_"))
#  	}



