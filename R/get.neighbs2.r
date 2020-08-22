# use reg.select2.r
get.neighbs2<-function(X,kmax=trunc(nrow(X)/10),ff=0)
  {
    # get all p regression models to form neighbour matrix for high dimensional covariance selection
    # X is a mean corrected data matrix (using a suitable mean model)
    # kmax is maximum number of predictors allowed in any regression
    # if ff=0 penalty is 2*log(ncol(X)) otherwise log(nrow(X))
    # output is a sparse dgCMatrix  a with nonzero elements equal to 1 or 2
    # elements which are 2 appear in both regressions y on x and x on y
    p<-ncol(X)
    N<-nrow(X)
    pen<-log(N)
    if(ff==0)pen<-2*log(ncol(X))
    #
    indx<-1:p
    ii<-NULL
    jj<-NULL
    for(i in 1:p){
      if(i%%100==0)cat("processing variable ",i,"\n")
      res<-reg.select2(X[,-i],X[,i],pen=pen,kmax=kmax,ff=ff,N=N)
      res$selected<-(indx[-i])[res$selected]
      q<-length(res$selected)
      # cat(i,"q=",q,res$selected,"\n")
      if(q>0){
        ii<-c(ii,rep(i,q))
        jj<-c(jj,res$selected)
      }
    }
    a<-new("dgTMatrix",i=as.integer(ii-1),j=as.integer(jj-1),x=rep(1,length(ii)),Dim=as.integer(c(p,p)))
    a<-as(a,"dgCMatrix")  
    a<-a+t(a)
    diag(a)<-1
    a
  }

         
         
