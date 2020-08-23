# use reg.select3.r  # modified to handle multiple BICs and to use lars for speedup
get.neighbs<-function(X,kmax=trunc(nrow(X)/10),ff=0,cols=1:ncol(X),
                      lars.type="stepwise",BIC01.only=FALSE)
  {
    # get all p regression models to form neighbour matrix for high dimensional covariance selection
    # X is a mean corrected data matrix (using a suitable mean model)
    # kmax is maximum number of predictors allowed in any regression
    # if ff=0 penalty is 2*log(ncol(X)) otherwise log(nrow(X))
    # output is a sparse dgCMatrix  a with nonzero elements equal to 1 or 2
    # elements which are 2 appear in both regressions y on x and x on y
    # cols - specify which cols to do - useful for cluster computing to speed things up
    # require(Matrix) packages already attached by Depends
    # compute scale factors and scale matrix outside of lars for efficiency
     sf<-colSums(X*X)
     sf<-sf^0.5
     X<-sweep(X,2,sf,"/")
   #
    lars.normalise=FALSE
    lars.intercept=TRUE
    p<-ncol(X)
    N<-nrow(X)
    pen<-log(N)
    if(ff==0)pen<-2*log(ncol(X))
    #
    indx<-1:p
    ii00<-NULL ;ii05<-NULL;ii01<-NULL;ii2p<-NULL;iiff<-NULL
    jj00<-NULL ;jj05<-NULL;jj01<-NULL;jj2p<-NULL;jjff<-NULL
    for(i in cols){
      if(i%%1000==0)cat("processing variable ",i,"\n")
      res<-reg.select(X[,-i],X[,i]*sf[i],kmax=kmax,ff=ff,N=N,
                      lars.type=lars.type,lars.normalise=lars.normalise, lars.intercept = lars.intercept)
      
      if (BIC01.only==TRUE){
        vars<-(indx[-i])[res$selected[[3]]]
        q<-length(vars)
        if(q>0){
          ii01<-c(ii01,rep(i,q))
          jj01<-c(jj01,vars)
        }
        
      }
      else{
        # bic00
        vars<-(indx[-i])[res$selected[[1]]]
        q<-length(vars)
        if(q>0){
          ii00<-c(ii00,rep(i,q))
          jj00<-c(jj00,vars)
        }
        # bic05
        vars<-(indx[-i])[res$selected[[2]]]
        q<-length(vars)
        if(q>0){
          ii05<-c(ii05,rep(i,q))
          jj05<-c(jj05,vars)
        }
        # bic01
        vars<-(indx[-i])[res$selected[[3]]]
        q<-length(vars)
        if(q>0){
          ii01<-c(ii01,rep(i,q))
          jj01<-c(jj01,vars)
        }
        #bic2p
        vars<-(indx[-i])[res$selected[[4]]]
        q<-length(vars)
        if(q>0){
          ii2p<-c(ii2p,rep(i,q))
          jj2p<-c(jj2p,vars)
        }
        # bicff
        vars<-(indx[-i])[res$selected[[5]]]
        q<-length(vars)
        if(q>0){
          iiff<-c(iiff,rep(i,q))
          jjff<-c(jjff,vars)
        }
      }
    }
        # form sparse neighbour matrices
    if (BIC01.only==TRUE){
      a01<-new("dgTMatrix",i=as.integer(ii01-1),j=as.integer(jj01-1),x=rep(1,length(ii01)),Dim=as.integer(c(p,p)))
      a01<-as(a01,"dgCMatrix")
      a01<-a01+t(a01)
      diag(a01)<-1
      return(list(a01=a01))
    }
    else{
      a00<-new("dgTMatrix",i=as.integer(ii00-1),j=as.integer(jj00-1),x=rep(1,length(ii00)),Dim=as.integer(c(p,p)))
      a05<-new("dgTMatrix",i=as.integer(ii05-1),j=as.integer(jj05-1),x=rep(1,length(ii05)),Dim=as.integer(c(p,p)))
      a01<-new("dgTMatrix",i=as.integer(ii01-1),j=as.integer(jj01-1),x=rep(1,length(ii01)),Dim=as.integer(c(p,p)))
      a2p<-new("dgTMatrix",i=as.integer(ii2p-1),j=as.integer(jj2p-1),x=rep(1,length(ii2p)),Dim=as.integer(c(p,p)))
      aff<-new("dgTMatrix",i=as.integer(iiff-1),j=as.integer(jjff-1),x=rep(1,length(iiff)),Dim=as.integer(c(p,p)))
      # coerce to sparse form and symmetrise
      a00<-as(a00,"dgCMatrix")
      a05<-as(a05,"dgCMatrix")
      a01<-as(a01,"dgCMatrix")
      a2p<-as(a2p,"dgCMatrix")
      aff<-as(aff,"dgCMatrix")
      #   
      a00<-a00+t(a00); a05<-a05+t(a05); a01<-a01+t(a01); a2p<-a2p+t(a2p); aff<-aff+t(aff);
      diag(a00)<-1; diag(a05)<-1; diag(a01)<-1; diag(a2p)<-1; diag(aff)<-1;
      return(list(a00=a00,a05=a05,a01=a01,a2p=a2p,aff=aff))
    }
  }

    
         
         
