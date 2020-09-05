# changed to allow for "lar" and "lasso" option
# altered for simulation purposes to chose model on min bic and use stepwise lars
# where bic is bic00 bic05 bic01 bic2p bicff
# no backward step performed and forward step goes all the way to kmax
# then find minimum of various bics
reg.select<-function(X,y,kmax=5,ff=1,N=nrow(X),
                     lars.type="stepwise",lars.normalise=TRUE, lars.intercept = TRUE)
{
# function for variable selection in p>n linear regression models
# y -  response
# BICs are-  penalty term # bic00=log(N)/N # bic05=bic00+logtaua # bic01 bic05+logtau
#                         # bic2p=2*log(p)/N # bicff=log(RSSQ/N+ff)+k*log(N)/n
# kmax -(>1) maximum size of selected mmodel including constant-no  (forward selection stops at this size)
# ff - value for fudge factor in modified BIC (can be zero)
# forward selection - always put constant in first
# require(lars) packages already attached by Depends
    p<-ncol(X)
    res<-lars(X,y,type=lars.type,max.steps=kmax,use.Gram=FALSE,normalize=lars.normalise,
              intercept=lars.intercept)
    # kmax<-min(kmax,ncol(X)) #
    rssq<-res$RSS
    #df<-res$df
    isav<-apply(res$beta,1,which.not.zero)
    nisav<-length(isav)
    df<-rep(0,nisav)
    for(i in 1:nisav){
        df[i]<-length(isav[[i]])+1
    }
    # get usual regression sum of squares for other types
    if(lars.type!="stepwise"){
        for(i in 2:length(isav)){         # was kmax+1
            rtmp<-lm(y~X[,isav[[i]]])$residuals
            rssq[i]<-sum(rtmp^2)
        }
    }
    # Compute BICs
    BICS<-matrix(0,nrow=kmax,ncol=5)      # kmax+1 ?
    colnames(BICS)<-c("bic00","bic05","bic01","bic2p","bicff")
    nconst<-log(N)/N
    pconst<-2*log(p)/N
    lrssq<-log(rssq/N)
    lrssqff<-log(rssq/N+ff)
    kkmax<-min(nisav,kmax)
    for(i in 1:kkmax){           # kmax+1 ?
        klogtau<-logtauj(p,df[i])/N
        BICS[i,1]<-lrssq[i]+df[i]*nconst
        BICS[i,2]<-BICS[i,1]+klogtau
        BICS[i,3]<-BICS[i,2]+klogtau
        BICS[i,4]<-lrssq[i]+df[i]*pconst
        BICS[i,5]<-lrssqff[i]+df[i]*nconst
    }
    # get minima of bics
    mids<-apply(BICS,2,which.min)
    selected00<-isav[[mids[1]]]
    selected05<-isav[[mids[2]]]
    selected01<-isav[[mids[3]]]
    selected2p<-isav[[mids[4]]]
    selectedff<-isav[[mids[5]]]
    selected<-list()
    selected[[1]]<-selected00
    selected[[2]]<-selected05
    selected[[3]]<-selected01
    selected[[4]]<-selected2p
    selected[[5]]<-selectedff
    # 0 means constant is in the model (always)- return  variable numbers
    # length of "selected" is zero if no variables picked
    list(selected=selected,bics=BICS)
}
# function required by  modified bic
#
logtauj<-function(p,j)
{
    sf<-0
    if(j!=0){
        f1<-1:j
        f2<-(p-j)+f1
        sf<-sum(log(f2)-log(f1))
    }
    sf
}
#
which.not.zero<-function(x)
{
    which(x!=0)
}
