             sigma.hat<-function(si,rows,entire.rows=FALSE,col.names=NULL)
             {
             # computes the "rows" of the fitted covariance matrix from a sparse representation of
             # the fitted  inverse covariance  si
             # row order is same as in "rows"
             # si is the inverse covariance matrix
             # rows is a subset of 1:ncol(si)
             # gets symmetric "rows by rows" diagonal block only, if entire.row=F
             rnames<-NULL
             if(!is.null(col.names)){
             rnames<-col.names[rows]
             }
             temp<-si[-rows,-rows,drop=FALSE]
             temp1<-si[-rows,rows,drop=FALSE]
             res<-Cholesky(temp,LDL=FALSE,perm=TRUE)
             z<-solve(res,temp1)
             sigma<-si[rows,rows]-si[rows,-rows,drop=FALSE]%*%z
             sigma<-solve((sigma+t(sigma))/2)
             colnames(sigma)<-rnames
             rownames(sigma)<-rnames
             #
             if(entire.rows){
             nc<-ncol(si)
             nr<-length(rows)
             sigma1<-t(solve(res,-temp1%*%sigma))
             output<-matrix(0,nrow=nr,ncol=nc)
             output[,rows]<-as.matrix(sigma)
             output[,-rows]<-as.matrix(sigma1)
             sigma<-as(output,"dgeMatrix")
             colnames(sigma)<-col.names
             rownames(sigma)<-rnames
             }
             sigma
             }
             
