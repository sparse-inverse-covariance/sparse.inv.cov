  dgr2dgc<-function(a)
  {
  # convert from row sparse to col sparse format
  a<-t(a)
  b<-new("dgCMatrix",p=a@p,i=a@j,x=a@x,Dim=a@Dim)
  b
  }
  
  dgc2dgr<-function(a)
  {
  # convert from col sparse to row sparse format
  a<-t(a)
  b<-new("dgRMatrix",p=a@p,j=a@i,x=a@x,Dim=a@Dim)
  b
  }
  
