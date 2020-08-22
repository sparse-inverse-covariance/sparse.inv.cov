         subroutine threshit(x,n,p,nlarge,nb,inb,zstor,istor,jstor,
     1                       perm,nzs,itop,jtop,ztop,z,ndz,tmp,iy,nbin2)
c  sequentially computes and stores the largest nlarge absolute correlations and the histogram
c  of the correlation matrix without ever storing all of it at once
c  x is n by p scaled matrix (so x'x is a correlation matrix)- now stored as a vector
c  nlarge is no of largest absolute correlations to pick
c  nb is length inb integer vector defining blocks of columns of  x to sequentially process
c  e.g. 1,n1,n2,...,nk,p+1 so blocks are (1,n1-1) (n1,n2-1),...,(nk,p)
c  zstor, istor,jstor, perm are work arrays of length  nzs=nlarge+r*p-r*(r+1)/2 where r=nb(2)-1
c  top nlarge absolute correlations and their indices are in itop, jtop and ztop.
c  z is dimension ndz=p*r-r(r+1)/2 where r=nb(2)-1
        integer n,p,nlarge,inb,nb(inb),istor(nzs),jstor(nzs),perm(nzs)
        integer itop(nlarge),jtop(nlarge),irow(ndz),jrow(ndz),l,nbin2
        integer iy(nbin2)
        double precision x(*),zstor(nzs),ztop(nlarge),z(ndz),
     1                   tmp(nzs)
c
             zn=n
             do 5 ib=1,inb-1
             l=0
             do 10 i=nb(ib),nb(ib+1)-1
             
             do 10 j=i+1,p
             l=l+1
             z(l)=0.0d0
             call iprod(x,n,i,j,z(l))
c             do 20 k=1,n
c  20         z(l)=z(l)+x(k,i)*x(k,j)
              irow(l)=i
              jrow(l)=j
  10          continue
c
              call histupd(z,l,nbin2,iy)
c
              if(ib.eq.1)then
              call sortit0(z,irow,jrow,l,perm,ztop,itop,jtop,nlarge,tmp)
              else
              ntop1=l

c concatenate best so far with new data

              do 30 ii=1,nlarge
              zstor(ii)=ztop(ii)
              istor(ii)=itop(ii)
 30           jstor(ii)=jtop(ii)
c add new data
              do 40 ii=1,ntop1
              ik=ii+nlarge
              zstor(ik)=z(ii)
              istor(ik)=irow(ii)
 40           jstor(ik)=jrow(ii)
c find largest ones and update best
              nz=nlarge+ntop1
          call sortit0(zstor,istor,jstor,nz,perm,ztop,itop,jtop,nlarge,
     1                 tmp)
              endif
  5           continue
              return
              end
