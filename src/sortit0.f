          subroutine sortit0(z,irow,jrow,nz,perm,ztop,itop,jtop,nlarge
     1                      ,tmp)
          integer nz,nlarge,irow(nz),jrow(nz),itop(nlarge),jtop(nlarge)
          integer perm(nz)
          double precision z(nz),ztop(nlarge),tmp(nz)
           im=nz+1
           do 5 i=1,nz
   5       tmp(i)=dabs(z(i))
           call qsorti0(perm,nz,tmp)
           do 10 i=1,nlarge
           im=im-1
           ztop(i)=tmp(perm(im))
           itop(i)=irow(perm(im))
           jtop(i)=jrow(perm(im))
  10       continue
           return
           end
           
