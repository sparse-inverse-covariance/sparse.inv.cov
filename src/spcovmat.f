        subroutine spcovmat(p,ia,ja,x,n,s,idia)
c using fspak sparse representation (dsRMatrix) for sigma inverse defined by p, ia, ja
c ja is ordered within rows
c compute the sample covariance between pairs corresponding to nonzero
c entries in sigma inverse and produce locations of diagonal elements
c x is the n by p data matrix ASSUMED to be mean corrected
c p,ia,ja,s is the sparse reresentation of the relevant sample covariances
      implicit none
      integer kk, i, j, k, l
      integer ia(1),ja(1),p,n,idia(1)
      real*8 x(n,p),s(1)

c      write(6,*) (ia(i),i=1,100)
c      write(6,*) (ja(i),i=1,100)
c     
      l=1
      do 10 i=1,p
         do 10 j=ia(i),ia(i+1)-1
            kk=ja(j)
            if(i.eq.kk)idia(i)=l
            s(l)=0
            do 20 k=1,n
               s(l)=s(l)+x(k,i)*x(k,kk)
 20         continue
            s(l)=s(l)/n
            l=l+1
 10      continue
         return
         end
      
