
         subroutine getperms(p,ia,ja,iat,jat,perm,permi,na)
c given ia ja computes iat jat perm and permi
c na is in fact ia(p+1)-1
         integer p,ia(*),ja(*),iat(*),jat(*),perm(*),permi(*),na
         integer tmp(p)
         double precision a(na),b(na)
         do 5 i=1,na
 5       a(i)=dble(i)
         call rowtocol(ia,ja,a,iat,jat,b,tmp,p)
         do 10 i=1,na
 10      perm(i)=idint(b(i))
         call qsorti(permi,na,perm)
         return
         end

c     -------------------
      subroutine rowtocol(aia,aja,aa,bia,bja,ba,tmp,n)
c     -------------------
c Matrix a, in sparse i-j-a form and stored row-wise, is converted to b,
c stored column-wise; after the move, the rows of b are sorted in
c increasing order.
c a and b are have n rows and columns, and tmp is
c is a temporary integer vector of size n.
c I. Misztal
      integer n,aia(*),aja(*),bia(*),bja(*),tmp(*),i,j,k
      double precision aa(*),ba(*)

c count the number of entries in each row of a
      call zero(n,tmp)
      do i=1,n
         do j=aia(i),aia(i+1)-1
            tmp(aja(j))=tmp(aja(j))+1
         enddo
      enddo

c create the row count for b
      bia(1)=1
      do i=1,n
         bia(i+1)=bia(i)+tmp(i)
      enddo

c load a into b
      call zero(n,tmp)
      do i=1,n
         do j=aia(i),aia(i+1)-1
            k=bia(aja(j))+tmp(aja(j))
            bja(k)=i
            ba(k)=aa(j)
            tmp(aja(j))=tmp(aja(j))+1
         enddo
      enddo
      end

c     ---------------
      subroutine zero (n,x)
c     ---------------
      integer n,x(n),i
      do i=1,n
         x(i)=0
      enddo
      end
