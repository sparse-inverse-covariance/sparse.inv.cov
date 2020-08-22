c sparse cholesky factorisation
        subroutine spchol(p,ia,ja,a,l,na,iflag)
c
c uses sparse row representation of lower triangle of a to produce l where
c the sparse representation includes fillin elements
c na is lengh of a
        integer ia(*),ja(*),p,na ,iflag
        double precision a(*),l(*),li(p),sij,si ,tmp
        iflag=0
        call rzero(na,l)
        if(a(1).le.0)then
        iflag=1
        return
        endif
        l(1)=a(1)**0.5D0
c        write(6,*)l
        do 10 i=2,p
        call rzero(p,li)
        if((ia(i+1)-ia(i)).gt.1)then
        do 20 j1=ia(i),(ia(i+1)-2)
        j=ja(j1)
        sij=0.0d0
        if((ia(j+1)-ia(j)).gt.1)then
        do 30 k=ia(j),(ia(j+1)-2) 
        sij=sij+li(ja(k))*l(k)
 30     continue 
        endif
        li(ja(j1))=(a(j1)-sij)/l(ia(j+1)-1)
 20     continue
c get ith diagonal of l
        si=0.0d0
        do 40 k =ia(i),(ia(i+1)-2)
        si=si+li(ja(k))**2
 40     continue
        ind=ia(i+1)-1
        tmp=a(ind)-si
        if(tmp.gt.0)then
        li(i)=tmp**0.5D0
        else
        iflag=1
        return
        endif
c        li(i)=(a(ind)-si)**0.5D0
        else
        ind=ia(i+1)-1
        if(a(ind).gt.0)then
        li(i)=a(ind)**0.5D0
        else
        iflag=1
        return
        endif
        endif
c put result back into l
c        write(6,*)l(*),li
        do 50 k=ia(i),(ia(i+1)-1)
        l(k)=li(ja(k))
 50     continue
 10     continue
        return
        end
        

c     ----------------
      subroutine rzero (n,b)
c     ----------------
c initialize a real vector
c MPE
      double precision b(*)
      do i=1,n
         b(i)=0.0D0
      enddo
      return
      end
