       subroutine dwrite(nc,n,x,unit)
c write the double vector x to unit with nc columns
c in the file where n is the length of x
       double precision x(1)
       integer nc,n,unit
       k=int(n/nc)
       if(k.eq.0)then
        write(unit,*)(x(i),i=1,n)
       else
        do 10 i=1,k
        istart=(i-1)*nc+1
        istop=i*nc
 10     write(unit,*)(x(l),l=istart,istop)
        knc=k*nc+1
         if(knc.le.n)then
         write(unit,*)(x(l),l=knc,n)
         endif
       endif
       return
       end
       
       subroutine iwrite(nc,n,ix,unit)
c write the integer vector ix to unit with nc columns
c in the file where n is the length of ix
       integer ix(1)
       integer nc,n,unit
       k=int(n/nc)
       if(k.eq.0)then
        write(unit,*)(ix(i),i=1,n)
       else
        do 10 i=1,k
        istart=(i-1)*nc+1
        istop=i*nc
 10     write(unit,*)(ix(l),l=istart,istop)
        knc=k*nc+1
         if(knc.le.n)then
         write(unit,*)(ix(l),l=knc,n)
         endif
       endif
       return
       end
