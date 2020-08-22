c works both ways to map beween regressison parameterisation and inverse parameterisation
         subroutine gtosi(g,si,ig,jg,p)
         double precision g(*),si(*),d
         integer p,ig(*),jg(*)
         d=g(1)
         si(1)=1.0d0/d
         do 10 i=2,p
         d=1.0d0/g(ig(i+1)-1)
         do 10 k=ig(i),ig(i+1)-1
c         write(6,*)i,jg(k)
         if(i.eq.jg(k))then
         si(k)=d
         else
         si(k)=-g(k)*d
         endif
 10      continue
         return
         end
         
