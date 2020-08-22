c        subroutines to map between rpo and sigma.inverse parameterisations  both ways
         subroutine rho2si(g,si,ig,jg,p)
         integer p,ig(*),jg(*)
         double precision g(*),si(*),d,tmp(p)
         si(1)=g(1)
         tmp(1)=si(1)**.5
c        collect diagonal elements
         do 5 i=2,p
  5      tmp(i)=g(ig(i+1)-1)**0.5
         do 10 i=2,p
         do 10 k=ig(i),ig(i+1)-1
c         write(6,*)i,jg(k)
         if(i.eq.jg(k))then
         si(k)=g(k)
         else
         si(k)=g(k)*tmp(i)*tmp(jg(k))
         endif
 10      continue
         return
         end
         
         subroutine si2rho(si,g,ig,jg,p)
         integer p,ig(*),jg(*)
         double precision g(*),si(*),d,tmp(p)
         g(1)=si(1)
         tmp(1)=si(1)**0.5
c        collect diagonal elements
         do 5 i=2,p
 5        tmp(i)=si(ig(i+1)-1)**0.5
         do 10 i=2,p
         do 10 k=ig(i),ig(i+1)-1
c         write(6,*)i,jg(k)
         if(i.eq.jg(k))then
         g(k)=si(k)
         else
         g(k)=si(k)/(tmp(i)*tmp(jg(k)))
         endif
 10      continue
         return
         end
