         subroutine getg(n,igf,g,ig,jg)
         double precision g(n)
         integer n,igf(n),ig(*),jg(n)
         do 10 i=1,n
         jg(i)=igf(i)
         g(i)=1.0d0
 10      ig(i)=i
         ig(n+1)=n+1
         return
         end
         
