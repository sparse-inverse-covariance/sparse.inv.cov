           subroutine dldgcalc(g,ig,jg,p,dldsi,dldg)
           double precision g(*),dldsi(*),dldg(*),d,d2,xs
           integer ig(*),jg(*),p
           dldg(1)=-dldsi(1)/g(1)**2
           do 10 i=2,p
           d=1/g(ig(i+1)-1)
           d2=d*d
           xs=0.0d0
           do 10 k=ig(i),ig(i+1)-1
           if(i.ne.jg(k))then
           dldg(k)=-dldsi(k)*d
           xs=xs+g(k)*d2*dldsi(k)
           else
           dldg(k)=xs-dldsi(k)*d2
           endif
 10        continue
           return
           end
           
