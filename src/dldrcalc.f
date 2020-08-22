           subroutine dldrcalc(r,ir,jr,p,dldsi,dldr)
           integer ir(*),jr(*),p
           double precision r(*),dldsi(*),dldr(*),d(p)
c
c extract diagonal elements
           do 10 i=1,p
 10        d(i)=r(ir(i+1)-1)**0.5
c compute derivatives wrt r
           do 20 i=1,p
           do 20 k=ir(i),ir(i+1)-1
             j=jr(k)
             if(i.ne.j)then
             dldr(k)=d(i)*d(j)*dldsi(k)
           else
             dldr(k)=dldsi(k)
c compute ith row terms
             do 30 kk=ir(i),ir(i+1)-2
 30          dldr(k)=dldr(k)+0.5d0*(d(jr(kk))/d(i))*r(kk)*dldsi(kk)
c compute remaining terms
             do 40 kk=i+1,p
             do 45 l=ir(kk),ir(kk+1)-2
              if(i.eq.jr(l))then
              dldr(k)=dldr(k)+0.5d0*(d(jr(l))/d(i))*r(l)*dldsi(l)
              go to 40
              endif
 45          continue
 40          continue
            endif
 20        continue
           return
           end
           
