        subroutine dhess(ia,ja,a,p,n,dh)
c using lower triangle sparse representation of sigma=a p by p, length(a)=n
c compute diagonals of hessian matrix dh
        integer i,k,l,n,p,ia(*),ja(n)
        double precision a(n),dh(n),dij,sii(p)
c       store diagonals
        do 10 i=1,p
 10     sii(i)=a(ia(i+1)-1)
c       compute diagonal elements of hessian
        l=0
        do 20 i=1,p
        do 20 k=ia(i),ia(i+1)-1
        j=ja(k)
        l=l+1
        if(i.eq.j)then
        dij=1.0d0
        else
        dij=0.0d0
        endif
        dh(l)=(2-dij)*sii(i)*sii(j)+2*(1-dij)*a(l)*a(l)
 20     continue
        return
        end
                
                

                
