    
        subroutine sptakahashi(p,perm,il,jl,l,ilt,jlt,lt,a,at,na)    
c need storage for ajk p by 1
c na is length(a)
c sparse Takahashi method
c need row representation of l ial,jal,l
c and col representation of l ialt,jalt,lt   (row rep of transpose)
c row representation of a ia, ja a          (allowing for fill in)
c and column representation of a iat,jat,at
c get diagonals of l
c lt[perm]=l
        integer p,perm(*),il(*),jl(*),ilt(*),jlt(*),na,adiags(p)
        integer atdiags(p)
        double precision l(*),lt(*),a(*),at(*),d(p),ajk(p),sjj
        double precision sji
        call rzero(p,d)
        do 5 i=1,p
        adiags(i)=il(i+1)-1
c       adiags=il(2:(p+1))-1
c       atdiags=ilt(1:p)
        atdiags(i)=ilt(i)
 5      continue
        do 10 i = 1,p
        d(i)=lt(ilt(i))    
c assumes first element of row (col) is the diagonal
 10     continue    
c do diagonals of inverse
        do 20 j = p,1,-1
        call rzero(p,ajk)
        sjj=0.0d0
        if(j.eq.p)then
        ajk(j)=1/d(j)**2
        a(na)=ajk(j)
        at(na)=ajk(j)
        else
        if(ilt(j).le.(ilt(j+1)-2))then
        do 30 k=(ilt(j)+1),(ilt(j+1)-1)
        sjj=sjj+at(k)*lt(k)
 30     continue  
        endif
        ajk(j)=1/d(j)**2-sjj/d(j)
        a(adiags(j))=ajk(j)
        at(atdiags(j))=ajk(j)
        endif
c get off diagonals  - start with a=0
c first get part of jth row of a into ajk
        if((il(j+1)-il(j)).gt.1)then
        do 40 k = il(j),(il(j+1)-1)
        ajk(jl(k))=a(k)
 40     continue    
        do 50 k=ilt(j),(ilt(j+1)-1)
        ajk(jlt(k))=at(k)
 50      continue
c do sum to get aji
        if(j.ne.1)then   
        do 60 k1=(il(j+1)-2),il(j),-1    
cwhat if only diagonal is non zero?
        i=jl(k1)
        sji=0.0d0
        do 70 k=(ilt(i)+1),(ilt(i+1)-1)
        sji=sji+ajk(jlt(k))*lt(k)
 70     continue    
        ajk(i)=-sji/d(i)
 60     continue
        endif
c store non zero elements of ajk in sparse a  (lower triangle)  +sparse col repn
c cat(j,ajk,"\n")
        endif 
        do 80 k1 = il(j),(il(j+1)-1) 
        a(k1)=ajk(jl(k1))
 80     continue
        do 90 k2 = (il(j+1)-1),il(j),-1 
        at(perm(k2))=ajk(jl(k2))
 90     continue
    
 20      continue    
         return
         end
