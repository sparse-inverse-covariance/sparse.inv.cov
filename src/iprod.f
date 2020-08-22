           subroutine iprod(x,nr,i,j,stor)
           double precision x(*),stor
           integer nr,i,j
           ic=(i-1)*nr
           jc=(j-1)*nr
           do 10 k=1,nr
 10        stor=stor+x(ic+k)*x(jc+k)
           return
           end
