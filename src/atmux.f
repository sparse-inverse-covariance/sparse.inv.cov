c-----------------------------------------------------------------------
      subroutine atmux (n,m, x, y, a, ja, ia)
      real*8 x(*), y(*), a(*)
      integer n,m, ia(*), ja(*)
c-----------------------------------------------------------------------
c         transp( A ) times a vector    hacked by hk (added row dim m)
c-----------------------------------------------------------------------
c multiplies the transpose of a matrix by a vector when the original
c matrix is stored in compressed sparse row storage. Can also be
c viewed as the product of a matrix by a vector when the original
c matrix is stored in the compressed sparse column format.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n     = row dimension of A
c m     = col dimension of A
c x     = real array of length equal to the column dimension of
c         the A' matrix.
c a, ja,
c    ia = input matrix in compressed sparse row format.
c
c on return:
c-----------
c y     = real array of length m, containing the product y=transp(A)*x
c
c-----------------------------------------------------------------------
c     local variables
c
      integer i, k
c-----------------------------------------------------------------------
c
c     zero out output vector
c
      do 1 i=1,m
         y(i) = 0.0d0
 1    continue
c
c loop over the rows
c
      do 100 i = 1,n
         do 99 k=ia(i), ia(i+1)-1
            y(ja(k)) = y(ja(k)) + x(i)*a(k)
 99      continue
 100  continue
c
      return
      end
c-------------end-of-atmux----------------------------------------------
c-----------------------------------------------------------------------
