c----------------------------------------------------------------------c
c 1)     M A T R I X    B Y    V E C T O R     P R O D U C T S         c
c----------------------------------------------------------------------c
      subroutine amux(n,m, x, y, a,ja,ia)
      real*8  x(*), y(*), a(*)
      integer n,m, ja(*), ia(*)
c-----------------------------------------------------------------------
c         A times a vector        # hack by hk added dummy argument m
c-----------------------------------------------------------------------
c multiplies a matrix by a vector using the dot product form
c Matrix A is stored in compressed sparse row storage.
c
c on entry:
c----------
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c a, ja,
c    ia = input matrix in compressed sparse row format.
c
c on return:
c-----------
c y     = real array of length n, containing the product y=Ax
c
c-----------------------------------------------------------------------
c local variables
c
      real*8 t
      integer i, k
c-----------------------------------------------------------------------
      do 100 i = 1,n
c
c     compute the inner product of row i with vector x
c
         t = 0.0d0
         do 99 k=ia(i), ia(i+1)-1
            t = t + a(k)*x(ja(k))
 99      continue
c
c     store result in y(i)
c
         y(i) = t
 100  continue
c
      return
      end
c---------end-of-amux---------------------------------------------------
c-----------------------------------------------------------------------
