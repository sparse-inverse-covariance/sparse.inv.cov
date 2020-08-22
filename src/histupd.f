           subroutine histupd(z,nz,nbin2,iy)
           double precision z(nz)
           integer nbin2, iy(nbin2)
           zbin=(nbin2-1)/2.0d0
           do 10 i=1,nz
           k=dnint((z(i)+1.0d0)*zbin+1.0d0)
 10        iy(k)=iy(k)+1
           return
           end
