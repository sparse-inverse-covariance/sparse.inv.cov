             subroutine mapit(p,ia,ja,a,iaf,jaf,af,iasub,cmulta)
c given the row representation ia ja a and the fillin representation
c indices iaf jaf, compute the vector af from a and the vector of indices iasub
c so that a=af[iasub]
c to lift from a to af use af[iasub(k))=a(k)
c cmulta is 2 if i!=j and 1 otherwise of same length as a
             integer p, ia(*),ja(*),iaf(*),jaf(*),iasub(*)
             double precision a(*),af(*),tmp(p),cmulta(*)
c             na=ia(p+1)-1
             naf=iaf(p+1)-1
             call rzero(naf,af)
             do 10 i=1,p
             call rzero(p,tmp)
             do 20 k=ia(i),ia(i+1)-1
             if(ja(k).eq.i)then
             cmulta(k)=1.0D0
             else
             cmulta(k)=2.0D0
             endif
 20          tmp(ja(k))=a(k)
             do 30 k=iaf(i),iaf(i+1)-1

c             WRITE(6,*) i
c             WRITE(6,*) iaf(i)
c             WRITE(6,*) k
c             WRITE(6,*) jaf(k)
c             WRITE(6,*) tmp(jaf(k))
c             WRITE(6,*) af(k)
 30          af(k)=tmp(jaf(k))
 10          continue
             k=1
             do 40 j=1,naf
             if(af(j).ne.0)then
             iasub(k)=j
             k=k+1
             endif
 40          continue
             return
             end
             
             
