SUBROUTINE indexx(n,arr,indx)
  INTEGER n,indx(n),M,NSTACK
  REAL arr(n)
  PARAMETER (M=7,NSTACK=50)
  INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
  REAL a
  DO j=1,n
     indx(j)=j
  ENDDO
  jstack=0
  l=1
  ir=n
1 IF(ir-l.LT.M)THEN
     DO j=l+1,ir
        indxt=indx(j)
        a=arr(indxt)
        DO i=j-1,1,-1
           IF(arr(indx(i)).LE.a)GOTO 2
           indx(i+1)=indx(i)
        ENDDO
        i=0
2       indx(i+1)=indxt
     ENDDO
     IF(jstack.EQ.0)RETURN
     ir=istack(jstack)
     l=istack(jstack-1)
     jstack=jstack-2
  ELSE
     k=(l+ir)/2
     itemp=indx(k)
     indx(k)=indx(l+1)
     indx(l+1)=itemp
     IF(arr(indx(l+1)).GT.arr(indx(ir)))THEN
        itemp=indx(l+1)
        indx(l+1)=indx(ir)
        indx(ir)=itemp
     ENDIF
     IF(arr(indx(l)).GT.arr(indx(ir)))THEN
        itemp=indx(l)
        indx(l)=indx(ir)
        indx(ir)=itemp
     ENDIF
     IF(arr(indx(l+1)).GT.arr(indx(l)))THEN
        itemp=indx(l+1)
        indx(l+1)=indx(l)
        indx(l)=itemp
     ENDIF
     i=l+1
     j=ir
     indxt=indx(l)
     a=arr(indxt)
3    continue
     i=i+1
     if(arr(indx(i)).lt.a)goto 3
4    continue
     j=j-1
     if(arr(indx(j)).gt.a)goto 4
     if(j.lt.i)goto 5
     itemp=indx(i)
     indx(i)=indx(j)
     indx(j)=itemp
     goto 3
5    indx(l)=indx(j)
     indx(j)=indxt
     jstack=jstack+2
     if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
     if(ir-i+1.ge.j-l)then
        istack(jstack)=ir
        istack(jstack-1)=i
        ir=j-1
     else
        istack(jstack)=j-1
        istack(jstack-1)=l
        l=i
     endif
  endif
  goto 1
END subroutine indexx

SUBROUTINE rank(n,indx,irank)
  INTEGER n,indx(n),irank(n)
  INTEGER j
  DO j=1,n
     irank(indx(j))=j
  ENDDO
  RETURN
END SUBROUTINE rank
