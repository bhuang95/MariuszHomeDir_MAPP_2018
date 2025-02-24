SUBROUTINE indexx_numi(n,arr,indx)

  INTEGER n,indx(n),M,NSTACK
  INTEGER, DIMENSION(n) :: arr
  PARAMETER (M=7,NSTACK=50)
  INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
  INTEGER ::  a
  DO  j=1,n
     indx(j)=j
  ENDDO
  jstack=0
  l=1
  ir=n
1 IF(ir-l < M)THEN
     DO j=l+1,ir
        indxt=indx(j)
        a=arr(indxt)
        DO  i=j-1,1,-1
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
     IF(arr(indx(l+1)) > arr(indx(ir)))THEN
        itemp=indx(l+1)
        indx(l+1)=indx(ir)
        indx(ir)=itemp
     ENDIF
     IF(arr(indx(l)) > arr(indx(ir)))THEN
        itemp=indx(l)
        indx(l)=indx(ir)
        indx(ir)=itemp
     ENDIF
     IF(arr(indx(l+1)) > arr(indx(l)))THEN
        itemp=indx(l+1)
        indx(l+1)=indx(l)
        indx(l)=itemp
     ENDIF
     i=l+1
     j=ir
     indxt=indx(l)
     a=arr(indxt)
3    CONTINUE
     i=i+1
     IF(arr(indx(i)) < a)GOTO 3
4    CONTINUE
     j=j-1
     IF(arr(indx(j)) > a)GOTO 4
     IF(j < i)GOTO 5
     itemp=indx(i)
     indx(i)=indx(j)
     indx(j)=itemp
     GOTO 3
5    indx(l)=indx(j)
     indx(j)=indxt
     jstack=jstack+2
     IF(jstack > NSTACK)PAUSE 'NSTACK too small in indexx'
     IF(ir-i+1 >= j-l)THEN
        istack(jstack)=ir
        istack(jstack-1)=i
        ir=j-1
     ELSE
        istack(jstack)=j-1
        istack(jstack-1)=l
        l=i
     ENDIF
  ENDIF
  GOTO 1
END SUBROUTINE indexx_numi
