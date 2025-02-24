PROGRAM testchar

  IMPLICIT NONE

  CHARACTER(len=10) :: string1
  CHARACTER(len=12) :: string2
  CHARACTER(len=30) :: string3,string4

  REAL(8) :: r1,r2

  REAL :: single=1.,rsingle

  INTEGER :: iutc,i,ii

  iutc=12

  DO i=1,4
     ii=MOD(iutc+i*6,24)-1
     PRINT *,ii
  ENDDO
  
  STOP

  rsingle = TINY(single)

  PRINT *,LOG(rsingle)

  stop


!  EQUIVALENCE(string3,r1)
!  EQUIVALENCE(string4,r2)

  string1='abcdefghij'
  string2='abcdefghij'

  string3='abcdefghijklmnoprstuw1234567'

  IF (TRIM(string1) /= TRIM(string2)) THEN
     PRINT *,'notequal'
  ELSE
     PRINT *,'equal'
  ENDIF

  READ(string3,*)r1

  WRITE(string4,'(a30)')r1

  PRINT *,string3,r1

  PRINT *,string4


END PROGRAM testchar
