PROGRAM test

!to extract variable names when separated by commas or another character

  IMPLICIT NONE

  CHARACTER(len=50) :: string="aabc"
  CHARACTER(len=1) :: separator=','


  CHARACTER(len=20), DIMENSION(10) :: varnames
  INTEGER :: i,j,strlength
  INTEGER, DIMENSION(10) :: indices

  varnames=''
  indices=0

  strlength=LEN(TRIM(string))

  indices(1)=1

  j=1
  DO i=1,strlength
     IF (string(i:i)==separator) THEN
        j=j+1
        indices(j)=i+1
     ENDIF
  ENDDO
        
  DO i=1,j-1
     varnames(i)=string(indices(i):indices(i+1)-2)
  ENDDO

  varnames(j)=string(indices(j):strlength)

  PRINT *,j,indices

  DO i=1,j
     PRINT *,varnames(i)
  ENDDO

  PRINT *,string

END PROGRAM test


