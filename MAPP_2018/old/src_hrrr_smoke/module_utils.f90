MODULE module_utils

  USE kinds, only : i_kind

  PUBLIC :: upper2lower, lower2upper, replace_text, getindex

CONTAINS

  FUNCTION upper2lower(str) RESULT(string)

    IMPLICIT NONE

    CHARACTER(*), INTENT(in) :: str
    CHARACTER(LEN(str))      :: string

    INTEGER :: ic, i

    CHARACTER(26), PARAMETER :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(26), PARAMETER :: lower = 'abcdefghijklmnopqrstuvwxyz'

!   lowcase each letter if it is lowecase
    string = str
    DO i = 1, LEN_TRIM(str)
       ic = INDEX(upper, str(i:i))
       IF (ic > 0) string(i:i) = lower(ic:ic)
    END DO

  END FUNCTION upper2lower

  FUNCTION lower2upper(str) RESULT (string)

    IMPLICIT NONE

    CHARACTER(*), INTENT(in) :: str
    CHARACTER(LEN(str))      :: string

    INTEGER :: ic, i

    CHARACTER(26), PARAMETER :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    CHARACTER(26), PARAMETER :: lower = 'abcdefghijklmnopqrstuvwxyz'

!   lowcase each letter if it is lowecase
    string = str
    DO i = 1, LEN_TRIM(str)
       ic = INDEX(lower, str(i:i))
       IF (ic > 0) string(i:i) = upper(ic:ic)
    END DO

  END FUNCTION lower2upper

  FUNCTION replace_text(s,text,rep) RESULT(outs) 
    CHARACTER(*)        :: s,text,rep
    CHARACTER(LEN(s)+100) :: outs  ! provide outs with extra 100 char len
    INTEGER             :: i, nt, nr

    outs = s ; nt = LEN_TRIM(text) ; nr = LEN_TRIM(rep)
    DO
       i = INDEX(outs,text(:nt)) ; IF (i == 0) EXIT
       outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
    END DO

  END FUNCTION replace_text

  INTEGER(i_kind) FUNCTION getindex(varnames,usrname)
    IMPLICIT NONE
    CHARACTER(len=*),INTENT(in) :: varnames(:)
    CHARACTER(len=*),INTENT(in) :: usrname
    INTEGER(i_kind) i
    getindex=-1
    DO i=1,SIZE(varnames)
       IF(TRIM(usrname)==TRIM(varnames(i))) THEN
          getindex=i
          EXIT
       ENDIF
    ENDDO
  END FUNCTION getindex
  
END MODULE module_utils
