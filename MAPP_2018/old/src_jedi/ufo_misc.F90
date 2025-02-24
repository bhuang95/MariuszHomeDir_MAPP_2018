MODULE ufo_misc

  USE netcdf

  PRIVATE
  PUBLIC :: get_names
  PUBLIC :: max_name_length,max_string_length,max_vars
  PUBLIC :: obs2model_table
  PUBLIC :: separator_model,separator_obs,separator_varnames


  INTEGER, PARAMETER :: max_name_length=50,max_string_length=500, &
       &max_vars=20

  CHARACTER(len=1), PARAMETER :: separator_model='|',&
       &separator_obs=':',separator_varnames=','

  CHARACTER(len=max_string_length) :: obs2model_table='obs2model_vars.txt'

CONTAINS
  
  SUBROUTINE extract_names(string,separator,nvars,names)

    CHARACTER(len=max_string_length), INTENT(in) :: string
    CHARACTER(len=1), INTENT(in) :: separator
    INTEGER, INTENT(inout) :: nvars
    CHARACTER(len=max_name_length), DIMENSION(max_vars), &
         &INTENT(inout) :: names

    INTEGER, DIMENSION(max_vars) :: indices

    INTEGER :: i,j,strlength

    nvars=0
    names=''
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
       names(i)=string(indices(i):indices(i+1)-2)
    ENDDO

    names(j)=string(indices(j):strlength)

    nvars=j

  END SUBROUTINE extract_names

  SUBROUTINE get_names(obs,model,nvars,names)

    CHARACTER(max_name_length), INTENT(in) :: obs,model
    INTEGER, INTENT(inout) :: nvars
    CHARACTER(max_name_length), DIMENSION(max_vars), INTENT(inout) :: &
         &names
    
    INTEGER :: inunit=25
    CHARACTER(max_name_length) :: model_local,obs_local
    CHARACTER(max_string_length) :: string_local
    INTEGER :: i
    LOGICAL :: exist

    nvars=0

    INQUIRE(file=obs2model_table,exist=exist)
    IF (.NOT. exist) THEN
       PRINT *,'file '//TRIM(obs2model_table),' missing - put this file in &
            &build/ufo/test'
       STOP
    ENDIF

    OPEN(unit=inunit,file=obs2model_table,form='formatted')

    DO WHILE(.TRUE.) 
       READ(inunit,*,END=100)string_local
       CALL extract_names(string_local,separator_model,i,names)
       model_local=names(1)
       IF (upper2lower(TRIM(model)) == upper2lower(TRIM(model_local))) &
            &THEN
          string_local=string_local(LEN_TRIM(model_local)+2:)
          PRINT *,model_local
          CALL extract_names(string_local,separator_obs,i,names)
          obs_local=names(1)
          PRINT *,obs_local
          IF (upper2lower(TRIM(obs)) == upper2lower(TRIM(obs_local))) &
            &THEN
             string_local=string_local(LEN_TRIM(obs_local)+2:)
             CALL extract_names(string_local,separator_varnames,nvars,names)
             PRINT *,names
             EXIT
          ENDIF
       ENDIF
    END DO

100 CONTINUE
    
    IF (nvars == 0) THEN
       PRINT *,'Model '//TRIM(model)//' and obs '//TRIM(obs)//&
            &' not in '//TRIM(obs2model_table)
       STOP
    ENDIF

    CLOSE(inunit)

  END SUBROUTINE get_names

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

  INTEGER FUNCTION getindex(names,usrname)
    IMPLICIT NONE
    CHARACTER(len=*),INTENT(in) :: names(:)
    CHARACTER(len=*),INTENT(in) :: usrname
    INTEGER i
    getindex=-1
    DO i=1,SIZE(names)
       IF(TRIM(usrname)==TRIM(names(i))) THEN
          getindex=i
          EXIT
       ENDIF
    ENDDO
  END FUNCTION getindex
  
END MODULE ufo_misc
