MODULE module_utils

  USE netcdf

  USE module_misc, ONLY: max_name_length,max_vars,&
       &separator_model,separator_obs,separator_varnames,aerostring,&
       &r_earth

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: get_names,extract_names,upper2lower,lower2upper,replace_text,getindex,&
       &sphere_distance

CONTAINS
  
  SUBROUTINE extract_names(string,separator,nvars,names)

    CHARACTER(len=max_name_length), INTENT(in) :: string
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

  SUBROUTINE get_names(obs,model,table,nvars,allnames,&
       &aerosoltable,naeros,aeronames)

    CHARACTER(max_name_length), INTENT(in) :: obs,model
    CHARACTER(max_name_length), INTENT(in) :: table
    INTEGER, INTENT(inout) :: nvars
    CHARACTER(max_name_length), DIMENSION(max_vars), INTENT(inout) :: &
         &allnames

    CHARACTER(max_name_length), OPTIONAL, INTENT(in) :: aerosoltable
    INTEGER, OPTIONAL, INTENT(inout) :: naeros
    CHARACTER(max_name_length), DIMENSION(max_vars), OPTIONAL, &
         &INTENT(inout) :: aeronames
    
    INTEGER :: inunit=25
    CHARACTER(max_name_length) :: model_local,obs_local
    CHARACTER(max_name_length) :: string_local
    INTEGER :: i,nvarphys
    LOGICAL :: exist
    LOGICAL :: logaero

    nvars=0
    naeros=0
    allnames=''
    aeronames=''

    INQUIRE(file=table,exist=exist)
    IF (.NOT. exist) THEN
       PRINT *,'file '//TRIM(table),' missing - stopping'
       STOP
    ENDIF

    OPEN(unit=inunit,file=table,form='formatted')

    DO WHILE(.TRUE.) 
       READ(inunit,*,END=100)string_local
       CALL extract_names(string_local,separator_model,i,allnames)
       model_local=allnames(1)
       IF (upper2lower(TRIM(model)) == upper2lower(TRIM(model_local))) &
            &THEN
          string_local=string_local(LEN_TRIM(model_local)+2:)
          CALL extract_names(string_local,separator_obs,i,allnames)
          obs_local=allnames(1)
          IF (upper2lower(TRIM(obs)) == upper2lower(TRIM(obs_local))) &
            &THEN
             string_local=string_local(LEN_TRIM(obs_local)+2:)
             CALL extract_names(string_local,separator_varnames,nvars,allnames)
             EXIT
          ENDIF
       ENDIF
    END DO

100 CONTINUE
    
    IF (nvars == 0) THEN
       PRINT *,'Model '//TRIM(model)//' and obs '//TRIM(obs)//&
            &' not in '//TRIM(table)
       STOP
    ENDIF

    CLOSE(inunit)

    DO i=1,nvars
       IF (upper2lower(allnames(i)) == upper2lower(aerostring)) THEN
          IF (PRESENT(aerosoltable) .AND. PRESENT(naeros) .AND. &
               &PRESENT(aeronames)) THEN

             IF (upper2lower(allnames(i))==upper2lower(allnames(i)) &
                  &.AND. i/= nvars) THEN
                PRINT *,aerostring//' must appear as last in '//&
                     &TRIM(table)//' Stopping'
                STOP
             ENDIF

             INQUIRE(file=aerosoltable,exist=exist)

             IF (.NOT. exist) THEN
                PRINT *,'file '//TRIM(aerosoltable)//' missing - stopping'
                STOP
             ENDIF

             logaero=.TRUE.

             EXIT
          ELSE
             PRINT *,'Aerostring in '//TRIM(table)//' but not in ',&
                  &' get_names argument list - Stopping'
             STOP
          ENDIF
       ENDIF
    ENDDO

    IF (logaero) THEN

       nvarphys=nvars-1
       allnames(nvars)=''
       OPEN(unit=inunit,file=aerosoltable,form='formatted')
       
       DO WHILE(.TRUE.) 
          READ(inunit,*,END=200)string_local
          CALL extract_names(string_local,separator_model,i,aeronames)
          IF (upper2lower(TRIM(model)) == &
               &upper2lower(TRIM(model_local))) THEN
             string_local=string_local(LEN_TRIM(model_local)+2:)
             CALL extract_names(string_local,separator_varnames,&
                  &naeros,aeronames)
             EXIT
          ENDIF
       END DO

200    CONTINUE

       allnames(nvarphys+1:nvarphys+naeros)=aeronames(1:naeros)

       nvars=nvarphys+naeros

    ENDIF


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

  REAL FUNCTION sphere_distance(phi1,phi2,lambda1,lambda2)

    REAL, INTENT(in) :: phi1,phi2,lambda1,lambda2
  
    sphere_distance=r_earth*ACOS( SIN(phi1)*SIN(phi2) + COS(phi1)*COS(phi2) * &
         &COS(lambda2 - lambda1) )
    
  END FUNCTION sphere_distance

END MODULE module_utils
