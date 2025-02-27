MODULE modis2ioda_init

  IMPLICIT NONE

CONTAINS

  SUBROUTINE init

    USE modis2ioda_utils, ONLY: infile, outfile,&
         &validtimestr,validtimeint,validtime,satellite
    USE datetime_mod

    IMPLICIT NONE

    INTEGER :: nargs,iarg
    LOGICAL :: existin
    INTEGER :: yyyy,mm,dd,hh

    INTEGER :: iargc

    IF (iargc() /= 3) THEN
       WRITE(*,*) "Needs three arguments"
       WRITE(*,*) "valid cycle time and input and output file names"
       STOP
    END IF

    CALL getarg(1,validtimestr)
    READ( validtimestr(1:4), '(i4)' )  yyyy
    READ( validtimestr(5:6), '(i2)' )  mm
    READ( validtimestr(7:8), '(i2)' )  dd
    READ( validtimestr(9:10), '(i2)' )  hh
    READ( validtimestr, '(i10)') validtimeint
    validtime = create_datetime(year=yyyy,month=mm,day=dd,hour=hh)

    CALL getarg(2,infile)
    CALL getarg(3,outfile)    

    INQUIRE(file=infile,exist=existin)
    
    IF (INDEX(infile,"MYD") > 0) THEN
       satellite="aqua"
    ELSE IF (INDEX(infile,"MOD") > 0) THEN
       satellite="terra"
    ELSE
       PRINT *,"Unknown satellite - skipping"
    ENDIF

    IF (.NOT. existin) THEN
       WRITE(*,*) "Input netCDF file does not exist. Abort!"
       STOP 1
    END IF

  END SUBROUTINE init

END MODULE modis2ioda_init
