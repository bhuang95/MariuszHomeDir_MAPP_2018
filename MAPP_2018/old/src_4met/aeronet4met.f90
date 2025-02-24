PROGRAM aeronet4met

!to format aeronet obs for met ascii convertor to ncdf

  USE tension_mod

  IMPLICIT NONE

  INTEGER,  PARAMETER :: stlength=10
  
  CHARACTER(len=250) :: infileconst,infile,outfile
  INTEGER :: iargc,i,j,inunit=51,outunit=61

  INTEGER :: year,month,day,hour
  REAL :: lat,lon,elev,aod_550
  CHARACTER(len=stlength) :: stid
  INTEGER :: istid

  CHARACTER(len=10) :: cdate,ctime
  CHARACTER(len=4) :: ctrash

  INTEGER, PARAMETER :: nwaves=4
  REAL, DIMENSION(nwaves) :: wavelengths
  REAL :: aod(nwaves), aod_error(nwaves), pressure
  INTEGER, DIMENSION(nwaves) :: indx
  INTEGER :: qc_value=0


  DOUBLE PRECISION, DIMENSION(nwaves) :: x,y,yp,sigma
  DOUBLE PRECISION :: xi=550.,yi
  INTEGER :: ier,siger

  INTEGER :: nobs

  LOGICAL :: exist
  
  IF (iargc() < 2) THEN
     PRINT *,'Needs input and output file names - Stopping'
     STOP
  ENDIF

  CALL getarg(1,infile)
  CALL getarg(2,outfile)


  OPEN(inunit,file=infile,form='formatted')
  OPEN(outunit,file=outfile,form='formatted')


  READ(inunit,*,END=101)ctrash,ctrash,wavelengths(1:nwaves)

  CALL indexx(nwaves,wavelengths,indx)

  x=wavelengths(indx(:))

  pressure=1000.0

  i=0
  DO WHILE(.TRUE.)
     READ(inunit,*,END=101)cdate,ctime,aod,stid,lat,lon,elev

     i=i+1

!obtain aod @550nm

     IF (MINVAL(aod) < 0.) CYCLE

     y=aod(indx(:))

     CALL TSPSI (nwaves,x,y,yp,sigma,ier,siger)

     IF (ier /= 0) THEN
        PRINT *,ier,'@1'
        STOP
     ENDIF
     
     aod_550=HVAL(xi,nwaves,x,y,yp,sigma,ier)

     IF (ier /= 0) THEN
        PRINT *,ier,'@2'
        STOP
     ENDIF


     WRITE(outunit,111,advance='NO')'ADPSFC',stid,&
          &cdate(7:10)//cdate(4:5)//cdate(1:2)//'_'//ctime(1:2)//ctime(4:5)//ctime(7:8),&
          &lat,lon,elev,'AOD',pressure,elev,qc_value
     WRITE(outunit,'(f6.3)',advance='YES')aod_550

  ENDDO

101 CONTINUE

  nobs=i

  CLOSE(inunit)
  CLOSE(outunit)

111 FORMAT(a6,1x,a10,1x,a15,1x,f8.3,1x,f8.3,1x,f7.1,1x,a3,1x,f7.1,1x,f7.1,1x,i2,1x)

END PROGRAM aeronet4met
