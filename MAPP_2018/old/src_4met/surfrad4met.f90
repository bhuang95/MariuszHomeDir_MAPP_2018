PROGRAM surfrad4met

!to format surfrad obs for met ascii convertor to ncdf

  USE iso_fortran_env, ONLY: real64
  USE datetime_module, ONLY: datetime, timedelta
  USE tension_mod

  IMPLICIT NONE

  TYPE(datetime)  :: utctime
  TYPE(timedelta) :: dt

  INTEGER,  PARAMETER :: stlength=10
  
  CHARACTER(len=250) :: infileconst,infile,outfile
  INTEGER :: iargc,i,inunit=51,outunit=61

  INTEGER :: year,month,day,hour
  REAL :: lat,lon,elev,aod_550
  CHARACTER(len=stlength) :: stid
  INTEGER :: istid
  CHARACTER(len=4) :: cyear
  CHARACTER(len=2) :: cmonth,cday,chour
  CHARACTER(len=8) :: cdate
  CHARACTER(len=4) :: chhmm

  INTEGER, PARAMETER :: nwaves=5
  REAL, DIMENSION(nwaves) :: wavelengths
  REAL :: aod(nwaves), aod_error(nwaves), pressure
  INTEGER, DIMENSION(nwaves) :: indx
  INTEGER :: qc_value=0


  DOUBLE PRECISION, DIMENSION(nwaves) :: x,y,yp,sigma
  DOUBLE PRECISION :: xi=550.,yi
  INTEGER :: ier,siger

  INTEGER :: nobs,utcshift

  INTEGER :: hhmm,qc

  logical :: exist
  
  IF (iargc() < 4) THEN
     PRINT *,'Needs four inputs and output file names - Stopping'
     STOP
  ENDIF

  CALL getarg(1,infileconst)
  CALL getarg(2,cdate)
  CALL getarg(3,infile)
  CALL getarg(4,outfile)

  cyear=cdate(1:4)
  cmonth=cdate(5:6)
  cday=cdate(7:8)

  OPEN(inunit,file=infileconst,form='formatted')
  READ(inunit,*)stid
  READ(inunit,*)lat,lon,elev,utcshift
  CLOSE(inunit)

  OPEN(inunit,file=infile,form='formatted')
!  INQUIRE(file=outfile,exist=exist)
  OPEN(outunit,file=outfile,form='formatted',position='APPEND')

  DO i=1,2
     READ(inunit,*,END=101)
  ENDDO

  READ(inunit,*,END=101)wavelengths

  CALL indexx(nwaves,wavelengths,indx)

  x=wavelengths(indx(:))

  DO i=1,3
     READ(inunit,*,END=101)
  ENDDO

  READ(cyear,'(i4)')year
  READ(cmonth,'(i2)')month
  READ(cday,'(i2)')day
  
  i=0
  DO WHILE(.TRUE.)
     READ(inunit,*,END=101)hhmm,qc,aod(:),aod_error(:),pressure
     IF (qc /= 0) CYCLE

     i=i+1

!obtain aod @550nm

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

!convert date/time from DST to UTC

     hour=hhmm/100

     utctime=datetime(year=year,month=month,day=day,hour=hour)

     dt=timedelta(hours=utcshift)
     utctime=utctime+dt

     WRITE(cyear,'(i4)')utctime%getyear()
     WRITE(cmonth,'(i2.2)')utctime%getmonth()
     WRITE(cday,'(i2.2)')utctime%getday()

     WRITE(chhmm,'(i4.4)')utctime%gethour()*100+MOD(hhmm,100)

     WRITE(outunit,111,advance='NO')'ADPSFC',stid,&
          &cyear//cmonth//cday//'_'//chhmm//'00',&
          &lat,lon,elev,'AOD',pressure,elev,qc_value
     WRITE(outunit,'(f6.3)',advance='YES')aod_550

  ENDDO

101 CONTINUE

  nobs=i

  CLOSE(inunit)
  CLOSE(outunit)

111 FORMAT(a6,1x,a10,1x,a15,1x,f8.3,1x,f8.3,1x,f7.1,1x,a3,1x,f7.1,1x,f7.1,1x,i2,1x)

END PROGRAM surfrad4met
