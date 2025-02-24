PROGRAM aeronet2gsi_bundle

!mp, April 2014

!to format aeronet files for gsi bundle format - to be converted to bufr
!includes site code,character and elevation

  IMPLICIT NONE

  INTEGER, PARAMETER :: nmaxstations=5000.,naods=7

  REAL, PARAMETER :: unknown=-9999.,unknownp=unknown+1.

  INTEGER :: inunit=101,outunit=201
  
  REAL :: llat,llon,hheight

  CHARACTER(len=120) :: aodfile,sitefile,outfile
  INTEGER :: iargc,i,j,k,year,month,day,hour,nstations,min,sec

  CHARACTER(len=9) :: code
  CHARACTER(len=12) :: species

  REAL :: lat,lon,height
  REAL, DIMENSION(naods) :: aod
  INTEGER :: sitechar
  REAL :: aodmax,jday
  INTEGER :: ndata,ddmmyyyy,hhmmss

  CHARACTER(len=256) :: header='year month day hour min lat lon AOT_1640 AOT_1020 AOT_870 AOT_675 AOT_500 AOT_440 AOT_340 sitechar elev name'

  IF (iargc() < 2) THEN
     PRINT *,'Requires 2 names as input'
     PRINT *,'See sfc4gsi.f90 Stopping'
     STOP
  ENDIF

  CALL getarg(1,aodfile)
  CALL getarg(2,outfile)

  OPEN(unit=inunit,file=aodfile,form='formatted')
  OPEN(outunit,file=outfile,form='formatted')
!  WRITE(outunit,'(A)')TRIM(header)

  i=1

  DO WHILE (.TRUE.)
     READ(inunit,*,END=501)ndata
     READ(inunit,*)code
     READ(inunit,*)lon,lat,height
     sitechar=0

     DO j=1,ndata
        READ(inunit,*)ddmmyyyy,hhmmss,jday,aod
        day=ddmmyyyy/1e6
        month=(ddmmyyyy-day*1e6)/1e4
        year=(ddmmyyyy-day*1e6-month*1e4)
        
        hour=hhmmss/1e4
        min=(hhmmss-hour*1e4)/1e2
        sec=(hhmmss-hour*1e4-min*1e2)
        
        WRITE(outunit,'(5i5,2f11.5,7e15.7,i3,f10.1,a11)')&
             &year,month,day,hour,min,&
             &lat,lon,aod,sitechar,height,code
     ENDDO

     i=i+1
     
  ENDDO
  
501 CONTINUE

  CLOSE(inunit)

  nstations=i-1
  
  CLOSE(outunit)

END PROGRAM aeronet2gsi_bundle

