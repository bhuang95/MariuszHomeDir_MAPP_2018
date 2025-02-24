PROGRAM airnow2gsi_bundle_tmpfix

!mp, May 2010
!does not workdue to char mixup
!to format airnow files for o3 and pmfine for gsi
!will eliminate duplicate stations
!this is for the gsi format after bundle was implemented
!there is a change in output format - 
!includes site code,character and elevation

  IMPLICIT NONE

  INTEGER, PARAMETER :: nmaxstations=5000.

  REAL, PARAMETER :: unknown=-9999.,unknownp=unknown+1.,&
       &o3max=200.,pm25max=100.,pm10max=150. ! changed from pm25max=100.

  INTEGER :: concunit=101,siteunit=102,outunit=201
  
  REAL :: llat,llon,hheight

  CHARACTER(len=120) :: concfile,sitefile,outfile
  INTEGER :: iargc,i,j,k,year,month,day,hour,scode,nstations,mdy,min=30

  CHARACTER(len=20), ALLOCATABLE, DIMENSION(:) :: name
  CHARACTER(len=9), ALLOCATABLE, DIMENSION(:) :: code
  INTEGER :: ccode
  CHARACTER(len=12) :: species
  REAL, ALLOCATABLE, DIMENSION(:) :: lat,lon,height,conc
  INTEGER, ALLOCATABLE, DIMENSION(:) :: sitechar
  REAL :: concmax

  IF (iargc() < 4) THEN
     PRINT *,'Requires names of 3 files as input'
     PRINT *,'See sfc4gsi.f90 Stopping'
     STOP
  ENDIF

  CALL getarg(1,species)
  CALL getarg(2,concfile)
  CALL getarg(3,sitefile)
  CALL getarg(4,outfile)

  IF (species=='o3') THEN
     concmax=o3max
  ELSE IF (species=='PM2_5_DRY') THEN
     concmax=pm25max
     species='pm2_5'
  ELSE  IF (species=='PM10') THEN
     concmax=pm10max
     species='pm10'
  ELSE
     PRINT *,'Species outside range Stopping'
     STOP
  ENDIF

  ALLOCATE(code(nmaxstations),lat(nmaxstations),lon(nmaxstations),&
       &height(nmaxstations),sitechar(nmaxstations))

  lat=unknown
  lon=unknown
  height=unknown
  
  ALLOCATE(conc(nmaxstations))
  
  OPEN(unit=concunit,file=concfile,form='formatted')

  i=1

  DO WHILE (.TRUE.)
     READ(concunit,*,END=501)mdy,hour,code(i),conc(i) 
     IF (conc(i) < 0. .OR. conc(i) > concmax) CYCLE
     IF (i > 1) THEN
        IF (code(i)==code(i-1)) CYCLE
     ENDIF
     i=i+1
  ENDDO

501 CONTINUE

  CLOSE(concunit)

  nstations=i-1

  month=mdy/1e4
  day=(mdy-month*1e4)/1e2
  year=2000+mdy-month*1e4-day*1e2

  OPEN(unit=siteunit,file=sitefile,form='formatted')
  DO WHILE (.TRUE.)
     READ(siteunit,'(i9,2f15.6,f10.2,i3)',END=502)ccode,llat,llon,hheight,scode

     DO i=1,nstations
        IF (ccode==code(i)) THEN
           lat(i)=llat
           lon(i)=llon
           height(i)=hheight
           sitechar(i)=scode
           EXIT
        ENDIF
     ENDDO
  ENDDO

502 CONTINUE

  CLOSE(siteunit)

  OPEN(outunit,file=outfile,form='formatted')

  j=0
  k=0

!  WRITE(outunit,'(a12)') species

  DO i=1,nstations
     IF (lat(i) > unknownp .AND. lon(i) > unknownp) THEN
        IF (conc(i) >= 0. .AND. conc(i) < 200. ) THEN
           WRITE(outunit,'(5i5,2f11.5,f7.2,i3,f10.1,a11)')&
                &year,month,day,hour,min,&
                &lat(i),lon(i),conc(i),sitechar(i),height(i),code(i)
           IF (height(i) < unknownp) k=k+1
        ENDIF
     ELSE
        j=j+1
        PRINT *,'Station ',species,'  ',code(i),&
             &' will be neglected since latlon unknown'

     ENDIF
  ENDDO

  PRINT *, 'DATE: ',year,month,day,hour
  PRINT *,j,' stations neglected out of ',nstations
  PRINT *,k,' stations elevation unknown'

  CLOSE(outunit)

END PROGRAM airnow2gsi_bundle_tmpfix

