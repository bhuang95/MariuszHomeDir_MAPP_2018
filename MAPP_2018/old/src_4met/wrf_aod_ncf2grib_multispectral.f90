PROGRAM wrf_aod_ncf2grib

!----------convert aod wrf output to grib format 
!
!  by MP Apr, 2018 and MP Aug 2015
!  from      Youhua Tang, Jun 2009
!  modified for cmaq471 by Jianping Huang, July 2011

  USE netcdf_io

  IMPLICIT NONE

  INTEGER, PARAMETER :: nchan_nnr=6,nchan_nnr_550=4

  
  REAL, ALLOCATABLE :: work(:,:), aod(:,:,:,:),tmp(:,:,:)

  LOGICAL, ALLOCATABLE :: lb(:,:)   ! logical mask

  INTEGER, PARAMETER :: naerospec=1, nwrf=1,nlayers=1
  CHARACTER(len=10) :: varlist(nwrf)

  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  INTEGER,           DIMENSION(4)                   :: dims
  CHARACTER(len=25)                                 :: varstringname
  CHARACTER*50 ::         attribute

  CHARACTER(len=10) :: start_date
  
  INTEGER :: i,j,k
  CHARACTER(len=250), ALLOCATABLE, DIMENSION(:) :: infiles
  CHARACTER(len=250) :: filename,indir
  CHARACTER(len=50) :: pattern
  CHARACTER(len=2) :: ci
  INTEGER kpds(200),kgds(200),gribid,gribtb
  LOGICAL :: ave1hr,exist
  INTEGER :: fcst_length,nfiles

  INTEGER :: year,month,day,hour,yy

  INTEGER :: id_gribdomain,imax,jmax,kmax,imonth,&
       &idate,ntt,ierr,maxrec1,nx,ny,nchan,nt

  REAL :: truelat1,truelat2,standlon,dx,dy,lat1,lon1

!cmaq stuff to be found out
  INTEGER :: ncols3d,nrows3d,nlays3d,mxrec3d

  INTEGER :: gribunit=51

  ave1hr=.FALSE.
  gribtb=141
  gribid=129

!PMTFsfc
!  gribid=157
!  gribtb=129


  varlist=''
  infiles=''

  NAMELIST /control/varlist,start_date,fcst_length,indir,pattern,id_gribdomain
  OPEN(7,file='wrf_aod2grib.ini')      
  READ(7,control)
  CLOSE(7)


  nfiles=fcst_length+1 !include zero hour fcst
  ALLOCATE(infiles(nfiles))

  j=INDEX(pattern,'*')

  DO i=1,nfiles
     WRITE(ci,'(i2.2)')i-1
     infiles(i)=TRIM(indir)//'/'//pattern(1:j-1)//TRIM(ci)//TRIM(pattern(j+1:))
  ENDDO

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "nchannels"
  dimstring(4) = "Time"

  filename=infiles(1)

  CALL netcdfdimension(filename,4,dimstring,dims)

  nx=dims(1)
  ny=dims(2)
  nchan=dims(3)
  nt=dims(4)

  IF (nt /= 1) THEN
     PRINT *,'nt must equal 1 for routine to work - stopping'
     stop
  ENDIF

  ncols3d=nx
  nrows3d=ny
  nlays3d=nlayers
  mxrec3d=nfiles

  IF(id_gribdomain /= 255) THEN
     PRINT*,'domain dimensions use defined'     
     STOP
  ENDIF

  attribute='TRUELAT1'
  CALL globalattribute_real(filename,attribute,truelat1)
  truelat1=truelat1*1.e3

  attribute='TRUELAT2'
  CALL globalattribute_real(filename,attribute,truelat2)
  truelat2=truelat2*1.e3

  attribute='DX'
  CALL globalattribute_real(filename,attribute,dx)  

  attribute='DY'
  CALL globalattribute_real(filename,attribute,dy)  

  attribute='STAND_LON'
  CALL globalattribute_real(filename,attribute,standlon)  
  standlon=standlon*1.e3

  PRINT *,start_date

  READ(start_date(1:4),'(i4)')year
  READ(start_date(3:4),'(i2)')yy
  READ(start_date(5:6),'(i2)')month
  READ(start_date(7:8),'(i2)')day
  READ(start_date(9:10),'(i2)')hour
  
  ALLOCATE(aod(nx,ny,nchan,nt),tmp(nx,ny,nt))
  
  varstringname='XLAT'
  CALL readnetcdfdata3(filename,tmp,varstringname,nx,ny,nt)
  lat1=tmp(1,1,1)*1.e3

  varstringname='XLONG'
  CALL readnetcdfdata3(filename,tmp,varstringname,nx,ny,nt)
  lon1=tmp(1,1,1)*1.e3


  imax=ncols3d
  jmax=nrows3d
  kmax=nlays3d

  ALLOCATE(lb(imax,jmax))
  lb(:,:)=.TRUE.
  ALLOCATE(work(imax,jmax))


!-----Grib file header information      

  kpds(1)=07          ! ID OF CENTER, NCEP
  kpds(2)=211         ! Generating process ID number, table A
  kpds(3)=id_gribdomain      ! Grid Identification (255 = User defined grid, defined in kgds)
  kpds(4)=128        !  Flag indicating presence of grid description section (almost always
!  included in NCEP grib files) or bit map section (BMS) (not usually 
!  included, but does happen to be included for your sst files).  It's 
!  a binary value; 128 = GDS(yes), BMS(no); 192 = GDS(yes), BMS(yes).

  kpds(6)=1
  kpds(7)=0

!      kpds(7)=10000   ! Actual value of the height or pressure level.  0 = surface.

  kpds(8)=yy	! Initial yy of analysis or forecast 
  kpds(9)=month     ! Initial mm of analysis or forecast 
  kpds(10)=day               ! Initial dd of analysis or forecast
  kpds(11)=hour       ! Initial hh of analysis or forecast
  kpds(12)=0		        ! Initial min of analysis or forecast

  kpds(13)=1      ! forecast time unit of kpds(14), table 4, 1= hour

  kpds(15)=0      ! However, if the data in this GRIB record contain, for 
! example, an average of a value from one time to another, kpds(14) will 
! hold the value of the beginning time of the average, and kpds(15) will 
! hold the ending time.
  IF(ave1hr) THEN
     kpds(16)=3
     kpds(17)=1
  ELSE
     kpds(16)=0     ! time range indicator, table 5
  ENDIF
  kpds(18)=1     ! grib version
  kpds(19)=gribtb   ! Version number of Parameter Table (table 2)
  kpds(20)=0     ! number missing from average; meaningless for this data 
  kpds(21)=year/100+1     ! Century of initial time of your data 

!  kpds(22)=3     ! Units decimal scale factor

!----kdgs start
  IF(id_gribdomain.EQ.255) THEN	   ! 

! http://www.nco.ncep.noaa.gov/pmb/docs/libs/w3lib/putgb.html

     kgds(1)=3 		! Data representation type (map projection).  0 = Lat/Lon grid. See table 6
     kgds(2)=imax		  ! Number of grid points in x-direction
     kgds(3)=jmax		  ! Number of grid points in y-direction
     kgds(4)=NINT(lat1)		 ! LA1 LAT OF ORIGIN (LOWER LEFT)
     kgds(5)=NINT(lon1)		 ! LO1 LON OF ORIGIN (LOWER LEFT
     kgds(6)=136		  ! (1001000) Resolution Flag (see Table 7 in GG).  
     kgds(7)=NINT(standlon)		 ! LOV - ORIENTATION OF GRID
     kgds(8)=NINT(dx)		 ! DX - X-DIR INCREMENT
     kgds(9)=NINT(dy)		 ! DY - Y-DIR INCREMENT      
     kgds(10)=0		 !  PROJECTION CENTER FLAG
!	 Bit 1 set to 0 if the North pole is on the projection plane.
!	 Bit 1 set to 1 if the South pole is on the projection plane.
     kgds(11)=64	    ! SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
     kgds(12)=NINT(truelat1)	      ! LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
     kgds(13)=NINT(truelat2)	      ! LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
  ENDIF

!-----read WRF files

  kpds(15)=0

  CALL baopenw(gribunit,"grib",ierr)

  IF(ierr /= 0) THEN
     PRINT*,'can not open gribfile'
     STOP 2001
  ENDIF
  
  DO ntt=1,mxrec3d

     PRINT *,ntt-1

     kpds(14)=ntt-1  ! time range. For the forecast, valid time is kpds(8-12)+kpds(14)

     kpds(7)=0

     kpds(5)=gribid        ! parameter ID
     kpds(19)=gribtb       ! table version
     kpds(22)=8-alog10(MAXVAL(work))     ! Units decimal scale factor	

     varstringname='AOD'
     filename=infiles(ntt)
     INQUIRE(file=filename,exist=exist)
     IF (exist) THEN
        CALL readnetcdfdata4(filename,aod,varstringname,nx,ny,nchan,nt)
     ELSE
        PRINT *,'Forecast file '//TRIM(filename)
        PRINT *, 'missing - stopping'
        STOP
     ENDIF

     work(1:imax,1:jmax)=aod(1:imax,1:jmax,nchan_nnr_550,nt)
     
     CALL gribit(lb,work,imax,jmax,gribunit,kpds(1:25),kgds(1:20))

  ENDDO     ! time loop

  DEALLOCATE(lb,work,aod,tmp)

  CALL baclose(gribunit,ierr)
  
  IF(ierr /= 0) THEN
     PRINT*,'can not close gribfile'
     STOP 2002
  ENDIF
  
END PROGRAM wrf_aod_ncf2grib
       
SUBROUTINE gribit(ln,ozout,im,jm,iunit,kpds,kgds)

  IMPLICIT NONE
  
  INTEGER :: mxbit,lenpds,lengds,ibm,nout,itype,ibitl,nbit,ideci,ipflag,igflag,&
       &igrid,icomp,iblen,k,iunit,ier,itot,im,jm,ibitm,ibflag,npts
  REAL :: sgdg,gmin,gmax,truelat1,truelat2

  LOGICAL, DIMENSION(im,jm) :: ln
  
  PARAMETER (mxbit=16,lenpds=28,lengds=32)
  CHARACTER*1  kbuf(30+lenpds+lengds+im*jm*(mxbit+2)/8)
  
  CHARACTER*1   pds*28
  INTEGER ibdsfl(9), igrd(im,jm),igds(18), ibmap(im,jm)
  INTEGER KPDS(25),id(25),kgds(20)
  REAL ozout(im,jm),mydata(im,jm)
  
!      save kbuf
!****************************************************************
!     PREPARE GRIB PDS
!     SET ARRAY ID VALUES TO GENERATE GRIB1 PDS.  
  
  id(1) =28 !NUMBER OF BYTES IN PRODUCT DEFINITION SECTION(PDS)
  id(2) =KPDS(19)!PARAMETER TABLE VERSION NO (2 or 129 or 130)
  id(3) =KPDS(1)  !IDENTIFICATION OF ORIGINATING CENTER
  id(4) =KPDS(2)!MODEL IDENTIFICATION (BY ORIGINATING CENTER)
  id(5) =KPDS(3)!GRID IDENTIFICATION
  id(6) =1  !IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
  id(7) =0  !IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
  id(8) =KPDS(5)
  id(9) =  KPDS(6)                !Had temporarily for 5x made into 105
  id(11) = KPDS(7)
  id(10) = KPDS(7)/256
  id(12)=KPDS(8)          !  YEAR OF CENTURY
  id(13)=KPDS(9)          !  MONTH OF YEAR
  id(14)=KPDS(10)         !  DAY OF MONTH
  id(15)=KPDS(11)         !  HOUR OF DAY
  id(16)=KPDS(12)         !  MINUTE OF HOUR
  id(17)=KPDS(13)         !  FCST TIME UNIT: 1 for h
  id(18)=KPDS(14)         !  P1 PERIOD OF TIME
  id(19)=KPDS(15)         !  P2 PERIOD OF TIME
  id(20)=KPDS(16)         !  TIME RANGE INDICATOR
  id(21)=KPDS(17)         !  NUMBER INCLUDED IN AVERAGE
  id(22)=0                   !  NUMBER MISSING FROM AVERAGES
  id(23)=KPDS(21)         !  CENTURY 
  id(24)=0                   !  RESERVED - SET TO 0
  
  sgdg = 5.0        !  MAXIMUM SIGNIFICANT DIGITS TO KEEP
!  (E.G. SGDS=3.0 KEEPS 3 SIGNIFICANT DIGITS)
!  OR BINARY PRECISION IF <0
!  (E.G. SGDS=-2.0 KEEPS FIELD TO NEAREST 1/4
!             -3.0 "                    " 1/8
!  2**SGDS PRECISION)
  ibm=0
  ibitm=0
  ibitm = im*jm
  ibmap=1
  ibm =1    !as dictated by id(7)=0
  
  nout = im*jm
  
  ibm=1
  ibmap=1
  
  CALL getbit(ibm,0,INT(sgdg),nout,ibmap,ozout,mydata,gmin,gmax,nbit)
  
  ideci=0
  
  id(25) =ideci     !   SCALING POWER OF 10
  
  itype=0
  ibitl = MIN(nbit,mxbit)
  ipflag=0
  igflag=1
  igrid=id(5)
  
  truelat1=kgds(12)
  truelat2=kgds(13)
  
  igds( 1) = 0
  igds( 2) = 255
  igds( 3) = 3
  igds( 4) = im
  igds( 5) = jm
  igds( 6) = kgds(4)
  igds( 7) = kgds(5)
  igds( 8) = 136
  igds( 9) = kgds(7)
  igds(10) = kgds(8)
  igds(11) = kgds(9)
  IF(truelat2<0)THEN
     igds(12) = 128  !for southern hemisphere
  ELSE
     igds(12) = 0
  END IF
  igds(13) = 64
  igds(14) = 0
  igds(15) = truelat2
  igds(16) = truelat1
  IF (truelat1 .LT. 0) THEN
     igds(17) = -90000
     igds(18) = 0
  ELSE
     igds(17) = 0
     igds(18) = 0
  END IF
  
  icomp=1
  ibflag=0
  iblen=nout
  DO  k = 1,9
     ibdsfl(k) = 0
  ENDDO
     
  CALL w3fi72(itype,mydata,igrd,ibitl,      &
       &            ipflag,id,pds,               &
       &            igflag,igrid,igds,icomp,     &
       &            ibflag,ibmap,iblen,          &
       &            ibdsfl,                      &
       &            npts,kbuf,itot,ier)

  CALL wryte(iunit,itot,kbuf)
  
  RETURN
END SUBROUTINE gribit

  
SUBROUTINE W3FI71 (IGRID, IGDS, IERR)
  
  INTEGER       IGRID
  INTEGER       IGDS  (*)
  
  IERR = 0
  
  DO I = 1,18
     IGDS(I) = 0
  END DO
  
END SUBROUTINE W3FI71
