PROGRAM wrf_pmsfc_ncf2grib

!----------convert pmsfc wrf output to grib format 
!
!  by MP Aug, 2015
!  from      Youhua Tang, Jun 2009
!  modified for cmaq471 by Jianping Huang, July 2011

  USE netcdf_io

  IMPLICIT NONE
  
  REAL, ALLOCATABLE :: work(:,:), pm25(:,:,:,:)

  LOGICAL, ALLOCATABLE :: lb(:,:)   ! logical mask

  INTEGER, PARAMETER :: naerospec=1, nwrf=1,nlayers=1
  CHARACTER*10 aerospec(naerospec),varlist(nwrf)

  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  INTEGER,           DIMENSION(4)                   :: dims
  CHARACTER(len=25)                                 :: varstringname,pm2_5_spec='PM2_5_DRY'
  CHARACTER*50 ::         attribute

  CHARACTER(len=80) :: start_date


  CHARACTER*250 :: infile,outfile,filename
  CHARACTER chtmp*2
  INTEGER kpds(200),kgds(200),gribid,gribtb

  LOGICAL i3dvar(nwrf),iflag,ave1hr

  INTEGER indexwrf(nwrf)    ! file start and ending in YYYYDDDHH
  INTEGER :: year,month,day,hour,yy

  INTEGER :: id_gribdomain,l,nspwrf,l2,imax,jmax,kmax,k,n,imonth,&
       &idate,ntt,ierr,maxrec1,nx,ny,nz,nt

!cmaq stuff to be found out
  INTEGER :: ncols3d,nrows3d,nlays3d,mxrec3d

  aerospec='PM2.5'
  gribid=157
  gribtb=129

  DATA varlist/nwrf*'   '/ 

  NAMELIST /control/varlist,infile,outfile,id_gribdomain,&   !   (table A)
       ave1hr
  OPEN(7,file='wrf_pmsfc2grib.ini')      
  READ(7,control)
  CLOSE(7)

  nspwrf=1

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "bottom_top"
  dimstring(4) = "Time"

  filename=infile

  CALL netcdfdimension(filename,4,dimstring,dims)

  nx=dims(1)
  ny=dims(2)
  nz=dims(3)
  nt=dims(4)

  ncols3d=nx
  nrows3d=ny
  nlays3d=nz
  mxrec3d=nt

  IF(id_gribdomain.EQ.255.AND.&
       (ncols3d.NE.440.OR.nrows3d.NE.284)) THEN
     PRINT*,'domain dimension does not match'
     STOP
  ENDIF

  attribute='SIMULATION_START_DATE'

  CALL globalattribute_text(filename,attribute,start_date)

  READ(start_date(1:4),'(i4)')year
  READ(start_date(3:4),'(i2)')yy
  READ(start_date(6:7),'(i2)')month
  READ(start_date(9:10),'(i2)')day
  READ(start_date(12:13),'(i2)')hour
  
  varstringname=pm2_5_spec

  ALLOCATE(pm25(nx,ny,nz,nt))
  
  CALL readnetcdfdata4(filename,pm25,varstringname,nx,ny,nz,nt)

  imax=ncols3d
  jmax=nrows3d
  kmax=nlays3d

  maxrec1=mxrec3d

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
!      kpds(19)=129   ! Version number of Parameter Table (table 2)
  kpds(20)=0     ! number missing from average; meaningless for this data 
  kpds(21)=year/100+1     ! Century of initial time of your data 

!      kpds(22)=6     ! Units decimal scale factor

!----kdgs start
  IF(id_gribdomain.EQ.255) THEN	   ! ravan's conus

! http://www.nco.ncep.noaa.gov/pmb/docs/libs/w3lib/putgb.html

     kgds(1)=3 		! Data representation type (map projection).  0 = Lat/Lon grid. See table 6
     kgds(2)=imax		  ! Number of grid points in x-direction
     kgds(3)=jmax		  ! Number of grid points in y-direction
     kgds(4)=21123		 ! LA1 LAT OF ORIGIN (LOWER LEFT)
     kgds(5)=-122321		 ! LO1 LON OF ORIGIN (LOWER LEFT
     kgds(6)=136		  ! (1001000) Resolution Flag (see Table 7 in GG).  
     kgds(7)=-97000		 ! LOV - ORIENTATION OF GRID
     kgds(8)=12000		 ! DX - X-DIR INCREMENT
     kgds(9)=12000		 ! DY - Y-DIR INCREMENT      
     kgds(10)=0		 !  PROJECTION CENTER FLAG
!	 Bit 1 set to 0 if the North pole is on the projection plane.
!	 Bit 1 set to 1 if the South pole is on the projection plane.
     kgds(11)=64	    ! SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
     kgds(12)=33000	      ! LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
     kgds(13)=45000	      ! LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER
  ENDIF

!-----read WRF files

  DO ntt=1,maxrec1

     kpds(14)=ntt-1  ! time range. For the forecast, valid time is kpds(8-12)+kpds(14)

     IF(ave1hr) THEN
        kpds(15)=ntt
        WRITE(chtmp,'(i2.2)')ntt
     ELSE
        WRITE(chtmp,'(i2.2)')ntt-1
     ENDIF

     CALL baopenw(51,TRIM(outfile)//chtmp,ierr)
     IF(ierr.NE.0) THEN
        PRINT*,'can not open ',TRIM(outfile)//chtmp
        STOP 2001
     ENDIF

     kpds(7)=0

     work(1:imax,1:jmax)=pm25(1:imax,1:jmax,1,ntt)

     kpds(5)=gribid        ! parameter ID
     kpds(19)=gribtb       ! table version
     kpds(22)=8-alog10(MAXVAL(work))     ! Units decimal scale factor	
     
     CALL gribit(lb,work,imax,jmax,51,kpds(1:25),kgds(1:20))

     CALL baclose(51,ierr)

  ENDDO     ! time loop

  DEALLOCATE(lb,work,pm25)

END PROGRAM wrf_pmsfc_ncf2grib
       

      SUBROUTINE gribit(ln,ozout,im,jm,iunit,kpds,kgds)

        implicit none

        INTEGER :: mxbit,lenpds,lengds,ibm,nout,itype,ibitl,nbit,ideci,ipflag,igflag,&
             &igrid,icomp,iblen,k,iunit,ier,itot,im,jm,ibitm,ibflag,npts
        REAL :: sgdg,gmin,gmax,truelat1,truelat2

        LOGICAL, DIMENSION(im,jm) :: ln

      parameter (mxbit=16,lenpds=28,lengds=32)
      character*1  kbuf(30+lenpds+lengds+im*jm*(mxbit+2)/8)

      character*1  iflag*1, pds*28
      integer ibdsfl(9), igrd(im,jm),igds(18), ibmap(im,jm)
      integer KPDS(25),id(25),kgds(20)
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
      ibitl = min(nbit,mxbit)
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
      if(truelat2<0)then
         igds(12) = 128  !for southern hemisphere
      else
         igds(12) = 0
      end if
      igds(13) = 64
      igds(14) = 0
      igds(15) = truelat2
      igds(16) = truelat1
      if (truelat1 .lt. 0) then
         igds(17) = -90000
         igds(18) = 0
      else
         igds(17) = 0
         igds(18) = 0
      end if

      icomp=1
      ibflag=0
      iblen=nout
      do 30 k = 1,9
         ibdsfl(k) = 0
 30   continue

      call w3fi72(itype,mydata,igrd,ibitl,      &
     &            ipflag,id,pds,               &
     &            igflag,igrid,igds,icomp,     &
     &            ibflag,ibmap,iblen,          &
     &            ibdsfl,                      &
     &            npts,kbuf,itot,ier)

      call wryte(iunit,itot,kbuf)

      return
      end


      SUBROUTINE W3FI71 (IGRID, IGDS, IERR)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    W3FI71      MAKE ARRAY USED BY GRIB PACKER FOR GDS
!   PRGMMR: R.E.JONES        ORG: W/NMC42    DATE: 93-03-26
!
! ABSTRACT: W3FI71 MAKES A 18, 37, 55, 64, OR 91 WORD INTEGER ARRAY
!     USED BY W3FI72 GRIB PACKER TO MAKE THE GRID DESCRIPTION SECTION
!     (GDS) - SECTION 2.
!
! PROGRAM HISTORY LOG:
!   92-02-21  R.E.JONES
!   92-07-01  M. FARLEY    ADDED REMARKS FOR 'IGDS' ARRAY ELEMENTS.
!			   ADDED LAMBERT CONFORMAL GRIDS AND ENLARGED
!			   IDGS ARRAY FROM 14 TO 18 WORDS.
!   92-10-03  R.E.JONES    ADDED CORRECTIONS TO AWIPS GRIB TABLES
!   92-10-16  R.E.JONES    ADD GAUSSIAN GRID 126 TO TABLES
!   92-10-18  R.E.JONES    CORRECTIONS TO LAMBERT CONFORMAL TABLES
!			   AND OTHER TABLES
!   92-10-19  R.E.JONES    ADD GAUSSIAN GRID  98 TO TABLES
!   93-01-25  R.E.JONES    ADD ON84 GRIDS 87, 106, 107 TO TABLES
!   93-03-10  R.E.JONES    ADD ON84 GRIDS 1, 55, 56 TO TABLES
!   93-03-26  R.E.JONES    ADD GRIB GRIDS 2, 3 TO TABLES
!   93-03-29  R.E.JONES    ADD SAVE STATEMENT
!   93-06-15  R.E.JONES    ADD GRIB GRIDS 37 TO 44 TO TABLES
!   93-09-29  R.E.JONES    GAUSSIAN GRID DOCUMENT NOT CORRECT,
!			   W3FI74 WILL BE CHANGED TO AGREE WITH
!			   IT. GAUSSIAN GRID 98 TABLE HAS WRONG
!			   VALUE.
!   93-10-12  R.E.JONES    CHANGES FOR ON388 REV. OCT 8,1993 FOR
!			   GRID 204, 208.
!   93-10-13  R.E.JONES    CORRECTION FOR GRIDS 37-44, BYTES 7-8,
!			   24-25 SET TO ALL BITS 1 FOR MISSING.
!   93-11-23  R.E.JONES    ADD GRIDS 90-93 FOR ETA MODEL
!			   ADD GRID 4 FOR 720*361 .5 DEG. GRID
!   94-04-12  R.E.JONES    CORRECTION FOR GRID 28
!   94-06-01  R.E.JONES    ADD GRID 45, 288*145 1.25 DEG. GRID
!   94-06-22  R.E.JONES    ADD GRIDS 94, 95 FOR ETA MODEL
!   95-04-11  R.E.JONES    ADD GRIDS 96, 97 FOR ETA MODEL
!   95-05-19  R.E.JONES    ADD FROM 20 KM ETA MODEL AWIPS GRID 215
!   95-10-19  R.E.JONES    ADD FROM 20 KM ETA MODEL ALASKA GRID 216
!   95-10-31  IREDELL	   REMOVED SAVES AND PRINTS
!   96-05-08  IREDELL	   CORRECT FIRST LATITUDE FOR GRIDS 27 AND 28
!   96-07-02  R.E.JONES    ADD FROM 10 KM ETA MODEL OLYMPIC GRID 218
!   96-07-02  R.E.JONES    ADD 196 FOR ETA MODEL
!   96-08-15  R.E.JONES    ADD O.N. 84 GRID 8 AND 53 AS GRIB GRID 8
! 			   AND 53
!   96-11-29  R.E.JONES    CORRECTION TO TABLES FOR GRID 21-26, 61-64
!   97-01-31  IREDELL	   CORRECT FIRST LATITUDE FOR GRID 30
!   97-10-20  IREDELL	   CORRECT LAST LONGITUDE FOR GRID 98
!   98-07-07  Gilbert	   Add grids 217 and 219 through 235
!   98-09-21  BALDWIN	   ADD GRIDS 190, 192 FOR ETA MODEL
!   99-01-20  BALDWIN	   ADD GRIDS 236, 237
!   99-08-18  IREDELL	   ADD GRID 170
!   01-03-08  ROGERS	   CHANGED ETA GRIDS 90-97, ADDED ETA GRIDS
!			   194, 198. ADDED AWIPS GRIDS 241,242,243,
!			   245, 246, 247, 248, AND 250
!   01-03-19  VUONG	   ADDED AWIPS GRIDS 238,239,240, AND 244
!   01-04-02  VUONG	   CORRECT LAST LONGITUDE FOR GRID 225
!   01-05-03  ROGERS	   ADDED GRID 249
!   01-10-10  ROGERS	   REDEFINED 218 FOR 12-KM ETA
!			   REDEFINED GRID 192 FOR NEW 32-KM ETA GRID
!   02-03-27  VUONG	   ADDED RSAS GRID 88 AND AWIPS GRIDS 251 AND 252
!    02-08-06  ROGERS	    REDEFINED GRIDS 90-93,97,194,245-250 FOR THE
!			    8KM HI-RES-WINDOW MODEL AND ADD AWIPS GRID 253
!  2003-06-30  GILBERT      ADDED GRIDS 145 and 146 for CMAQ
!			    and GRID 175 for AWIPS over GUAM.
!  2003-07-08  VUONG	    CORRECTED LATITUDE FOR GRID 253 AND 170, ADD GRID
!  2005-02-08  PLee	    GRIDS 138, 147, 148 for CMAQ, 255 CONUS
!			    110, 127, 171 AND 172
!  2009-07-08  PLee	    GRIDS 139, 140 for CMAQ, HI and AK
!
!  USAGE:    CALL W3FI71 (IGRID, IGDS, IERR)
!    INPUT ARGUMENT LIST:
!      IGRID	   - GRIB GRID NUMBER, OR OFFICE NOTE 84 GRID NUMBER
!
!    OUTPUT ARGUMENT LIST:
!      IGDS	 - 18, 37, 55, 64, OR 91 WORD INTEGER ARRAY WITH
!		   INFORMATION TO MAKE A GRIB GRID DESCRIPTION SECTION.
!      IERR	  - 0  CORRECT EXIT
!		    1  GRID TYPE IN IGRID IS NOT IN TABLE
!
!  REMARKS:
!     1) OFFICE NOTE GRID TYPE 26 IS 6 IN GRIB, 26 IS AN
!	 INTERNATIONAL EXCHANGE GRID.
!
!     2) VALUES RETURNED IN 18, 37, 55, 64, OR 91 WORD INTEGER ARRAY
!	  IGDS VARY DEPENDING ON GRID REPRESENTATION TYPE.
!
!	 LAT/LON GRID:
!	     IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	     IGDS( 2) = PV, PL OR 255
!	     IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!	     IGDS( 4) = NO. OF POINTS ALONG A LATITUDE
!	     IGDS( 5) = NO. OF POINTS ALONG A LONGITUDE MERIDIAN
!	     IGDS( 6) = LATITUDE OF ORIGIN (SOUTH - IVE)
!	     IGDS( 7) = LONGITUDE OF ORIGIN (WEST -IVE)
!	     IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!	     IGDS( 9) = LATITUDE OF EXTREME POINT (SOUTH - IVE)
!	     IGDS(10) = LONGITUDE OF EXTREME POINT (WEST - IVE)
!	     IGDS(11) = LATITUDE INCREMENT
!	     IGDS(12) = LONGITUDE INCREMENT
!	     IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	     IGDS(14) = ... THROUGH ...
!	     IGDS(18) =   ... NOT USED FOR THIS GRID
!	     IGDS(19) - IGDS(91) FOR GRIDS 37-44, NUMBER OF POINTS
!			IN EACH OF 73 ROWS.
!
!	 GAUSSIAN GRID:
!	     IGDS( 1) = ... THROUGH ...
!	     IGDS(10) =   ... SAME AS LAT/LON GRID
!	     IGDS(11) = NUMBER OF LATITUDE LINES BETWEEN A POLE
!			AND THE EQUATOR
!	     IGDS(12) = LONGITUDE INCREMENT
!	     IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	     IGDS(14) = ... THROUGH ...
!	     IGDS(18) =   ... NOT USED FOR THIS GRID
!
!	 SPHERICAL HARMONICS:
!	     IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	     IGDS( 2) = PV, PL OR 255
!	     IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!	     IGDS( 4) = J - PENTAGONAL RESOLUTION PARAMETER
!	     IGDS( 5) = K - PENTAGONAL RESOLUTION PARAMETER
!	     IGDS( 6) = M - PENTAGONAL RESOLUTION PARAMETER
!	    IGDS( 7) = REPRESENTATION TYPE (CODE TABLE 9)
!	    IGDS( 8) = REPRESENTATION MODE (CODE TABLE 10)
!	    IGDS( 9) = ... THROUGH ...
!	    IGDS(18) =   ... NOT USED FOR THIS GRID
!
!	POLAR STEREOGRAPHIC:
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!	    IGDS( 4) = NO. OF POINTS ALONG X-AXIS
!	    IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
!	    IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
!	    IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
!	    IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!	    IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
!	    IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
!	    IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
!	    IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
!					       1=SOUTH POLE ON PLANE,
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = ... THROUGH ...
!	    IGDS(18) =   .. NOT USED FOR THIS GRID
!
!	MERCATOR:
!	    IGDS( 1) = ... THROUGH ...
!	    IGDS(12) =   ... SAME AS LAT/LON GRID
!	    IGDS(13) = LATITUDE AT WHICH PROJECTION CYLINDER
!			 INTERSECTS EARTH
!	    IGDS(14) = SCANNING MODE FLAGS
!	    IGDS(15) = ... THROUGH ...
!	    IGDS(18) =   .. NOT USED FOR THIS GRID
!
!	LAMBERT CONFORMAL:
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
!	    IGDS( 4) = NO. OF POINTS ALONG X-AXIS
!	    IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
!	    IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
!	    IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
!	    IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
!	    IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
!	    IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
!	    IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
!	    IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
!					       1=SOUTH POLE ON PLANE,
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = NOT USED
!	    IGDS(15) = FIRST LATITUDE FROM THE POLE AT WHICH THE
!		       SECANT CONE CUTS THE SPERICAL EARTH
!	    IGDS(16) = SECOND LATITUDE ...
!	    IGDS(17) = LATITUDE OF SOUTH POLE (MILLIDEGREES)
!	    IGDS(18) = LONGITUDE OF SOUTH POLE (MILLIDEGREES)
!
!	ARAKAWA SEMI-STAGGERED E-GRID ON ROTATED LAT/LON GRID
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [201]
!	    IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
!			     INCLUDED ON GRID
!	    IGDS( 5) = NJ  - DUMMY SECOND DIMENSION; SET=1
!	    IGDS( 6) = LA1 - LATITUDE  OF FIRST GRID POINT
!	    IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!	    IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!	    IGDS( 9) = LA2 - NUMBER OF MASS POINTS ALONG
!			     SOUTHERNMOST ROW OF GRID
!	    IGDS(10) = LO2 - NUMBER OF ROWS IN EACH COLUMN
!	    IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!	    IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = ... THROUGH ...
!	    IGDS(18) = ... NOT USED FOR THIS GRID (SET TO ZERO)
!
!	ARAKAWA FILLED E-GRID ON ROTATED LAT/LON GRID
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [202]
!	    IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
!			     INCLUDED ON GRID
!	    IGDS( 5) = NJ  - DUMMY SECOND DIMENTION; SET=1
!	    IGDS( 6) = LA1 - LATITUDE LATITUDE OF FIRST GRID POINT
!	    IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!	    IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!	    IGDS( 9) = LA2 - NUMBER OF (ZONAL) POINTS IN EACH ROW
!	    IGDS(10) = LO2 - NUMBER OF (MERIDIONAL) POINTS IN EACH
!			     COLUMN
!	    IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!	    IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = ... THROUGH ...
!	    IGDS(18) = ... NOT USED FOR THIS GRID
!
!	ARAKAWA STAGGERED E-GRID ON ROTATED LAT/LON GRID
!	    IGDS( 1) = NUMBER OF VERTICAL COORDINATES
!	    IGDS( 2) = PV, PL OR 255
!	    IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [203]
!	    IGDS( 4) = NI  - NUMBER OF DATA POINTS IN EACH ROW
!	    IGDS( 5) = NJ  - NUMBER OF ROWS
!	    IGDS( 6) = LA1 - LATITUDE OF FIRST GRID POINT
!	    IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
!	    IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
!	    IGDS( 9) = LA2 - CENTRAL LATITUDE
!	    IGDS(10) = LO2 - CENTRAL LONGTITUDE
!	    IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
!	    IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
!	    IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
!	    IGDS(14) = ... THROUGH ...
!	    IGDS(18) = ... NOT USED FOR THIS GRID
!
!   SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM SP
!


      INTEGER       IGRID
      INTEGER       IGDS  (*)
      INTEGER       GRD1  (18)
      INTEGER       GRD2  (18)
      INTEGER       GRD3  (18)
      INTEGER       GRD4  (18)
      INTEGER       GRD5  (18)
      INTEGER       GRD6  (18)
      INTEGER       GRD8  (18)
      INTEGER       GRD21 (55)
      INTEGER       GRD22 (55)
      INTEGER       GRD23 (55)
      INTEGER       GRD24 (55)
      INTEGER       GRD25 (37)
      INTEGER       GRD26 (37)
      INTEGER       GRD27 (18)
      INTEGER       GRD28 (18)
      INTEGER       GRD29 (18)
      INTEGER       GRD30 (18)
      INTEGER       GRD33 (18)
      INTEGER       GRD34 (18)
      INTEGER       GRD37 (91)
      INTEGER       GRD38 (91)
      INTEGER       GRD39 (91)
      INTEGER       GRD40 (91)
      INTEGER       GRD41 (91)
      INTEGER       GRD42 (91)
      INTEGER       GRD43 (91)
      INTEGER       GRD44 (91)
      INTEGER       GRD45 (18)
      INTEGER       GRD53 (18)
      INTEGER       GRD55 (18)
      INTEGER       GRD56 (18)
      INTEGER       GRD61 (64)
      INTEGER       GRD62 (64)
      INTEGER       GRD63 (64)
      INTEGER       GRD64 (64)
      INTEGER       GRD85 (18)
      INTEGER       GRD86 (18)
      INTEGER       GRD87 (18)
      INTEGER       GRD88 (18)
      INTEGER       GRD90 (18)
      INTEGER       GRD91 (18)
      INTEGER       GRD92 (18)
      INTEGER       GRD93 (18)
      INTEGER       GRD94 (18)
      INTEGER       GRD95 (18)
      INTEGER       GRD96 (18)
      INTEGER       GRD97 (18)
      INTEGER       GRD98 (18)
      INTEGER       GRD100(18)
      INTEGER       GRD101(18)
      INTEGER       GRD103(18)
      INTEGER       GRD104(18)
      INTEGER       GRD105(18)
      INTEGER       GRD106(18)
      INTEGER       GRD107(18)
      INTEGER       GRD110(18)
      INTEGER       GRD126(18)
      INTEGER       GRD127(18)
!***TLO 11 Mar 04 and PLEE July 8 2009 ***start
      INTEGER       GRD138(18)
      INTEGER       GRD139(18)
      INTEGER       GRD140(18)
      INTEGER       GRD255(18)
!PL*PLee 08 Feb 2005 ***Comment out starts
!PL   INTEGER       GRD139(18)
!***TLO 11 Mar 04 ***end
!***TLO 08 Mar 04 ***start
!PL   INTEGER       GRD140(18)
!PL   INTEGER       GRD141(18)
!PL   INTEGER       GRD142(18)
!PL*PLee 08 Feb 2005 ***renaming Grid 142 to 147 starts
      INTEGER       GRD147(18)
!PL*PLee 08 Feb 2005 ***renaming Grid 142 to 147 ends
!PL   INTEGER       GRD144(18)
!***TLO 08 Mar 04 ***end
      INTEGER       GRD145(18)
      INTEGER       GRD146(18)
!***TLO 08 Mar 04 ***start
!PL   INTEGER       GRD148(18)
!PL   INTEGER       GRD149(18)
!PL   INTEGER       GRD150(18)
!PL*PLee 08 Feb 2005 ***renaming Grid 150 to 148 starts
      INTEGER       GRD148(18)
!PL*PLee 08 Feb 2005 ***renaming Grid 150 to 148 ends
!***TLO 08 Mar 04 ***end
      INTEGER       GRD170(18)
      INTEGER       GRD171(18)
      INTEGER       GRD172(18)
      INTEGER       GRD175(18)
      INTEGER       GRD190(18)
      INTEGER       GRD192(18)
      INTEGER       GRD194(18)
      INTEGER       GRD196(18)
      INTEGER       GRD198(18)
      INTEGER       GRD201(18)
      INTEGER       GRD202(18)
      INTEGER       GRD203(18)
      INTEGER       GRD204(18)
      INTEGER       GRD205(18)
      INTEGER       GRD206(18)
      INTEGER       GRD207(18)
      INTEGER       GRD208(18)
      INTEGER       GRD209(18)
      INTEGER       GRD210(18)
      INTEGER       GRD211(18)
      INTEGER       GRD212(18)
      INTEGER       GRD213(18)
      INTEGER       GRD214(18)
      INTEGER       GRD215(18)
      INTEGER       GRD216(18)
      INTEGER       GRD217(18)
      INTEGER       GRD218(18)
      INTEGER       GRD219(18)
      INTEGER       GRD220(18)
      INTEGER       GRD221(18)
      INTEGER       GRD222(18)
      INTEGER       GRD223(18)
      INTEGER       GRD224(18)
      INTEGER       GRD225(18)
      INTEGER       GRD226(18)
      INTEGER       GRD227(18)
      INTEGER       GRD228(18)
      INTEGER       GRD229(18)
      INTEGER       GRD230(18)
      INTEGER       GRD231(18)
      INTEGER       GRD232(18)
      INTEGER       GRD233(18)
      INTEGER       GRD234(18)
      INTEGER       GRD235(18)
      INTEGER       GRD236(18)
      INTEGER       GRD237(18)
      INTEGER       GRD238(18)
      INTEGER       GRD239(18)
      INTEGER       GRD240(18)
      INTEGER       GRD241(18)
      INTEGER       GRD242(18)
      INTEGER       GRD243(18)
      INTEGER       GRD244(18)
      INTEGER       GRD245(18)
      INTEGER       GRD246(18)
      INTEGER       GRD247(18)
      INTEGER       GRD248(18)
      INTEGER       GRD249(18)
      INTEGER       GRD250(18)
      INTEGER       GRD251(18)
      INTEGER       GRD252(18)
      INTEGER       GRD253(18)

      DATA  GRD1  / 0, 255, 1,  73, 23, -48090,       0, 128,   48090,  &
     &       0, 513669,513669, 22500, 64, 0, 0, 0, 0/
      DATA  GRD2  / 0, 255, 0, 144, 73,  90000,       0, 128,  -90000,  &
     &   -2500,   2500, 2500,  0, 0, 0, 0, 0, 0/
      DATA  GRD3  / 0, 255, 0, 360,181,  90000,       0, 128,  -90000,  &
     &   -1000,   1000, 1000,  0, 0, 0, 0, 0, 0/
      DATA  GRD4  / 0, 255, 0, 720,361,  90000,       0, 128,  -90000,  &
     &    -500,    500,  500,  0, 0, 0, 0, 0, 0/
      DATA  GRD5  / 0, 255, 5,  53, 57,   7647, -133443,   8, -105000,  &
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD6  / 0, 255, 5,  53, 45,   7647, -133443,   8, -105000,  &
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD8  / 0, 255, 1, 116, 44, -48670,    3104, 128,   61050,  &
     &       0, 318830, 318830, 22500, 64, 0, 0, 0, 0/
      DATA  GRD21 / 0,  33, 0,65535,37,      0,       0, 128,   90000,  &
     &  180000,   2500, 5000, 64, 0, 0, 0, 0, 0,                        &
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,      &
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,      &
     & 37, 37, 37, 37, 37, 37,  1/
      DATA  GRD22 / 0,  33, 0,65535,37,      0, -180000, 128,   90000,  &
     &       0,   2500, 5000, 64, 0, 0, 0, 0, 0,                        &
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,      &
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,      &
     & 37, 37, 37, 37, 37, 37,  1/
      DATA  GRD23 / 0,  33, 0,65535, 37, -90000,       0, 128,       0, &
     &  180000,   2500, 5000, 64, 0, 0, 0, 0, 0,			&
     &  1, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,	&
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,	&
     & 37, 37, 37, 37, 37, 37, 37/
      DATA  GRD24 / 0,  33, 0,65535, 37, -90000, -180000, 128,       0, &
     &       0,   2500, 5000, 64, 0, 0, 0, 0, 0,			&
     &  1, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,	&
     & 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,	&
     & 37, 37, 37, 37, 37, 37, 37/
      DATA  GRD25 / 0,  33, 0,65535, 19,      0,       0, 128,   90000, &
     &  355000,   5000, 5000, 64, 0, 0, 0, 0, 0,			&
     & 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,	&
     & 72, 72, 72,  1/							
      DATA  GRD26 / 0,  33, 0,65535, 19, -90000,       0, 128,       0, &
     &  355000,   5000, 5000, 64, 0, 0, 0, 0, 0,			&
     &  1, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,	&
     & 72, 72, 72, 72/							
      DATA  GRD27 / 0, 255, 5,  65, 65, -20826, -125000,   8,  -80000,  &
     &  381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD28 / 0, 255, 5,  65, 65,  20826,  145000,   8,  100000,  &
     &  381000, 381000,128, 64, 0, 0, 0, 0, 0/
      DATA  GRD29 / 0, 255, 0, 145, 37,      0,       0, 128,   90000,  &
     &  360000,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD30 / 0, 255, 0, 145, 37,  -90000,      0, 128,       0,  &
     &  360000,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD33 / 0, 255, 0, 181, 46,      0,       0, 128,   90000,  &
     &  360000,   2000, 2000, 64, 0, 0, 0, 0, 0/
      DATA  GRD34 / 0, 255, 0, 181, 46, -90000,       0, 128,       0,  &
     &  360000,   2000, 2000, 64, 0, 0, 0, 0, 0/
      DATA  GRD37 / 0,  33, 0,65535,73,      0,  -30000, 128,   90000,  &
     &   60000,  1250,65535, 64, 0, 0, 0, 0, 0,                         &
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,      &  
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,	&
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,	&
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,	&
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD38 / 0,  33, 0,65535,73,      0,   60000, 128,   90000,  &
     &  150000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,	&
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,	&
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,      &
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,      &
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD39 / 0,  33, 0,65535,73,      0,  150000, 128,   90000,  &
     & -120000,  1250,65535, 64, 0, 0, 0, 0, 0,                         &
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70, 	&
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,	&
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,	&
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,      &
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD40 / 0,  33, 0,65535,73,       0, -120000, 128,   90000, &
     &  -30000,  1250,65535, 64, 0, 0, 0, 0, 0,                         &
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,	&
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,	&
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,	&
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,      &
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
      DATA  GRD41 / 0,  33, 0,65535,73, -90000,  -30000, 128,       0,  &
     &   60000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,	&
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,	&
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,      &
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,      &
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD42 / 0,  33, 0,65535,73, -90000,   60000, 128,       0,  &
     &  150000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,	&
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,	&
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,      &
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,      &
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD43 / 0,  33, 0,65535,73, -90000,  150000, 128,       0,  &
     & -120000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,	&
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,	&
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,      &
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,      & 
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD44 / 0,  33, 0,65535,73, -90000, -120000, 128,       0,  &
     &  -30000,  1250,65535, 64, 0, 0, 0, 0, 0, 			&
     &  2,  3,  5,  6,  8,  9, 11, 12, 14, 16, 17, 19, 20, 22, 23,	&
     & 25, 26, 28, 29, 30, 32, 33, 35, 36, 38, 39, 40, 42, 43, 44,	&
     & 45, 47, 48, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 60,      &
     & 61, 62, 63, 64, 65, 65, 66, 67, 67, 68, 69, 69, 70, 70, 71,      &
     & 71, 71, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73/
      DATA  GRD45 / 0, 255, 0, 288,145,  90000,       0, 128,  -90000,  &
     &   -1250,   1250, 1250,  0, 0, 0, 0, 0, 0/
      DATA  GRD53 / 0, 255, 1, 117, 51, -61050,       0, 128,   61050,  &
     &       0,  318830, 318830, 22500, 64, 0, 0, 0, 0/
      DATA  GRD55 / 0, 255, 5,  87, 71, -10947, -154289,   8, -105000,  &
     &  254000, 254000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD56 / 0, 255, 5,  87, 71,   7647, -133443,   8, -105000,  &
     &  127000, 127000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD61 / 0,  33, 0,65535, 46,      0,       0, 128,   90000, &
     &  180000,   2000, 2000, 64, 0, 0, 0, 0, 0,                        & 
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     &  1/
      DATA  GRD62 / 0,  33, 0,65535, 46,      0, -180000, 128,   90000, &
     &       0,   2000, 2000, 64, 0, 0, 0, 0, 0,                        &
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     &  1/
      DATA  GRD63 / 0,  33, 0,65535, 46,      0,  -90000, 128,       0, &
     &  180000,   2000, 2000, 64, 0, 0, 0, 0, 0,			&
     &  1, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,      &
     & 91/
      DATA  GRD64 / 0,  33, 0,65535, 46, -90000, -180000, 128,       0, &
     &       0,   2000, 2000, 64, 0, 0, 0, 0, 0,			&
     &  1, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,	&
     & 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,      &
     & 91/
      DATA  GRD85 / 0, 255, 0, 360, 90,    500,     500, 128,   89500,  &
     &  359500,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD86 / 0, 255, 0, 360, 90, -89500,     500, 128,    -500,  &
     &  359500,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD87 / 0, 255, 5,  81, 62,  22876, -120491,   8, -105000,  &
     &   68153,  68153, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD88 / 0, 255, 5, 580,548,  10000, -128000,   8, -105000,  &
     &   15000,  15000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD90 / 0, 255,203,223,501,  23060,  -92570, 136,   37000,  &
     & -80000,     53,53,64, 0, 0, 0, 0, 0/
      DATA  GRD91 / 0, 255,203,223,501,  23060, -110570, 136,   37000,  &
     & -98000,     53,53,64, 0, 0, 0, 0, 0/
      DATA  GRD92 / 0, 255,203,223,501,  25986, -127871, 136,   40000,  &
     & -115000,    53,53,64, 0, 0, 0, 0, 0/
      DATA  GRD93 / 0, 255,203,223,501,  44232, -169996, 136,   63000,  &
     & -150000,    67,66,64, 0, 0, 0, 0, 0/
      DATA  GRD94 / 0, 255,203,345,569,  -3441, -148799, 136,   50000,  &
     & -111000,    154,141,64, 0, 0, 0, 0, 0/
      DATA  GRD95 / 0, 255,203,146,247,  35222, -131741, 136,   44000,  &
     & -240000,     67, 66,64, 0, 0, 0, 0, 0/
      DATA  GRD96 / 0, 255,203,606,1067, -3441, -148799, 136,   50000,  &
     & -111000,     88,75,64, 0, 0, 0, 0, 0/
      DATA  GRD97 / 0, 255,203, 89,143,  14451,  -71347, 136,   18000,  &
     &  -66500,     53, 53,64, 0, 0, 0, 0, 0/
      DATA  GRD98 / 0, 255, 4, 192, 94,  88542,       0, 128,  -88542,  &
     &    -1875, 47,1875, 0, 0, 0, 0, 0, 0/
      DATA  GRD100/ 0, 255, 5,  83, 83,  17108, -129296,   8, -105000,  &
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD101/ 0, 255, 5, 113, 91,  10528, -137146,   8, -105000,	&
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD103/ 0, 255, 5,  65, 56,  22405, -121352,   8, -105000,	&
     &   91452,  91452, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD104/ 0, 255, 5, 147,110,   -268, -139475,   8, -105000,	&
     &   90755,  90755, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD105/ 0, 255, 5,  83, 83,  17529, -129296,   8, -105000,	&
     &   90755,  90755, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD106/ 0, 255, 5, 165,117,  17533, -129296,   8, -105000,	&
     &   45373,  45373, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD107/ 0, 255, 5, 120, 92,  23438, -120168,   8, -105000,	&
     &   45373,  45373, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD110/ 0, 255, 0, 464,224,  25063, -124938, 128,   52938,  &
     & -67063,    125, 125, 64, 0, 0, 0, 0, 0/
      DATA  GRD126/ 0, 255, 4, 384,190,  89277,       0, 128,  -89277,  &
     &    -938,    95, 938, 0, 0, 0, 0, 0, 0/
      DATA  GRD127/ 0, 255, 4, 768,384,  89642,       0, 128,  -89642,  &
     &    -469,   192, 469, 0, 0, 0, 0, 0, 0/
!***TLO 11 Mar 04 ***start
      DATA  GRD138/ 0, 255, 3, 468,288,  21017,-123282,   8,   -97000,  &
     &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!!    DATA  GRD139/ 0, 255, 3, 64,44,  18144, -161076,  8,   -157500,   &
!!   &  12000, 12000, 0, 64, 0, 19000, 21000, 0, 0/
      DATA  GRD139/ 0, 255, 3, 80,52,  17721, -161973,  8,   -157500,   &
     &  12000, 12000, 0, 64, 0, 19000, 21000, 0, 0/
!!   DATA  GRD140/ 0, 255, 3, 198,162,  53033, -166459,  8,   -148600, &
!!   &  12000, 12000, 0, 64, 0, 57000, 63000, 0, 0/
      DATA  GRD140/ 0, 255, 3, 199,163, 53020, -166477,  8,  -148600,  &
     &  12000, 12000, 0, 64, 0, 57000, 63000, 0, 0/
!CER   DATA  GRD255/ 0, 255,203,420, 769, 13194, -143539, 136,   50000,	&
!CER  & -111000,     88,75,64, 0, 0, 0, 0, 0/
      DATA  GRD255/ 0, 255,203,419, 768, 13194, -143540, 136,   50000,	&
     & -111000,     88,75,64, 0, 0, 0, 0, 0/
!PL*PLee 08 Feb 2005 ***Comment out starts				
!PL   DATA  GRD139/ 0, 255, 3, 469,289,  20953,-123319,   8,   -97000,  &
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/			
!***TLO 11 Mar 04 ***end
!***TLO 08 Mar 04 ***start						
!PL   DATA  GRD140/ 0, 255, 3, 270,261,  24486,-101108,   8,   -97000,  &
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/			
!PL   DATA  GRD141/ 0, 255, 3, 271,262,  24431,-101163,   8,   -97000,  &
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!PL   DATA  GRD142/ 0, 255, 3, 268,259,  24595,-100998,   8,   -97000,	&
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!***PLee 08 Feb 2005 ***renaming Grid 142 to 147 starts 		
      DATA  GRD147/ 0, 255, 3, 268,259,  24595,-100998,   8,   -97000,  &
     &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/			
!***PLee 08 Feb 2005 ***renaming Grid 142 to 147 ends
!PL   DATA  GRD144/ 0, 255, 3, 168,144,  32233, -90104,   8,   -79500,	&
!PL  &  12000, 12000, 0, 64, 0, 36000, 46000, 0, 0/
!***PLee 08 Feb 2005 ***Comment out ends				
!***TLO 08 Mar 04 ***end
!***TLO 12 Dec 02 ***start						
      DATA  GRD145/ 0, 255, 3, 169,145,  32174, -90159,   8,   -79500,  &
     &  12000, 12000, 0, 64, 0, 36000, 46000, 0, 0/
!***TLO 12 Dec 02 ***end
!***TLO 08 Mar 04 ***start
      DATA  GRD146/ 0, 255, 3, 166,142,  32353, -89994,   8,   -79500,  &
     &  12000, 12000, 0, 64, 0, 36000, 46000, 0, 0/
!***PLee 08 Feb 2005 ***Comment out starts
!PL   DATA  GRD148/ 0, 255, 3, 444,267,  21694,-120707,   8,   -97000, 
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!PL   DATA  GRD149/ 0, 255, 3, 445,268,  21630,-120747,   8,   -97000,
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!PL   DATA  GRD150/ 0, 255, 3, 442,265,  21821,-120628,   8,   -97000,
!PL  &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!***PLee 08 Feb 2005 ***renaming Grid 150 to 148 starts
      DATA  GRD148/ 0, 255, 3, 442,265,  21821,-120628,   8,   -97000,  &
     &  12000, 12000, 0, 64, 0, 33000, 45000, 0, 0/
!***PLee 08 Feb 2005 ***renaming Grid 150 to 148 ends
!***PLee 08 Feb 2005 ***Comment out ends
!***TLO 08 Mar 04 ***end
      DATA  GRD170/ 0, 255, 4,2880,1440, 89938,      62,  72,  -89938,  &
     &     -62,   125, 125,64, 0, 0, 0, 0, 0/
      DATA  GRD171/ 0, 255, 5, 770,930,  25009, -119560,  72,  -80000,	&
     &   12700,  12700, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD172/ 0, 255, 5, 690,710,  36900, -220194,  72, -260000,	&
     &   12700,  12700, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD175/ 0, 255, 0, 556,334,      0,  130000, 128,   30060,	&
     &  180040,    90,  90, 64, 0, 0, 0, 0, 0/
      DATA  GRD190 / 0, 255,203, 92,141,   182, -149887, 136,   52000,	&
     & -111000,    577,538,64, 0, 0, 0, 0, 0/
      DATA  GRD192 / 0, 255,203,237,387, -3441, -148799, 136,   50000,	&
     & -111000,    225,207,64, 0, 0, 0, 0, 0/
      DATA  GRD194 / 0, 255,203, 89,143, 16444, -162244, 136,   20250,	&
     & -157350,     53, 53,64, 0, 0, 0, 0, 0/
      DATA  GRD196/ 0, 255,201,45903,1,  23476,  -96745, 136,     151,  &
     &     305,     67, 66, 64, 0, 0, 0, 0, 0/
      DATA  GRD198/ 0, 255,203,160,261,  -3441, -148799, 136,   50000,	&
     & -111000,    333,308,64, 0, 0, 0, 0, 0/
      DATA  GRD201/ 0, 255, 5,  65, 65, -20826, -150000,   8, -105000,	&
     &  381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD202/ 0, 255, 5,  65, 43,   7838, -141028,   8, -105000,	&
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD203/ 0, 255, 5,  45, 39,  19132, -185837,   8, -150000,	&
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD204/ 0, 255, 1,  93, 68, -25000,  110000, 128,   60644,	&
     & -109129, 160000, 160000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD205/ 0, 255, 5,  45, 39,    616,  -84904,   8,  -60000,	&
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD206/ 0, 255, 3,  51, 41,  22289, -117991,   8, - 95000,  &
     &   81271,  81271, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD207/ 0, 255, 5,  49, 35,  42085, -175641,   8, -150000,	&
     &   95250,  95250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD208/ 0, 255, 1,  29, 27,   9343, -167315, 128,   28092,	&
     & -145878, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD209/ 0, 255, 3, 275,223,  -4850, -151100,   8, -111000,	&
     &   44000,  44000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD210/ 0, 255, 1,  25, 25,   9000,  -77000, 128,   26422,	&
     &  -58625, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD211/ 0, 255, 3,  93, 65,  12190, -133459,   8,  -95000,	&
     &   81271,  81271, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD212/ 0, 255, 3, 185,129,  12190, -133459,   8,  -95000,	&
     &   40635,  40635, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD213/ 0, 255, 5, 129, 85,   7838, -141028,   8, -105000,  &
     &   95250,  95250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD214/ 0, 255, 5,  97, 69,  42085, -175641,   8, -150000,	&
     &   47625,  47625, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD215/ 0, 255, 3, 369,257,  12190, -133459,   8,  -95000,	&
     &   20318,  20318, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD216/ 0, 255, 5, 139,107,  30000, -173000,   8, -135000,	&
     &   45000,  45000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD217/ 0, 255, 5, 277,213,  30000, -173000,   8, -135000,	&
     &   22500,  22500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD218/ 0, 255, 3, 614,428,  12190, -133459,   8,  -95000,	&
     &   12191,  12191, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD219/ 0, 255, 5, 385,465,  25008, -119559,  72,  -80000,	&
     &   25400,  25400, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD220/ 0, 255, 5, 345,355, -36889, -220194,  72, -260000,  &
     &   25400,  25400, 1, 64, 0, 0, 0, 0, 0/
      DATA  GRD221/ 0, 255, 3, 349,277,   1000, -145500,   8, -107000,	&
     &   32463,  32463, 0, 64, 0, 50000, 50000, 0, 0/
      DATA  GRD222/ 0, 255, 3, 138,112,  -4850, -151100,   8, -111000,	&
     &   88000,  88000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD223/ 0, 255, 5, 129,129, -20826, -150000,   8, -105000,	&
     &  190500, 190500, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD224/ 0, 255, 5,  65, 65,  20826,  120000,   8, -105000,	&
     &  381000, 381000, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD225/ 0, 255, 1, 185,135, -25000, -250000, 128,   60640,	&
     & -109129, 80000, 80000, 20000, 64, 0, 0, 0, 0/
      DATA  GRD226/ 0, 255, 3, 737,513,  12190, -133459,   8,  -95000,	&
     &   10159,  10159, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD227/ 0, 255, 3,1473,1025,  12190, -133459,   8, -95000,  &
     &    5079,   5079, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD228/ 0, 255, 0, 144, 73,  90000,       0, 128,  -90000,	&
     &   -2500,   2500, 2500, 64, 0, 0, 0, 0, 0/
      DATA  GRD229/ 0, 255, 0, 360,181,  90000,       0, 128,  -90000,	&
     &   -1000,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD230/ 0, 255, 0, 720,361,  90000,       0, 128,  -90000,	&
     &    -500,    500,  500, 64, 0, 0, 0, 0, 0/
      DATA  GRD231/ 0, 255, 0, 720,181,      0,       0, 128,   90000,	&
     &    -500,    500,  500, 64, 0, 0, 0, 0, 0/
      DATA  GRD232/ 0, 255, 0, 360, 91,      0,       0, 128,   90000,	&
     &   -1000,   1000, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD233/ 0, 255, 0, 288,157,  78000,       0, 128,  -78000,	&
     &   -1250,   1250, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD234/ 0, 255, 0, 133,121,  15000,  -98000, 128,  -45000,  &
     &  -65000,    250,  250, 64, 0, 0, 0, 0, 0/
      DATA  GRD235/ 0, 255, 0, 720,360,  89750,     250,  72,  -89750,	&
     &    -250,    250, 1000, 64, 0, 0, 0, 0, 0/
      DATA  GRD236/ 0, 255, 3, 151,113,  16281,  233862,   8,  -95000,	&
     &   40635,  40635, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD237/ 0, 255, 3,  54, 47,  16201,  285720,   8, -107000,	&
     &   32463,  32463, 0, 64, 0, 50000, 50000, 0, 0/
      DATA  GRD238/ 0, 255, 0, 275, 203,  50750, 261750,  72,    -205,  & 
     &   -29750, 0,  0, 64, 0, 0, 0, 0, 0/
      DATA  GRD239/ 0, 255, 0, 155, 123, 75750,  159500,  72,   44750,  &
     &  -123500,  0, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD240/ 0, 255, 5, 1121, 881, 23098, -119036,  8, -105000,	&
     &   47625,  47625, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD241/ 0, 255, 3, 549,445,  -4850, -151100,   8, -111000,  &
     &   22000,  22000, 0, 64, 0, 45000, 45000, 0, 0/
      DATA  GRD242/ 0, 255, 5, 553,425,  30000, -173000,   8, -135000,	&
     &   11250,  11250, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD243/ 0, 255, 0, 126,101,  10000, -170000, 128,   50000,	&
     &  -120000, 400, 400, 64, 0, 0, 0, 0, 0/
      DATA  GRD244/ 0, 255, 0, 275, 203,  50750, 261750,  72,    -205,  & 
     &   -29750, 0,  0, 64, 0, 0, 0, 0, 0/
      DATA  GRD245/ 0, 255, 3, 336,372,  22980, -92840,   8,   -80000,	&
     &   8000,  8000, 0, 64, 0, 35000, 35000, 0, 0/
      DATA  GRD246/ 0, 255, 3, 332,371,  25970, -127973,  8,  -115000,	&
     &   8000,  8000, 0, 64, 0, 40000, 40000, 0, 0/
      DATA  GRD247/ 0, 255, 3, 336,372,  22980, -110840,   8,  -98000,	&
     &   8000,  8000, 0, 64, 0, 35000, 35000, 0, 0/
      DATA  GRD248/ 0, 255, 0, 135,101,  14500,  -71500, 128,   22000,  &
     &  -61450,    75,  75, 64, 0, 0, 0, 0, 0/
      DATA  GRD249/ 0, 255, 5, 367,343,  45400, -171600,   8, -150000,	&
     &   9868,  9868, 0, 64, 0, 0, 0, 0, 0/
      DATA  GRD250/ 0, 255, 0, 135,101,  16500, -162000, 128,   24000,	&
     & -151950,    75,  75, 64, 0, 0, 0, 0, 0/
      DATA  GRD251/ 0, 255, 0, 332,210,  26350,  -83050, 128,   47250,	&
     &  -49950,    100,  100, 64, 0, 0, 0, 0, 0/
      DATA  GRD252/ 0, 255, 3, 301,225,  16281, -126138,  8,   -95000,	&
     &   20317,  20317, 0, 64, 0, 25000, 25000, 0, 0/
      DATA  GRD253/ 0, 255, 0, 373,224,   6050, -170250,  72,    4750,	&
     &  -77250,    75,  75, 64, 0, 0, 0, 0, 0/
									
      IERR = 0

        DO 1 I = 1,18
          IGDS(I) = 0
 1      CONTINUE

      IF (IGRID.GE.37.AND.IGRID.LE.44) THEN
        DO 2 I = 19,91
          IGDS(I) = 0
 2      CONTINUE
      END IF

      IF (IGRID.GE.21.AND.IGRID.LE.24) THEN
        DO I = 19,55
          IGDS(I) = 0
        END DO
      END IF

      IF (IGRID.GE.25.AND.IGRID.LE.26) THEN
        DO I = 19,37
          IGDS(I) = 0
        END DO
      END IF

      IF (IGRID.GE.61.AND.IGRID.LE.64) THEN
        DO I = 19,64
          IGDS(I) = 0
        END DO
      END IF

      IF (IGRID.EQ.1) THEN
        DO 3 I = 1,18
          IGDS(I) = GRD1(I)
  3     CONTINUE

      ELSE IF (IGRID.EQ.2) THEN
        DO 4 I = 1,18
          IGDS(I) = GRD2(I)
  4     CONTINUE

      ELSE IF (IGRID.EQ.3) THEN
        DO 5 I = 1,18
          IGDS(I) = GRD3(I)
  5     CONTINUE

      ELSE IF (IGRID.EQ.4) THEN
        DO 6 I = 1,18
          IGDS(I) = GRD4(I)
  6     CONTINUE

      ELSE IF (IGRID.EQ.5) THEN
        DO 10 I = 1,18
          IGDS(I) = GRD5(I)
 10     CONTINUE

      ELSE IF (IGRID.EQ.6) THEN
        DO 20 I = 1,18
          IGDS(I) = GRD6(I)
 20     CONTINUE

      ELSE IF (IGRID.EQ.8) THEN
        DO I = 1,18
          IGDS(I) = GRD8(I)
        END DO

      ELSE IF (IGRID.EQ.21) THEN
        DO 30 I = 1,55
          IGDS(I) = GRD21(I)
 30     CONTINUE

      ELSE IF (IGRID.EQ.22) THEN
        DO 40 I = 1,55
          IGDS(I) = GRD22(I)
 40     CONTINUE

      ELSE IF (IGRID.EQ.23) THEN
        DO 50 I = 1,55
          IGDS(I) = GRD23(I)
 50     CONTINUE

      ELSE IF (IGRID.EQ.24) THEN
        DO 60 I = 1,55
          IGDS(I) = GRD24(I)
 60     CONTINUE

      ELSE IF (IGRID.EQ.25) THEN
        DO 70 I = 1,37
          IGDS(I) = GRD25(I)
 70     CONTINUE

      ELSE IF (IGRID.EQ.26) THEN
        DO 80 I = 1,37
          IGDS(I) = GRD26(I)
 80     CONTINUE

      ELSE IF (IGRID.EQ.27) THEN
        DO 90 I = 1,18
          IGDS(I) = GRD27(I)
 90     CONTINUE

      ELSE IF (IGRID.EQ.28) THEN
        DO 100 I = 1,18
          IGDS(I) = GRD28(I)
 100    CONTINUE

      ELSE IF (IGRID.EQ.29) THEN
        DO 110 I = 1,18
          IGDS(I) = GRD29(I)
 110    CONTINUE

      ELSE IF (IGRID.EQ.30) THEN
        DO 120 I = 1,18
          IGDS(I) = GRD30(I)
 120    CONTINUE

      ELSE IF (IGRID.EQ.33) THEN
        DO 130 I = 1,18
          IGDS(I) = GRD33(I)
 130     CONTINUE

      ELSE IF (IGRID.EQ.34) THEN
        DO 140 I = 1,18
          IGDS(I) = GRD34(I)
 140    CONTINUE

      ELSE IF (IGRID.EQ.37) THEN
        DO 141 I = 1,91
          IGDS(I) = GRD37(I)
 141    CONTINUE

      ELSE IF (IGRID.EQ.38) THEN
        DO 142 I = 1,91
          IGDS(I) = GRD38(I)
 142    CONTINUE

      ELSE IF (IGRID.EQ.39) THEN
        DO 143 I = 1,91
          IGDS(I) = GRD39(I)
 143    CONTINUE

      ELSE IF (IGRID.EQ.40) THEN
        DO 144 I = 1,91
          IGDS(I) = GRD40(I)
 144    CONTINUE

      ELSE IF (IGRID.EQ.41) THEN
        DO 145 I = 1,91
          IGDS(I) = GRD41(I)
 145    CONTINUE

      ELSE IF (IGRID.EQ.42) THEN
        DO 146 I = 1,91
          IGDS(I) = GRD42(I)
 146    CONTINUE

      ELSE IF (IGRID.EQ.43) THEN
        DO 147 I = 1,91
          IGDS(I) = GRD43(I)
 147    CONTINUE

      ELSE IF (IGRID.EQ.44) THEN
        DO 148 I = 1,91
          IGDS(I) = GRD44(I)
 148    CONTINUE

      ELSE IF (IGRID.EQ.45) THEN
        DO 149 I = 1,18
          IGDS(I) = GRD45(I)
 149    CONTINUE

      ELSE IF (IGRID.EQ.53) THEN
        DO I = 1,18
          IGDS(I) = GRD53(I)
        END DO

      ELSE IF (IGRID.EQ.55) THEN
        DO 152 I = 1,18
          IGDS(I) = GRD55(I)
 152    CONTINUE

      ELSE IF (IGRID.EQ.56) THEN
        DO 154 I = 1,18
          IGDS(I) = GRD56(I)
 154    CONTINUE

      ELSE IF (IGRID.EQ.61) THEN
        DO 160 I = 1,64
          IGDS(I) = GRD61(I)
 160    CONTINUE

      ELSE IF (IGRID.EQ.62) THEN
        DO 170 I = 1,64
          IGDS(I) = GRD62(I)
 170    CONTINUE

      ELSE IF (IGRID.EQ.63) THEN
        DO 180 I = 1,64
          IGDS(I) = GRD63(I)
 180    CONTINUE

      ELSE IF (IGRID.EQ.64) THEN
        DO 190 I = 1,64
          IGDS(I) = GRD64(I)
 190    CONTINUE

      ELSE IF (IGRID.EQ.85) THEN
        DO 192 I = 1,18
          IGDS(I) = GRD85(I)
 192    CONTINUE

      ELSE IF (IGRID.EQ.86) THEN
        DO 194 I = 1,18
          IGDS(I) = GRD86(I)
 194    CONTINUE

      ELSE IF (IGRID.EQ.87) THEN
        DO 195 I = 1,18
          IGDS(I) = GRD87(I)
 195    CONTINUE

      ELSE IF (IGRID.EQ.88) THEN
        DO 2195 I = 1,18
          IGDS(I) = GRD88(I)
2195    CONTINUE

      ELSE IF (IGRID.EQ.90) THEN
        DO 196 I = 1,18
          IGDS(I) = GRD90(I)
 196    CONTINUE

      ELSE IF (IGRID.EQ.91) THEN
        DO 197 I = 1,18
          IGDS(I) = GRD91(I)
 197    CONTINUE

      ELSE IF (IGRID.EQ.92) THEN
        DO 198 I = 1,18
          IGDS(I) = GRD92(I)
 198    CONTINUE

      ELSE IF (IGRID.EQ.93) THEN
        DO 199 I = 1,18
          IGDS(I) = GRD93(I)
 199    CONTINUE

      ELSE IF (IGRID.EQ.94) THEN
        DO 200 I = 1,18
          IGDS(I) = GRD94(I)
 200    CONTINUE

      ELSE IF (IGRID.EQ.95) THEN
        DO 201 I = 1,18
          IGDS(I) = GRD95(I)
 201    CONTINUE

      ELSE IF (IGRID.EQ.96) THEN
        DO 202 I = 1,18
          IGDS(I) = GRD96(I)
 202    CONTINUE

      ELSE IF (IGRID.EQ.97) THEN
        DO 203 I = 1,18
          IGDS(I) = GRD97(I)
 203    CONTINUE

      ELSE IF (IGRID.EQ.98) THEN
        DO 204 I = 1,18
          IGDS(I) = GRD98(I)
 204    CONTINUE

      ELSE IF (IGRID.EQ.100) THEN
        DO 205 I = 1,18
          IGDS(I) = GRD100(I)
 205    CONTINUE

      ELSE IF (IGRID.EQ.101) THEN
        DO 210 I = 1,18
          IGDS(I) = GRD101(I)
 210    CONTINUE

      ELSE IF (IGRID.EQ.103) THEN
        DO 220 I = 1,18
          IGDS(I) = GRD103(I)
 220   CONTINUE

      ELSE IF (IGRID.EQ.104) THEN
        DO 230 I = 1,18
          IGDS(I) = GRD104(I)
 230    CONTINUE

      ELSE IF (IGRID.EQ.105) THEN
        DO 240 I = 1,18
          IGDS(I) = GRD105(I)
 240    CONTINUE

      ELSE IF (IGRID.EQ.106) THEN
        DO 242 I = 1,18
          IGDS(I) = GRD106(I)
 242    CONTINUE

      ELSE IF (IGRID.EQ.107) THEN
        DO 244 I = 1,18
          IGDS(I) = GRD107(I)
 244    CONTINUE

      ELSE IF (IGRID.EQ.110) THEN
        DO I = 1,18
          IGDS(I) = GRD110(I)
        ENDDO

      ELSE IF (IGRID.EQ.126) THEN
        DO 245 I = 1,18
          IGDS(I) = GRD126(I)
 245    CONTINUE

      ELSE IF (IGRID.EQ.127) THEN
        DO I = 1,18
          IGDS(I) = GRD127(I)
        ENDDO
!***TLO 11 Mar 04 and PLEE July 8, 2009 ***start
      ELSE IF (IGRID.EQ.138) THEN
        DO 1008 I = 1,18
          IGDS(I) = GRD138(I)
1008    CONTINUE
      ELSE IF (IGRID.EQ.139) THEN
        DO 1039 I = 1,18
          IGDS(I) = GRD139(I)
1039    CONTINUE
      ELSE IF (IGRID.EQ.140) THEN
        DO 1040 I = 1,18
          IGDS(I) = GRD140(I)
1040    CONTINUE
      ELSE IF (IGRID.EQ.255) THEN
        DO 1009 I = 1,18
          IGDS(I) = GRD255(I)
1009    CONTINUE
!PL*PLee 08 Feb 2005 ***Comment out starts
!***TLO 11 Mar 04 ***end
!***TLO 08 Mar 04 ***start
!PL   ELSE IF (IGRID.EQ.140) THEN
!PL     DO 1108 I = 1,18
!PL       IGDS(I) = GRD140(I)
!PL 1108    CONTINUE
!PL   ELSE IF (IGRID.EQ.141) THEN
!PL     DO 1109 I = 1,18
!PL       IGDS(I) = GRD141(I)
!PL 1109    CONTINUE
!PL   ELSE IF (IGRID.EQ.142) THEN
!PL     DO 1110 I = 1,18
!PL       IGDS(I) = GRD142(I)
!PL 1110    CONTINUE
!***PLee 08 Feb 2005 ***renaming Grid 142 to 147 starts
      ELSE IF (IGRID.EQ.147) THEN
        DO 1110 I = 1,18
          IGDS(I) = GRD147(I)
1110    CONTINUE
!***PLee 08 Feb 2005 ***renaming Grid 142 to 147 ends
!PL   ELSE IF (IGRID.EQ.144) THEN
!PL     DO 1208 I = 1,18
!PL       IGDS(I) = GRD144(I)
!PL 1208    CONTINUE
!***TLO 08 Mar 04 ***end
!***TLO 16 Dec 02 ***start
      ELSE IF (IGRID.EQ.145) THEN
        DO 1209 I = 1,18
          IGDS(I) = GRD145(I)
1209    CONTINUE
!***TLO 16 Dec 02 ***end
!***TLO 08 Mar 04 ***start
      ELSE IF (IGRID.EQ.146) THEN
        DO 1210 I = 1,18
          IGDS(I) = GRD146(I)
1210    CONTINUE
!PL   ELSE IF (IGRID.EQ.148) THEN
!PL     DO 1308 I = 1,18
!PL       IGDS(I) = GRD148(I)
!PL 1308    CONTINUE
!PL   ELSE IF (IGRID.EQ.149) THEN
!PL     DO 1309 I = 1,18
!PL       IGDS(I) = GRD149(I)
!PL 1309    CONTINUE
!PL   ELSE IF (IGRID.EQ.150) THEN
!PL     DO 1310 I = 1,18
!PL       IGDS(I) = GRD150(I)
!PL 1310    CONTINUE
!PL*PLee 08 Feb 2005 ***renaming Grid 150 to 148 starts
      ELSE IF (IGRID.EQ.148) THEN
        DO 1310 I = 1,18
          IGDS(I) = GRD148(I)
1310    CONTINUE
!PL*PLee 08 Feb 2005 ***renaming Grid 150 to 148 ends
!PL*PLee 08 Feb 2005 ***Comment out ends
!***TLO 08 Mar 04 ***end

      ELSE IF (IGRID.EQ.170) THEN
        DO I = 1,18
          IGDS(I) = GRD170(I)
        ENDDO

      ELSE IF (IGRID.EQ.171) THEN
        DO I = 1,18
          IGDS(I) = GRD171(I)
        ENDDO

      ELSE IF (IGRID.EQ.172) THEN
        DO I = 1,18
          IGDS(I) = GRD172(I)
        ENDDO

      ELSE IF (IGRID.EQ.175) THEN
        DO I = 1,18
          IGDS(I) = GRD175(I)
        ENDDO

      ELSE IF (IGRID.EQ.190) THEN
        DO 2190 I = 1,18
          IGDS(I) = GRD190(I)
 2190   CONTINUE

      ELSE IF (IGRID.EQ.192) THEN
        DO 2191 I = 1,18
          IGDS(I) = GRD192(I)
 2191   CONTINUE

      ELSE IF (IGRID.EQ.194) THEN
        DO 2192 I = 1,18
          IGDS(I) = GRD194(I)
 2192   CONTINUE

      ELSE IF (IGRID.EQ.196) THEN
        DO 249 I = 1,18
          IGDS(I) = GRD196(I)
 249    CONTINUE

      ELSE IF (IGRID.EQ.198) THEN
        DO 2490 I = 1,18
          IGDS(I) = GRD198(I)
 2490   CONTINUE

      ELSE IF (IGRID.EQ.201) THEN
        DO 250 I = 1,18
          IGDS(I) = GRD201(I)
 250    CONTINUE

      ELSE IF (IGRID.EQ.202) THEN
        DO 260 I = 1,18
          IGDS(I) = GRD202(I)
 260    CONTINUE

      ELSE IF (IGRID.EQ.203) THEN
        DO 270 I = 1,18
          IGDS(I) = GRD203(I)
 270    CONTINUE

      ELSE IF (IGRID.EQ.204) THEN
        DO 280 I = 1,18
          IGDS(I) = GRD204(I)
 280    CONTINUE

      ELSE IF (IGRID.EQ.205) THEN
        DO 290 I = 1,18
          IGDS(I) = GRD205(I)
 290    CONTINUE

      ELSE IF (IGRID.EQ.206) THEN
        DO 300 I = 1,18
          IGDS(I) = GRD206(I)
 300    CONTINUE

      ELSE IF (IGRID.EQ.207) THEN
        DO 310 I = 1,18
          IGDS(I) = GRD207(I)
 310    CONTINUE

      ELSE IF (IGRID.EQ.208) THEN
        DO 320 I = 1,18
          IGDS(I) = GRD208(I)
 320    CONTINUE

      ELSE IF (IGRID.EQ.209) THEN
        DO 330 I = 1,18
          IGDS(I) = GRD209(I)
 330    CONTINUE

      ELSE IF (IGRID.EQ.210) THEN
        DO 340 I = 1,18
          IGDS(I) = GRD210(I)
 340    CONTINUE

      ELSE IF (IGRID.EQ.211) THEN
        DO 350 I = 1,18
          IGDS(I) = GRD211(I)
 350    CONTINUE

      ELSE IF (IGRID.EQ.212) THEN
        DO 360 I = 1,18
          IGDS(I) = GRD212(I)
 360    CONTINUE

      ELSE IF (IGRID.EQ.213) THEN
        DO 370 I = 1,18
          IGDS(I) = GRD213(I)
 370    CONTINUE

      ELSE IF (IGRID.EQ.214) THEN
        DO 380 I = 1,18
          IGDS(I) = GRD214(I)
 380    CONTINUE

      ELSE IF (IGRID.EQ.215) THEN
        DO 390 I = 1,18
          IGDS(I) = GRD215(I)
 390    CONTINUE

      ELSE IF (IGRID.EQ.216) THEN
        DO 400 I = 1,18
          IGDS(I) = GRD216(I)
 400    CONTINUE

      ELSE IF (IGRID.EQ.217) THEN
        DO 401 I = 1,18
          IGDS(I) = GRD217(I)
 401    CONTINUE

      ELSE IF (IGRID.EQ.218) THEN
        DO 410 I = 1,18
          IGDS(I) = GRD218(I)
 410    CONTINUE

      ELSE IF (IGRID.EQ.219) THEN
        DO 411 I = 1,18
          IGDS(I) = GRD219(I)
 411    CONTINUE

      ELSE IF (IGRID.EQ.220) THEN
        DO 412 I = 1,18
          IGDS(I) = GRD220(I)
 412    CONTINUE

      ELSE IF (IGRID.EQ.221) THEN
        DO 413 I = 1,18
          IGDS(I) = GRD221(I)
 413    CONTINUE

      ELSE IF (IGRID.EQ.222) THEN
        DO 414 I = 1,18
          IGDS(I) = GRD222(I)
 414    CONTINUE

      ELSE IF (IGRID.EQ.223) THEN
        DO 415 I = 1,18
          IGDS(I) = GRD223(I)
 415    CONTINUE

      ELSE IF (IGRID.EQ.224) THEN
        DO 416 I = 1,18
          IGDS(I) = GRD224(I)
 416    CONTINUE

      ELSE IF (IGRID.EQ.225) THEN
        DO 417 I = 1,18
          IGDS(I) = GRD225(I)
 417    CONTINUE

      ELSE IF (IGRID.EQ.226) THEN
        DO 418 I = 1,18
          IGDS(I) = GRD226(I)
 418    CONTINUE

      ELSE IF (IGRID.EQ.227) THEN
        DO 419 I = 1,18
          IGDS(I) = GRD227(I)
 419    CONTINUE

      ELSE IF (IGRID.EQ.228) THEN
        DO 420 I = 1,18
          IGDS(I) = GRD228(I)
 420    CONTINUE

      ELSE IF (IGRID.EQ.229) THEN
        DO 421 I = 1,18
          IGDS(I) = GRD229(I)
 421    CONTINUE

      ELSE IF (IGRID.EQ.230) THEN
        DO 422 I = 1,18
          IGDS(I) = GRD230(I)
 422    CONTINUE

      ELSE IF (IGRID.EQ.231) THEN
        DO 423 I = 1,18
          IGDS(I) = GRD231(I)
 423    CONTINUE

      ELSE IF (IGRID.EQ.232) THEN
        DO 424 I = 1,18
          IGDS(I) = GRD232(I)
 424    CONTINUE

      ELSE IF (IGRID.EQ.233) THEN
        DO 425 I = 1,18
          IGDS(I) = GRD233(I)
 425    CONTINUE

      ELSE IF (IGRID.EQ.234) THEN
        DO 426 I = 1,18
          IGDS(I) = GRD234(I)
 426    CONTINUE

      ELSE IF (IGRID.EQ.235) THEN
        DO 427 I = 1,18
          IGDS(I) = GRD235(I)
 427    CONTINUE

      ELSE IF (IGRID.EQ.236) THEN
        DO 428 I = 1,18
          IGDS(I) = GRD236(I)
 428    CONTINUE

      ELSE IF (IGRID.EQ.237) THEN
        DO 429 I = 1,18
          IGDS(I) = GRD237(I)
 429    CONTINUE

      ELSE IF (IGRID.EQ.238) THEN
        DO I = 1,18
          IGDS(I) = GRD238(I)
        END DO

      ELSE IF (IGRID.EQ.239) THEN
        DO I = 1,18
          IGDS(I) = GRD239(I)
        END DO

      ELSE IF (IGRID.EQ.240) THEN
        DO I = 1,18
          IGDS(I) = GRD240(I)
        END DO

      ELSE IF (IGRID.EQ.241) THEN
        DO 430 I = 1,18
          IGDS(I) = GRD241(I)
 430    CONTINUE

      ELSE IF (IGRID.EQ.242) THEN
        DO 431 I = 1,18
          IGDS(I) = GRD242(I)
 431    CONTINUE

      ELSE IF (IGRID.EQ.243) THEN
        DO 432 I = 1,18
          IGDS(I) = GRD243(I)
 432    CONTINUE

      ELSE IF (IGRID.EQ.244) THEN
        DO I = 1,18
          IGDS(I) = GRD244(I)
        END DO

      ELSE IF (IGRID.EQ.245) THEN
        DO 433 I = 1,18
          IGDS(I) = GRD245(I)
 433    CONTINUE

      ELSE IF (IGRID.EQ.246) THEN
        DO 434 I = 1,18
          IGDS(I) = GRD246(I)
 434    CONTINUE

      ELSE IF (IGRID.EQ.247) THEN
        DO 435 I = 1,18
          IGDS(I) = GRD247(I)
 435    CONTINUE

      ELSE IF (IGRID.EQ.248) THEN
        DO 436 I = 1,18
          IGDS(I) = GRD248(I)
 436    CONTINUE

      ELSE IF (IGRID.EQ.249) THEN
        DO 437 I = 1,18
          IGDS(I) = GRD249(I)
 437    CONTINUE

      ELSE IF (IGRID.EQ.250) THEN
        DO 438 I = 1,18
          IGDS(I) = GRD250(I)
 438    CONTINUE

      ELSE IF (IGRID.EQ.251) THEN
        DO 439 I = 1,18
          IGDS(I) = GRD251(I)
 439    CONTINUE

      ELSE IF (IGRID.EQ.252) THEN
        DO 440 I = 1,18
          IGDS(I) = GRD252(I)
 440    CONTINUE
      ELSE IF (IGRID.EQ.253) THEN
        DO 441 I = 1,18
          IGDS(I) = GRD253(I)
 441    CONTINUE

      ELSE
        IERR = 1
      ENDIF

      RETURN
      END
