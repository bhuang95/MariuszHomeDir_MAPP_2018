PROGRAM interp_anal2obs

!Mariusz Pagowski, CIRA, Jan. 2012

! program to interpolate analyses to sfc obs
! uses WPS modules for map projections and gsi input files
  
  USE map_utils
  USE llxy_module
  USE interp_module

  IMPLICIT NONE

  REAL, PARAMETER :: o3max=200.,pmmax=60.
  INTEGER, PARAMETER :: icut=2,jcut=2
  
  REAL :: latc,lonc,truelat_1,truelat_2,stdlon,dx,dy

  CHARACTER(len=120) :: prefix,mapfile
  CHARACTER(len=12)species,spec
  CHARACTER(len=120) :: suffix='.bin'
  CHARACTER(len=120) :: obsfile,geoptfile,outfile,fcstdir

  INTEGER ::  nxin,nyin,nf,nx,ny,nz,ndays,nfcst

  REAL, PARAMETER :: eps_min=EPSILON(1.),grav=9.81,&
       &blank=0.,blankp=blank-.01,unknown=-999.,&
       &unknownp=unknown+1,halfhour=0.5/24.+eps_min

  INTEGER, DIMENSION(1) :: interp_method=FOUR_POINT
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: varin,geoptin
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: var
  INTEGER, PARAMETER :: nmaxread=1e6
  REAL, DIMENSION(nmaxread) :: gridxobs=unknown,gridyobs=unknown,&
       &gridiobs=unknown,gridjobs=unknown,height,geopt
  CHARACTER(len=9), DIMENSION(nmaxread) :: code

  TYPE(proj_info) :: proj

  CHARACTER(len=2) :: icchar,ctrash
  CHARACTER(len=19), ALLOCATABLE, DIMENSION(:) :: ctime
  CHARACTER(len=19) :: obstime
  CHARACTER(len=4) :: year
  CHARACTER(len=2) :: month,day,hour,mins,sec
  CHARACTER(len=120) :: input_name
  CHARACTER(len=120), ALLOCATABLE, DIMENSION(:) :: fnames
  INTEGER :: itrash,julobs,nst,nstin,proj_code,&
       &julmin,julmax,jdate
  REAL :: jul,julout
  REAL, DIMENSION(nmaxread) :: lat,lon,julstamp
  INTEGER :: iyear,imonth,iday,ihour,imin
  INTEGER, DIMENSION(nmaxread) :: site
  LOGICAL, DIMENSION(nmaxread) :: outside
  REAL :: ilon,jlat,sumi,sumj,bi,bj,sdi,sdj,corrsum,rnf,rtrash,concmax
  REAL, DIMENSION(nmaxread) :: obs,fcst
  INTEGER, ALLOCATABLE, DIMENSION(:) :: iyearinit,imonthinit,&
       &idayinit,ihourinit


  CHARACTER (LEN=120)                 :: command

  INTEGER  :: iargc,i,j,k,l,fcstunit=200,ix,iy,&
       &mapunit=50,obsunit=51,geoptunit=52,outunit=53,&
       &count_start,count_end,crate,loslen
  REAL :: ric,rjc
  

  CALL SYSTEM_CLOCK(count_start,crate)

  IF (iargc() < 6) THEN
     PRINT *,'Requires 6 input parameters: see script'
     PRINT *,'Stopping'
     STOP
  ENDIF

  CALL getarg(1,mapfile)
  CALL getarg(2,species)
  call getarg(3,obsfile)
  CALL getarg(4,fcstdir)
  call getarg(5,geoptfile)
  CALL getarg(6,outfile)


  OPEN(unit=mapunit,file=mapfile,form='formatted')
  READ(mapunit,*)ctrash
  READ(mapunit,*)latc,lonc,truelat_1,truelat_2,stdlon,dx
  CLOSE(mapunit)

  dy=dx
  
  input_name=TRIM(fcstdir)//'/*'//suffix

  loslen = LEN ( command )
  CALL all_spaces ( command , loslen )

  WRITE ( command , FMT='("ls -1 ",A," > .foo")' ) TRIM (input_name)

  CALL SYSTEM ( TRIM ( command ) )
  CALL SYSTEM ( '( cat .foo | wc -l > .foo1 )' )

!  Read the number of files.
  OPEN (FILE   = '.foo1'       , &
       UNIT   = 112           , &
       STATUS = 'OLD'         , &
       ACCESS = 'SEQUENTIAL'  , &
       FORM   = 'FORMATTED'     )

  READ ( 112 , * ) nf
  CLOSE ( 112 )
  PRINT *,'There are nf = ',nf,' forecast files'

  ALLOCATE(fnames(nf))

!  Open the file that has the list of filenames.
  OPEN (FILE   = '.foo'        , &
       UNIT   = 111           , &
       STATUS = 'OLD'         , &
       ACCESS = 'SEQUENTIAL'  , &
       FORM   = 'FORMATTED'     )
  
!  Read all of the file names and store them.

  DO k = 1 , nf
     READ ( 111 , FMT='(A)' ) fnames(k)
  ENDDO

  CLOSE(111)

  CALL SYSTEM ( '/bin/rm -f .foo'  )
  CALL SYSTEM ( '/bin/rm -f .foo1' )

  ALLOCATE(iyearinit(nf),imonthinit(nf),idayinit(nf),ihourinit(nf))

  DO k=1,nf
     i=INDEX(fnames(k),'/',BACK=.TRUE.)+1
     year=fnames(k)(i+1:i+4)
     month=fnames(k)(i+6:i+7)
     day=fnames(k)(i+9:i+10)
     hour=fnames(k)(i+12:i+13)
     READ(year,'(i4)') iyearinit(k)
     READ(month,'(i2)') imonthinit(k)
     READ(day,'(i2)') idayinit(k)
     READ(hour,'(i2)') ihourinit(k)
  ENDDO

  julmin=jdate(iyearinit(1),imonthinit(1),idayinit(1))

  julmax=jdate(iyearinit(nf),imonthinit(nf),idayinit(nf))+1

  OPEN(fcstunit,file=fnames(1),form='unformatted')
  READ(fcstunit)nxin,nyin,nz,nfcst
  ALLOCATE(varin(nxin,nyin,nfcst))
  nx=nxin-2*icut
  ny=nyin-2*jcut
  CLOSE(fcstunit)

  ALLOCATE(geoptin(nxin,nyin,1))
  
  PRINT *,'Reading model geopt file'
  
  OPEN(geoptunit,file=geoptfile,form='unformatted')
  READ(geoptunit)ix,iy
  IF (ix /= nxin .OR. iy /= nyin) THEN
     PRINT *,'DIMENSION MISTMATCH FOR GEOPT - STOPPING'
     STOP
  ENDIF
  READ(geoptunit)geoptin
  CLOSE(geoptunit)

  geoptin=geoptin/grav

  CALL map_init(proj)

  proj_code = PROJ_LC

  ric=.5*REAL(nxin-1)+1.
  rjc=.5*REAL(nyin-1)+1.

  CALL map_set(proj_code=proj_code,proj=proj,&
       &truelat1=truelat_1,truelat2=truelat_2,&
       &lat1=latc,lon1=lonc,&
       &knowni=ric,knownj=rjc,&
       &stdlon=stdlon,&
       &dx=dx,&
       &r_earth=EARTH_RADIUS_M)

  
  julstamp=unknown
  l=1

  IF (species=='PM2_5_DRY') THEN 
     species='pm2_5'
     concmax=pmmax
  ELSE IF (species=='o3') THEN
     concmax=o3max
  ENDIF
  
  OPEN(obsunit,file=obsfile,form='formatted')
  READ(obsunit,'(a)')spec
  IF (TRIM(spec) /= TRIM(species)) THEN
     PRINT *,'Wrong species in input file ',TRIM(obsfile)
     PRINT *,TRIM(spec),TRIM(species)
     PRINT *,'Stopping'
     STOP
  ENDIF
  
  DO WHILE(.TRUE.)

     READ(obsunit,'(5i5,2f11.5,f7.2,i3,f10.1,a11)',END=100)&
          &iyear,imonth,iday,ihour,imin,&
          &lat(l),lon(l),obs(l),site(l),height(l),code(l)
     
     julobs=jdate(iyear,imonth,iday)

     IF (julobs >= julmin .AND. julobs <= julmax .AND. &
          &obs(l) >= 0. .AND. obs(l) < concmax ) THEN

        julstamp(l)=REAL(julobs-julmin+1)+REAL(ihour)/24.
     
        CALL lltoxy(proj, lat(l), lon(l), ilon, jlat, HH)
!     CALL xytoll(proj, ilon, jlat, lat(l), lon(l), HH)
     
        IF (  jlat > jcut .AND. jlat < nyin-jcut .AND. &
             &ilon > icut .AND. ilon < nxin-icut ) THEN 
           gridiobs(l)=ilon
           gridjobs(l)=jlat
           gridxobs(l)=ilon-1.
           gridyobs(l)=jlat-1.
           l=l+1
        ENDIF
     ENDIF
  ENDDO
  
100 CONTINUE
  
  CLOSE(obsunit)
  
  nst=l

  WRITE(*,*)'There are ',nst,' valid obs for this period'
  
  WRITE(*,*)'Reading background files'
  OPEN(outunit,file=outfile,form='formatted')

  DO k=1,nf
     PRINT *,fnames(k)
     fcstunit=k+200
     OPEN(fcstunit,file=fnames(k),form='unformatted')
     READ(fcstunit)ix,iy
     IF (ix /= nxin .OR. iy /= nyin) THEN
        PRINT *,'DIMENSION MISTMATCH - STOPPING'
        STOP
     ENDIF
     READ(fcstunit)varin
     CLOSE(fcstunit)

     IF (TRIM(species)=='o3') THEN
        varin = varin*1.e3
     ENDIF
     
     ihour=ihourinit(k)

     i=1

     jul=REAL(jdate(iyearinit(k),imonthinit(k),&
          &idayinit(k))-julmin+1)+REAL(ihour)/24.

     julout=(jul-REAL(ihour)/24.)*1.e2+REAL(ihour)

     WRITE(outunit,'(i7,4f10.1,i5,4f10.4,i10)')NINT(unknown),&
          &julout,unknown,&
          &unknown,unknown,NINT(unknown),unknown,unknown,&
          &unknown,unknown,NINT(unknown)
     
     DO l=1,nst
        
        IF (ABS(jul-julstamp(l)) <= halfhour) THEN
!assigning within one hour window +-1/2 hour
           
           fcst(l)=four_pt(gridiobs(l), gridjobs(l),&
                &1,varin(:,:,i),1,nxin,1,nyin,1,1,unknown,&
                &interp_method,1)
           geopt(l)=four_pt(gridiobs(l), gridjobs(l),&
                &1,geoptin,1,nxin,1,nyin,1,1,unknown,&
                &interp_method,1)
           
           WRITE(outunit,'(i7,4f10.1,i5,4f10.4,a10)')l,&
                &obs(l),height(l),&
                &fcst(l),geopt(l),site(l),lat(l),lon(l),&
                &gridiobs(l),gridjobs(l),code(l)
           
        ENDIF
           
     ENDDO

  ENDDO

  WRITE(outunit,'(i7,4f10.1,i5,4f10.4,i10)')NINT(unknown),&
       &julout,unknown,&
       &unknown,unknown,NINT(unknown),unknown,unknown,&
       &unknown,unknown,NINT(unknown)
  
  CLOSE(outunit)

  CALL SYSTEM_CLOCK(count_end,crate)
  
  PRINT *,(count_end-count_start)/REAL(crate),' sec'

END PROGRAM interp_anal2obs


