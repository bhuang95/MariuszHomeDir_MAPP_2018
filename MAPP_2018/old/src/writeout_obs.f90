PROGRAM writeout_obs

!Mariusz Pagowski, CIRA, April, 2013

!write out obs with locations averaged over a specific period
!for plotting concentrations with gmt

  USE map_utils
  USE llxy_module
  USE interp_module

  IMPLICIT NONE

  REAL, PARAMETER :: o3max=200.,pmmax=60.,deltat=0.5 !30 mins
!changed from pmmax=100

  INTEGER, PARAMETER :: nmaxread=2e5
  INTEGER, PARAMETER :: nstmax=2000 ! there is no more than 2000 AIRNow sites
  INTEGER, PARAMETER :: icut=0,jcut=0

  
  CHARACTER(len=1) :: delim=','
  REAL :: latc,lonc,truelat_1,truelat_2,stdlon,dx,dy

  CHARACTER(len=120) :: species,mapfile,spec
  CHARACTER(len=120) :: obsfile,geoptfile,outfile

  TYPE(proj_info) :: proj

  REAL, PARAMETER :: eps_min=EPSILON(1.),&
       &blank=0.,blankp=blank-.01,unknown=-999.,unknownp=unknown+1

  REAL, DIMENSION(nmaxread) :: gridxobs=unknown,gridyobs=unknown,&
       &gridiobs=unknown,gridjobs=unknown,height,geopt
  INTEGER, DIMENSION(nmaxread) :: code

  INTEGER, DIMENSION(nmaxread) :: position

  CHARACTER(len=2) :: icchar,ctrash
  CHARACTER(len=19), ALLOCATABLE, DIMENSION(:) :: ctime
  CHARACTER(len=19) :: obstime
  INTEGER :: startyear,endyear,startmonth,startday,starthour,&
       &endmonth,endday,endhour
  CHARACTER(len=10) :: cstartdate,cenddate

  INTEGER :: itrash,jul1st,julobs,nread,nst,julmin,julmax,jdate,proj_code
  REAL, DIMENSION(nmaxread) :: lat,lon,julstamp
  REAL, DIMENSION(nstmax) :: latst,lonst,obsave
  INTEGER :: iyear,imonth,iday,ihour,imin,nx,ny
  INTEGER, DIMENSION(nmaxread) :: site
  INTEGER, DIMENSION(nstmax) :: sitest,codest
  REAL :: ilon,jlat,sumi,sumj,bi,bj,sdi,sdj,corrsum,rnf,rtrash,concmax
  REAL, DIMENSION(nmaxread) :: obs
  REAL :: ric,rjc

  INTEGER  :: iargc,i,j,k,l,fcstunit=200,ix,iy,&
       &mapunit=50,obsunit=51,geoptunit=52,outunit=53,&
       &count_start,count_end,crate,loslen

  IF (iargc() < 6) THEN
     PRINT *,'Requires 6 input parameters: see script'
     PRINT *,'Stopping'
     STOP
  ENDIF

  CALL getarg(1,mapfile)
  CALL getarg(2,species)
  CALL getarg(3,obsfile)
  CALL getarg(4,cstartdate)
  CALL getarg(5,cenddate)
  CALL getarg(6,outfile)
  
  READ(cstartdate(1:4),'(i4)') startyear
  READ(cstartdate(5:6),'(i2)') startmonth
  READ(cstartdate(7:8),'(i2)') startday
  READ(cstartdate(9:10),'(i2)') starthour
  
  READ(cenddate(1:4),'(i4)') endyear
  READ(cenddate(5:6),'(i2)') endmonth
  READ(cenddate(7:8),'(i2)') endday
  READ(cenddate(9:10),'(i2)') endhour

  PRINT *,'Reading mapunit ',mapfile

  OPEN(unit=mapunit,file=mapfile,form='formatted')
  READ(mapunit,*)ctrash
  READ(mapunit,*)latc,lonc,truelat_1,truelat_2,stdlon,dx,nx,ny
  CLOSE(mapunit)

  dy=dx
  
  CALL map_init(proj)
  
  proj_code = PROJ_LC

  ric=.5*REAL(nx-1)+1.
  rjc=.5*REAL(ny-1)+1.

  CALL map_set(proj_code=proj_code,proj=proj,&
       &truelat1=truelat_1,truelat2=truelat_2,&
       &lat1=latc,lon1=lonc,&
       &knowni=ric,knownj=rjc,&
       &stdlon=stdlon,&
       &dx=dx,&
       &r_earth=EARTH_RADIUS_M)

  
  julmin=jdate(startyear,startmonth,startday)*100+starthour
  
  julmax=jdate(endyear,endmonth,endday)*100+endhour

  jul1st=jdate(startyear,1,1)-1


  julstamp=unknown
  code=0
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
     PRINT *,'Stopping'
     STOP
  ENDIF

  l=1
  
  DO WHILE(.TRUE.)

     READ(obsunit,'(5i5,2f11.5,f7.2,i3,f10.1,i11)',END=100)&
          &iyear,imonth,iday,ihour,imin,&
          &lat(l),lon(l),obs(l),site(l),height(l),code(l)
     
     julobs=jdate(iyear,imonth,iday)*100+ihour+deltat
     
     IF (julobs >= julmin .AND. julobs <= julmax .AND. &
          &obs(l) >= 0. .AND. obs(l) < concmax) THEN

        CALL lltoxy(proj, lat(l), lon(l), ilon, jlat, HH)
        
        IF (  jlat > jcut .AND. jlat < ny-jcut .AND. &
             &ilon > icut .AND. ilon < nx-icut ) THEN

           julstamp(l)=jdate(iyear,imonth,iday)+REAL(ihour)/24.-jul1st
           l=l+1

        ENDIF

     ENDIF

  ENDDO
  
100 CONTINUE
  
  CLOSE(obsunit)
  
  nread=l-1

  WRITE(*,*)'There are ',nread,' valid obs for this period'

  CALL indexx_numi(nread,code(1:nread),position(1:nread))

  sumi=obs(position(1))
  i=0
  j=1

  DO l=2,nread
     
     IF (code(position(l-1)) == code(position(l)) ) THEN
        sumi=sumi+obs(position(l))
        j=j+1
     ELSE
        i=i+1
        obsave(i)=sumi/REAL(j)
        latst(i)=lat(position(l-1))
        lonst(i)=lon(position(l-1))
        sitest(i)=site(position(l-1))
        codest(i)=code(position(l-1))
        sumi=obs(position(l))
        j=1
     ENDIF

  ENDDO

  nst=i

  PRINT *, 'There are ',nst,' observation sites'

  OPEN(outunit,file=outfile,form='formatted')

  DO i=1,nst
     WRITE(outunit,FMT=102)codest(i),delim,latst(i),delim,lonst(i),&
          &delim,sitest(i),delim,obsave(i)
  ENDDO

  CLOSE(outunit)

102 FORMAT(i10,2(a,f10.3),a,i5,3(a,f10.4))

END PROGRAM writeout_obs


