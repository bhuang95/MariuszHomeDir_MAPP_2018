PROGRAM ds_mask_omp

!MZP May 16, 2016
!uses Barnes analysis approach signle pass only

  USE omp_lib
  USE netcdf_io
  USE kinds, ONLY: i_kind,r_kind

  IMPLICIT NONE

  INTEGER, PARAMETER :: nmaxobs=1e7
  REAL, PARAMETER :: rearth=6371.e3, epsilon=1.e-6, grid_fraction=1.,sqrt2=SQRT(2.)

  REAL, DIMENSION(nmaxobs) :: latin,lonin 
  INTEGER, DIMENSION(nmaxobs) :: maskin
  REAL, DIMENSION(nmaxobs) :: dsaindexin

  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  CHARACTER(len=25)                                 :: varstringname
  INTEGER,           DIMENSION(4)                   :: dims
  REAL, ALLOCATABLE :: lat(:,:,:), lon(:,:,:),mapscale(:,:,:),&
       &maskout(:,:),dsaindexout(:,:)
  INTEGER :: nx,ny,nt,nin,i,j,k
  CHARACTER*250 :: inwrffile,inmaskfile,outfile,filename 
  CHARACTER(len=10) :: cidate
  INTEGER :: outunit=101

  INTEGER(i_kind) :: nx_kind,ny_kind
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: dsmask_kind

  REAL, PARAMETER  :: pi=ATAN(1.0)*4.
  CHARACTER(len=50) :: attribute
  REAL :: dx,dr
  REAL :: dx_obs=750. !viirs resolution

  REAL :: distance,weight,summ,sumw,sumi

  INTEGER :: nthreads,tid

  CHARACTER(len=8) :: cdatestart,cdateend
  CHARACTER(len=10) :: ctimestart,ctimeend
  REAL :: timestart,timeend

  IF (iargc() < 3) THEN
     WRITE(*,*)' Needs 2 input filenames and output filename'
     STOP
  ENDIF

  CALL getarg(1,inmaskfile)
  CALL getarg(2,inwrffile)
  CALL getarg(3,outfile)

  i=1
  OPEN(unit=11,file=inmaskfile,form='formatted')
  DO WHILE (.TRUE.)
     READ(11,'(2f13.5,a15,i3,f13.5)',END=100)latin(i),lonin(i),cidate,maskin(i),dsaindexin(i)
     IF (i > nmaxobs) THEN
        PRINT *,'increase nmaxobs, number of obs = ',i
        STOP
     ENDIF
!     IF (MOD(i,2) == 0) THEN
!        WRITE(110,'(2f15.7,i5)')latin(i,:),lonin(i,:),maskin(i)
!     ENDIF
     i=i+1
  ENDDO
  
100 CONTINUE
     
  nin=i-1

  filename=inwrffile

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "Time"

  CALL netcdfdimension(filename,3,dimstring,dims)
  
  nx=dims(1)
  ny=dims(2)
  nt=dims(3)

  varstringname='XLAT'

  ALLOCATE(lat(nx,ny,nt),lon(nx,ny,nt),mapscale(nx,ny,nt),maskout(nx,ny),&
       &dsaindexout(nx,ny))

  CALL readnetcdfdata3(filename,lat,varstringname,nx,ny,nt)

  varstringname='XLONG'

  CALL readnetcdfdata3(filename,lon,varstringname,nx,ny,nt)

  varstringname='MAPFAC_M'
  CALL readnetcdfdata3(filename,mapscale,varstringname,nx,ny,nt)
  attribute="DX"
  CALL globalattribute_real(filename,attribute,dx)

  latin(1:nin)=latin(1:nin)/180.*pi
  lonin(1:nin)=lonin(1:nin)/180.*pi
  lat=lat/180.*pi
  lon=lon/180.*pi

!  WRITE(100,'(2i5)')nx,ny

!$omp parallel default(none) &
  
!$omp private(i,j,k,tid,dr,summ,sumi,sumw,distance,weight,cdatestart,ctimestart,timestart,cdateend,ctimeend,timeend) shared(nx,ny,nin,nthreads,mapscale,lat,latin,lon,lonin,maskin,maskout,dsaindexin,dsaindexout,dx_obs,dx)

  tid = omp_get_thread_num()

  IF (tid == 0) THEN
     nthreads = omp_get_num_threads()
     PRINT *, 'nthreads = ',nthreads
  END IF

  DO i=1,nx

     IF (tid==0) THEN
        CALL DATE_AND_TIME(cdatestart,ctimestart)
        READ(ctimestart(3:10),'(f8.3)') timestart
     ENDIF

!$omp do schedule(static)

     DO j=1,ny
        dr=MAX(dx/(mapscale(i,j,1)*sqrt2)*grid_fraction,dx_obs)
        
        summ=0.
        sumi=0.
        sumw=0.
        
        DO k=1,nin
           distance=rearth*SQRT(&
                &(lat(i,j,1)-latin(k))**2+&
                &(COS(0.5*(lat(i,j,1)+latin(k)))*&
                &(lon(i,j,1)-lonin(k)))**2)
           weight=EXP(-(distance/dr)**2)

           IF (weight > epsilon) THEN
              summ=summ+weight*REAL(maskin(k))
              sumi=sumi+weight*REAL(dsaindexin(k))
              sumw=sumw+weight
           ENDIF
           
           IF (sumw > epsilon) THEN
              maskout(i,j)=summ/sumw
              dsaindexout(i,j)=sumi/sumw
           ELSE
              maskout(i,j)=0.
              dsaindexout(i,j)=0.
           ENDIF
           
        ENDDO

!        WRITE(101+tid,'(f15.7)')maskout(i,j)
        
     ENDDO

!$omp end do nowait


     IF (tid==0) THEN
        CALL DATE_AND_TIME(cdateend,ctimeend)
        READ(ctimeend(3:10),'(f8.3)') timeend
        PRINT *,timeend-timestart, ' tid==0'
     ENDIF

  ENDDO

!$omp end parallel

  OPEN(unit=outunit,file=filename,form='unformatted')
  WRITE(outunit)nx_kind,ny_kind
  WRITE(outunit)dsmask_kind
  CLOSE(outunit)

  DEALLOCATE(lat,lon,mapscale,maskout,dsaindexout)

END PROGRAM ds_mask_omp
