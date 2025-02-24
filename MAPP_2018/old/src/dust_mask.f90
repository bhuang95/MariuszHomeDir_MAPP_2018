PROGRAM dust_mask

!uses Barnes analysis approach signle pass only

  USE netcdf_io

  IMPLICIT NONE

  INTEGER, PARAMETER :: nmaxobs=1e7
  REAL, PARAMETER :: rearth=6371.e3, epsilon=1.e-6, grid_fraction=1.5

  REAL, DIMENSION(nmaxobs) :: latin,lonin 
  INTEGER, DIMENSION(nmaxobs) :: maskin

  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  CHARACTER(len=25)                                 :: varstringname
  INTEGER,           DIMENSION(4)                   :: dims
  REAL, ALLOCATABLE :: lat(:,:,:), lon(:,:,:),mapscale(:,:,:),&
       &maskout(:,:)
  INTEGER :: nx,ny,nt,nin,i,j,k
  CHARACTER*250 :: inwrffile,inmaskfile,outfile,filename 
  CHARACTER(len=10) :: cidate
  INTEGER :: outunit=101

  REAL, PARAMETER  :: pi=ATAN(1.0)*4.
  CHARACTER(len=50) :: attribute
  REAL :: dx,dr
  REAL :: dx_obs=750. !viirs resolution

  REAL :: distance,weight,summ,sumw

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
     READ(11,'(2f13.5,a15,i3)',END=100)latin(i),lonin(i),cidate,maskin(i)
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

  ALLOCATE(lat(nx,ny,nt),lon(nx,ny,nt),mapscale(nx,ny,nt),maskout(nx,ny))

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

  WRITE(100,'(2i5)')nx,ny
  
  DO i=1,nx
     IF (MOD(i,100)==0) PRINT *,i,'th column'
     DO j=1,ny
        dr=MAX(dx/mapscale(i,j,1)*grid_fraction,dx_obs)
        
        summ=0.
        sumw=0.
        
        DO k=1,nin
           distance=rearth*SQRT(&
                &(lat(i,j,1)-latin(k))**2+&
                &(COS(0.5*(lat(i,j,1)+latin(k)))*&
                &(lon(i,j,1)-lonin(k)))**2)
           weight=EXP(-(distance/dr)**2)

           IF (weight > epsilon) THEN
              summ=summ+weight*REAL(maskin(k))
              sumw=sumw+weight
           ENDIF
           
           IF (sumw > epsilon) THEN
              maskout(i,j)=summ/sumw
           ELSE
              maskout(i,j)=0.
           ENDIF
           
        ENDDO

!        WRITE(100,'(f15.7)')maskout(i,j)
        
     ENDDO
  ENDDO

  OPEN(unit=outunit,file=outfile,form='unformatted')
  WRITE(outunit)nx,ny
  WRITE(outunit)maskout
  CLOSE(outunit)

END PROGRAM dust_mask
