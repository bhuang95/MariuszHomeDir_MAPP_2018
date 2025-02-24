PROGRAM dust_mask

!uses Barnes analysis approach

  USE netcdf_io

  IMPLICIT NONE

  INTEGER, PARAMETER :: nmaxobs=1e6, npasses=2
  REAL, PARAMETER :: rearth=6371.e3, epsilon=1.e-6, gamma_o=0.5

  REAL, DIMENSION(nmaxobs) :: latin,lonin 
  INTEGER, DIMENSION(nmaxobs) :: maskin,maskin_npass

  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  CHARACTER(len=10)                                 :: varstringname
  INTEGER,           DIMENSION(4)                   :: dims
  REAL, ALLOCATABLE :: lat(:,:,:), lon(:,:,:),mapscale(:,:,:),&
       &maskout(:,:)
  INTEGER :: nx,ny,nt,nout,nin,i,j,k,it
  CHARACTER*250 :: inwrffile,inmaskfile,outfile,filename 
  CHARACTER(len=10) :: cidate

  REAL, PARAMETER  :: pi=ATAN(1.0)*4.
  CHARACTER(len=50) :: attribute
  REAL :: dx,dn,dx_dn=0.5 !  0.333 < dx_dn  < 0.5 !(based on 1983 MWR)

!Barnes parameters
  REAL, PARAMETER :: steve_koch=5.052*(2./pi)**2 !(based on 1983 MWR)
  REAL :: gamma,kappa

  REAL :: distance,weight,summ,sumw
  REAL, DIMENSION(4) :: distances
  INTEGER, DIMENSION(4,2) :: indices

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

  maskout=0.
  maskin_npass=0.

  DO it=1,npasses
     IF (it==1) THEN
        gamma=1.
     ELSE
        gamma=gamma_o
     ENDIF

     WRITE(100+it,'(2i5)')nx,ny


     DO i=1,nx
        DO j=1,ny
           dn=(dx/mapscale(i,j,1))/dx_dn
           kappa=steve_koch*dn**2*gamma
           
           summ=0.
           sumw=0.

           distances=1.e9
           indices=1e9

           DO k=1,nin
              distance=rearth*SQRT(&
                   &(lat(i,j,1)-latin(k))**2+&
                   &(COS(0.5*(lat(i,j,1)+latin(k)))*&
                   &(lon(i,j,1)-lonin(k)))**2)
              weight=EXP(-distance**2/kappa)
              IF (weight > epsilon) THEN
                 IF (distance < distances(1)) THEN
                    distances(2:4)=distances(1:3)
                    distannces(1)=distance
                    indices(2:4,:)=indices(1:3,:)
                    indices(1,1)=i
                    indices(1,2)=j
                 ENDIF
                 summ=summ+weight*(REAL(maskin(k))-maskin_npass(k))
                 sumw=sumw+weight
           ENDDO

           IF (sumw > epsilon) THEN
              maskout(i,j)=maskout(i,j)+summ/sumw
           ELSE
              maskout(i,j)=0.
           ENDIF

           WRITE(100+it,'(f15.7)')maskout(i,j)

        ENDDO
     ENDDO

  ENDDO

END PROGRAM dust_mask
