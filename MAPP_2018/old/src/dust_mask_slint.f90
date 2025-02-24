PROGRAM dust_mask

!to test slint working

  USE slint, ONLY: slint_init,bilinear_interp,nn_interp
  USE netcdf_io

  IMPLICIT NONE

  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  CHARACTER(len=25)                                 :: varstringname
  INTEGER,           DIMENSION(4)                   :: dims
  REAL, ALLOCATABLE :: lat(:,:,:), lon(:,:,:),maskout_nx_ny(:,:)
  INTEGER :: nx,ny,nt,nout,nin,i,j,ij
  CHARACTER*250 :: inwrffile,inmaskfile,outfile,filename 
  CHARACTER(len=10) :: cidate

  REAL, PARAMETER  :: pi=ATAN(1.0)*4.

  REAL, ALLOCATABLE, DIMENSION(:,:) :: latlonin 
  INTEGER, ALLOCATABLE, DIMENSION(:) :: maskin

  REAL, ALLOCATABLE, DIMENSION(:,:) :: latlonout 
  REAL, ALLOCATABLE, DIMENSION(:) :: maskout


  IF (iargc() < 3) THEN
     WRITE(*,*)' Needs 2 input filenames and output filename'
     STOP
  ENDIF

  CALL getarg(1,inwrffile)
  CALL getarg(2,inmaskfile)
  CALL getarg(3,outfile)

  filename=inwrffile

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "Time"

  CALL netcdfdimension(filename,3,dimstring,dims)
  
  nx=dims(1)
  ny=dims(2)
  nt=dims(3)

  varstringname='XLAT'

  ALLOCATE(lat(nx,ny,nt),lon(nx,ny,nt),maskout_nx_ny(nx,ny))

  CALL readnetcdfdata3(filename,lat,varstringname,nx,ny,nt)

  varstringname='XLONG'

  CALL readnetcdfdata3(filename,lon,varstringname,nx,ny,nt)

  nout=nx*ny

  ALLOCATE(latlonout(nout,2),maskout(nout))

  ij=0
  DO i=1,nx
     DO j=1,ny
        ij=ij+1
        latlonout(ij,1)=lat(i,j,1)/180.*pi
        latlonout(ij,2)=lon(i,j,1)/180.*pi
     ENDDO
  ENDDO

  ALLOCATE(latlonin(200000,2),maskin(200000))

  i=1
  OPEN(unit=11,file=inmaskfile,form='formatted')
  DO WHILE (.TRUE.)
     READ(11,'(2f13.5,a15,i3)',END=100)latlonin(i,1),latlonin(i,2),cidate,maskin(i)
!     IF (MOD(i,2) == 0) THEN
        WRITE(110,'(2f15.7,i5)')latlonin(i,:),maskin(i)
!     ENDIF

     i=i+1
  ENDDO
  
100 CONTINUE
  
  nin=i-1

  stop

  latlonin=latlonin/180.*pi

!  DO i=1,nin
!     PRINT *,latlonin(i,1),latlonin(i,2),maskin(i)
!  ENDDO

!  PRINT *,'***'

  CALL slint_init(latlonin,nin,latlonout,nout)

  CALL bilinear_interp(real(maskin),maskout)

!  DO i=1,nout
!     PRINT *,latlonout(i,1),latlonout(i,2),maskout(i)
!  ENDDO

  WRITE(120,'(2i7)')nx,ny

  ij=0
  DO i=1,nx
     DO j=1,ny
        ij=ij+1
        maskout_nx_ny(i,j)=maskout(ij)
        WRITE(120,'(f15.7)')maskout_nx_ny(i,j)
     ENDDO
  ENDDO


!  PRINT *,'***'

!  CALL nn_interp(topoin,topoout)

!  DO i=1,nout
!     PRINT *,latlonout(i,1),latlonout(i,2),topoout(i)
!  ENDDO

END PROGRAM dust_mask
