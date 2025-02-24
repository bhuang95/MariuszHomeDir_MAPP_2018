PROGRAM smoke_weights

!Mariusz Pagowski Nov, 2016
!to calculate optimal least-square non-negative weights for smoke-> gocart conversion 
!using wnnls from slatec

  USE netcdf_io

  IMPLICIT NONE

  INTEGER, PARAMETER :: maxdata=1e5
  REAL, PARAMETER :: aod_threshold=1.0
  REAL, ALLOCATABLE, DIMENSION(:,:) :: w, wsave
  REAL, ALLOCATABLE, DIMENSION(:) :: x
  REAL :: rnorm,rnorme,rnorml
  INTEGER :: mode
  INTEGER :: mdw,me,ma,mg,n,l,i,j,k,nx,ny,nt,ii,jj,kk,neqs,ieqs

  REAL, ALLOCATABLE, DIMENSION(:) :: work
  INTEGER, ALLOCATABLE, DIMENSION(:) :: iwork
  REAL,  DIMENSION(1) :: prgopt_w
  REAL,  DIMENSION(3) :: prgopt_l

  CHARACTER(len=250), ALLOCATABLE, DIMENSION(:) :: &
       &filegocart,filebc1,fileoc1,filep25,filebc2,fileoc2

  INTEGER, PARAMETER :: nvardims =3
  CHARACTER(len=20), DIMENSION(nvardims) :: dimstring=&
       &(/"west_east","south_north","Time"/)
  INTEGER, DIMENSION(nvardims) :: end_dims

  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: aodgocart,aodbc1,&
       &aodoc1,aodp25,aodbc2,aodoc2

  CHARACTER(len=250) :: filegocart_template,filebc1_template,&
       &fileoc1_template,filep25_template,filebc2_template,&
       &fileoc2_template

  CHARACTER(len=250) :: fileout_weights

  CHARACTER (LEN=120) :: command
  CHARACTER(len=25) :: varname='AOD'

  LOGICAL :: bc2oc2=.FALSE.

  INTEGER :: loslen,ndates
  INTEGER :: iargc

  IF (iargc() < 5) THEN
     PRINT *,"needs at least four input files : filegocart, filebc1, fileoc1, filep25"
     PRINT *,"and output file:  weights"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,filegocart_template)
  CALL getarg(2,filebc1_template)
  CALL getarg(3,fileoc1_template)
  CALL getarg(4,filep25_template)

  IF (iargc() == 5) THEN
     CALL getarg(5,fileout_weights)
     bc2oc2=.FALSE.
  ELSE IF (iargc() == 7) THEN
     CALL getarg(5,filebc2_template)
     CALL getarg(6,fileoc2_template)
     CALL getarg(7,fileout_weights)
     bc2oc2=.TRUE.
  ELSE
     PRINT *,'only 5 or 7 inputs possible. Stopping'
     STOP
  ENDIF

  loslen = LEN ( command )
  CALL all_spaces ( command , loslen )

  WRITE ( command , FMT='("ls -1 ",A," > .foo")' ) &
       &TRIM(filegocart_template)//'.??.nc'

  CALL SYSTEM ( TRIM ( command ) )
  CALL SYSTEM ( '( cat .foo | wc -l > .foo1 )' )

  OPEN (FILE   = '.foo1'       , &
       UNIT   = 112           , &
       STATUS = 'OLD'         , &
       ACCESS = 'SEQUENTIAL'  , &
       FORM   = 'FORMATTED'     )

  READ ( 112 , * ) ndates
  CLOSE ( 112 )
  PRINT *,'There are ndates = ',ndates,' forecast dates'

  ALLOCATE(filegocart(ndates),filebc1(ndates),fileoc1(ndates),&
       &filep25(ndates))

  IF (bc2oc2) ALLOCATE(filebc2(ndates),fileoc2(ndates))

  WRITE ( command , FMT='("ls -1 ",A," >> .foo")' ) &
       &TRIM(filebc1_template)//'.??.nc'
  CALL SYSTEM ( TRIM ( command ) )
  WRITE ( command , FMT='("ls -1 ",A," >> .foo")' ) &
       &TRIM(fileoc1_template)//'.??.nc'
  CALL SYSTEM ( TRIM ( command ) )
  WRITE ( command , FMT='("ls -1 ",A," >> .foo")' ) &
       &TRIM(filep25_template)//'.??.nc'
  CALL SYSTEM ( TRIM ( command ) )

  IF (bc2oc2) THEN
     WRITE ( command , FMT='("ls -1 ",A," >> .foo")' ) &
          &TRIM(filebc2_template)//'.??.nc'
     CALL SYSTEM ( TRIM ( command ) )
     WRITE ( command , FMT='("ls -1 ",A," >> .foo")' ) &
          &TRIM(fileoc2_template)//'.??.nc'
     CALL SYSTEM ( TRIM ( command ) )
  ENDIF

  OPEN (FILE   = '.foo'        , &
       UNIT   = 111           , &
       STATUS = 'OLD'         , &
       ACCESS = 'SEQUENTIAL'  , &
       FORM   = 'FORMATTED'     )

!  Read all of the file names and store them.

  DO k = 1 , ndates
     READ ( 111 , FMT='(A)' ) filegocart(k)
  ENDDO
  DO k = 1 , ndates
     READ ( 111 , FMT='(A)' ) filebc1(k)
  ENDDO
  DO k = 1 , ndates
     READ ( 111 , FMT='(A)' ) fileoc1(k)
  ENDDO
  DO k = 1 , ndates
     READ ( 111 , FMT='(A)' ) filep25(k)
  ENDDO

  IF (bc2oc2) THEN
     
     DO k = 1 , ndates
        READ ( 111 , FMT='(A)' ) filebc2(k)
     ENDDO
     DO k = 1 , ndates
        READ ( 111 , FMT='(A)' ) fileoc2(k)
     ENDDO

  ENDIF

  CALL netcdfdimension(filegocart(1),nvardims,dimstring,end_dims)

  nx=end_dims(1)
  ny=end_dims(2)
  nt=end_dims(3)

  ALLOCATE(aodgocart(nx,ny,nt),aodbc1(nx,ny,nt),&
       &aodoc1(nx,ny,nt),aodp25(nx,ny,nt))

  mdw=maxdata+1

  IF (bc2oc2) THEN
     ALLOCATE(aodbc2(nx,ny,nt),aodoc2(nx,ny,nt))
     n=5
  ELSE
     n=3
  ENDIF

  ALLOCATE(w(mdw,n+1))

  w=0.

  ii=1  
  w(ii,:)=1. !sum of all weights=1

!weights have similar magnitudes
!  ii=2
!  w(ii,1)=1. !bc1 is 0.2 oc1
!  w(ii,2)=-1.
!  w(ii,3)=0.
!  w(ii,4)=0.
!  ii=3
!  w(ii,1)=0. !p25 is 1. oc1
!  w(ii,2)=1.
!  w(ii,3)=-1.
!  w(ii,4)=0.
!  ii=4
!  w(ii,1)=1. !p25 is 1. oc1
!  w(ii,2)=0.
!  w(ii,3)=-1.
!  w(ii,4)=0.
!
!  ieqs=3
!
  ii=1
  ieqs=0

  jj=0

  DO k=1,ndates
     CALL readnetcdfdata3(filegocart(k),aodgocart,varname,nx,ny,nt)
     CALL readnetcdfdata3(filebc1(k),aodbc1,varname,nx,ny,nt)
     CALL readnetcdfdata3(fileoc1(k),aodoc1,varname,nx,ny,nt)
     CALL readnetcdfdata3(filep25(k),aodp25,varname,nx,ny,nt)
     IF (bc2oc2) THEN
        CALL readnetcdfdata3(filebc2(k),aodbc2,varname,nx,ny,nt)
        CALL readnetcdfdata3(fileoc2(k),aodoc2,varname,nx,ny,nt)
     ENDIF

     jj=jj+COUNT(aodgocart > aod_threshold)
     PRINT *,jj
     DO i=1,nx
        DO j=1,ny
           IF (aodgocart(i,j,nt) > aod_threshold) THEN
              ii=ii+1
              IF (ii > maxdata) THEN
                 PRINT *,'increase array size mdw > ',maxdata
                 PRINT *,'Stopping'
                 STOP
              ENDIF
              w(ii,1)=aodbc1(i,j,nt)
              w(ii,2)=aodoc1(i,j,nt)
              w(ii,3)=aodp25(i,j,nt)
              IF (bc2oc2) THEN
                 w(ii,4)=aodbc2(i,j,nt)
                 w(ii,5)=aodoc2(i,j,nt)
                 w(ii,6)=aodgocart(i,j,nt)
              ELSE
                 w(ii,4)=aodgocart(i,j,nt)
              ENDIF
           ENDIF
        ENDDO
     ENDDO
  ENDDO

  neqs=ii
  PRINT *,neqs

  mdw=neqs
  me=1
  ma=mdw-me
  l=n
!  l=0
!when l=0 then need to set 3 eqs. above ieqs=3
!when l=n then do not eqs. above

  ALLOCATE(wsave(mdw,n+1),work(mdw+5*n),iwork(mdw+n),x(n))

  iwork(1)=mdw+5*n
  iwork(2)=mdw+n

  wsave=w
  DEALLOCATE(w)
  ALLOCATE(w(mdw,n+1))
  w=wsave

  prgopt_w(1)=1.
  CALL wnnls (w, mdw, me, ma, n, l, prgopt_w, x, rnorm, mode, iwork, work)

  OPEN(unit=101,file=fileout_weights,form='formatted',position='append')
  WRITE(101,'(i3)')n
  WRITE(101,'(f15.7)')x
  WRITE(101,'(i2,i7,f15.7,f7.3)')mode,neqs-1,rnorm,aod_threshold
  CLOSE(101)

  PRINT *,rnorm, mode

  PRINT *,x

!  OPEN(unit=102,file=fileout_scatter,form='unformatted')
!  WRITE(102)neqs-1
!  WRITE(102)wsave(2:neqs,4),DOT_PRODUCT(wsave(2:neqs,1:3),x(2:neqs))
!  CLOSE(102)

  STOP

  mg=3
  mdw=neqs-ieqs+mg
  me=1
  ma=mdw-me-mg

  DEALLOCATE(iwork,work)
  DEALLOCATE(w)
  ALLOCATE(w(mdw,n+1))
  w(1,:)=1.
  w(2:,:)=wsave(ieqs+1:,:)
  w(neqs-ieqs+1:,:)=0.
  w(neqs-ieqs+1,1)=1.
  w(neqs-ieqs+2,2)=1.
  w(neqs-ieqs+3,3)=1.

  k=MAX(ma+mg,n)
  ALLOCATE(work(2*(me+n)+k+(mg+2)*(n+7)),iwork(mg+2*n+2))
  
  prgopt_l(1)=1.
  prgopt_l(2)=1.
  prgopt_l(3)=1.

  CALL lsei(w,mdw,me,ma,mg,n,prgopt_l(1:3),x,rnorme,rnorml,mode,work,iwork)

  PRINT *,rnorme,rnorml,mode

  PRINT *,x
  
  DO i=1,n
     PRINT *,w(i,1:n)!/w(i,i)
  ENDDO

  DEALLOCATE(iwork,work)
  DEALLOCATE(w,wsave)

!  PRINT *,'dotprod_1'

!  DO i=1,neqs
!     PRINT *,DOT_PRODUCT(wsave(i,1:3),x),wsave(i,4)
!  ENDDO

END PROGRAM smoke_weights

