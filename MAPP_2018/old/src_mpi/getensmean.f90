PROGRAM getensmean

!to get an ensemble  mean of variables in a netcdf file

  USE netcdf_io

  IMPLICIT NONE

  INTEGER :: nx,ny,nz,nt,nxloc,nyloc,nzloc
  INTEGER :: i,ivar
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: var,vardata
  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  INTEGER,           DIMENSION(4)                   :: dims
  CHARACTER(len=25)                                 :: varstringname
  CHARACTER(len=250) :: filenamein,filenameout,prefix
  INTEGER,PARAMETER :: nvarmax=50
  CHARACTER(len=25) :: varnames3d(1:nvarmax)='missing',&
       &varnames2d(1:nvarmax)='missing'
  INTEGER :: nvars2d,nvars3d
  LOGICAL :: isfile=.FALSE.,logemiss=.FALSE.
  INTEGER :: nens,iens
  CHARACTER(len=3) :: cnens,ciens

  INTEGER :: iargc
  
! mpi definitions.
  INCLUDE 'mpif.h'
  INTEGER MPI_Status(MPI_STATUS_SIZE),numproc,nproc,iret
  
  NAMELIST /ensmean_nl/ logemiss,varnames2d,varnames3d
  
  CALL MPI_Init(iret)
! nproc is process number, numproc is total number of processes.
  CALL MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
  CALL MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)

  IF (iargc() < 2) THEN
     PRINT *,'Requires number of members and prefix with member location as input'
     PRINT *,'Stopping'
     CALL MPI_Abort(MPI_COMM_WORLD,101,iret)
  ENDIF

  CALL getarg(1,cnens)
  READ(cnens,'(i3)') nens
  CALL getarg(2,prefix)

 IF (nens /= numproc) THEN
     PRINT *,'Number of members = ',nens
     PRINT *,'Number of processors = ',numproc
     PRINT *,'nens needs to equal numproc'
     PRINT *,'Stopping'
     CALL MPI_Abort(MPI_COMM_WORLD,102,iret)
  ENDIF

  INQUIRE(file='ensmean.nl', exist=isfile)
  
  IF ( .NOT. isfile ) THEN
     PRINT *,'Namelist ensmean.nl missing'
     PRINT *,'Stopping'
     STOP
  ENDIF
  
  OPEN(unit=101, file='ensmean.nl', &
       form='formatted', status='old', action='read')
  READ(101, ensmean_nl)
  CLOSE(101)
  
  nvars3d = COUNT(varnames3d .NE. 'missing')
  nvars2d = COUNT(varnames2d .NE. 'missing')

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  IF (logemiss) THEN
     dimstring(3) = "emissions_zdim"
     nvars2d=0
  ELSE
     dimstring(3) = "bottom_top"
  ENDIF
  dimstring(4) = "Time"

  filenameout=TRIM(prefix)//'.ensmean'
  IF (nproc .EQ. 0) PRINT *,TRIM(filenameout)
  
  CALL netcdfdimension(filenameout,4,dimstring,dims)  
  
  nx=dims(1)
  ny=dims(2)
  nz=dims(3)
  nt=dims(4)
  
  DO ivar=1,nvars3d

     nxloc=nx
     nyloc=ny
     nzloc=nz

     varstringname=varnames3d(ivar)
     IF (nproc == 0) PRINT *,varstringname

     IF (varstringname=='U') THEN
        nxloc=nx+1
     ELSE IF (varstringname=='V') THEN
        nyloc=ny+1
     ELSE IF (varstringname=='W' .OR. varstringname=='PH') THEN
        nzloc=nz+1
     ENDIF

     IF (.NOT. ALLOCATED(var)) ALLOCATE(var(nxloc,nyloc,nzloc,nt))
     IF (.NOT. ALLOCATED(vardata)) ALLOCATE(vardata(nxloc,nyloc,nzloc,nt))

     var=0.

     iens=nproc + 1
     WRITE(ciens,'(i3.3)')iens
     filenamein=TRIM(prefix)//'.mem'//TRIM(ciens)
     CALL readnetcdfdata4(filenamein,vardata,varstringname,&
          &nxloc,nyloc,nzloc,nt)

     CALL mpi_allreduce(vardata,var,SIZE(vardata),mpi_real,mpi_sum,mpi_comm_world,iret)

     IF (nproc .EQ. 0) THEN 
        var=var/REAL(nens)
        CALL writenetcdfdata4(filenameout,var,varstringname,&
             &nxloc,nyloc,nzloc,nt)
     ENDIF

     IF (ALLOCATED(var)) DEALLOCATE(var)
     IF (ALLOCATED(vardata)) DEALLOCATE(vardata)

  ENDDO

  DO ivar=1,nvars2d

     varstringname=varnames2d(ivar)
     IF (nproc == 0) PRINT *,varstringname

     nxloc=nx
     nyloc=ny
     nzloc=1

     IF (.NOT. ALLOCATED(var)) ALLOCATE(var(nxloc,nyloc,nt,1))
     IF (.NOT. ALLOCATED(vardata)) ALLOCATE(vardata(nxloc,nyloc,nt,1))

     var=0.

     iens=nproc + 1
     WRITE(ciens,'(i3.3)')iens
     filenamein=TRIM(prefix)//'.mem'//TRIM(ciens)
     CALL readnetcdfdata3(filenamein,vardata(:,:,:,1),varstringname,&
          &nxloc,nyloc,nt)

     CALL mpi_allreduce(vardata,var,SIZE(vardata),mpi_real,mpi_sum,mpi_comm_world,iret)

     IF (nproc .EQ. 0) THEN 
        var=var/REAL(nens)
        CALL writenetcdfdata3(filenameout,var(:,:,:,1),varstringname,&
             &nxloc,nyloc,nt)
     ENDIF

     IF (ALLOCATED(var)) DEALLOCATE(var)
     IF (ALLOCATED(vardata)) DEALLOCATE(vardata)

  ENDDO

  CALL MPI_Barrier(MPI_COMM_WORLD,iret)
  IF (nproc .EQ. 0) WRITE(6,*) 'all done!'
  CALL MPI_Finalize(iret)
  IF (iret .NE. 0) THEN
     PRINT *, 'MPI_Finalize error status = ',iret
  END IF

END PROGRAM getensmean

