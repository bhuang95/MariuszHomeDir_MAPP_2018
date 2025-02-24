PROGRAM calc_mean_increment_fv3

!to claculate an mean increment of variables in a fv3 netcdf file

  USE netcdf_io

  IMPLICIT NONE

  INTEGER :: nx,ny,nz,nt,nxloc,nyloc,nzloc,ntloc
  INTEGER :: i,ivar
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: var,vardata
  CHARACTER(len=20), DIMENSION(3)                   :: dimstring
  INTEGER,           DIMENSION(3)                   :: dims
  CHARACTER(len=25)                                 :: varstringname
  CHARACTER(len=250) :: filenamein,filenameout,prefix
  INTEGER,PARAMETER :: nvarmax=50
  CHARACTER(len=10) :: varnames3d(1:nvarmax)='missing',&
       &varnames2d(1:nvarmax)='missing'
  INTEGER :: nvars2d,nvars3d
  LOGICAL :: isfile=.FALSE.
  INTEGER :: nens,iens
  CHARACTER(len=3) :: cnens,ciens
  CHARACTER(len=1) :: history !=T for history file, =F for restart file

  INTEGER :: iargc
  
! mpi definitions.
  INCLUDE 'mpif.h'
  INTEGER MPI_Status(MPI_STATUS_SIZE),numproc,nproc,iret
  
  NAMELIST /increment_mean_nml/ varnames2d,varnames3d
  
  CALL MPI_Init(iret)
! nproc is process number, numproc is total number of processes.
  CALL MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
  CALL MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)

  IF (iargc() < 2) THEN
     PRINT *,'Requires number of members, prefix as input'
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

  INQUIRE(file='increment_mean.nml', exist=isfile)
  
  IF ( .NOT. isfile ) THEN
     PRINT *,'Namelist ensmean.nl missing'
     PRINT *,'Stopping'
     STOP
  ENDIF
  
  OPEN(unit=101, file='increment_mean.nml', &
       form='formatted', status='old', action='read')
  READ(101, increment_mean_nml)
  CLOSE(101)
  
  nvars3d = COUNT(varnames3d .NE. 'missing')
  nvars2d = COUNT(varnames2d .NE. 'missing')

  filenameout=TRIM(prefix)//'.ensmean'
!  IF (nproc .EQ. 0) PRINT *,TRIM(filenameout)

  dimstring(1)="lon"
  dimstring(2)="lat"
  dimstring(3)="lev"
  
  CALL netcdfdimension(filenameout,3,dimstring,dims)  
  
  nx=dims(1)
  ny=dims(2)
  nz=dims(3)

  DO ivar=1,nvars3d
     
     nxloc=nx
     nyloc=ny
     nzloc=nz
     
     varstringname=varnames3d(ivar)
     
     IF (nproc == 0) PRINT *,varstringname

     iens=nproc + 1
     WRITE(ciens,'(i3.3)')iens
     filenamein=TRIM(prefix)//'.mem'//TRIM(ciens)
     
     IF (.NOT. ALLOCATED(var)) ALLOCATE(var(nxloc,nyloc,nzloc))
     IF (.NOT. ALLOCATED(vardata)) ALLOCATE(vardata(nxloc,nyloc,nzloc))
     
     var=0.
     
     CALL readnetcdfdata3(filenamein,vardata,varstringname,&
          &nxloc,nyloc,nzloc)
     
     CALL mpi_allreduce(vardata,var,SIZE(vardata),mpi_real,mpi_sum,mpi_comm_world,iret)
     
     IF (nproc .EQ. 0) THEN 
        var=var/REAL(nens)
        CALL writenetcdfdata3(filenameout,var,varstringname,&
             &nxloc,nyloc,nzloc)
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

     IF (.NOT. ALLOCATED(var)) ALLOCATE(var(nxloc,nyloc,nzloc))
     IF (.NOT. ALLOCATED(vardata)) ALLOCATE(vardata(nxloc,nyloc,nzloc))

     var=0.

     iens=nproc + 1
     WRITE(ciens,'(i3.3)')iens
     filenamein=TRIM(prefix)//'.mem'//TRIM(ciens)
     CALL readnetcdfdata3(filenamein,vardata(:,:,:),varstringname,&
          &nxloc,nyloc,nzloc)

     CALL mpi_allreduce(vardata,var,SIZE(vardata),mpi_real,mpi_sum,mpi_comm_world,iret)

     IF (nproc .EQ. 0) THEN 
        var=var/REAL(nens)
        CALL writenetcdfdata3(filenameout,var(:,:,:),varstringname,&
             &nxloc,nyloc,nzloc)
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

END PROGRAM calc_mean_increment_fv3
