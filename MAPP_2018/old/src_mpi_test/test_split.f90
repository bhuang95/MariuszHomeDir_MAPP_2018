PROGRAM test_split

  INTEGER, PARAMETER :: nx=2,ny=4,ntiles=6

  REAL, DIMENSION(nx,ny) :: domaing
  REAL, ALLOCATABLE, DIMENSION(:,:) :: domain
  CHARACTER(len=50) :: fname
  CHARACTER(len=1) :: cproc

  ! mpi definitions.
  INCLUDE 'mpif.h'
  INTEGER MPI_Status(MPI_STATUS_SIZE),numprocs,nproc,iret,ierr
  INTEGER :: color,key,newcomm,newnumprocs,newnproc,numprocs_per_tile,dx

  CALL MPI_Init(iret)
! nproc is process number, numprocs is total number of processes.

  CALL MPI_Comm_size(MPI_COMM_WORLD,numprocs,iret)
  CALL MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)

  IF (MOD(numprocs,ntiles) /= 0) CALL MPI_Abort(MPI_COMM_WORLD,101,iret)

  numprocs_per_tile=numprocs/ntiles

  IF (MOD(nx,numprocs_per_tile) /= 0) &
       &CALL MPI_Abort(MPI_COMM_WORLD,102,iret)
  dx=nx/numprocs_per_tile  

  ALLOCATE(domain(nx/numprocs_per_tile,ny))

  color=nproc/numprocs_per_tile
  key=MOD(nproc,numprocs_per_tile)
  IF (key == 0) THEN
     WRITE(cproc,'(i1)')(color+1)
     fname='test_split.txt.'//cproc
     OPEN(11,file=TRIM(fname),form='formatted')
     DO i=1,ny
        READ(11,*)domaing(:,i)
     ENDDO
  ENDIF

  CALL MPI_Comm_split(MPI_COMM_WORLD,color,key,newcomm,iret)

  CALL MPI_Bcast(domaing,nx*ny,MPI_real,0,newcomm,iret)

  domain=domaing(key+1:key+dx,:)

  CALL MPI_Comm_size(newcomm,newnumprocs,iret)
  CALL MPI_Comm_rank(newcomm,newnproc,iret)
  
  PRINT *,nproc,newnumprocs,newnproc,color+1,&
       &MINVAL(domaing),MAXVAL(domaing),&
       &MINVAL(domain),MAXVAL(domain)

  DEALLOCATE(domain)

  CALL MPI_Barrier(MPI_COMM_WORLD,iret)
  CALL MPI_Finalize(iret)
  
END PROGRAM test_split
