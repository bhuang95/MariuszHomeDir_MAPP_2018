MODULE fv3_interface

!=======================================================================

! define associated modules and subroutines

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

  USE mpi_interface
  USE namelist_def
  USE netcdfio_interface
  USE variable_interface


!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------

! define all data and structure types for routine; these variables
! are variables required by the subroutines within this module

  TYPE analysis_grid
     CHARACTER(len=500)                                            :: filename
     CHARACTER(len=500)                                            :: filename2d
     INTEGER                                                       :: nx
     INTEGER                                                       :: ny
     INTEGER                                                       :: nz
     INTEGER                                                       :: ntime
  END TYPE analysis_grid ! type analysis_grid

! define global variables

  INTEGER n2dvar,n3dvar,ntvars,nrecs,nvvars
  REAL(r_single),                   DIMENSION(:,:,:,:),  ALLOCATABLE :: fv3_var_3d
  REAL(r_single),                   DIMENSION(:,:,:),    ALLOCATABLE :: fv3_var_2d                   

!-----------------------------------------------------------------------

! define interfaces and attributes for module routines

  PRIVATE

!-----------------------------------------------------------------------

CONTAINS

!-----------------------------------------------------------------------

  SUBROUTINE fv3_regrid_nemsio()

! define variables computed within routine

    IMPLICIT NONE
    TYPE(analysis_grid)                                                  :: anlygrd(ngrids)
    TYPE(varinfo), ALLOCATABLE, DIMENSION(:)                             :: var_info,var_info2d,var_info3d
    TYPE(gridvar)                                                        :: invar,invar2
    CHARACTER(len=20)                                                    :: var_name
    CHARACTER(len=20)                                                    :: nems_levtyp

    REAL(r_single),               DIMENSION(:),                ALLOCATABLE :: pk
    REAL(r_single),               DIMENSION(:),                ALLOCATABLE :: bk
    REAL, DIMENSION(:),  ALLOCATABLE :: sendbuffer,recvbuffer
    INTEGER                                                              :: fhour
    INTEGER                                                              :: ncoords
    INTEGER nems_lev,ndims,istatus,ncol,levs_fix
    LOGICAL clip

! define counting variables

    INTEGER                                                              :: i, j, k, l,nlev,k2,k3,nrec

!=====================================================================

! define local variables

! loop through local variables

    IF(mpi_procid .EQ. mpi_masternode) THEN
       PRINT *,'variable table'
       PRINT *,'--------------'
       OPEN(912,file=TRIM(variable_table),form='formatted')
       ntvars=0; n2dvar=0; n3dvar=0
       nrecs = 0
       istatus=0
       loop_read:  DO WHILE (istatus == 0)
          READ(912,199,iostat=istatus) var_name,nems_name,nems_levtyp,nems_lev,itrptyp,clip,ndims
          IF( istatus /= 0 ) EXIT loop_read

          nrecs = nrecs + 1
          IF(var_name(1:1) .NE. "#") THEN
             ntvars = ntvars + 1
             ntvars = ntvars + 1
             IF (ndims == 2) THEN
                n2dvar = n2dvar+1
             ELSE IF (ndims == 3) THEN
                n3dvar = n3dvar+1
             ELSE
                PRINT *,'ndims must be 2 or 3 in variable_table.txt'
                CALL mpi_abort(mpi_comm_world,-91,mpi_ierror)
                STOP
             ENDIF
!print *,'ntvars,n2dvar,n3dvar',ntvars,n2dvar,n3dvar
!write(6,199) var_name, nems_name,nems_levtyp,nems_lev,itrptyp,clip,ndims
          ENDIF
       ENDDO loop_read
       CLOSE(912)
       PRINT *,'nrecs,ntvars,n2dvar,n3dvar',nrecs,ntvars,n2dvar,n3dvar
    ENDIF
    CALL mpi_bcast(nrecs,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    CALL mpi_bcast(n2dvar,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    CALL mpi_bcast(n3dvar,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    CALL mpi_bcast(ntvars,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    IF (ntvars == 0) THEN
       PRINT *,'empty variable_table.txt!'
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    ALLOCATE(var_info(ntvars))
    OPEN(912,file=TRIM(variable_table),form='formatted')
    k = 0
    nvvars = 0 ! number of vector variables
    DO nrec = 1, nrecs
       READ(912,199,iostat=istatus) var_name,nems_name,nems_levtyp,nems_lev,itrptyp,clip,ndims
       IF (var_name(1:1) .NE. "#") THEN
          k = k + 1
          var_info(k)%var_name = var_name
          var_info(k)%nems_name = nems_name
          var_info(k)%nems_levtyp = nems_levtyp
          var_info(k)%nems_lev = nems_lev
          var_info(k)%itrptyp = itrptyp
	  IF (itrptyp.EQ.'vector') THEN
             nvvars=nvvars+1
	  ENDIF
          var_info(k)%clip = clip
          var_info(k)%ndims = ndims
          IF(mpi_procid .EQ. mpi_masternode) THEN
             WRITE(6,199) var_info(k)%var_name, var_info(k)%nems_name,var_info(k)%nems_levtyp, &
                  var_info(k)%nems_lev,var_info(k)%itrptyp,var_info(k)%clip,var_info(k)%ndims
          ENDIF
       ENDIF
    END DO ! do k = 1, ntvars
! assume vectors are in pairs
    nvvars=nvvars/2
    CALL mpi_bcast(nvvars,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    CLOSE(912)
199 FORMAT(a20,1x,a20,1x,a20,1x,i1,1x,a20,1x,l1,1x,i1)
    ALLOCATE(var_info3d(n3dvar+2))
    ALLOCATE(var_info2d(n2dvar))
    k2 = 0
    k3 = 0
    DO k=1,ntvars
       IF (var_info(k)%ndims == 2) THEN
          k2 = k2 + 1
          var_info2d(k2) = var_info(k)
       ENDIF
       IF (var_info(k)%ndims == 3 .OR.  &
            TRIM(var_info(k)%nems_name) == 'pres' .OR. &
            TRIM(var_info(k)%nems_name) == 'orog') THEN
          k3 = k3 + 1
          var_info3d(k3) = var_info(k)
! orography called 'hgt' in 3d file, not 'orog'
          IF (TRIM(var_info(k)%nems_name) == 'orog') THEN
             var_info3d(k3)%nems_name = 'hgt                 '
          ENDIF
       ENDIF
    ENDDO


    DO i = 1, ngrids
       anlygrd(i)%filename = analysis_filename(i)
       anlygrd(i)%filename2d = analysis_filename2d(i)
       CALL fv3_regrid_initialize(anlygrd(i))
    END DO ! do i = 1, ngrids

! define local variables

    ncxdim                 = anlygrd(1)%nx
    ncydim                 = anlygrd(1)%ny
    IF (n3dvar > 0) THEN
       nczdim               = anlygrd(1)%nz
    ELSE
       nczdim               = 0
    ENDIF
    nctdim                 = anlygrd(1)%ntime
    ncoords                = ncxdim*ncydim
    invar%ncoords          = ncoords*ngrids
    invar2%ncoords          = ncoords*ngrids
    CALL mpi_barrier(mpi_comm_world,mpi_ierror)

! allocate memory for local variables

    IF(.NOT. ALLOCATED(fv3_var_2d) .AND. n2dvar > 0)                        &
         & ALLOCATE(fv3_var_2d(ngrids,ncxdim,ncydim))
    IF (mpi_nprocs /= nczdim+1) THEN
       CALL mpi_barrier(mpi_comm_world, mpi_ierror)
       IF (mpi_procid .EQ. mpi_masternode) THEN
          PRINT *,'number of mpi tasks must be equal to number of levels + 1'
          PRINT *,'mpi procs = ',mpi_nprocs,' levels = ',nczdim
       ENDIF
       CALL mpi_interface_terminate()
       STOP
    ENDIF
!print *,'allocate fv3_var_3d',ngrids,ncxdim,ncydim,nczdim,mpi_procid
    IF(.NOT. ALLOCATED(fv3_var_3d) .AND. n3dvar > 0)                        &
         & ALLOCATE(fv3_var_3d(ngrids,ncxdim,ncydim,nczdim))
!print *,'done allocating fv3_var_3d',ngrids,ncxdim,ncydim,nczdim,mpi_procid

! check local variable and proceed accordingly

    CALL mpi_barrier(mpi_comm_world,mpi_ierror)
    IF(mpi_procid .EQ. mpi_masternode) THEN

! allocate memory for local variables

       IF (n3dvar > 0) THEN
          IF(.NOT. ALLOCATED(pk)) ALLOCATE(pk(nczdim+1))
          IF(.NOT. ALLOCATED(bk)) ALLOCATE(bk(nczdim+1))

! define local variables

          CALL netcdfio_values_1d(anlygrd(1)%filename,'pk',pk)
          CALL netcdfio_values_1d(anlygrd(1)%filename,'bk',bk)
       ENDIF

    END IF ! if(mpi_procid .eq. mpi_masternode)

    DO l = 1, nctdim

       ncrec = l  ! time level to read from netcdf file

! define local variables

! wait here.
       CALL mpi_barrier(mpi_comm_world,mpi_ierror)

! loop through local variables
       k2=1
       DO k = 1, ntvars - nvvars

! do 2d variables.

          IF(var_info(k2)%ndims .EQ. 2) THEN

! check local variable and proceed accordingly

             IF(mpi_procid .EQ. mpi_masternode) THEN

! check local variable and proceed accordingly

                CALL fv3_grid_read(anlygrd(1:ngrids), var_info(k2)%var_name,.TRUE.,.FALSE.)

! define local variables
                CALL mpi_barrier(mpi_comm_world,mpi_ierror)

             ENDIF

          END IF ! if(var_info(k2)%ndims .eq. 2)

! do 3d variables.

          IF(var_info(k2)%ndims .EQ. 3) THEN

! read 3d grid on master node, send to other tasks
             IF(mpi_procid .EQ. mpi_masternode) THEN
                CALL fv3_grid_read(anlygrd(1:ngrids), var_info(k2)%var_name,.FALSE.,.TRUE.)
                DO nlev=1,nczdim
                   CALL mpi_send(fv3_var_3d(1,1,1,nlev),ngrids*ncxdim*ncydim,mpi_real,&
                        nlev,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
                ENDDO
             ENDIF

          ELSE IF (mpi_procid .LE. nczdim) THEN

! do interpolation, one level on each task.
             CALL mpi_recv(fv3_var_3d(1,1,1,mpi_procid),ngrids*ncxdim*ncydim,mpi_real,&
                  0,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
          END IF ! if(mpi_procid .ne. mpi_masternode .and.             &
! mpi_procid .le. nczdim)

! gather results back on root node to write out.

          k2=k2+1
       END DO ! do k = 1, ntvars

! wait here.

       CALL mpi_barrier(mpi_comm_world,mpi_ierror)

! finalize  and cleanup

       IF(ALLOCATED(workgrid)) DEALLOCATE(workgrid)

    END DO ! do l = 1, nctdim


!=====================================================================

  END SUBROUTINE fv3_regrid_nemsio

!=======================================================================

! fv3_regrid_initialize.f90:

!-----------------------------------------------------------------------

  SUBROUTINE fv3_regrid_initialize(grid)

! define variables passed to routine

    IMPLICIT NONE
    TYPE(analysis_grid)                                                  :: grid

!=====================================================================

! define local variables

    CALL netcdfio_dimension(grid%filename,'grid_xt',grid%nx)
    CALL netcdfio_dimension(grid%filename,'grid_yt',grid%ny)
    IF (n3dvar > 0) THEN
       CALL netcdfio_dimension(grid%filename,'pfull',grid%nz)
    ELSE
       grid%nz = 0
    ENDIF
    CALL netcdfio_dimension(grid%filename,'time',grid%ntime)

!=====================================================================

  END SUBROUTINE fv3_regrid_initialize

!=======================================================================

! fv3_grid_read.f90:

!-----------------------------------------------------------------------

  SUBROUTINE fv3_grid_read(anlygrd,varname,is_2d,is_3d)

! define variables passed to subroutine

    TYPE(analysis_grid)                                                  :: anlygrd(ngrids)
    CHARACTER(len=20)                                                    :: varname
    LOGICAL                                                              :: is_2d
    LOGICAL                                                              :: is_3d

! define counting variables

    INTEGER                                                              :: i, j, k

!=====================================================================

! loop through local variable

    DO k = 1, ngrids

! check local variable and proceed accordingly

       IF(debug) WRITE(6,500) ncrec, k
       IF(is_2d) THEN

! define local variables

! orog and psfc are in 3d file.
          IF (TRIM(varname) == 'orog' .OR. TRIM(varname) == 'psfc') THEN
             CALL netcdfio_values_2d(anlygrd(k)%filename,varname,             &
                  & fv3_var_2d(k,:,:))
          ELSE
             CALL netcdfio_values_2d(anlygrd(k)%filename2d,varname,             &
                  & fv3_var_2d(k,:,:))
          ENDIF

       END IF ! if(is_2d)

! check local variable and proceed accordingly

       IF(is_3d) THEN

! define local variables

          CALL netcdfio_values_3d(anlygrd(k)%filename,varname,             &
               & fv3_var_3d(k,:,:,:))

       END IF ! if(is_3d)

    END DO ! do k = 1, ngrids

!=====================================================================

! define format statements

500 FORMAT('fv3_grid_read: time record = ', i6, '; cubed sphere face = ',  &
         & i1,'.')

!=====================================================================

  END SUBROUTINE fv3_grid_read

!=======================================================================

! fv3_grid_fhour.f90:

!-----------------------------------------------------------------------

  SUBROUTINE fv3_grid_fhour(grid,fhour)

! define variables passed to routine

    IMPLICIT NONE
    TYPE(analysis_grid)                                                  :: grid
    INTEGER                                                              :: fhour

    REAL(r_single)                                                         :: start_jday
    REAL(r_single)                                                         :: fcst_jday
    INTEGER                                                              :: year
    INTEGER                                                              :: month
    INTEGER                                                              :: day
    INTEGER                                                              :: hour
    INTEGER                                                              :: minute
    INTEGER                                                              :: second, iw3jdn
    CHARACTER(len=80) timeunits

!=====================================================================

! define local variables

    READ(forecast_timestamp(1:4),  '(i4)') year
    READ(forecast_timestamp(5:6),  '(i2)') month
    READ(forecast_timestamp(7:8), '(i2)') day
    READ(forecast_timestamp(9:10),'(i2)') hour
    minute = 0; second = 0

! compute local variables

! 'flux day' (days since dec 31 1900)
    CALL date2wnday(start_jday,year,month,day)
! same as above, but valid after 2099 
    start_jday=REAL(iw3jdn(year,month,day)-iw3jdn(1900,12,31))
    start_jday = start_jday + REAL(hour)/24.0 + REAL(minute)/1440.0 + &
         & REAL(second)/86400.0

! define local variables

    CALL netcdfio_values_1d(grid%filename,'time',workgrid)
    CALL netcdfio_variable_attr(grid%filename,'time','units',timeunits)

! compute local variables

! ncrec is a global variable in the netcdfio-interface module
    IF (timeunits(1:4) == 'days') THEN
       fcst_jday = start_jday + workgrid(ncrec)
    ELSE IF (timeunits(1:5) == 'hours') THEN
       fcst_jday = start_jday + workgrid(ncrec)/24.
    ELSE IF (timeunits(1:7) == 'seconds') THEN
       fcst_jday = start_jday + workgrid(ncrec)/86400.0
    ELSE
       PRINT *,'unrecognized time units',TRIM(timeunits)
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    fhour     = NINT((86400*(fcst_jday - start_jday))/3600.0)

!===================================================================== 

  END SUBROUTINE fv3_grid_fhour

  SUBROUTINE date2wnday(wday, iyr,mon,idy)
    IMPLICIT NONE
    INTEGER iyr,mon,idy
    REAL    wday
!!
!!**********
!!*
!!  1) convert date into 'flux day'.
!!
!!  2) the 'flux day' is the number of days since 001/1901 (which is 
!!      flux day 1.0).
!!     for example:
!!      a) iyr=1901,mon=1,idy=1, represents 0000z hrs on 01/01/1901
!!         so wday would be 1.0.
!!      a) iyr=1901,mon=1,idy=2, represents 0000z hrs on 02/01/1901
!!         so wday would be 2.0.
!!     year must be no less than 1901.0, and no greater than 2099.0.
!!     note that year 2000 is a leap year (but 1900 and 2100 are not).
!!
!!  3) alan j. wallcraft, naval research laboratory, july 2002.
!!*
!!**********
!!
    INTEGER nleap
    REAL    wday1
    REAL month(13)
    DATA month / 0,  31,  59,  90, 120, 151, 181, &
         212, 243, 273, 304, 334, 365 /
!     find the right year.
    nleap = (iyr-1901)/4
    wday  = 365.0*(iyr-1901) + nleap + month(mon) + idy
    IF     (MOD(iyr,4).EQ.0 .AND. mon.GT.2) THEN
       wday  = wday + 1.0
    ENDIF
  END SUBROUTINE date2wnday

!=======================================================================

END MODULE fv3_interface
