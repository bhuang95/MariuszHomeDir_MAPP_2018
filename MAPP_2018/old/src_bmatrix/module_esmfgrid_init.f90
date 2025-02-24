MODULE module_esmfgrid_init

!extracted from regrid_nemsio.fd/interpolation_interface.f90

  USE kinds
  USE netcdf
  USE netcdfio_interface
  USE mpi_interface

!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------

! Define interfaces and attributes for module routines

  PRIVATE
  PUBLIC :: esmfgrid_init_sub
  PUBLIC :: esmfgrid

  TYPE esmfgrid
     CHARACTER(len=500)                                                :: filename
     REAL(r_double),                    DIMENSION(:),      ALLOCATABLE :: s
     INTEGER,                           DIMENSION(:),      ALLOCATABLE :: col
     INTEGER,                           DIMENSION(:),      ALLOCATABLE :: row
     REAL(r_double),                    DIMENSION(:),      ALLOCATABLE :: inlats
     REAL(r_double),                    DIMENSION(:),      ALLOCATABLE :: inlons
     REAL(r_double),                    DIMENSION(:),      ALLOCATABLE :: outlats
     REAL(r_double),                    DIMENSION(:),      ALLOCATABLE :: outlons
     INTEGER                                                           :: n_s,n_a,n_b
  END TYPE esmfgrid ! type esmfgrid

CONTAINS

  SUBROUTINE esmfgrid_init_sub(grid)

! Define variables passed to routine

    TYPE(esmfgrid) :: grid

!=====================================================================

! Define local variables

    INTEGER :: ncstatus,ncfileid,ncdimid,ncvarid

    ncstatus = nf90_open(path=TRIM(ADJUSTL(grid%filename)),mode=  &
         & nf90_nowrite,ncid=ncfileid)
    ncstatus = nf90_inq_dimid(ncfileid,'n_s',ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=grid%n_s)
    ncstatus = nf90_inq_dimid(ncfileid,'n_a',ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=grid%n_a)
    ncstatus = nf90_inq_dimid(ncfileid,'n_b',ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=grid%n_b)


! Allocate memory for local variables

    ALLOCATE(grid%s(grid%n_s))
    ALLOCATE(grid%row(grid%n_s))
    ALLOCATE(grid%col(grid%n_s))

    ALLOCATE(grid%inlats(grid%n_a))
    ALLOCATE(grid%inlons(grid%n_a))
    ALLOCATE(grid%outlats(grid%n_b))
    ALLOCATE(grid%outlons(grid%n_b))

    ncstatus = nf90_inq_varid(ncfileid,'col',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%col)
    ncstatus = nf90_inq_varid(ncfileid,'row',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%row)
    ncstatus = nf90_inq_varid(ncfileid,'S',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%s)
    ncstatus = nf90_inq_varid(ncfileid,'yc_a',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%inlats)
    ncstatus = nf90_inq_varid(ncfileid,'xc_a',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%inlons)
    WHERE(grid%inlons .LT. 0.0) 
       grid%inlons=360+grid%inlons
    ENDWHERE
    ncstatus = nf90_inq_varid(ncfileid,'yc_b',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%outlats)
    ncstatus = nf90_inq_varid(ncfileid,'xc_b',ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,grid%outlons)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

  END SUBROUTINE esmfgrid_init_sub


!=======================================================================
END MODULE module_esmfgrid_init
