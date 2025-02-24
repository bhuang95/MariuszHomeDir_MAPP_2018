MODULE netcdfio_interface

!=======================================================================

! Define associated modules and subroutines

!-----------------------------------------------------------------------

  USE kinds

!-----------------------------------------------------------------------

  USE netcdf
  USE mpi_interface

!-----------------------------------------------------------------------

  IMPLICIT NONE

!-----------------------------------------------------------------------

! Define global variables

  LOGICAL :: debug=.FALSE.

  LOGICAL                                                              :: ncstatic
  INTEGER                                                              :: ncrec
  INTEGER                                                              :: ncxdim
  INTEGER                                                              :: ncydim
  INTEGER                                                              :: nczdim
  INTEGER                                                              :: nctdim
  INTEGER                                                              :: ncfileid
  INTEGER                                                              :: ncvarid
  INTEGER                                                              :: ncdimid
  INTEGER                                                              :: ncstatus

!-----------------------------------------------------------------------

! Define interfaces and attributes for module routines

  PRIVATE
  INTERFACE netcdfio_values_1d
     MODULE PROCEDURE netcdfio_values_1d_dblepr
     MODULE PROCEDURE netcdfio_values_1d_realpr
     MODULE PROCEDURE netcdfio_values_1d_intepr
  END INTERFACE netcdfio_values_1d ! interface netcdfio_values_2d
  INTERFACE netcdfio_values_2d
     MODULE PROCEDURE netcdfio_values_2d_dblepr
     MODULE PROCEDURE netcdfio_values_2d_realpr
     MODULE PROCEDURE netcdfio_values_2d_intepr
  END INTERFACE netcdfio_values_2d ! interface netcdfio_values_2d
  INTERFACE netcdfio_values_3d
     MODULE PROCEDURE netcdfio_values_3d_dblepr
     MODULE PROCEDURE netcdfio_values_3d_realpr
     MODULE PROCEDURE netcdfio_values_3d_intepr
  END INTERFACE netcdfio_values_3d ! interface netcdfio_values_3d
  INTERFACE netcdfio_global_attr
     MODULE PROCEDURE netcdfio_global_attr_char
  END INTERFACE netcdfio_global_attr ! interface netcdfio_global_attr
  INTERFACE netcdfio_variable_attr
     MODULE PROCEDURE netcdfio_variable_attr_char
  END INTERFACE netcdfio_variable_attr ! interface netcdfio_variable_attr
  PUBLIC :: netcdfio_values_1d
  PUBLIC :: netcdfio_values_2d
  PUBLIC :: netcdfio_values_3d
  PUBLIC :: netcdfio_dimension
  PUBLIC :: netcdfio_global_attr
  PUBLIC :: netcdfio_variable_attr
  PUBLIC :: ncrec
  PUBLIC :: ncxdim
  PUBLIC :: ncydim
  PUBLIC :: nczdim
  PUBLIC :: nctdim
  PUBLIC :: ncstatic

!-----------------------------------------------------------------------

CONTAINS

!=======================================================================

! netcdfio_global_attr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_global_attr_char(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    CHARACTER(len=*)                                                     :: varvalue

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_get_att(ncfileid,nf90_global,TRIM(ADJUSTL(varname)),  &
         & varvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

  END SUBROUTINE netcdfio_global_attr_char

  SUBROUTINE netcdfio_variable_attr_char(filename,varname,attribute,varvalue)

    IMPLICIT NONE

!=======================================================================

! Define variables passed to subroutine

    CHARACTER(len=500),INTENT(in) ::       filename
    CHARACTER(len=*),INTENT(in) ::         attribute
    CHARACTER(len=*),INTENT(in) ::         varname

! Define variables returned by subroutine

    CHARACTER(len=80),INTENT(out) ::    varvalue

! Define variables for decoding netCDF data

    INTEGER               ncid, varid, ncstatus

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,ncid=ncid)
    ncstatus = nf90_inq_varid(ncid,TRIM(ADJUSTL(varname)),varid)
    ncstatus = nf90_get_att(ncid,varid,TRIM(ADJUSTL(attribute)),varvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

  END SUBROUTINE netcdfio_variable_attr_char

!=======================================================================

! netcdfio_values_1d_dblepr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_1d_dblepr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    REAL(r_double)                                                       :: varvalue(:)

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF (ncstatus /= 0) THEN
       varvalue = -1.e30
    ELSE
       ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue)
       IF (ncstatus .NE. 0) THEN
          PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
          CALL mpi_interface_terminate()
          STOP
       ENDIF
    ENDIF
    ncstatus = nf90_close(ncfileid)

!=====================================================================

  END SUBROUTINE netcdfio_values_1d_dblepr

!=======================================================================

! netcdfio_values_2d_dblepr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_2d_dblepr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    REAL(r_double),             DIMENSION(ncxdim,ncydim)                 :: varvalue

! Define variables computed within routine

    INTEGER,                    DIMENSION(3)                             :: start
    INTEGER,                    DIMENSION(3)                             :: count

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(ncstatic)       start = (/1,1,1/)
    IF(.NOT. ncstatic) start = (/1,1,ncrec/)
    count    = (/ncxdim,ncydim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(debug) WRITE(6,500) TRIM(ADJUSTL(varname)), MINVAL(varvalue),      &
         & MAXVAL(varvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

! Define format statements

500 FORMAT('NETCDFIO_VALUES_2D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

!=====================================================================

  END SUBROUTINE netcdfio_values_2d_dblepr

!=======================================================================

! netcdfio_values_3d_dblepr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_3d_dblepr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    REAL(r_double),             DIMENSION(ncxdim,ncydim,nczdim)          :: varvalue

! Define variables computed within routine

    INTEGER,                    DIMENSION(4)                             :: start
    INTEGER,                    DIMENSION(4)                             :: count

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(ncstatic)       start = (/1,1,1,1/)
    IF(.NOT. ncstatic) start = (/1,1,1,ncrec/)
    count    = (/ncxdim,ncydim,nczdim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(debug) WRITE(6,500) TRIM(ADJUSTL(varname)), MINVAL(varvalue),      &
         & MAXVAL(varvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

! Define format statements

500 FORMAT('NETCDFIO_VALUES_3D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

!=====================================================================

  END SUBROUTINE netcdfio_values_3d_dblepr

!=======================================================================

! netcdfio_values_1d_realpr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_1d_realpr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    REAL(r_kind)                                                         :: varvalue(:)

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF (ncstatus /= 0) THEN
       varvalue = -1.e30
    ELSE
       ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue)
       IF (ncstatus .NE. 0) THEN
          PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
          CALL mpi_interface_terminate()
          STOP
       ENDIF
    ENDIF
    ncstatus = nf90_close(ncfileid)

!=====================================================================

  END SUBROUTINE netcdfio_values_1d_realpr

!=======================================================================

! netcdfio_values_2d_realpr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_2d_realpr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    REAL(r_kind),               DIMENSION(ncxdim,ncydim)                 :: varvalue

! Define variables computed within routine

    INTEGER,                    DIMENSION(3)                             :: start
    INTEGER,                    DIMENSION(3)                             :: count

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(ncstatic)       start = (/1,1,1/)
    IF(.NOT. ncstatic) start = (/1,1,ncrec/)
    count    = (/ncxdim,ncydim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(debug) WRITE(6,500) TRIM(ADJUSTL(varname)), MINVAL(varvalue),      &
         & MAXVAL(varvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

! Define format statements

500 FORMAT('NETCDFIO_VALUES_2D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

!=====================================================================

  END SUBROUTINE netcdfio_values_2d_realpr

!=======================================================================

! netcdfio_values_3d_realpr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_3d_realpr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    REAL(r_kind),               DIMENSION(ncxdim,ncydim,nczdim)          :: varvalue

! Define variables computed within routine

    INTEGER,                    DIMENSION(4)                             :: start
    INTEGER,                    DIMENSION(4)                             :: count

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(ncstatic)       start = (/1,1,1,1/)
    IF(.NOT. ncstatic) start = (/1,1,1,ncrec/)
    count    = (/ncxdim,ncydim,nczdim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(debug) WRITE(6,500) TRIM(ADJUSTL(varname)), MINVAL(varvalue),      &
         & MAXVAL(varvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

! Define format statements

500 FORMAT('NETCDFIO_VALUES_3D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

!=====================================================================

  END SUBROUTINE netcdfio_values_3d_realpr

!=======================================================================

! netcdfio_values_1d_intepr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_1d_intepr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    INTEGER                                                              :: varvalue(:)

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF (ncstatus /= 0) THEN
       varvalue = -9999
    ELSE
       ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue)
    ENDIF
    ncstatus = nf90_close(ncfileid)

!=====================================================================

  END SUBROUTINE netcdfio_values_1d_intepr

!=======================================================================

! netcdfio_values_2d_intepr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_2d_intepr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    INTEGER,                    DIMENSION(ncxdim,ncydim)                 :: varvalue

! Define variables computed within routine

    INTEGER,                    DIMENSION(3)                             :: start
    INTEGER,                    DIMENSION(3)                             :: count

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF(ncstatic)       start = (/1,1,1/)
    IF(.NOT. ncstatic) start = (/1,1,ncrec/)
    count    = (/ncxdim,ncydim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    IF(debug) WRITE(6,500) TRIM(ADJUSTL(varname)), MINVAL(varvalue),      &
         & MAXVAL(varvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

! Define format statements

500 FORMAT('NETCDFIO_VALUES_2D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

!=====================================================================

  END SUBROUTINE netcdfio_values_2d_intepr

!=======================================================================

! netcdfio_values_3d_intepr.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_values_3d_intepr(filename,varname,varvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: varname
    INTEGER,                    DIMENSION(ncxdim,ncydim,nczdim)          :: varvalue

! Define variables computed within routine

    INTEGER,                    DIMENSION(4)                             :: start
    INTEGER,                    DIMENSION(4)                             :: count

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_varid(ncfileid,TRIM(ADJUSTL(varname)),ncvarid)
    IF (ncstatus .NE. 0) THEN
       PRINT *,'fv3 read failed for ',TRIM(ADJUSTL(varname))
       CALL mpi_interface_terminate()
       STOP
    ENDIF
    IF(ncstatic)       start = (/1,1,1,1/)
    IF(.NOT. ncstatic) start = (/1,1,1,ncrec/)
    count    = (/ncxdim,ncydim,nczdim,1/)
    ncstatus = nf90_get_var(ncfileid,ncvarid,varvalue,start,count)
    IF(debug) WRITE(6,500) TRIM(ADJUSTL(varname)), MINVAL(varvalue),      &
         & MAXVAL(varvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

! Define format statements

500 FORMAT('NETCDFIO_VALUES_3D: Variable name = ', a, '; (min,max) = (',  &
         & f13.5,',',f13.5,').')

!=====================================================================

  END SUBROUTINE netcdfio_values_3d_intepr

!=======================================================================

! netcdfio_dimension.f90:

!-----------------------------------------------------------------------

  SUBROUTINE netcdfio_dimension(filename,dimname,dimvalue)

! Define variables passed to routine

    CHARACTER(len=500)                                                   :: filename
    CHARACTER(len=*)                                                     :: dimname
    INTEGER                                                              :: dimvalue

!=====================================================================

! Define local variables

    ncstatus = nf90_open(path=TRIM(ADJUSTL(filename)),mode=nf90_nowrite,  &
         & ncid=ncfileid)
    ncstatus = nf90_inq_dimid(ncfileid,TRIM(ADJUSTL(dimname)),ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=dimvalue)
    ncstatus = nf90_close(ncfileid)

!=====================================================================

  END SUBROUTINE netcdfio_dimension

!=======================================================================

END MODULE netcdfio_interface
