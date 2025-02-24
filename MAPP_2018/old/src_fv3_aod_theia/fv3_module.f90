MODULE fv3_module

  USE netcdf
  USE constants
  USE kinds

CONTAINS
!-----------------------------------------------------------------------
  SUBROUTINE fv3_netcdf_read_1d(ncid,dimz,varname,data1d)

    IMPLICIT NONE
    INTEGER                         :: ncid
    INTEGER                         :: varid,stat
    INTEGER :: dimz
    REAL                            :: data1d(dimz)
    CHARACTER(len=12)      :: varname
    REAL                            :: tmp(dimz)
    INTEGER :: k

    

! loop through 1d data
    stat = nf90_inq_varid(ncid,TRIM(varname),varid)
!    PRINT*,stat,varid,TRIM(varname)
    CALL handle_err(stat)
    stat = nf90_get_var(ncid,varid,tmp,start=(/1/),count=(/dimz/))
    IF (stat .NE. 0 ) THEN
       PRINT*,'error reading ',varname
       STOP
    ENDIF

    DO k=1,dimz
       data1d(k)=tmp(dimz-k+1)
    ENDDO

  END SUBROUTINE    fv3_netcdf_read_1d

  SUBROUTINE fv3_netcdf_read_2d(ncid,ifhr,dimx,dimy,varname,data2d)

    IMPLICIT NONE
    INTEGER                         :: ncid
    INTEGER                         :: ifhr,varid,stat
    INTEGER :: dimx,dimy
    REAL                            :: data2d(dimx,dimy)
    CHARACTER(len=12)      :: varname

! loop through 2d data
    stat = nf90_inq_varid(ncid,TRIM(varname),varid)
!print*,stat,varid,trim(varname)
    stat = nf90_get_var(ncid,varid,data2d,start=(/1,1,ifhr/),count=(/dimx,dimy,1/))
    IF (stat .NE. 0 ) THEN
       PRINT*,'error reading ',varname
       STOP
    ENDIF

  END SUBROUTINE    fv3_netcdf_read_2d
!-----------------------------------------------------------------------

  SUBROUTINE fv3_netcdf_read_3d(ncid,ifhr,dimx,dimy,dimz,&
       &varname,data3d)

    IMPLICIT NONE

    INTEGER                         :: ncid
    INTEGER                         :: ifhr,varid,stat
    INTEGER :: dimx,dimy,dimz
    CHARACTER(len=12)      :: varname
    REAL                            :: data3d(dimx,dimy,dimz)
    REAL                            :: tmp(dimx,dimy,dimz)
    INTEGER :: k

    stat = nf90_inq_varid(ncid,TRIM(varname),varid)
    stat = nf90_get_var(ncid,varid,tmp,start=(/1,1,1,ifhr/),count=(/dimx,dimy,dimz,1/))

!reverse z coordinate
    DO k=1,dimz
       data3d(:,:,k)=tmp(:,:,dimz-k+1)
    ENDDO

    IF (stat .NE. 0 ) THEN
       PRINT*,'error reading ',varname
       STOP
    ENDIF

  END SUBROUTINE    fv3_netcdf_read_3d

  SUBROUTINE handle_err(status)
    INTEGER status
    IF (status /= nf90_noerr) THEN
       WRITE(6,*) 'Error number ',status
       WRITE(6,*) 'Error: ', nf90_strerror(status)
       STOP
    ENDIF
  END SUBROUTINE handle_err
  

!-----------------------------------------------------------------------

END MODULE fv3_module
