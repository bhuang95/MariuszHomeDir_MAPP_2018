PROGRAM ncdf_test

  USE, INTRINSIC :: iso_c_binding

  IMPLICIT NONE

  CHARACTER(len=100) :: infile_aeronet="aeronet_aod.2021100100.nc"

!  CALL ncdf_wwr( )

  CALL read_aeronet(infile_aeronet)

  STOP 0

END PROGRAM ncdf_test

SUBROUTINE ncdf_wwr()

  USE, INTRINSIC :: iso_c_binding
  USE netcdf

  IMPLICIT NONE

  INTERFACE
     SUBROUTINE ncdf_w ( ) BIND ( c, name='c_ncdf_w' )
       USE iso_c_binding
     END SUBROUTINE ncdf_w
  END INTERFACE

  INTERFACE
     SUBROUTINE ncdf_w_4d ( ) BIND ( c, name='c_ncdf_w_4d' )
       USE iso_c_binding
     END SUBROUTINE ncdf_w_4d
  END INTERFACE

  INTERFACE
     SUBROUTINE ncdf_r_4d ( ) BIND ( c, name='c_ncdf_r_4d' )
       USE iso_c_binding
     END SUBROUTINE ncdf_r_4d
  END INTERFACE


  INTEGER ( c_int ), PARAMETER :: n = 3

  REAL ( c_double ) eps
  REAL ( c_double ) s
  REAL ( c_double ) w1(n+1)
  REAL ( c_double ) w2(n+1)

  PRINT *,'testing ncdf_w'

  call ncdf_w( )
  call ncdf_w_4d( )
  CALL ncdf_r_4d( )

  RETURN

END SUBROUTINE ncdf_wwr
  

SUBROUTINE check_nc(status)

  USE netcdf

  IMPLICIT NONE

  INTEGER, INTENT(in) :: status

  IF(status /= nf90_noerr) THEN
     PRINT *, TRIM(nf90_strerror(status))
     STOP "netCDF error...Stopped."
  END IF

END SUBROUTINE check_nc

SUBROUTINE read_aeronet(infile_aeronet)

  USE, INTRINSIC :: iso_c_binding
  USE netcdf

  IMPLICIT NONE

  CHARACTER(len=*), INTENT(in) :: infile_aeronet

  INTEGER(c_int) :: ncid,grpid,varid,dimid,nlocs

  INTEGER(c_int), PARAMETER :: date_string_length=20

  INTERFACE
     SUBROUTINE ncdf_r_strings(grpid,varid,nlocs,date_string_length) &
          &BIND ( c, name='c_ncdf_r_strings' )
       USE iso_c_binding
       INTEGER ( c_int ), VALUE, INTENT(in) :: grpid
       INTEGER ( c_int ), VALUE, INTENT(in) :: varid
       INTEGER ( c_int ), VALUE, INTENT(in) :: nlocs
       INTEGER ( c_int ), VALUE, INTENT(in) :: date_string_length
     END SUBROUTINE ncdf_r_strings
  END INTERFACE


  CONTINUE

  CALL check_nc(nf90_open(infile_aeronet, nf90_nowrite, ncid))

  CALL check_nc(nf90_inq_grp_ncid(ncid,"MetaData",grpid))

  CALL check_nc(nf90_inq_dimid(ncid,"nlocs",dimid))
  CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nlocs))


  CALL check_nc(nf90_inq_varid(grpid,"datetime",varid))
  PRINT *,'@@@1',dimid,grpid,varid,nlocs

  CALL ncdf_r_strings(grpid,varid,nlocs,date_string_length)

  CALL check_nc(nf90_inq_varid(grpid,"station_id",varid))
  PRINT *,'@@@2',varid

  CALL check_nc(nf90_close(ncid))

END SUBROUTINE read_aeronet
