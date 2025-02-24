MODULE netcdf_io

!old crappy module that needs re-writing

  PRIVATE
  PUBLIC :: readnetcdfdata4, writenetcdfdata4, variableattribute_text, &
       &variableattribute_int,&
       &netcdfdimension, globalattribute_real,globalattribute_text,&
       &readnetcdfdata3,writenetcdfdata3,writenetcdfdata1,readnetcdfdata4_tslice

  PUBLIC :: handle_err

CONTAINS

! Subroutine will ingest a netcdf file and character strings, definining 
! the user specified dimensions, and return an array of variable integer 
! dimensions contained within the respective file

  SUBROUTINE netcdfdimension(netcdf_input,ndims,dimstring,dims)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'      

!=======================================================================

! Define array dimension variables

    INTEGER,INTENT(in) :: ndims

! Define variables passed to subroutine

    CHARACTER*250,INTENT(in) ::        netcdf_input
    CHARACTER*20,INTENT(in) ::        dimstring(ndims)
    INTEGER,INTENT(out) ::              dims(ndims)

! Define variables for decoding netCDF data

    CHARACTER*20         dimname
    INTEGER              ncid, dimid, status

! Define counting variables

    INTEGER              i

!=======================================================================

! Loop through dimensions of all arrays passed to subroutine

    DO i = 1, ndims

! Open analysis netCDF file passed to routine

       status = nf_open(netcdf_input,ncnowrit,ncid)

! Get record id of variable field

!dimid = ncdid(ncid,dimstring(i),rcode)
       status = nf_inq_dimid(ncid,dimstring(i),dimid)

! Get dimension name and size

!call ncdinq(ncid,dimid,dimname,dims(i),rcode)
       status = nf_inq_dim(ncid,dimid,dimname,dims(i))

! Close analysis netCDF file passed to routine

       status = nf_close(ncid)

    END DO

!=======================================================================

! Return calculated values

  END SUBROUTINE netcdfdimension

  SUBROUTINE readnetcdfdata3(netcdf_input,varname,varnamestring,&
       xdim,ydim,zdim)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define array dimension variables

    INTEGER,INTENT(in) ::              xdim, ydim, zdim

! Define variables passed to subroutine

    CHARACTER*250,INTENT(in) ::        netcdf_input
    CHARACTER*25,INTENT(in) ::         varnamestring

! Define variable returned by subroutine

    REAL,INTENT(out) ::                 varname(xdim,ydim,zdim)

! Define variables calculated with subroutine

    INTEGER              start(10), COUNT(10), vdims(10)

! Define variables for decoding netCDF data

    INTEGER              ncid, varid, nvatts
    INTEGER              nctype, nvdim, ndsize
    INTEGER              status
    CHARACTER*1024       strbuf

! Define counting variables

    INTEGER              i

!=======================================================================

    start = 1; count = 1

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_input,ncnowrit,ncid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Determine position withing netcdf file of variable

    status = nf_inq_varid(ncid,varnamestring,varid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Retrieve information for requested variable from netcdf file

!call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
    status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF

!=======================================================================

! Begin: Assign array dimensions for variable

    DO i = 1, nvdim

! Obtain variable dimensions

!call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
       status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)

       IF (status .NE. nf_noerr) THEN
          PRINT *, NF_STRERROR(STATUS)
          STOP 'Stopped'
       ENDIF

! Assign hyperslab starting index

       start(i) = 1

! Assign hyperslab ending index

       COUNT(i) = ndsize

    END DO



! End: Assign array dimensions for variable

!=======================================================================

! Get data for requested variable

!call ncvgt(ncid,varid,start,count,varname,rcode)
    status = nf_get_vara_real(ncid,varid,start(1:3),COUNT(1:3),varname)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Return calculated values

  END SUBROUTINE readnetcdfdata3


  SUBROUTINE readnetcdfdata4(netcdf_input,varname,varnamestring,&
       xdim,ydim,zdim,tdim)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define array dimension variables

    INTEGER,INTENT(in) ::              xdim, ydim, zdim, tdim

! Define variables passed to subroutine

    CHARACTER*250,INTENT(in) ::        netcdf_input
    CHARACTER*25,INTENT(in) ::         varnamestring

! Define variable returned by subroutine

    REAL,INTENT(out) ::                 varname(xdim,ydim,zdim,tdim)

! Define variables calculated with subroutine

    INTEGER              start(10), COUNT(10), vdims(10)

! Define variables for decoding netCDF data

    INTEGER              ncid, varid, nvatts
    INTEGER              nctype, nvdim, ndsize
    INTEGER              status
    CHARACTER*1024       strbuf

! Define counting variables

    INTEGER              i

!=======================================================================

    start = 1; count = 1

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_input,ncnowrit,ncid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Determine position withing netcdf file of variable

    status = nf_inq_varid(ncid,varnamestring,varid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Retrieve information for requested variable from netcdf file

!call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
    status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


!=======================================================================

! Begin: Assign array dimensions for variable

    DO i = 1, nvdim

! Obtain variable dimensions

!call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
       status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)

       IF (status .NE. nf_noerr) THEN
          PRINT *, NF_STRERROR(STATUS)
          STOP 'Stopped'
       ENDIF


! Assign hyperslab starting index

       start(i) = 1

! Assign hyperslab ending index

       COUNT(i) = ndsize

    END DO

! End: Assign array dimensions for variable

!=======================================================================

! Get data for requested variable

!call ncvgt(ncid,varid,start,count,varname,rcode)
    status = nf_get_vara_real(ncid,varid,start(1:4),COUNT(1:4),varname)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE readnetcdfdata4

!***********************************************************************

! Subroutine will ingest a variable array and write the variable 
! field to an external file

  SUBROUTINE writenetcdfdata3(netcdf_output,varname,varnamestring,&
       xdim,ydim,zdim)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define array dimension variables

    INTEGER,INTENT(in) ::              xdim, ydim, zdim

! Define variables passed to subroutine

    CHARACTER*250, INTENT(in) ::        netcdf_output
    CHARACTER*25, INTENT(in) ::         varnamestring
    REAL,INTENT(in) ::              varname(xdim,ydim,zdim)

! Define variables calculated with subroutine

    INTEGER              start(10), COUNT(10), vdims(10)

! Define variables for decoding netCDF data

    INTEGER              ncid, varid, nvatts
    INTEGER              nctype, nvdim, ndsize
    INTEGER              status
    CHARACTER*1024       strbuf

! Define counting variables

    INTEGER              i

!=======================================================================

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_output,ncwrite,ncid)

! Determine position withing netcdf file of variable

    status = nf_inq_varid(ncid,varnamestring,varid)

! Retrieve information for requested variable from netcdf file

!call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
    status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

!=======================================================================

! Begin: Assign array dimensions for variable

    DO i = 1, nvdim

! Obtain variable dimensions

!call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
       status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)

! Assign hyperslab starting index

       start(i) = 1

! Assign hyperslab ending index

       COUNT(i) = ndsize

    END DO

! End: Assign array dimensions for variable

!=======================================================================

! Get data for requested variable

!call ncvpt(ncid,varid,start,count,varname,rcode)
    status = nf_put_vara_real(ncid,varid,start(1:3),COUNT(1:3),varname)

! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE writenetcdfdata3

  SUBROUTINE writenetcdfdata4(netcdf_output,varname,varnamestring,&
       xdim,ydim,zdim,tdim)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define array dimension variables

    INTEGER,INTENT(in) ::              xdim, ydim, zdim, tdim

! Define variables passed to subroutine

    CHARACTER*250, INTENT(in) ::        netcdf_output
    CHARACTER*25, INTENT(in) ::         varnamestring
    REAL,INTENT(in) ::              varname(xdim,ydim,zdim,tdim)

! Define variables calculated with subroutine

    INTEGER              start(10), COUNT(10), vdims(10)

! Define variables for decoding netCDF data

    INTEGER              ncid, varid, nvatts
    INTEGER              nctype, nvdim, ndsize
    INTEGER              status
    CHARACTER*1024       strbuf

! Define counting variables

    INTEGER              i

!=======================================================================

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_output,ncwrite,ncid)

! Determine position withing netcdf file of variable

    status = nf_inq_varid(ncid,varnamestring,varid)

! Retrieve information for requested variable from netcdf file

!call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
    status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

!=======================================================================

! Begin: Assign array dimensions for variable

    DO i = 1, nvdim

! Obtain variable dimensions

!call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
       status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)

! Assign hyperslab starting index

       start(i) = 1

! Assign hyperslab ending index

       COUNT(i) = ndsize

    END DO

! End: Assign array dimensions for variable

!=======================================================================

! Get data for requested variable

!call ncvpt(ncid,varid,start,count,varname,rcode)
    status = nf_put_vara_real(ncid,varid,start(1:4),COUNT(1:4),varname)

! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE writenetcdfdata4

  SUBROUTINE writenetcdfdata1(netcdf_output,varname,varnamestring,tdim)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define array dimension variables

    INTEGER,INTENT(in) ::              tdim

! Define variables passed to subroutine

    CHARACTER*250, INTENT(in) ::        netcdf_output
    CHARACTER*25, INTENT(in) ::         varnamestring
    REAL,INTENT(in) ::              varname(tdim)

! Define variables calculated with subroutine

    INTEGER              start(10), COUNT(10), vdims(10)

! Define variables for decoding netCDF data

    INTEGER              ncid, varid, nvatts
    INTEGER              nctype, nvdim, ndsize
    INTEGER              status
    CHARACTER*1024       strbuf

! Define counting variables

    INTEGER              i

!=======================================================================

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_output,ncwrite,ncid)

! Determine position withing netcdf file of variable

    status = nf_inq_varid(ncid,varnamestring,varid)

! Retrieve information for requested variable from netcdf file

!call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
    status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

!=======================================================================

! Begin: Assign array dimensions for variable

    DO i = 1, nvdim

! Obtain variable dimensions

!call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
       status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)

! Assign hyperslab starting index

       start(i) = 1

! Assign hyperslab ending index

       COUNT(i) = ndsize

    END DO

! End: Assign array dimensions for variable

!=======================================================================

! Get data for requested variable

!call ncvpt(ncid,varid,start,count,varname,rcode)
    status = nf_put_vara_real(ncid,varid,start(1),COUNT(1),varname)

! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE writenetcdfdata1

! This subroutine will ingest a netcdf formatted file and return the value 
! pertaining to a character-string attribute variable specified  by the 
! user

  SUBROUTINE variableattribute_text(netcdf_input,varname,attribute,&
       VALUE)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define variables passed to subroutine

    CHARACTER*250,INTENT(in) ::        netcdf_input
    CHARACTER*50,INTENT(in) ::         attribute
    CHARACTER*25,INTENT(in) ::         varname

! Define variables returned by subroutine

    CHARACTER*25,INTENT(out) ::         VALUE

! Define variables for decoding netCDF data

    INTEGER               ncid, varid, status

!=======================================================================

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_input,ncnowrit,ncid)

! Define variable position within netcdf file

!varid = ncvid(ncid,varname,status)
    status = nf_inq_varid(ncid,varname,varid)

! Obtain global attribute value

    status = nf_get_att(ncid,varid,attribute,VALUE)

!=======================================================================

! Close netcdf data file passed to routine

    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE variableattribute_text


  SUBROUTINE variableattribute_int(netcdf_input,varname,attribute,&
       VALUE)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define variables passed to subroutine

    CHARACTER*250,INTENT(in) ::        netcdf_input
    CHARACTER*50,INTENT(in) ::         attribute
    CHARACTER*25,INTENT(in) ::         varname

! Define variables returned by subroutine

    INTEGER,INTENT(out) ::         VALUE

! Define variables for decoding netCDF data

    INTEGER               ncid, varid, status

!=======================================================================

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_input,ncnowrit,ncid)

! Define variable position within netcdf file

!varid = ncvid(ncid,varname,status)
    status = nf_inq_varid(ncid,varname,varid)

! Obtain global attribute value

    status = nf_get_att(ncid,varid,attribute,VALUE)

!=======================================================================

! Close netcdf data file passed to routine

    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE variableattribute_int



!***********************************************************************

! This module will ingest a netcdf formatted file and return the value 
! pertaining to a real-value global attribute variable specified by the 
! user


  SUBROUTINE globalattribute_real(netcdf_input,attribute,VALUE)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define variables passed to subroutine

    CHARACTER*250,INTENT(in) ::        netcdf_input
    CHARACTER*50,INTENT(in) ::         attribute

! Define variables returned by subroutine

    REAL,INTENT(out) ::                 VALUE

! Define variables for decoding netCDF data

    INTEGER              ncid, status

!=======================================================================

! Open netcdf file passed to subroutine

!ncid = ncopn(netcdf_input,ncnowrit,rcode)
    status = nf_open(netcdf_input, ncnowrit, ncid)

! Obtain global attribute value

!call ncagt(ncid,ncglobal,attribute,value,status)
    status = nf_get_att_real(ncid,ncglobal,attribute,VALUE)

!=======================================================================

! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE globalattribute_real

  SUBROUTINE globalattribute_text(netcdf_input,attribute,VALUE)

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define variables passed to subroutine

    CHARACTER*250,INTENT(in) ::        netcdf_input
    CHARACTER*50,INTENT(in) ::         attribute

! Define variables returned by subroutine

    CHARACTER(len=50),INTENT(out) ::                 VALUE

! Define variables for decoding netCDF data

    INTEGER              ncid, status

!=======================================================================

! Open netcdf file passed to subroutine

!ncid = ncopn(netcdf_input,ncnowrit,rcode)
    status = nf_open(netcdf_input, ncnowrit, ncid)

! Obtain global attribute value

!call ncagt(ncid,ncglobal,attribute,value,status)
    status = nf_get_att_text(ncid,ncglobal,attribute,VALUE)


!=======================================================================

! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE globalattribute_text

  SUBROUTINE readnetcdf_text(netcdf_input,varname,varnamestring,&
       strlen,tdim)

!this routine not working yet         

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

!=======================================================================

! Define array dimension variables

    INTEGER,INTENT(in) ::              strlen,tdim

! Define variables passed to subroutine

    CHARACTER*250,INTENT(in) ::        netcdf_input
    CHARACTER*25,INTENT(in) ::         varnamestring

! Define variable returned by subroutine

    CHARACTER(len=*),INTENT(out) ::                 varname(tdim)

! Define variables calculated with subroutine

    INTEGER              start(10), COUNT(10), vdims(10)

! Define variables for decoding netCDF data

    INTEGER              ncid, varid, nvatts
    INTEGER              nctype, nvdim, ndsize
    INTEGER              status
    CHARACTER*1024       strbuf

! Define counting variables

    INTEGER              i

!=======================================================================

    start = 1; count = 1

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_input,ncnowrit,ncid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Determine position withing netcdf file of variable

    status = nf_inq_varid(ncid,varnamestring,varid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Retrieve information for requested variable from netcdf file

!call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
    status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF

!=======================================================================

! Begin: Assign array dimensions for variable

    DO i = 1, nvdim

! Obtain variable dimensions

!call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
       status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)

       IF (status .NE. nf_noerr) THEN
          PRINT *, NF_STRERROR(STATUS)
          STOP 'Stopped'
       ENDIF


! Assign hyperslab starting index

       start(i) = 1

! Assign hyperslab ending index

       COUNT(i) = ndsize

    END DO

! End: Assign array dimensions for variable

    PRINT *,count
    PRINT *,nvdim

    STOP


!=======================================================================

! Get data for requested variable

!call ncvgt(ncid,varid,start,count,varname,rcode)
    status = nf_get_vara_real(ncid,varid,start(1:4),COUNT(1:4),varname)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE readnetcdf_text

  SUBROUTINE readnetcdfdata4_tslice(netcdf_input,varname,varnamestring,&
       xdim,ydim,zdim,tslice)

    implicit none

    include 'netcdf.inc'

!=======================================================================

! Define array dimension variables

!assume time slice is always last

    INTEGER,INTENT(in) ::              xdim, ydim, zdim, tslice

! Define variables passed to subroutine

    character*250,intent(in) ::        netcdf_input
    character*25,intent(in) ::         varnamestring

! Define variable returned by subroutine

    REAL,INTENT(out) ::                 varname(xdim,ydim,zdim,1)

! Define variables calculated with subroutine

    integer              start(10), count(10), vdims(10)

! Define variables for decoding netCDF data

    integer              ncid, varid, nvatts
    integer              nctype, nvdim, ndsize
    integer              status
    character*1024       strbuf

! Define counting variables

    integer              i

!=======================================================================

    start = 1; count = 1

! Open netcdf file passed to subroutine

    status = nf_open(netcdf_input,ncnowrit,ncid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Determine position withing netcdf file of variable

    status = nf_inq_varid(ncid,varnamestring,varid)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Retrieve information for requested variable from netcdf file

!call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
    status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


!=======================================================================

! Begin: Assign array dimensions for variable

    do i = 1, nvdim

! Obtain variable dimensions

!call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
       status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)



       IF (status .NE. nf_noerr) THEN
          PRINT *, NF_STRERROR(STATUS)
          STOP 'Stopped'
       ENDIF


! Assign hyperslab starting and ending index

       IF (i == nvdim) THEN
          start(i) = tslice
          count(i) = tslice
       ELSE
          start(i) = 1
          count(i) = ndsize
       ENDIF

    end do

! End: Assign array dimensions for variable

!=======================================================================

! Get data for requested variable

!call ncvgt(ncid,varid,start,count,varname,rcode)
    status = nf_get_vara_real(ncid,varid,start(1:4),count(1:4),varname)

    IF (status .NE. nf_noerr) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
    ENDIF


! Close netcdf data file passed to routine

!call ncclos(ncid,rcode)
    status = nf_close(ncid)

! Return calculated values

  END SUBROUTINE readnetcdfdata4_tslice


  SUBROUTINE handle_err(status)
    INTEGER status
    WRITE(6,*) 'Error number ',status
    STOP
  END SUBROUTINE handle_err

END MODULE netcdf_io
