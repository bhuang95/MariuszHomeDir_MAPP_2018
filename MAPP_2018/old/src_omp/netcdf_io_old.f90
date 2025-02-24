module netcdf_io

private
public :: readnetcdfdata4, writenetcdfdata4, variableattribute_char, &
          netcdfdimension, globalattribute_real,globalattribute_text,&
          &readnetcdfdata3,writenetcdfdata3
contains

      ! Subroutine will ingest a netcdf file and character strings, definining 
      ! the user specified dimensions, and return an array of variable integer 
      ! dimensions contained within the respective file

      subroutine netcdfdimension(netcdf_input,ndims,dimstring,dims)

      implicit none

      include 'netcdf.inc'      

      !=======================================================================

      ! Define array dimension variables

      integer,intent(in) :: ndims

      ! Define variables passed to subroutine

      character*250,intent(in) ::        netcdf_input
      character*20,intent(in) ::        dimstring(ndims)
      integer,intent(out) ::              dims(ndims)

      ! Define variables for decoding netCDF data

      character*20         dimname
      integer              ncid, dimid, status

      ! Define counting variables

      integer              i

      !=======================================================================

      ! Loop through dimensions of all arrays passed to subroutine

      do i = 1, ndims

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

      end do

      !=======================================================================

      ! Return calculated values

      end subroutine netcdfdimension

      subroutine readnetcdfdata3(netcdf_input,varname,varnamestring,&
           xdim,ydim,zdim)

      implicit none

      include 'netcdf.inc'

      !=======================================================================

      ! Define array dimension variables

      integer,intent(in) ::              xdim, ydim, zdim

      ! Define variables passed to subroutine

      character*250,intent(in) ::        netcdf_input
      character*10,intent(in) ::         varnamestring

      ! Define variable returned by subroutine

      real,intent(out) ::                 varname(xdim,ydim,zdim)

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
         
         ! Assign hyperslab starting index

         start(i) = 1

         ! Assign hyperslab ending index

         count(i) = ndsize

      end do



      ! End: Assign array dimensions for variable

      !=======================================================================

      ! Get data for requested variable
      
      !call ncvgt(ncid,varid,start,count,varname,rcode)
      status = nf_get_vara_real(ncid,varid,start(1:3),count(1:3),varname)

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

      end subroutine readnetcdfdata3


      subroutine readnetcdfdata4(netcdf_input,varname,varnamestring,&
           xdim,ydim,zdim,tdim)

      implicit none

      include 'netcdf.inc'

      !=======================================================================

      ! Define array dimension variables

      INTEGER,INTENT(in) ::              xdim, ydim, zdim, tdim

      ! Define variables passed to subroutine

      character*250,intent(in) ::        netcdf_input
      character*10,intent(in) ::         varnamestring

      ! Define variable returned by subroutine

      REAL,INTENT(out) ::                 varname(xdim,ydim,zdim,tdim)

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
         

         ! Assign hyperslab starting index

         start(i) = 1

         ! Assign hyperslab ending index

         count(i) = ndsize

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

      end subroutine readnetcdfdata4


      !***********************************************************************

      ! Subroutine will ingest a variable array and write the variable 
      ! field to an external file

      subroutine writenetcdfdata3(netcdf_output,varname,varnamestring,&
           xdim,ydim,zdim)

      implicit none

      include 'netcdf.inc'

      !=======================================================================

      ! Define array dimension variables

      integer,intent(in) ::              xdim, ydim, zdim

      ! Define variables passed to subroutine

      character*250, intent(in) ::        netcdf_output
      character*10, intent(in) ::         varnamestring
      real,intent(in) ::              varname(xdim,ydim,zdim)

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

      ! Open netcdf file passed to subroutine

      status = nf_open(netcdf_output,ncwrite,ncid)

      ! Determine position withing netcdf file of variable

      status = nf_inq_varid(ncid,varnamestring,varid)

      ! Retrieve information for requested variable from netcdf file

      !call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
      status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

      !=======================================================================

      ! Begin: Assign array dimensions for variable

      do i = 1, nvdim

         ! Obtain variable dimensions

         !call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
         status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)

         ! Assign hyperslab starting index

         start(i) = 1

         ! Assign hyperslab ending index

         count(i) = ndsize

      end do

      ! End: Assign array dimensions for variable

      !=======================================================================

      ! Get data for requested variable
      
      !call ncvpt(ncid,varid,start,count,varname,rcode)
      status = nf_put_vara_real(ncid,varid,start(1:3),count(1:3),varname)

      ! Close netcdf data file passed to routine

      !call ncclos(ncid,rcode)
      status = nf_close(ncid)

      ! Return calculated values

      end subroutine writenetcdfdata3

      subroutine writenetcdfdata4(netcdf_output,varname,varnamestring,&
           xdim,ydim,zdim,tdim)

      implicit none

      include 'netcdf.inc'

      !=======================================================================

      ! Define array dimension variables

      INTEGER,INTENT(in) ::              xdim, ydim, zdim, tdim

      ! Define variables passed to subroutine

      character*250, intent(in) ::        netcdf_output
      character*10, intent(in) ::         varnamestring
      REAL,INTENT(in) ::              varname(xdim,ydim,zdim,tdim)

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

      ! Open netcdf file passed to subroutine

      status = nf_open(netcdf_output,ncwrite,ncid)

      ! Determine position withing netcdf file of variable

      status = nf_inq_varid(ncid,varnamestring,varid)

      ! Retrieve information for requested variable from netcdf file

      !call ncvinq(ncid,varid,strbuf,nctype,nvdim,vdims,nvatts,rcode)
      status = nf_inq_var(ncid, varid, strbuf, nctype, nvdim, vdims, nvatts)

      !=======================================================================

      ! Begin: Assign array dimensions for variable

      do i = 1, nvdim

         ! Obtain variable dimensions

         !call ncdinq(ncid,vdims(i),strbuf,ndsize,rcode)
         status = nf_inq_dim(ncid,vdims(i),strbuf, ndsize)

         ! Assign hyperslab starting index

         start(i) = 1

         ! Assign hyperslab ending index

         count(i) = ndsize

      end do

      ! End: Assign array dimensions for variable

      !=======================================================================

      ! Get data for requested variable
      
      !call ncvpt(ncid,varid,start,count,varname,rcode)
      status = nf_put_vara_real(ncid,varid,start(1:4),count(1:4),varname)

      ! Close netcdf data file passed to routine

      !call ncclos(ncid,rcode)
      status = nf_close(ncid)

      ! Return calculated values

      end subroutine writenetcdfdata4

      ! This subroutine will ingest a netcdf formatted file and return the value 
      ! pertaining to a character-string attribute variable specified  by the 
      ! user

      subroutine variableattribute_char(netcdf_input,varname,attribute,&
           value)

      implicit none

      include 'netcdf.inc'

      !=======================================================================

      ! Define variables passed to subroutine

      character*250,intent(in) ::        netcdf_input
      character*50,intent(in) ::         attribute
      character*10,intent(in) ::         varname

      ! Define variables returned by subroutine

      character*10,intent(out) ::         value

      ! Define variables for decoding netCDF data

      integer               ncid, varid, status

      !=======================================================================

      ! Open netcdf file passed to subroutine
    
      status = nf_open(netcdf_input,ncnowrit,ncid)

      ! Define variable position within netcdf file

      !varid = ncvid(ncid,varname,status)
      status = nf_inq_varid(ncid,varname,varid)

      ! Obtain global attribute value

      status = nf_get_att_text(ncid,varid,attribute,value)

      !=======================================================================

      ! Close netcdf data file passed to routine

      status = nf_close(ncid)

      ! Return calculated values

      end subroutine variableattribute_char

      !***********************************************************************

      ! This module will ingest a netcdf formatted file and return the value 
      ! pertaining to a real-value global attribute variable specified by the 
      ! user


      subroutine globalattribute_real(netcdf_input,attribute,value)

      implicit none

      include 'netcdf.inc'

      !=======================================================================

      ! Define variables passed to subroutine

      character*250,intent(in) ::        netcdf_input
      character*50,intent(in) ::         attribute

      ! Define variables returned by subroutine

      real,intent(out) ::                 value

      ! Define variables for decoding netCDF data
    
      integer              ncid, status

      !=======================================================================

      ! Open netcdf file passed to subroutine
    
      !ncid = ncopn(netcdf_input,ncnowrit,rcode)
      status = nf_open(netcdf_input, ncnowrit, ncid)

      ! Obtain global attribute value

      !call ncagt(ncid,ncglobal,attribute,value,status)
      status = nf_get_att_real(ncid,ncglobal,attribute,value)

      !=======================================================================

      ! Close netcdf data file passed to routine

      !call ncclos(ncid,rcode)
      status = nf_close(ncid)

      ! Return calculated values

      end subroutine globalattribute_real

      subroutine globalattribute_text(netcdf_input,attribute,value)

      implicit none

      include 'netcdf.inc'

      !=======================================================================

      ! Define variables passed to subroutine

      character*250,intent(in) ::        netcdf_input
      character*50,intent(in) ::         attribute

      ! Define variables returned by subroutine

      character(len=80),intent(out) ::                 value

      ! Define variables for decoding netCDF data
    
      integer              ncid, status

      !=======================================================================

      ! Open netcdf file passed to subroutine
    
      !ncid = ncopn(netcdf_input,ncnowrit,rcode)
      status = nf_open(netcdf_input, ncnowrit, ncid)

      ! Obtain global attribute value

      !call ncagt(ncid,ncglobal,attribute,value,status)
      status = nf_get_att_text(ncid,ncglobal,attribute,value)


      !=======================================================================

      ! Close netcdf data file passed to routine

      !call ncclos(ncid,rcode)
      status = nf_close(ncid)

      ! Return calculated values

      end subroutine globalattribute_text


end module netcdf_io
