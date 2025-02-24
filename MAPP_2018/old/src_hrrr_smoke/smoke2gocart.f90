PROGRAM smoke2gocart

!Mariusz Pagowski Nov 2016
!convert hrrr-smoke species to gocart
  
  USE netcdf_io
  
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, PARAMETER :: nvars=17
  INTEGER :: i,j,nweights
  CHARACTER(len=10), DIMENSION(nvars) :: &
       &varnames = (/&
       &"tracer_1a","tracer_2a",&
       &"sulf","SEAS_1","SEAS_2","SEAS_3","SEAS_4",&
       &"BC1","BC2","OC1","OC2",&
       &"DUST_1","DUST_2","DUST_3","DUST_4","DUST_5","P25"/)
  
  
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: tracer_1,tracer_2,aerosol
  REAL, ALLOCATABLE, DIMENSION(:) :: weights

  INTEGER :: nx,ny,nz,nt
  INTEGER :: mcid,status
  REAL :: small
  
  INTEGER, PARAMETER :: nvardims =4, natts=6
  CHARACTER(len=20), DIMENSION(nvardims) :: dimstring=&
       &(/"west_east","south_north","bottom_top","Time"/)
  CHARACTER(len=50), DIMENSION(natts) :: attribnames =(/&
       &"FieldType","MemoryOrder","description","units","stagger","coordinates"/)
  CHARACTER(len=50), DIMENSION(natts) :: attribvals

  INTEGER, DIMENSION(nvardims) :: dimid
  INTEGER, DIMENSION(nvardims) :: end_dims, start_dims=(/1,1,1,1/)
  INTEGER, DIMENSION(nvars-2) :: varid
  CHARACTER(len=25) :: varstringname
  CHARACTER(len=80) :: string

  CHARACTER(len=250) :: aerofilein,weightsfilein,filename
  
  INTEGER :: iargc

  small=TINY(1.)

  IF (iargc() < 2) THEN
     PRINT *,"needs wrf input file amd wieghts file"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,aerofilein)
  CALL getarg(2,weightsfilein)

  OPEN(unit=101,file=weightsfilein,form='formatted')
  READ(101,'(i3)')nweights
  ALLOCATE(weights(nweights))
  DO i=1,nweights
     READ(101,'(f15.7)')weights(i)
  ENDDO
  CLOSE(101)

  WHERE (weights < 0.) weights=1.e-9

  filename=aerofilein

  CALL netcdfdimension(filename,nvardims,dimstring,end_dims)  

  nx=end_dims(1)
  ny=end_dims(2)
  nz=end_dims(3)
  nt=end_dims(4)

  ALLOCATE(tracer_1(nx,ny,nz,nt),tracer_2(nx,ny,nz,nt),aerosol(nx,ny,nz,nt))

  varstringname=varnames(1)
  CALL readnetcdfdata4(filename,tracer_1,varstringname,nx,ny,nz,nt)

!  varstringname=varnames(2)
!  CALL readnetcdfdata4(filename,tracer_2,varstringname,nx,ny,nz,nt)

  status = nf_open(filename,ncwrite,mcid)

  DO i=1,nvardims
     status = nf_inq_dimid(mcid,dimstring(i),dimid(i))
  ENDDO

  status = nf_redef(mcid)

  DO i = 3, nvars

     PRINT *, 'def ',varnames(i)
     status = nf_def_var(mcid, varnames(i), NF_REAL, nvardims, &
          &dimid,varid(i-2))
     IF (status /= nf_noerr) THEN
        PRINT *,'error 1'
        PRINT *, 'Error: ', nf_strerror(status)
     ENDIF

     j=1
     status = nf_put_att(mcid, varid(i-2), attribnames(j),NF_INT, 1, 104)
     j=2
     string="XYZ"
     status = nf_put_att_text(mcid, varid(i-2), attribnames(j),LEN_TRIM(string),string)
     j=3
     string=TRIM(varnames(i))//' conc.'
     status = nf_put_att_text(mcid, varid(i-2), attribnames(j),LEN_TRIM(string),string)
     j=4
     IF (varnames(i)=="sulf") THEN
        string="ppmv"
     ELSE
        string="ug/kg-dryair"
     ENDIF
     status = nf_put_att_text(mcid, varid(i-2), attribnames(j),LEN_TRIM(string),string)
     j=5
     string=""
     status = nf_put_att_text(mcid, varid(i-2), attribnames(j),LEN_TRIM(string),string)
     j=6
     string="XLONG XLAT"
     status = nf_put_att_text(mcid, varid(i-2), attribnames(j),LEN_TRIM(string),string)

  ENDDO

  status = nf_enddef(mcid)

  DO i = 3, nvars
     PRINT *, 'write ',varnames(i)
!case a 
!bc1:0.08, oc1:0.65, p25:0.27
!case b
!bc1:0.25, oc1:0.50, p25:0.25
!case c
!bc1:0.50, oc1:0.25, p25:0.25
!case d
!bc1:0.25, oc1:0.25, p25:0.50
!case e
!bc1:0.50*2, oc1:0.25*2, p25:0.25*2
!case bc1
!bc1:1.0, oc1:0.0, p25:0.0 
!case oc1
!bc1:0.0, oc1:1.0, p25:0.0 
!case p25
!bc1:0.0, oc1:0.0, p25:1.0 

     
     SELECT CASE ( TRIM(varnames(i)) )
     CASE ('BC1')
        aerosol=tracer_1*weights(1)
     CASE ('OC1')
        aerosol=tracer_1*weights(2)
     CASE ('P25')
        aerosol=tracer_1*weights(3)
     CASE ('BC2')
        IF (nweights==5) THEN 
           aerosol=tracer_1*weights(4)
        ELSE
           aerosol=small
        ENDIF
     CASE ('OC2')
        IF (nweights==5) THEN
           aerosol=tracer_1*weights(5)
        ELSE
           aerosol=small
        ENDIF
     CASE default
        aerosol=small
     END SELECT

     status = nf_put_vara_real(mcid,varid(i-2),start_dims,end_dims,&
          &aerosol(:,:,:,:))
     IF (status /= nf_noerr) THEN
        PRINT *,'error 2'
        PRINT *, 'Error: ', nf_strerror(status)
     ENDIF
  ENDDO

  status = nf_close(mcid)

  DEALLOCATE(tracer_1,tracer_2,aerosol)

END PROGRAM smoke2gocart

