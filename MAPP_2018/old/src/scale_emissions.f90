PROGRAM scale_emissions
!  Program to scale so2 and ec/oc emissions for unperturbed emissions
!  for GOCART species


  IMPLICIT NONE
  INCLUDE 'netcdf.inc'

!  LOGICAL :: request_random_seed=.TRUE.
  LOGICAL :: request_random_seed=.FALSE.

!  REAL, PARAMETER :: factor_so2=0.6,factor_ec_oc=0.7
  REAL, PARAMETER :: factor_so2=1.,factor_ec_oc=1.

  integer  :: jdim
  parameter (jdim=6)

  integer ncid, status
  integer ishape(jdim)
  integer ishape2(jdim)

  INTEGER iishape(jdim)

  CHARACTER cval*150,cval_lscale*150

  character name*31
  INTEGER, PARAMETER :: naero=11,varname_length=12
  
  CHARACTER(len=varname_length), DIMENSION(naero) :: aeronames=(/&
       &"E_PM25I","E_PM25J","E_ECI" ,"E_ECJ" ,"E_ORGI","E_ORGJ",&
       &"E_SO4I" ,"E_SO4J" ,"E_NO3I","E_NO3J","E_PM_10"/)

  CHARACTER(len=varname_length), DIMENSION(naero) :: lscale_aeronames
    
  LOGICAL :: log_scale=.FALSE.

  REAL, PARAMETER :: ratio_min=1.e-2,ratio_max=1.e1,data_min=1.e-10,&
       &value_min=1.e-16

  character (len=31),allocatable, dimension(:)      :: dname
  integer,           allocatable, dimension(:)       :: dval, dval2
  REAL,              ALLOCATABLE, DIMENSION(:,:,:,:) :: DATA, data2,&
       &ratios
  double precision,  allocatable, dimension(:,:,:,:) :: ddata, ddata2
  integer,           allocatable, dimension(:,:,:,:) :: idata, idata2
  character,         allocatable, dimension(:,:,:,:) :: text
  character omit(10)*80
  integer             :: start_dims(4)
  integer             :: dims_in(4), dims_out(4), box_start(3), box_end(3)

  INTEGER             :: dims_lscale(3)

  integer             :: firstS,firstE, secondS,secondE, thirdS,thirdE
  INTEGER             :: idm, ndims, nvars, natt, ngatts, nunlimdimid, iratio
  INTEGER             :: iidm,inatt,iitype,k

  INTEGER             :: i, ii, jj,j, iweg, jweg, isng, jsng, ibtg, jbtg, ix, iy
  integer             :: i_shape_we, i_shape_sn, i_shape_bt
  integer             :: i_shape_we_stag, i_shape_sn_stag, i_shape_bt_stag
  integer             :: ilen, itype, ival, na
  integer             :: mcid
  real                :: dx, rval
  real                :: new_cen
  real                :: okm
  character (len=250)  :: input_file, output_file
  character (len=10)  :: option
  logical             :: debug=.FALSE.
  logical             :: x_ave=.FALSE.
  logical             :: y_ave=.FALSE.
  logical             :: bit64
  integer :: seed_size
  INTEGER, allocatable :: seed_array(:)
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: rcv
  INTEGER :: nx,ny,nt
  REAL :: sum_cv,sum_cv2,mean_cv,rms_cv,std_dev_cv,factor
  INTEGER :: cv_size,stdout=6
  LOGICAL :: generate_random_vector=.TRUE.


  lscale_aeronames="L_"//aeronames

  CALL getarg(1,input_file)
  CALL getarg(2,output_file)

  write(6,*) 
  write(6,*) "#########################################"
  write(6,*) "Running IOWRF "
  write(6,*) 
  write(6,*) "INPUT FILE:         ",trim(input_file)
  write(6,*) "OUTPUT FILE:        ",trim(output_file)

! OPEN INPUT AND OUTPUT FILE
! output_file is input_file_new
  status = nf_open(input_file, 0, ncid)
  if (status .ne. nf_noerr) call handle_err(status)

  status = nf_create(output_file, 0, mcid)

  if (status .ne. nf_noerr) call handle_err(status)

! GET BASIC INFORMTION ABOUT THE FILE
! most important 
!   ndims:  number of dimensions
!   nvars:  number of variables
!   ngatts: number of global attributes
  status = nf_inq(ncid, ndims, nvars, ngatts, nunlimdimid)

  if (status .ne. nf_noerr) call handle_err(status)
  IF (debug) THEN
     write(6,*) 
     write(6,'(4(A,i4))') ' ndims = ',ndims,'    nvars = ',nvars,'    ngatts = ',ngatts, &
          '    nunlimdimid =',nunlimdimid
     write(6,*) 
  ENDIF

! ALLOCATE SOME VARIABLES
  allocate (dval(ndims))
  allocate(dval2(ndims))
  allocate(dname(ndims))

! GET SOME BASIC DIMS FROM INPUT_FILE

  status = nf_get_att_real (ncid, nf_global, 'DX', okm)

  status = nf_get_att_int (ncid, nf_global, 'WEST-EAST_GRID_DIMENSION', iweg)
  status = nf_get_att_int (ncid, nf_global, 'SOUTH-NORTH_GRID_DIMENSION', isng)
  status = nf_get_att_int (ncid, nf_global, 'BOTTOM-TOP_GRID_DIMENSION', ibtg)

! CALCULATE DIMS FOR OUTPUT FILE
  jweg = iweg
  jsng = isng
  jbtg = ibtg
  dx=okm

  IF (debug) THEN
     write(6,*) "BASICS for output file:"
     write(6,*) "       DX= ", okm
     write(6,*) "        X= ", jweg
     write(6,*) "        Y= ", jsng
     write(6,*) "        Z= ", jbtg
  ENDIF


  i_shape_we      = 0
  i_shape_sn      = 0
  i_shape_bt      = 0
  i_shape_we_stag = 0
  i_shape_sn_stag = 0
  i_shape_bt_stag = 0

  do i = 1, ndims
     status = nf_inq_dim(ncid, i, dname(i), dval(i))
     dval2(i) = dval(i)
!       CAUTION -- this stuff is hard-wired
     if (dname(i) .eq. 'west_east_stag') then
        dval2(i) = jweg
        i_shape_we_stag = i
     else if (dname(i) .eq. 'west_east') then
        dval2(i) = jweg-1
        i_shape_we = i
     else if (dname(i) .eq. 'south_north_stag') then
        dval2(i) = jsng
        i_shape_sn_stag = i
     else if (dname(i) .eq. 'south_north') then
        dval2(i) = jsng-1
        i_shape_sn = i
     else if (dname(i) .eq. 'bottom_top_stag') then
        dval2(i) = jbtg
        i_shape_bt_stag = i
     else if (dname(i) .eq. 'bottom_top') then
        dval2(i) = jbtg-1
        i_shape_bt = i
     endif
     if ( dname(i) == "Time" ) then
        status = nf_def_dim(mcid, dname(i), NF_UNLIMITED, i)
     else
        status = nf_def_dim(mcid, dname(i), dval2(i), i)
     end if
     IF (debug) THEN
        write(6,'(i4," : ",A," in = ",i4," (out = ",i4,")")') &
             i,dname(i),dval(i),dval2(i)
     ENDIF
  enddo
  IF (.not. debug) THEN
     write(6,*)
     write(6,*) "Set up file DIMENSIONS"
  ENDIF

! DEALING WITH THE GLOBAL ATTRIBUTES
  IF (debug) THEN
     write(6,*) 
     write(6,*) "FILE attributes:"
  ENDIF

  DO i = 1, ngatts
     status = nf_inq_attname(ncid, nf_global, i, name)
     status = nf_inq_atttype(ncid, nf_global, name, itype)
     status = nf_inq_attlen(ncid, nf_global, name, ilen)

     IF ( itype .EQ. 2 ) THEN        ! characters
        status = nf_get_att_text (ncid, nf_global, name, cval)
        IF (debug) THEN
           WRITE(6,'(i4," : ",A," in = ",A," (out = ",$)') &
                i,name,cval(1:ILEN)
        ENDIF
        IF(name(1:5) .EQ. 'TITLE') THEN
           cval = cval(1:ILEN)//" : iowrf"//option
           ILEN = LEN_TRIM(cval)
        ENDIF
        IF (debug) WRITE(6,'(A,")")') cval(1:ILEN)
        status = nf_put_att_text(mcid, nf_global, name, ILEN,&
             cval(1:ILEN))

     ELSEIF ( itype .EQ. 4 ) THEN     ! integers
        status = nf_get_att_int (ncid, nf_global, name, ival)
        IF (debug) THEN
           WRITE(6,'(i4," : ",A," in = ",i7," (out = ",$)') &
                i,name,ival
        ENDIF
        IF(name .EQ. 'WEST-EAST_GRID_DIMENSION') ival = jweg
        IF(name .EQ. 'SOUTH-NORTH_GRID_DIMENSION') ival = jsng
        IF(name .EQ. 'BOTTOM-TOP_GRID_DIMENSION') ival = jbtg
        IF (debug) WRITE(6,'(i7,")")') ival
        status = nf_put_att_int(mcid, nf_global, name, itype,&
             ILEN, ival)
        
     ELSE IF ( itype .EQ. 5 ) THEN    ! real

        status = nf_get_att_real (ncid, nf_global, name, rval)
        IF (debug) THEN
           WRITE(6,'(i4," : ",A," in = ",G18.10E2," (out = ",$)') &
                i,name,rval        
        ENDIF
        IF(name(1:2) .EQ. 'DX' .OR. name(1:2) .EQ. 'DY') rval = okm
        
        IF (debug) WRITE(6,'(G18.10E2,")")') rval
        status = nf_put_att_real(mcid, nf_global, name, itype,&
             ILEN, rval)

     ENDIF

  ENDDO

  k=0
  
! TRAIN FILE
  train:  DO i = 1, nvars
     status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
     ishape2 = ishape

     DO j=1,naero
        IF (TRIM(cval)==TRIM(lscale_aeronames(j))) CYCLE train
     ENDDO
     
     k=k+1

     status = nf_def_var(mcid, cval, itype, idm, ishape2, k)
     do na = 1, natt
        status = nf_inq_attname(ncid, i, na, name)
        status = nf_copy_att(ncid, i, name, mcid, k)
     enddo
  ENDDO train
  status = nf_enddef(mcid)

! ########## LOOP THROUGH THE DATA 

  write(6,*) "Write file VARIABLES:"
  start_dims = 1


  DO i = 1,nvars
     status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
     ishape2 = ishape
     IF (debug) THEN
        WRITE(6,*) 'VARIABLE: ',TRIM(cval)
     ENDIF

! GET THE DIMS FOR INPUT AND OUTPUT FROM THE SHAPE
     dims_in  = 1

     do ii = 1,idm
        dims_in(ii)  = dval(ishape(ii))
     enddo
  ENDDO

! ALLOCATE THE INPUT AND OUTPUT ARRAYS

  k=0

  outer: DO i = 1,nvars
     status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
     ishape2 = ishape
     IF (debug) THEN
        WRITE(6,*) 'VARIABLE: ',TRIM(cval)
     ENDIF

     DO j=1,naero
        IF (TRIM(cval)==TRIM(lscale_aeronames(j))) CYCLE outer
     ENDDO
  
     k=k+1
   
! GET THE DIMS FOR INPUT AND OUTPUT FROM THE SHAPE
     dims_in  = 1
     dims_out = 1
     DO ii = 1,idm
        dims_in(ii)  = dval(ishape(ii))
     ENDDO

     IF     (itype .EQ. 2) THEN          ! character
        ALLOCATE (text(dims_in(1), dims_in(2), dims_in(3), &
             dims_in(4)))
        status = nf_get_var_text(ncid, i, text)
        IF (debug) WRITE(6,*) '   SAMPLE VALUE = ',text(:,1,1,1)
        status = nf_put_vara_text (mcid, k, start_dims, dims_in, text)
        DEALLOCATE (text)
        
     ELSE IF (itype .EQ. 5) THEN          ! real

        print *,TRIM(cval)
        
        IF (TRIM(cval) == "E_ECI" .OR. &
             &trim(cval) == "E_ECJ" .or. &
             &trim(cval) == "E_ORGI" .or. &
             &trim(cval) == "E_ORGJ" ) then
           log_scale=.TRUE.
           factor=factor_ec_oc
        ELSE IF (TRIM(cval) == "E_SO2") THEN
           log_scale=.TRUE.
           factor=factor_so2
        ENDIF
        
        IF (log_scale) THEN

           ALLOCATE (DATA(dims_in(1), dims_in(2), dims_in(3), &
                dims_in(4)))
           dims_out=dims_in
           
           ALLOCATE(data2(dims_out(1),dims_out(2),dims_out(3), &
                dims_out(4)))
           status = nf_get_var_real(ncid, i, DATA)

           IF (log_scale)  THEN
              DO jj=1,dims_out(4)
                 data2(:,:,:,jj) = DATA(:,:,:,jj)*factor
              ENDDO
              log_scale=.FALSE.
           ELSE
              data2(:,:,:,jj) = DATA(:,:,:,jj)
           ENDIF

           status = nf_put_vara_real (mcid, k, start_dims, dims_out, data2)
           
           DEALLOCATE (DATA)
           DEALLOCATE (data2)
           
        ELSE

!           PRINT *,TRIM(cval),'OUT'
           
           ALLOCATE (DATA(dims_in(1), dims_in(2), dims_in(3), &
                dims_in(4)))
           dims_out=dims_in
           
           ALLOCATE(data2(dims_out(1),dims_out(2),dims_out(3), &
                dims_out(4)))
           status = nf_get_var_real(ncid, i, DATA)
           
           data2 = DATA
           
           status = nf_put_vara_real (mcid, k, start_dims, dims_out, data2)
           
           DEALLOCATE (DATA)
           DEALLOCATE (data2)
           
        ENDIF
        
     ELSE
        STOP 'trouble - do not know the variable type'
     ENDIF
     
  ENDDO outer     ! END OF VARIABLE LOOP
  
  status = nf_close(mcid)
  
  WRITE(6,*) 
  WRITE(6,*) "SUCCESS - we are out of here"      
  WRITE(6,*) "#########################################"

CONTAINS

!---------------------------------------------------------------------
  SUBROUTINE handle_err(status)
    INTEGER status
    WRITE(6,*) 'Error number ',status
    STOP
  END SUBROUTINE handle_err
!---------------------------------------------------------------------

END PROGRAM scale_emissions  
  
