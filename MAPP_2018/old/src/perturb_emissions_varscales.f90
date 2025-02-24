PROGRAM perturb_emissions_varscales
!  Program to produce ensemble of emissions based on
!  scales determined by R from emissions maps
!  for GOCART species
!  Uses recursive filters from wrfvar code
!  assumes that logarithms of emissions have normal distributions
!  added 40% reduction of so2 and 30% reduction for ec and oc
!  changed to new routines which accept variable horizontal lenghtscales

  USE da_rf_cv3

  IMPLICIT NONE
  INCLUDE 'netcdf.inc'

!  LOGICAL :: request_random_seed=.TRUE.
  LOGICAL :: request_random_seed=.FALSE.

!recursive filter parameters
  REAL, PARAMETER :: factor_so2=1.0,factor_ec_oc=1.0
  REAL, PARAMETER :: stdev_log=0.5 

  INTEGER :: seed_value,seed_newvalue
  CHARACTER(len=3) :: cseed_value
  
  INTEGER :: ndeg,nta
  REAL :: swidth,rsamp
  REAL, DIMENSION(:,:), ALLOCATABLE :: rtable
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: lscale,rf_scale

  REAL, DIMENSION(:), ALLOCATABLE :: rbe

  integer  :: jdim
  parameter (jdim=6)

  integer ncid, status
  integer ishape(jdim)
  integer ishape2(jdim)

  INTEGER iishape(jdim)

  CHARACTER cval*150,cval_lscale*150

  character name*31
  INTEGER, PARAMETER :: naero=6,varname_length=12
  
  CHARACTER(len=varname_length), DIMENSION(naero) :: aeronames=(/&
       &"E_PM25I","E_PM25J","E_ECI" ,"E_ECJ" ,"E_ORGI","E_ORGJ"/)
!       &"E_SO4I" ,"E_SO4J" ,"E_NO3I","E_NO3J","E_PM_10"/)

  CHARACTER(len=varname_length), DIMENSION(naero) :: lscale_aeronames
    
  LOGICAL :: logaero=.FALSE.,loggas=.FALSE.

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

  CALL prepare4filter

  CALL getarg(1,input_file)
  CALL getarg(2,output_file)
  CALL getarg(3,cseed_value)
  READ(cseed_value,*)seed_value


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
        
        IF (TRIM(cval) == "E_PM25I" .OR. TRIM(cval) == "E_PM25J") THEN
           seed_newvalue=seed_value
           logaero=.true.
           factor=1.
        else if (trim(cval) == "E_ECI" .or. &
             &trim(cval) == "E_ECJ") then
           seed_newvalue=seed_value+150
           logaero=.TRUE.
           factor=factor_ec_oc
        ELSE IF (TRIM(cval) == "E_ORGI" .OR. &
             &TRIM(cval) == "E_ORGJ" ) THEN
           seed_newvalue=seed_value+250
           logaero=.TRUE.
           factor=factor_ec_oc
        ELSE
           logaero=.FALSE.
           loggas=.FALSE.
        endif
        
        IF (logaero .OR. loggas) THEN

           IF (logaero) THEN

              IF (generate_random_vector) THEN
                 nx=dims_in(1)
                 ny=dims_in(2)
                 nt=dims_in(4)

                 IF (.NOT. ALLOCATED(rcv)) THEN 
                    ALLOCATE(rcv(nx,ny,nt))
                    cv_size=nx*ny
                    CALL RANDOM_SEED(size=seed_size)
                    ALLOCATE(seed_array(1:seed_size))
                 ENDIF
                 
                 IF (request_random_seed) THEN
                    CALL RANDOM_SEED()
                 ELSE
                    seed_array=seed_newvalue
                    CALL RANDOM_SEED(put=seed_array(1:seed_size))
                 ENDIF
                 
                 sum_cv = 0.0
                 sum_cv2 = 0.0
                 
                 DO ii=1,nx
                    DO jj=1,ny
                       CALL da_gauss_noise(rcv(ii,jj,1))
                       sum_cv = sum_cv + rcv(ii,jj,1)
                       sum_cv2 = sum_cv2 + rcv(ii,jj,1) * rcv(ii,jj,1)
                    ENDDO
                 ENDDO
                 
                 DO ii=1,nt
                    rcv(:,:,ii)=rcv(:,:,1)
                 ENDDO

                 mean_cv = sum_cv / REAL(cv_size)
                 rms_cv = SQRT(sum_cv2 / REAL(cv_size))
                 std_dev_cv = SQRT(rms_cv * rms_cv - mean_cv * mean_cv)
                 
                 WRITE(unit=stdout,fmt='(a)')&
                      &' Gaussian (Normal) noise statistics:'
                 WRITE(unit=stdout,fmt='(a)')'Mean, RMS, stdev'
                 WRITE(unit=stdout,fmt='(3f15.5)'),&
                      &mean_cv,rms_cv,std_dev_cv
                 
                 cval_lscale='L_'//TRIM(ADJUSTL(cval))
                 
                 WRITE(6,*) 'Calculating perturbations'

                 IF (debug) THEN
                    WRITE(6,*) 'VARIABLE: ',TRIM(cval)
                    WRITE(6,*) 'VARIABLE: ',TRIM(cval_lscale)
                 ENDIF

                 status = nf_inq_varid(ncid, cval_lscale,ii)
                 
                 IF (status /= 0) THEN
                    PRINT *,'nf_inq_varid problem - stopping ',status
                 ENDIF
                 
                 status = nf_inq_var(ncid, ii, cval_lscale, iitype, &
                      &iidm, iishape, inatt)

                 IF (status /= 0) THEN
                    PRINT *,'nf_inq_var problem - stopping ',status
                    STOP
                 ENDIF
                 
! GET THE DIMS FOR INPUT AND OUTPUT FROM THE SHAPE
                 
                 DO jj = 1,iidm
                    dims_lscale(jj)  = dval(iishape(jj))
                 ENDDO

                 IF (.NOT. ALLOCATED(lscale)) &
                      ALLOCATE(lscale(dims_lscale(1),dims_lscale(2),&
                      &dims_lscale(3)))
                 
                 IF (.NOT. ALLOCATED(rf_scale)) &
                      ALLOCATE(rf_scale(dims_lscale(1),dims_lscale(2),&
                      &dims_lscale(3)))
                 
                 status = nf_get_var_real(ncid, ii, lscale)

                 IF (status /= 0) THEN
                    PRINT *,'nf_get_var_real problem - stopping ',status
                    STOP
                 ENDIF

!this two below are equivalent
!                rf_scale=l_gaspari*0.388/SQRT(8.)
                 
!wrfvar scale
!                rf_scale=l_gaspari*SQRT(0.3)/4.

!ncep scale 2*wrfvar

                 rf_scale=1./(2.*lscale*SQRT(0.3)/2.)
                 
                 DO ii=1,nt
                    CALL smoothx(nx-1,ny-1,rcv(:,:,ii),rf_scale(:,:,ii),&
                         & ndeg,rbe,nta,swidth,rtable)
                    
                    CALL smoothy(nx-1,ny-1,rcv(:,:,ii),rf_scale(:,:,ii),&
                         &ndeg,rbe,nta,swidth,rtable)
                 ENDDO

                 sum_cv = 0.0
                 sum_cv2 = 0.0
                 
                 DO ii=1,nx
                    DO jj=1,ny
                       sum_cv = sum_cv + rcv(ii,jj,1)
                       sum_cv2 = sum_cv2 + rcv(ii,jj,1) * rcv(ii,jj,1)
                    ENDDO
                 ENDDO
                 
                 mean_cv = sum_cv / REAL(cv_size)
                 rms_cv = SQRT(sum_cv2 / REAL(cv_size))
                 std_dev_cv = SQRT(rms_cv * rms_cv - mean_cv * mean_cv)

                 rcv=rcv/std_dev_cv
                 
                 DO ii=1,nt
                    rcv(:,:,ii)=rcv(:,:,1)
                 ENDDO

              ENDIF
              
           ENDIF
           
           ALLOCATE (DATA(dims_in(1), dims_in(2), dims_in(3), &
                dims_in(4)))
           dims_out=dims_in
           
           ALLOCATE(data2(dims_out(1),dims_out(2),dims_out(3), &
                dims_out(4)))
           status = nf_get_var_real(ncid, i, DATA)

           IF (loggas)  THEN
              DO jj=1,dims_out(4)
                 data2(:,:,:,jj) = DATA(:,:,:,jj)*factor
              ENDDO
              loggas=.FALSE.
           ELSE
              DO jj=1,dims_out(4)
                 data2(:,:,1,jj) = DATA(:,:,1,jj)*&
!                      &(EXP(rcv(:,:,jj)*stdev_log)*factor
!correction so that mean is equal 1
!for lognormal distribution mean=exp(mu+sigma^2/2)
                      &MAX((EXP(rcv(:,:,jj)*stdev_log)-&
                      &EXP(0.5*stdev_log**2)+1.),0.)*factor
                 data2(:,:,2:,jj)=DATA(:,:,2:,jj)*factor
              ENDDO
              logaero=.FALSE.
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
  
  DEALLOCATE(lscale,rf_scale)

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
  
  SUBROUTINE prepare4filter

    USE kinds, ONLY: r_kind,i_kind
    USE constants, ONLY: ione,zero,half,one

    REAL(r_kind), DIMENSION(:,:), ALLOCATABLE :: table
    REAL(r_kind), DIMENSION(:), ALLOCATABLE :: be

! local variables
    REAL(r_kind), DIMENSION(:), ALLOCATABLE :: dsh,rate
    REAL(r_kind), DIMENSION(:,:), ALLOCATABLE :: turn
    
    REAL(r_kind) :: tin,samp

    ndeg=4
    nta=5600
    swidth=10.

    ALLOCATE(be(ndeg),table(nta,ndeg),rate(ndeg),turn(ndeg,ndeg))
    ALLOCATE(rtable(nta,ndeg),rbe(ndeg))

    CALL rfdpar1(be,rate,ndeg) 
    CALL rfdpar2(be,rate,turn,samp,ndeg) 
! that needs to be double prec. - taken from ncep routines in gsi distrib
! to prevent lack of convergence in a routine    

    ALLOCATE(dsh(nta))

    tin=swidth/real(nta)

    DO i=1,nta
       dsh(i)=REAL(i-1)*tin
    ENDDO

    CALL rfdparv(dsh,rate,table,nta,ndeg)
! that needs to be double prec. - taken from ncep routines in gsi distrib

    rtable=table
    rbe=be
    rsamp=samp

    DEALLOCATE(be,table,rate,turn)

  END SUBROUTINE prepare4filter
  
END PROGRAM perturb_emissions_varscales

!routines for wrfvar - redundant

SUBROUTINE da_recursive_filter_1d(pass, alpha, field, n)

!---------------------------------------------------------------------------
! Purpose: Perform one pass of recursive filter on 1D array.
!
! Method:  Perform right-moving filter followed by left-moving filter.
!---------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(in)    :: pass           ! Current pass of filter.
  REAL,    INTENT(in)    :: alpha          ! Alpha coefficient for RF.
  REAL, DIMENSION(n), INTENT(inout) :: field       ! Array to be filtered.
  INTEGER, INTENT(in)    :: n              ! Size of field array.

  INTEGER :: j              ! Loop counter.
  REAL    :: one_alpha      ! 1 - alpha.
  REAL    :: a(1:n)         ! Input field.
  REAL    :: b(1:n)         ! Field after left-right pass.
  REAL    :: c(1:n)         ! Field after right-left pass.

!  IF (trace_use_dull) CALL da_trace_entry("da_recursive_filter_1d")

!-------------------------------------------------------------------------
! [1.0] Initialise:
!-------------------------------------------------------------------------

  one_alpha = 1.0 - alpha

  a(1:n) = field(1:n)

!-------------------------------------------------------------------------
! [2.0] Perform right-moving filter:
!-------------------------------------------------------------------------

! use turning conditions as in the appendix of Hayden & Purser (1995):

  IF (pass == 1) THEN
     b(1) = one_alpha * a(1)
  ELSE IF (pass == 2) THEN
     b(1) = a(1) / (1.0 + alpha)
  ELSE
     b(1) = one_alpha * (a(1) - alpha**3 * a(2)) / (1.0 - alpha**2)**2
  END IF

! [2.2] Perform pass left to right:

  DO j = 2, n
     b(j) = alpha * b(j-1) + one_alpha * a(j)
  END DO

!-------------------------------------------------------------------------
! [3.0] Perform left-moving filter:
!-------------------------------------------------------------------------

! use turning conditions as in the appendix of Hayden & Purser (1995):

  IF (pass == 1) THEN
     c(n) = b(n) / (1.0 + alpha)
  ELSE
     c(n) = one_alpha * (b(n) - alpha**3 * b(n-1)) / (1.0 - alpha**2)**2
  END IF

! [3.2] Perform pass left to right:

  DO j = n-1, 1, -1
     c(j) = alpha * c(j+1) + one_alpha * b(j)
  END DO

  field(1:n) = c(1:n)


!  IF (trace_use_dull) CALL da_trace_exit("da_recursive_filter_1d")

END SUBROUTINE da_recursive_filter_1d

SUBROUTINE da_perform_2drf(ni, nj, num_passes, rf_scale, field)

!-----------------------------------------------------------------------
! Purpose: TBD
!-----------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER, INTENT(in)    :: ni               ! Array dimension 1.
  INTEGER, INTENT(in)    :: nj               ! Array dimension 2.
  INTEGER, INTENT(in)    :: num_passes       ! Number of passes of RF.
  REAL,    INTENT(in)    :: rf_scale         ! Recursive filter scaling parameter.
  REAL,    INTENT(inout) :: field(1:ni,1:nj) ! Field to be filtered.

  INTEGER               :: i, j, pass       ! Loop counters.
  REAL                  :: e, alpha         ! Recursive filter parameters.
  REAL                  :: mean_field       ! Mean field.

!  IF (trace_use) CALL da_trace_entry("da_perform_2drf")

  e = 0.25 * num_passes / (rf_scale * rf_scale)
  alpha = 1 + e - SQRT(e * (e + 2.0))

  mean_field = SUM(field(1:ni,1:nj)) / REAL(ni*nj)

  DO pass = 1, num_passes
! Perform filter in I-direction:
     DO j = 1, nj
        CALL da_recursive_filter_1d(pass, alpha, field(1:ni,j), ni)
     END DO


! Perform filter in J-direction:
     DO i = 1, ni
        CALL da_recursive_filter_1d(pass, alpha, field(i,1:nj), nj)
     END DO

  END DO

!  IF (trace_use) CALL da_trace_exit("da_perform_2drf")

END SUBROUTINE da_perform_2drf

SUBROUTINE da_gauss_noise( z)

!-----------------------------------------------------------------------
! Purpose: TBD
!-----------------------------------------------------------------------

  IMPLICIT NONE

  REAL,INTENT(out)     :: z
  REAL                 :: x, y, r, coeff

!  IF (trace_use) CALL da_trace_entry("da_gauss_noise")

! [2.1] Get two uniform variate random numbers in range 0 to 1:

  DO
     CALL RANDOM_NUMBER( x)
     CALL RANDOM_NUMBER( y)

! [2.2] Transform to range -1 to 1 and calculate sum of squares:

     x = 2.0 * x - 1.0
     y = 2.0 * y - 1.0
     r = x * x + y * y

     IF (r > 0.0 .AND. r < 1.0) EXIT        
  END DO

! [2.3] use Box-Muller transformation to get normal deviates:

  coeff = SQRT( -2.0 * LOG(r) / r)         
  z = coeff * x

!  IF (trace_use) CALL da_trace_exit("da_gauss_noise")

END SUBROUTINE da_gauss_noise

SUBROUTINE da_calculate_rf_factors (rf_lengthscale, rf_alpha, rf_scale_factor,num_passes,kz)

   !---------------------------------------------------------------------------
   ! Purpose: Calculate:
   !          1) Alpha value for recursive filter.
   !          2) Turning conditions appropriate for B=UU^T RF.
   !          3) Generic (depends only on num_passes) rescaling factor.
   !          4) Grid-dependent (hopefully temporary) scaling factor - removed.
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(in)    :: num_passes
   INTEGER, INTENT(in)    :: kz                     ! 3rd array dimension.


   REAL, INTENT(in)   :: rf_lengthscale(kz)      ! Non-dim. R.F. lengthscale.
   REAL, INTENT(out)  :: rf_alpha(kz)            ! RF alpha factor.
   REAL, INTENT(out)  :: rf_scale_factor(kz)     ! Variance scaling factor.


      
   INTEGER, PARAMETER :: n = 500                ! 2n +1 = # pts in delta func.
   INTEGER            :: pass                   ! Pass of recursive filter.
   INTEGER            :: k                      ! Loop counter
   ! integer          :: nn                       ! Loop counter
      
   REAL               :: rf_e                   ! RF E factor.      

   REAL, DIMENSION(-n:n) :: field_in, field_out  ! Working field.
   ! real, dimension(-n:n) :: field_out1  ! Working field.

   !-------------------------------------------------------------------------
   ! [1.0]: Initialise:
   !-------------------------------------------------------------------------  

   rf_scale_factor(:) = 0.0
   
   DO k = 1, kz

      !-------------------------------------------------------------------------
      ! [2.0]: Calculate RF alpha:
      !-------------------------------------------------------------------------  

      rf_e = 0.25 * num_passes / (rf_lengthscale(k) * rf_lengthscale(k))
      rf_alpha(k) = 1.0 + rf_e - SQRT(rf_e * (rf_e + 2.0))

      !-------------------------------------------------------------------------
      ! [3.0]: Calculate rescaling factor:
      !-------------------------------------------------------------------------

      ! [3.1]: Calculate generic rescaling (normalise zero distance to 1):
      ! For num_passes=2 (SOAR) = 4*rf_lengthscale.
      ! For num_passes=infinity (Gaussian) = sqrt(8*pi)*rf_lengthscale.

      field_in(-n:n) = 0.0
      field_in(0) = 1.0
      field_out(-n:n) = field_in(-n:n)

      DO pass = 1, num_passes / 2
         CALL da_recursive_filter_1d_adj(pass, rf_alpha(k), field_out, 2*n+1)
      END DO

      DO pass = 1, num_passes / 2
         CALL da_recursive_filter_1d(pass, rf_alpha(k), field_out, 2*n+1)
      END DO

      rf_scale_factor(k) = 1.0 / field_out(0)

      ! Uncomment the following to test equivalence of UU^T and RF:
      ! write(unit=stdout,fmt='(A,f15.5)') &
      !    ' RF Scaling Factor = ', 1.0 / field_out(0)
      ! field_out1(-n:n) = field_in(-n:n)
      ! do pass = 1, num_passes
      !    call da_recursive_filter_1d(pass, rf_alpha(k), field_out1, 2*n+1)
      ! end do

      ! do nn = -n, n
      !    write(unit=stdout,fmt='(2i5,4f12.5)')k, nn, field_in(nn), &
      !                             field_out(nn) / field_out(0), &
      !                             field_out1(nn) / field_out1(0), &
      !                             exp(-0.125*(real(nn)/rf_lengthscale(k))**2)
      ! end do

   END DO ! End loop over k

END SUBROUTINE da_calculate_rf_factors
      

SUBROUTINE da_recursive_filter_1d_adj(PASS, alpha, field, n)

   !---------------------------------------------------------------------------
   ! Purpose: Perform one pass of recursive filter on 1D array - adjoint.
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER, INTENT(in)    :: pass           ! Current pass of filter.
   REAL, INTENT(in)       :: alpha          ! Alpha coefficient for RF.
   REAL, INTENT(inout)    :: field(1:n)       ! Array to be filtered.
   INTEGER, INTENT(in)    :: n              ! Size of field array.
   
   INTEGER                :: j              ! Loop counter.
   REAL                   :: one_alpha      ! 1 - alpha.
   REAL                   :: a(1:n)         ! Input field.
   REAL                   :: b(1:n)         ! Field after left-right pass.
   REAL                   :: c(1:n)         ! Field after right-left pass.

   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------
   

   one_alpha = 1.0 - alpha

   !-------------------------------------------------------------------------
   ! [4.0] Copy and tidy up:
   !-------------------------------------------------------------------------

   c(1:n) = field(1:n)

   !-------------------------------------------------------------------------
   ! [3.0] Perform left-moving filter:
   !-------------------------------------------------------------------------

   ! [3.2] Perform pass left to right:

   b(1:n) = 0.0   

   DO j = 1, n-1
      c(j+1) = c(j+1) + alpha * c(j)
      b(j) = one_alpha * c(j)
   END DO

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   IF (pass == 1) THEN
      b(n) = b(n) + c(n) / (1.0 + alpha)
   ELSE
      b(n) = b(n) + one_alpha * c(n) / (1.0 - alpha**2)**2
      b(n-1) = b(n-1) - one_alpha * alpha**3 * c(n) / (1.0 - alpha**2)**2
   END IF

   !-------------------------------------------------------------------------
   ! [2.0] Perform right-moving filter:
   !-------------------------------------------------------------------------

   a(1:n) = 0.0

   ! [2.2] Perform pass left to right:

   DO j = n, 2, -1
      b(j-1) = b(j-1) + alpha * b(j)
      a(j) = a(j) + one_alpha * b(j)
   END DO

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   IF (pass == 1) THEN
      a(1) = a(1) + one_alpha * b(1)
   ELSE IF (pass == 2) THEN
      a(1) = a(1) + b(1) / (1.0 + alpha)
   ELSE
      a(1) = a(1) + one_alpha * b(1) / (1.0 - alpha**2)**2
      a(2) = a(2) - one_alpha * alpha**3 * b(1) / (1.0 - alpha**2)**2
   END IF

   field(1:n) = a(1:n)

 END SUBROUTINE da_recursive_filter_1d_adj





