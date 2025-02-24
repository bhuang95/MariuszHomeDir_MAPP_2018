      PROGRAM raqms_increment
!  Program to multiply raqms aerosols by ratios
!

      implicit none
      INCLUDE 'netcdf.inc'
      integer  :: jdim
      parameter (jdim=6)
      integer ncid, status
      integer ishape(jdim)
      integer ishape2(jdim)
      character cval*50
      character name*31
      REAL, PARAMETER :: ratio_min=1.e-2,ratio_max=1.e1,data_min=1.e-10,&
           &value_min=1.e-16,s_2_5=0.942,d_2_5=0.286

      REAL :: factor
      

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
      integer             :: firstS,firstE, secondS,secondE, thirdS,thirdE
      integer             :: idm, ndims, nvars, natt, ngatts, nunlimdimid, iratio
      integer             :: i, ii, j, iweg, jweg, isng, jsng, ibtg, jbtg, ix, iy
      integer             :: i_shape_we, i_shape_sn, i_shape_bt
      integer             :: i_shape_we_stag, i_shape_sn_stag, i_shape_bt_stag
      integer             :: ilen, itype, ival, na
      integer             :: mcid
      real                :: dx, rval
      real                :: new_cen
      real                :: okm
      character (len=80)  :: input_file, output_file
      character (len=10)  :: option
      logical             :: debug=.FALSE.
      logical             :: x_ave=.FALSE.
      logical             :: y_ave=.FALSE.
      logical             :: bit64

      CALL getarg(1,input_file)
      CALL getarg(2,output_file)


      write(6,*) 
      write(6,*) "#########################################"
      write(6,*) "Running IOWRF "
      write(6,*) 
      write(6,*) "INPUT FILE:         ",trim(input_file)
      write(6,*) "OUTPUT FILE:        ",trim(output_file)
      write(6,*) "OPTION:             ",option    

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

        IF ( itype .EQ. 5 ) THEN    ! real
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

! TRAIN FILE
      DO i = 1, nvars
        status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
        ishape2 = ishape

        status = nf_def_var(mcid, cval, itype, idm, ishape2, i)
        do na = 1, natt
          status = nf_inq_attname(ncid, i, na, name)
          status = nf_copy_att(ncid, i, name, mcid, i)
        enddo
     ENDDO
     status = nf_enddef(mcid)

! ########## LOOP THROUGH THE DATA 

      write(6,*) "Write file VARIABLES:"
      start_dims = 1

! get ratios

      DO i = 1,nvars
        status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
        ishape2 = ishape
        WRITE(6,*) 'VARIABLE: ',TRIM(cval)

! GET THE DIMS FOR INPUT AND OUTPUT FROM THE SHAPE
        dims_in  = 1

        do ii = 1,idm
          dims_in(ii)  = dval(ishape(ii))
        enddo


! ALLOCATE THE INPUT AND OUTPUT ARRAYS


        IF (itype .EQ. 5) THEN          ! real
           
           IF (cval=='ratios') THEN
              
              ALLOCATE (ratios(dims_in(1), dims_in(2), dims_in(3),&
                   &dims_in(4)))
              
              status = nf_get_var_real(ncid, i, ratios)
              
              WHERE (ratios < ratio_min) ratios=ratio_min
              WHERE (ratios > ratio_max) ratios=ratio_max
              
           ENDIF
           
        ENDIF

     ENDDO
     
!end get ratios

     DO i = 1,nvars
        status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
        ishape2 = ishape
        WRITE(6,*) 'VARIABLE: ',TRIM(cval)

        IF (TRIM(cval)=='DUST_2') THEN
           factor=1.! /d_2_5
        ELSE IF (TRIM(cval)=='SEAS_2') THEN
           factor=1.!/s_2_5
        ELSE
           factor=1.
        ENDIF
           
! GET THE DIMS FOR INPUT AND OUTPUT FROM THE SHAPE
        dims_in  = 1
        dims_out = 1
        DO ii = 1,idm
          dims_in(ii)  = dval(ishape(ii))
       ENDDO

       IF (itype .EQ. 5) THEN          ! real
          
          IF (cval=='ratios') THEN
             
             dims_out=dims_in
             
             status = nf_put_vara_real (mcid, i, start_dims, dims_out, ratios)
             
          ELSE
             
             IF (MINVAL(ratios) < ratio_min) THEN
                PRINT *,'Ratios not read. Check input file'
                PRINT *,'Stopping'
                STOP
             ENDIF
             
             ALLOCATE (DATA(dims_in(1), dims_in(2), dims_in(3), &
                   dims_in(4)))
             dims_out=dims_in
             
             ALLOCATE(data2(dims_out(1),dims_out(2),dims_out(3), &
                  dims_out(4)))
             status = nf_get_var_real(ncid, i, DATA)
             
             data2 = DATA*ratios*factor

             WHERE (data2 < value_min) data2=value_min

             status = nf_put_vara_real (mcid, i, start_dims, dims_out, data2)
             
             DEALLOCATE (DATA)
             DEALLOCATE (data2)
             
          ENDIF
          
       ELSE
          STOP 'trouble - do not know the variable type'
       ENDIF
       
    ENDDO     ! END OF VARIABLE LOOP
    
    status = nf_close(mcid)
    
    WRITE(6,*) 
    WRITE(6,*) "SUCCESS - we are out of here"      
    WRITE(6,*) "#########################################"
    
  END PROGRAM raqms_increment
!---------------------------------------------------------------------
      subroutine handle_err(status)
      integer status
      write(6,*) 'Error number ',status
      stop
      end subroutine
!---------------------------------------------------------------------
