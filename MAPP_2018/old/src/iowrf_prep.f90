      PROGRAM iowrf
!  Program to read/write wrf output. 
!program to dump binary from nc

      implicit none
      INCLUDE 'netcdf.inc'
      integer  :: jdim
      parameter (jdim=6)
      integer ncid, status
      integer ishape(jdim)
      integer ishape2(jdim)
      character cval*50
      character name*31
      character (len=31),allocatable, dimension(:)      :: dname
      integer,           allocatable, dimension(:)       :: dval, dval2
      real,              allocatable, dimension(:,:,:,:) :: data, data2
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
      character (len=120)  :: input_file, output_file
      character (len=10)  :: option
      character (len=19)  :: date
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
      WRITE(6,*) 
      WRITE(6,'(4(A,i4))') ' ndims = ',ndims,'    nvars = ',nvars,'    ngatts = ',ngatts, &
           '    nunlimdimid =',nunlimdimid
      WRITE(6,*) 

! ALLOCATE SOME VARIABLES
      allocate (dval(ndims))
      allocate(dval2(ndims))
      allocate(dname(ndims))

! GET SOME BASIC DIMS FROM INPUT_FILE
      dx = -99.
      status = nf_get_att_real (ncid, nf_global, 'DX', dx)
      status = nf_get_att_int (ncid, nf_global, 'WEST-EAST_GRID_DIMENSION', iweg)
      status = nf_get_att_int (ncid, nf_global, 'SOUTH-NORTH_GRID_DIMENSION', isng)
      status = nf_get_att_int (ncid, nf_global, 'BOTTOM-TOP_GRID_DIMENSION', ibtg)
      WRITE(6,*) "BASICS from input file:"
      WRITE(6,*) "       DX= ", dx
      WRITE(6,*) "        X= ", iweg
      WRITE(6,*) "        Y= ", isng
      WRITE(6,*) "        Z= ", ibtg-1
      if (dx .lt. 0.) stop 'dx is bad'

! CALCULATE DIMS FOR OUTPUT FILE
      okm = dx
      jweg = iweg
      jsng = isng
      jbtg = ibtg

      WRITE(6,*) "BASICS for output file:"
      WRITE(6,*) "       DX= ", okm
      WRITE(6,*) "        X= ", jweg
      WRITE(6,*) "        Y= ", jsng
      WRITE(6,*) "        Z= ", jbtg-1
      !! We also need to fix the CEN_LAT and CEN_LON later, so get
      !! the middle of the new domain
      ix = int((jweg-1)/2.)
      iy = int((jsng-1)/2.)
      if ( ix .eq. int(jweg/2.) ) x_ave = .TRUE.
      if ( iy .eq. int(jsng/2.) ) y_ave = .TRUE.
      ix = int(jweg/2.)
      iy = int(jsng/2.)


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

      enddo


! TRAIN FILE
      do i = 1, nvars
        status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
        ishape2 = ishape

        status = nf_def_var(mcid, cval, itype, idm, ishape2, i)
        do na = 1, natt
          status = nf_inq_attname(ncid, i, na, name)
          status = nf_copy_att(ncid, i, name, mcid, i)
        enddo
      enddo
      status = nf_enddef(mcid)

! ########## LOOP THROUGH THE DATA 
      write(6,*) "Write file VARIABLES:"
      start_dims = 1
      DO i = 1, nvars
         status = nf_inq_var(ncid, i, cval, itype, idm, ishape, natt)
         ishape2 = ishape
         WRITE(6,*) 'VARIABLE: ',TRIM(cval)
         
! GET THE DIMS FOR INPUT AND OUTPUT FROM THE SHAPE
         dims_in  = 1
         dims_out = 1
         DO ii = 1,idm
            dims_in(ii)  = dval(ishape(ii))
            dims_out(ii) = dval2(ishape2(ii))
         ENDDO

         IF (itype .EQ. 5) THEN          ! real
            ALLOCATE (DATA(dims_in(1), dims_in(2), dims_in(3), &
                 dims_in(4)))
            status = nf_get_var_real(ncid, i, DATA)

!dump binary

            OPEN(unit=51,file=output_file,form='unformatted')
            PRINT *,dims_in(1),dims_in(2),dims_in(3),dims_in(4)
            PRINT *,MINVAL(DATA),MAXVAL(DATA)
            WRITE(51)dims_in(1),dims_in(2),dims_in(3),dims_in(4)
            WRITE(51)DATA
            CLOSE(51)

         ENDIF

      ENDDO     ! END OF VARIABLE LOOP
      
      status = nf_close(mcid)

      write(6,*) 
      write(6,*) "SUCCESS - we are out of here"      
      write(6,*) "#########################################"

      end program iowrf
!---------------------------------------------------------------------
      subroutine handle_err(status)
      integer status
      write(6,*) 'Error number ',status
      stop
      end subroutine
!---------------------------------------------------------------------
