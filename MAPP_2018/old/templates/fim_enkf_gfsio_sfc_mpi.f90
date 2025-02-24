      program fim_enkf_gfsio

      ! convert fim output file (with EnKFIO=.true.) to GFSIO grib format
      ! so that it may then be passed on the GSI.

! ifort -O3 -xT -I../global_chgres.fd -o fim_enkf_gfsio.x fim_enkf_gfsio.f90 ../global_chgres.fd/gfsio_module.o ../global_chgres.fd/gfsio_rst.o -L./slint -lslint -L../lib -lw3_4 -lsp_4 -lsigio_4 -lsfcio_4

      USE SFCIO_MODULE

      implicit none

      TYPE(SFCIO_HEAD) :: SFCHEAD
      TYPE(SFCIO_DATA) :: SFCDATA

     
      INTEGER NREC,NVCOORD,K,IERR,N,IDATE(4),IVS,IENS(2),NANAL,&
              irealf,nfhour,nip,iunit,levso,&
              ihrinc,nptsin,ntrac,nt,lsoil,nglvl,imo,jmo
      REAL FHOUR,pdryini,ptop
      CHARACTER(len=250) datadir, filename
      CHARACTER(len=10) datestring, datestringm1
      character(len=16) curveheader, glvlheader
      character(len=7) charnanal
      character(len=2) charhr
      real, allocatable, dimension(:) :: vardata, lons, lats,zs
      logical lfirst

! mpi definitions.
      include 'mpif.h'
      integer MPI_Status(MPI_STATUS_SIZE),numproc,nproc,iret

      call MPI_Init(iret)
! nproc is process number, numproc is total number of processes.
      call MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
      call MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)

      lfirst = .true.

      call getarg(1,datestring)
      call getarg(2,datestringm1)
      call getarg(3,datadir)
      call getarg(4,charnanal)
      read(charnanal,FMT='(I2)') nglvl
      call getarg(5,charnanal)
      read(charnanal,FMT='(I4)') imo
      call getarg(6,charnanal)
      read(charnanal,FMT='(I4)') jmo
      call getarg(7,charnanal)
      
      read(datestringm1(1:4),'(i4)') idate(4)
      read(datestringm1(5:6),'(i2)') idate(2)
      read(datestringm1(7:8),'(i2)') idate(3)
      read(datestringm1(9:10),'(i2)') idate(1)
! initialize bilinear interpolation (icos to gaussian grid)
      nip=10*(2**nglvl)**2+2
      !print *,nip

!  ALLOCATE DATA FOR GFSIO OUTPUT
      
      allocate(vardata(nip))
      allocate(lons(nip),lats(nip))
      allocate(zs(nip))

      if (nproc .eq. 0) then
         charnanal = 'ensmean'
      else
         write(charnanal,'(a3,i3.3)') 'mem',nproc
      end if

      iunit = 82
      ihrinc = 1

      do nfhour = 3,9,ihrinc

      write(charhr,'(i2.2)') nfhour
      filename = trim(datadir)//"/"//trim(datestring)//"/"//"gfg_"//trim(datestring)//"_fhr"//charhr//"_"//trim(charnanal)
      open(iunit,file=filename,form="unformatted",status="old")
      read(iunit) nptsin, levso, ntrac, ptop
      if (nptsin .ne. nip ) then
         print *,'mismatch npts,should be',nip
         print *,'got',nptsin
         stop
      endif
      read(iunit) lons
      read(iunit) lats
      CALL bilinear_init_i2r_jsw(imo, jmo, lons, lats, nip, .true., 0)      
      read(iunit) zs
      close(iunit)

      ! now do surface files.
      filename = trim(datadir)//"/"//trim(datestring)//"/"//"bfg_"//trim(datestring)//"_fhr"//charhr//"_"//trim(charnanal)
      open(iunit,file=filename,form="unformatted",status="old")
      read(iunit) glvlheader
      read(iunit) curveheader
      lsoil = 4
      call sfcio_alhead(sfchead,ierr,jmo,lsoil)
!     populate sfchead structure.
!     sfcio_head        Surface file header information
!     clabsfc           Character(sfcio_lhead1) ON85 label
!     fhour             Real(sfcio_realkind) forecast hour
!     idate             Integer(sfcio_intkind)(4) initial date
!                       (hour, month, day, 4-digit year)
!     latb              Integer(sfcio_intkind) latitudes
!     lonb              Integer(sfcio_intkind) longitudes
!     ivs               Integer(sfcio_intkind) version number
!     lsoil             Integer(sfcio_intkind) soil levels
!     irealf            Integer(sigio_intkind) floating point flag
!                       (=1 for 4-byte ieee, =2 for 8-byte ieee)
!     lpl               Integer(sfcio_intkind)(latb/2) lons per lat
!     zsoil             Real(sfcio_realkind) soil depths (meter)
      sfchead%fhour = real(nfhour)
      sfchead%idate = idate
      sfchead%latb = jmo
      sfchead%lonb = imo
      sfchead%ivs = 200509
      sfchead%lsoil = lsoil
      sfchead%irealf = 1
      sfchead%lpl = imo
      sfchead%zsoil=(/-0.1,-0.4,-1.0,-2.0/)
      call sfcio_aldata(sfchead,sfcdata,ierr)
      ! use nearest neighbor interp.
      CALL bilinear_init_i2r_jsw(imo, jmo, lons, lats, nip, .true., 1)
!     stc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil temperature in K
      do k=1,lsoil
         read(iunit) vardata
         call bilinear_interp_i2r(1, 1, vardata, sfcdata%stc(:,:,k))
      enddo
!     smc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil volumetric water content in fraction
      do k=1,lsoil
         read(iunit) vardata
         call bilinear_interp_i2r(1, 1, vardata, sfcdata%smc(:,:,k))
      enddo
!     slc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil liquid water content
      do k=1,lsoil
         read(iunit) vardata 
         call bilinear_interp_i2r(1, 1, vardata, sfcdata%slc(:,:,k))
      enddo
!     tsea              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       surface temperature in K
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%tsea)
      sfcdata%tisfc=sfcdata%tsea
!     sheleg            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       snow depth in m
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%sheleg)
!     tg3               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       deep soil temperature in K
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%tg3)
!     zorl              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       roughness in cm
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%zorl)
!     cv                Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud cover in fraction
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%cv)
!     cvb               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud bottom in kpa
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%cvb)
!     cvt               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud top in kpa
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%cvt)
!     alvsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible scattered in fraction
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%alvsf)
!     alvwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible beam in fraction
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%alvwf)
!     alnsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR scattered in fraction
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%alnsf)
!     alnwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR beam in fraction
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%alnwf)
!     slmsk             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       sea-land-ice mask (0-sea, 1-land, 2-ice)
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%slmsk)
!     vfrac             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation fraction in fraction
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%vfrac)
!     canopy            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       canopy water in m
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%canopy)
!     f10m              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       10-meter wind speed over lowest model wind speed
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%f10m)
!     t2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter temperature in K
      read(iunit) vardata 
      !print *,'t2m',minval(vardata),maxval(vardata)
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%t2m)
!     q2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter specific humidity in kg/kg
      read(iunit) vardata 
      !print *,'q2m',minval(vardata),maxval(vardata)
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%q2m)
!     vtype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation type in integer 1-13
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%vtype)
!     stype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       soil type in integer 1-9
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%stype)
!     facsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%facsf)
!     facwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%facwf)
!     uustar            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%uustar)
!     ffmm              Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%ffmm)
!     ffhh              Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%ffhh)
!     hice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%hice)
!     fice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%fice)
!     tprcp             Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%tprcp)
!     srflag            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%srflag)
!     snwdph            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%snwdph)
!     shdmin            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%shdmin)
!     shdmax            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%shdmax)
!     slope             Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%slope)
!     snoalb            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      call bilinear_interp_i2r(1, 1, vardata, sfcdata%snoalb)
!     orog              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       orography in m
      call bilinear_interp_i2r(1, 1, zs, sfcdata%orog)


      filename = trim(datadir)//'/'//trim(datestring)//'/fimfg_'//datestring//'_bf'//charhr//'_'//trim(charnanal)
      print *,'write',filename
      call sfcio_swohdc(iunit,filename,sfchead,sfcdata,ierr)
      call sfcio_axdata(sfcdata,ierr)


      enddo ! loop over nfhour

      call MPI_Barrier(MPI_COMM_WORLD,iret)
      if (nproc .eq. 0) write(6,*) 'all done!'
      call MPI_Finalize(iret)
      if (iret .ne. 0) then
       print *, 'MPI_Finalize error status = ',iret
      end if

      end program fim_enkf_gfsio
