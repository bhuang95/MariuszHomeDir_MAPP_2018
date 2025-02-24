      program fim_enkf_ensmean

      ! convert fim output file (with EnKFIO=.true.) to GFSIO grib format
      ! so that it may then be passed on the GSI.

! ifort -O3 -xT -I../global_chgres.fd -o fim_enkf_ensmean.x fim_enkf_ensmean.f90 ../global_chgres.fd/gfsio_module.o ../global_chgres.fd/gfsio_rst.o -L./slint -lslint -L../lib -lw3_4 -lsp_4 -lsigio_4 -lsfcio_4

      implicit none

      integer, parameter :: ntracm=3
      integer, parameter :: levso=64

      INTEGER NREC,NVCOORD,K,IERR,N,IDATE(4),IVS,IENS(2),NANAL,&
      icen2,igen,irealf,itrun,ixgr,idpp,idsl,idvc,idvm,idvt,&
      idusr,ncldt,jrec,iorder,idrun,jcap,idrt,nfhour,nip,iunit,&
      ihrinc,nptsin,levsoin,ntracin,nt,lsoil,nanals,nglvl
      REAL FHOUR,pdryini,ptop
      CHARACTER(len=250) datadir, filename
      CHARACTER(len=10) datestring
      character(len=16) curveheader, glvlheader
      character(len=20) charnanal,prefix
      character(len=2) charhr
      real, allocatable, dimension(:) :: vardata, lons, lats, zs, &
       tsea,sheleg,tg3,zorl,cv,cvb,cvt,alvsf,alvwf,alnsf,alnwf,slmsk,&
       vfrac,canopy,f10m,t2m,q2m,vtype,stype,facsf,facwf,uustar,ffmm,&
       ffhh,hice,fice,slope,tisfc,tprcp,srflag,snwdph,shdmin,shdmax,&
       snoalb
      real, allocatable, dimension(:,:) :: &
        stc,smc,slc,u,v,temp,pressi,pressl
      real, allocatable, dimension(:,:,:) :: tr

! mpi definitions.
      include 'mpif.h'
      integer MPI_Status(MPI_STATUS_SIZE),numproc,nproc,iret

      call MPI_Init(iret)
! nproc is process number, numproc is total number of processes.
      call MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
      call MPI_Comm_size(MPI_COMM_WORLD,numproc,iret)


      call getarg(1,charnanal)
      read(charnanal,'(i2)') nglvl
      call getarg(2,datestring)
      call getarg(3,charnanal)
      read(charnanal,'(i3.3)') nanals
      call getarg(4,datadir)
      call getarg(5,prefix)

      nip=10*(2**nglvl)**2+2
      iunit = 82
      ihrinc = 1
      lsoil = 4
      allocate(vardata(nip),zs(nip),lons(nip),lats(nip))
      allocate(pressi(nip,levso),pressl(nip,levso),u(nip,levso),v(nip,levso))
      allocate(temp(nip,levso),tr(nip,levso,ntracm))
      allocate(slc(nip,lsoil),stc(nip,lsoil),smc(nip,lsoil))
      allocate(tsea(nip),sheleg(nip),tg3(nip),zorl(nip),cv(nip),cvb(nip),cvt(nip))
      allocate(alvsf(nip),alvwf(nip),alnsf(nip),alnwf(nip),slmsk(nip))
      allocate(vfrac(nip),canopy(nip),f10m(nip),t2m(nip),q2m(nip),vtype(nip),stype(nip))
      allocate(facsf(nip),facwf(nip),uustar(nip),ffmm(nip),ffhh(nip),hice(nip),fice(nip),slope(nip))
      allocate(tisfc(nip),tprcp(nip),srflag(nip),snwdph(nip),shdmin(nip),shdmax(nip),snoalb(nip))
      
      nfhour = 3+nproc*ihrinc

      u=0;v=0;pressi=0;pressl=0;tr=0;temp=0
      stc=0;smc=0;slc=0
      tsea=0;sheleg=0;tg3=0;zorl=0;cv=0;cvb=0;cvt=0
      alvsf=0;alvwf=0;alnsf=0;alnwf=0;slmsk=0
      vfrac=0;canopy=0;f10m=0;t2m=0;q2m=0;vtype=0;stype=0
      facsf=0;facwf=0;uustar=0;ffmm=0;ffhh=0;hice=0;fice=0;slope=0
      tisfc=0;tprcp=0;srflag=0;snwdph=0;shdmin=0;shdmax=0;snoalb=0

      write(charhr,'(i2.2)') nfhour

      do nanal=1,nanals
      write(charnanal,'(a3,i3.3)') 'mem',nanal

      filename = trim(datadir)//"/"//trim(datestring)//"/"//trim(prefix)//trim(datestring)//"_fhr"//charhr//"_"//trim(charnanal)
      print *,trim(filename)
      open(iunit,file=filename,form="unformatted",status="old")
      read(iunit) nptsin, levsoin, ntracin, ptop
      if (nptsin .ne. nip .or. levsoin .ne. levso .or. &
          ntracin .ne. ntracm) then
         print *,'mismatch npts,levso,ntracin should be',nip,levso,ntracm
         print *,'got',nptsin,levsoin,ntracin
         stop
      endif
      read(iunit) lons
      read(iunit) lats
      read(iunit) zs
      do k=1,levso
         read(iunit) vardata
         pressl(:,k) = pressl(:,k) + vardata/float(nanals)
      enddo
      do k=1,levso
         read(iunit) vardata
         pressi(:,k) = pressi(:,k) + vardata/float(nanals)
      enddo
      do k=1,levso
        read(iunit) vardata
        temp(:,k) = temp(:,k) + vardata/float(nanals)
      enddo
      do k=1,levso
        read(iunit) vardata
        u(:,k) = u(:,k) + vardata/float(nanals)
      enddo
      do k=1,levso
        read(iunit) vardata
        v(:,k) = v(:,k) + vardata/float(nanals)
      enddo
      ! read "tracers"
      do nt=1,ntracm
      do k=1,levso
        read(iunit) vardata
        tr(:,k,nt) = tr(:,k,nt) + vardata/float(nanals)
      enddo
      enddo
      close(iunit) 

      ! now do surface files.
      filename = trim(datadir)//"/"//trim(datestring)//"/"//"bfg_"//trim(datestring)//"_fhr"//charhr//"_"//trim(charnanal)
      print *,trim(filename)
      open(iunit,file=filename,form="unformatted",status="old")
      read(iunit) glvlheader
      read(iunit) curveheader
      !print *,glvlheader
      !print *,curveheader
!     stc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil temperature in K
      do k=1,lsoil
         read(iunit) vardata
         stc(:,k) = stc(:,k) + vardata/float(nanals)
      enddo
!     smc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil volumetric water content in fraction
      do k=1,lsoil
         read(iunit) vardata
         smc(:,k) = smc(:,k) + vardata/float(nanals)
      enddo
!     slc               Real(sfcio_realkind)(:,:,:) pointer to lonb*latb*lsoil
!                       soil liquid water content
      do k=1,lsoil
         read(iunit) vardata 
         slc(:,k) = slc(:,k) + vardata/float(nanals)
      enddo
!     tsea              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       surface temperature in K
      read(iunit) vardata 
      tsea = tsea + vardata/float(nanals)
!     sheleg            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       snow depth in m
      read(iunit) vardata 
      sheleg = sheleg + vardata/float(nanals)
!     tg3               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       deep soil temperature in K
      read(iunit) vardata 
      tg3 = tg3 + vardata/float(nanals)
!     zorl              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       roughness in cm
      read(iunit) vardata 
      zorl = zorl + vardata/float(nanals)
!     cv                Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud cover in fraction
      read(iunit) vardata 
      cv = cv + vardata/float(nanals)
!     cvb               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud bottom in kpa
      read(iunit) vardata 
      cvb = cvb + vardata/float(nanals)
!     cvt               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       convective cloud top in kpa
      read(iunit) vardata 
      cvt = cvt + vardata/float(nanals)
!     alvsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible scattered in fraction
      read(iunit) vardata 
      alvsf = alvsf + vardata/float(nanals)
!     alvwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for visible beam in fraction
      read(iunit) vardata 
      alvwf = alvwf + vardata/float(nanals)
!     alnsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR scattered in fraction
      read(iunit) vardata 
      alnsf = alnsf + vardata/float(nanals)
!     alnwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       albedo for near-IR beam in fraction
      read(iunit) vardata 
      alnwf = alnwf + vardata/float(nanals)
!     slmsk             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       sea-land-ice mask (0-sea, 1-land, 2-ice)
      read(iunit) slmsk
!     vfrac             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation fraction in fraction
      read(iunit) vardata 
      vfrac = vfrac + vardata/float(nanals)
!     canopy            Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       canopy water in m
      read(iunit) vardata 
      canopy = canopy + vardata/float(nanals)
!     f10m              Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       10-meter wind speed over lowest model wind speed
      read(iunit) vardata 
      f10m = f10m + vardata/float(nanals)
!     t2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter temperature in K
      read(iunit) vardata 
      t2m = t2m + vardata/float(nanals)
      !print *,'t2m',minval(vardata),maxval(vardata)
!     q2m               Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       2-meter specific humidity in kg/kg
      read(iunit) vardata 
      q2m = q2m + vardata/float(nanals)
      !print *,'q2m',minval(vardata),maxval(vardata)
!     vtype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       vegetation type in integer 1-13
      read(iunit) vtype
!     stype             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       soil type in integer 1-9
      read(iunit) stype
!     facsf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
      read(iunit) vardata 
      facsf = facsf + vardata/float(nanals)
!     facwf             Real(sfcio_realkind)(:,:) pointer to lonb*latb
!                       xxx in fraction
      read(iunit) vardata 
      facwf = facwf + vardata/float(nanals)
!     uustar            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      uustar = uustar + vardata/float(nanals)
!     ffmm              Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      ffmm = ffmm + vardata/float(nanals)
!     ffhh              Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      ffhh = ffhh + vardata/float(nanals)
!     hice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      hice = hice + vardata/float(nanals)
!     fice              Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      fice = fice + vardata/float(nanals)
!     tisfc             Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      tisfc = tisfc + vardata/float(nanals)
!     tprcp             Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      tprcp = tprcp + vardata/float(nanals)
!     srflag            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) srflag
!     snwdph            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      snwdph = snwdph + vardata/float(nanals)
!     shdmin            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      shdmin = shdmin + vardata/float(nanals)
!     shdmax            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      shdmax = shdmax + vardata/float(nanals)
!     slope             Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) slope
!     snoalb            Real(sfcio_realkind)(:,:) pointer to lonb*latb
      read(iunit) vardata 
      snoalb = snoalb + vardata/float(nanals)


      enddo ! end loop over ensemble member.

      filename = trim(datadir)//"/"//trim(datestring)//"/"//trim(prefix)//trim(datestring)//"_fhr"//charhr//"_ensmean"
      open(iunit,file=filename,form="unformatted",status="replace")
      write(iunit) nip, levso, ntracm, ptop
      write(iunit) lons
      write(iunit) lats
      write(iunit) zs
      do k=1,levso
         write(iunit) pressl(:,k)
      enddo
      do k=1,levso
         write(iunit) pressi(:,k)
      enddo
      do k=1,levso
         write(iunit) temp(:,k)
      enddo
      do k=1,levso
         write(iunit) u(:,k)
      enddo
      do k=1,levso
         write(iunit) v(:,k)
      enddo
      do nt=1,ntracm
      do k=1,levso
         write(iunit) tr(:,k,nt)
      enddo
      enddo
      close(iunit) 

      filename = trim(datadir)//"/"//trim(datestring)//"/"//"bfg_"//trim(datestring)//"_fhr"//charhr//"_ensmean"
      open(iunit,file=filename,form="unformatted",status="replace")
      write(iunit) glvlheader
      write(iunit) curveheader
      do k=1,lsoil
         write(iunit) stc(:,k)
      enddo
      do k=1,lsoil
         write(iunit) smc(:,k)
      enddo
      do k=1,lsoil
         write(iunit) slc(:,k)
      enddo
      write(iunit) tsea
      write(iunit) sheleg
      write(iunit) tg3
      write(iunit) zorl
      write(iunit) cv
      write(iunit) cvb
      write(iunit) cvt
      write(iunit) alvsf
      write(iunit) alvwf
      write(iunit) alnsf
      write(iunit) alnwf
      write(iunit) slmsk
      write(iunit) vfrac
      write(iunit) canopy
      write(iunit) f10m
      write(iunit) t2m
      write(iunit) q2m
      write(iunit) vtype
      write(iunit) stype
      write(iunit) facsf
      write(iunit) facwf
      write(iunit) uustar
      write(iunit) ffmm
      write(iunit) ffhh
      write(iunit) hice
      write(iunit) fice
      write(iunit) tisfc
      write(iunit) tprcp
      write(iunit) srflag
      write(iunit) snwdph
      write(iunit) shdmin
      write(iunit) shdmax
      write(iunit) slope
      write(iunit) snoalb
      close(iunit)

      call MPI_Barrier(MPI_COMM_WORLD,iret)
      if (nproc .eq. 0) write(6,*) 'all done!'
      call MPI_Finalize(iret)
      if (iret .ne. 0) then
       print *, 'MPI_Finalize error status = ',iret
      end if

      end program fim_enkf_ensmean
