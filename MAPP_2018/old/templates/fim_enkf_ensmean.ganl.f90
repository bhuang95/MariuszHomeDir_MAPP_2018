      program fim_enkf_ensmean

      ! convert fim output file (with EnKFIO=.true.) to GFSIO grib format
      ! so that it may then be passed on the GSI.

! ifort -O3 -xT -I../global_chgres.fd -o fim_enkf_ensmean.x fim_enkf_ensmean.f90 ../global_chgres.fd/gfsio_module.o ../global_chgres.fd/gfsio_rst.o -L./slint -lslint -L../lib -lw3_4 -lsp_4 -lsigio_4 -lsfcio_4

      implicit none

      integer, parameter :: ntracm=3
      integer, parameter :: levso=64

      INTEGER NREC,NVCOORD,K,IERR,N,IDATE(4),IVS,IENS(2),NANAL,&
      icen2,igen,irealf,itrun,ixgr,idpp,idsl,idvc,idvm,idvt,&
      idusr,ncldt,jrec,iorder,idrun,jcap,idrt,nip,iunit,&
      nptsin,levsoin,ntracin,nt,lsoil,nanals,nglvl
      REAL FHOUR,pdryini,ptop
      CHARACTER(len=250) datadir, filename
      CHARACTER(len=10) datestring
      character(len=16) curveheader, glvlheader
      character(len=20) charnanal
      real, allocatable, dimension(:) :: vardata, lons, lats, zs, &
       tsea,sheleg,tg3,zorl,cv,cvb,cvt,alvsf,alvwf,alnsf,alnwf,slmsk,&
       vfrac,canopy,f10m,t2m,q2m,vtype,stype,facsf,facwf,uustar,ffmm,&
       ffhh,hice,fice,slope,tisfc,tprcp,srflag,snwdph,shdmin,shdmax,&
       snoalb
      real, allocatable, dimension(:,:) :: &
        stc,smc,slc,u,v,temp,pressi,pressl
      real, allocatable, dimension(:,:,:) :: tr

      call getarg(1,charnanal)
      read(charnanal,'(i2)') nglvl
      call getarg(2,datestring)
      call getarg(3,charnanal)
      read(charnanal,'(i3.3)') nanals
      call getarg(4,datadir)

      nip=10*(2**nglvl)**2+2
      iunit = 82
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
      

      u=0;v=0;pressi=0;pressl=0;tr=0;temp=0
      stc=0;smc=0;slc=0
      tsea=0;sheleg=0;tg3=0;zorl=0;cv=0;cvb=0;cvt=0
      alvsf=0;alvwf=0;alnsf=0;alnwf=0;slmsk=0
      vfrac=0;canopy=0;f10m=0;t2m=0;q2m=0;vtype=0;stype=0
      facsf=0;facwf=0;uustar=0;ffmm=0;ffhh=0;hice=0;fice=0;slope=0
      tisfc=0;tprcp=0;srflag=0;snwdph=0;shdmin=0;shdmax=0;snoalb=0


      DO nanal=1,nanals
      write(charnanal,'(a3,i3.3)') 'mem',nanal

      filename = trim(datadir)//"/"//trim(datestring)//"/ganl_"//trim(datestring)//"_"//trim(charnanal)
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
      DO k=1,levso
         read(iunit) vardata
         pressl(:,k) = pressl(:,k) + vardata/float(nanals)
      enddo
      DO k=1,levso
         read(iunit) vardata
         pressi(:,k) = pressi(:,k) + vardata/float(nanals)
      enddo
      DO k=1,levso
        read(iunit) vardata
        temp(:,k) = temp(:,k) + vardata/float(nanals)
      enddo
      DO k=1,levso
        read(iunit) vardata
        u(:,k) = u(:,k) + vardata/float(nanals)
      enddo
      DO k=1,levso
        read(iunit) vardata
        v(:,k) = v(:,k) + vardata/float(nanals)
      enddo
      ! read "tracers"
      DO nt=1,ntracm
      DO k=1,levso
        read(iunit) vardata
        tr(:,k,nt) = tr(:,k,nt) + vardata/float(nanals)
      enddo
      enddo
      close(iunit) 
      ENDDO
      filename = trim(datadir)//"/"//trim(datestring)//"/ganl_"//trim(datestring)//"_ensmean"
      open(iunit,file=filename,form="unformatted",status="replace")
      write(iunit) nip, levso, ntracm, ptop
      write(iunit) lons
      write(iunit) lats
      write(iunit) zs
      DO k=1,levso
         write(iunit) pressl(:,k)
      enddo
      DO k=1,levso
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

      end program fim_enkf_ensmean
