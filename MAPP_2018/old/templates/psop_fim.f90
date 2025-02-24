program psop
 use constants, only: pi, rd, grav, cp, rearth, deg2rad, rad2deg, init_constants, init_constants_derived
 use random_normal
 implicit none
 character(len=120) filenamein,obsfile,filename,obsfileout
 character(len=10) datestring
 integer npts,ntrac,k,nt,ierr,nanals,nfhr,nobstot,nlevs,&
         nob,nanal,numproc,nproc,i,iunit,iunitsig,fhmin,fhmax,fhout,fhanal,ntimes,&
         nchar,nreal,ii,nn,nlevt1,nlevt2,ntime,np,nobsh,izob,iunit_nml,iunito,idate
 real dtob,zerr,anal_obt,anal_oblapse,anal_obp,zob_pert,&
      slpob,bias,preduce,palt,zthresh,zdiff,altob,ptop
 character(len=2) charfhr
 character(len=3) charnanal
 real, dimension(:), allocatable :: lats, lons
 real, dimension(:), allocatable :: oblocx,oblocy,ob,zob,obtime,stdev,&
                                    anal_ob,anal_obz,oborig,ob_pert,stdevorig
 real, dimension(:), allocatable :: zsg,analzs
 real, dimension(:,:), allocatable :: tempg,psig,pslg,ob_ens,&
       src_llpoints,tgt_llpoints,analpress,analtemp,anallapse,analps,zg,rdiagbuf
 integer, allocatable, dimension(:) :: stattype,iuseob
 character(8),allocatable,dimension(:):: cdiagbuf
 character (len=1) :: obtype
 character(len=19), allocatable :: statid(:)
 character (len=30) :: statname
 character (len=13) :: obid
 namelist /nam_psop/nlevt1,nlevt2,zerr,fhmin,fhmax,fhout,fhanal,datestring,&
                    nanals,npts,obsfile,zthresh
! mpi definitions.
 include 'mpif.h'
 integer MPI_Status(MPI_STATUS_SIZE)

 call MPI_Init(ierr)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

 call init_constants(.false.) ! initialize constants.
 call init_constants_derived()

 nchar=1; nreal=19
 iunit = 7
 iunitsig = 22
 iunit_nml = 912
 iunito = 9

 ! read namelist from file on all processors.
 zthresh = 9.9e31
 zerr = 0
 open(iunit_nml,file='psop.nml',form='formatted')
 read(iunit_nml,nam_psop)
 close(iunit_nml)
 if (nproc .eq. 0) write(6,nam_psop)


 if (numproc .lt. nanals) then
    print *,numproc,nanals
    print *,'warning, numproc too small!'
    call MPI_Finalize(ierr)
    stop
 end if

 ntimes = 1+((fhmax-fhmin)/fhout)

 nanal = nproc + 1

 call set_random_seed(0,nproc) ! set random seed.

!==> read in obs data (on root process).
 
 if (nproc .eq. 0) then
 
 print *,trim(adjustl(obsfile))
 open(149,form="formatted",file=trim(adjustl(obsfile)))
 print *, filename
 nobstot = 0
 do 
   read(149,9801,err=199,end=199) obtype
   nobstot = nobstot + 1
 enddo
 199 continue

 end if
 
 call MPI_Bcast(nobstot,1,MPI_INTEGER,0,MPI_COMM_WORLD,MPI_Status,ierr)

!==> allocate some arrays for obs and obs metadata.
  
 allocate(statid(nobstot))
 allocate(anal_ob(nobstot))
 allocate(anal_obz(nobstot))
 allocate(stattype(nobstot))
 allocate(iuseob(nobstot))
 allocate(oblocx(nobstot))
 allocate(oblocy(nobstot))
 allocate(ob(nobstot))
 allocate(oborig(nobstot))
 allocate(ob_pert(nobstot))
 allocate(ob_ens(nanals,nobstot))
 allocate(zob(nobstot))
 allocate(obtime(nobstot))
 allocate(stdev(nobstot))
 allocate(stdevorig(nobstot))
 allocate(cdiagbuf(nobstot),rdiagbuf(nreal,nobstot))
 
 if (nproc .eq. 0) then
 
 rewind(149)
 nobsh = 0
 do nob=1,nobstot
      read(149,9801) statid(nob),stattype(nob),obtype,oblocx(nob),oblocy(nob),&
           izob,obtime(nob),ob(nob),slpob,bias,stdevorig(nob),statname,obid
      oborig(nob) = ob(nob)
      zob(nob) = izob
 9801 format(a19,1x,i3,1x,a1,1x,f7.2,1x,f6.2,1x,i5,1x,f6.2,1x,f7.1,&
             1x,f7.1,1x,e10.3,1x,f5.2,1x,a30,1x,a13)
      if (oblocy(nob) .lt. 0.) nobsh = nobsh + 1
      if (oblocx(nob) .lt. 0.) oblocx(nob) = oblocx(nob) + 360.
 enddo
 print *, nobstot,' total obs'
 print *, nobsh,' total obs in SH'
 close(149)

 end if
 
 call MPI_Bcast(oblocx,nobstot,MPI_REAL,0, &
               MPI_COMM_WORLD,MPI_Status,ierr)
 call MPI_Bcast(oblocy,nobstot,MPI_REAL,0, &
               MPI_COMM_WORLD,MPI_Status,ierr)
 call MPI_Bcast(obtime,nobstot,MPI_REAL,0, &
               MPI_COMM_WORLD,MPI_Status,ierr)
 call MPI_Bcast(ob,nobstot,MPI_REAL,0, &
               MPI_COMM_WORLD,MPI_Status,ierr)
 call MPI_Bcast(oborig,nobstot,MPI_REAL,0, &
               MPI_COMM_WORLD,MPI_Status,ierr)
 call MPI_Bcast(zob,nobstot,MPI_REAL,0, &
               MPI_COMM_WORLD,MPI_Status,ierr)
 call MPI_Bcast(stdevorig,nobstot,MPI_REAL,0, &
               MPI_COMM_WORLD,MPI_Status,ierr)
 call MPI_Bcast(stattype,nobstot,MPI_INTEGER,0, &
               MPI_COMM_WORLD,MPI_Status,ierr)
 
 write(charnanal,'(i3.3)') nanal
 if (nanal .eq. nanals+1) then
    obsfileout = "diag_conv_ges."//datestring//"_ensmean"
 else
    obsfileout = "diag_conv_ges."//datestring//"_mem"//charnanal
 end if
 open(iunito,form="unformatted",file=trim(adjustl(obsfileout)),status='replace',convert='native')
 
 ntime = 0
 do nfhr=fhmin,fhmax,fhout
    ntime = ntime + 1

    write(charfhr,'(i2.2)') nfhr
    if (nanal .eq. nanals+1) then
    filenamein = "gfg_"//datestring//"_fhr"//charfhr//"_ensmean"
    else
    filenamein = "gfg_"//datestring//"_fhr"//charfhr//"_mem"//charnanal
    end if
   
    open(iunit,form="unformatted",file=trim(filenamein),convert='native')
    read(iunit) npts,nlevs,ntrac,ptop
    if (nfhr .eq. fhmin) then
        allocate(zsg(npts))
        allocate(tempg(npts,nlevs))
        allocate(zg(npts,nlevs))
        allocate(pslg(npts,nlevs))
        allocate(psig(npts,nlevs+1))
        allocate(lats(npts))
        allocate(lons(npts))
        allocate(analps(npts,ntimes))
        allocate(analtemp(npts,ntimes))
        allocate(analpress(npts,ntimes))
        allocate(anallapse(npts,ntimes))
        allocate(analzs(npts))
    end if

    read(iunit) lons
    read(iunit) lats
    read(iunit) zsg
    do k=1,nlevs
       read(iunit) (pslg(i,k),i=1,npts)
    enddo
    do k=1,nlevs
       read(iunit) (psig(i,k),i=1,npts)
    enddo
    psig(:,nlevs+1)=ptop
    do k=1,nlevs
       read(iunit) (tempg(i,k),i=1,npts)
    enddo
    close(iunit) 
   
 ! integrate hydrostatic equation to get heights.
    call temptoz(npts,nlevs,real(rd),real(cp),real(grav),psig,pslg,zsg,tempg,zg)

    analps(:,ntime) = psig(:,1)
    if (nfhr .eq. fhmin) analzs = zsg
    analtemp(:,ntime) = tempg(:,nlevt2)
    analpress(:,ntime) = pslg(:,nlevt2)
    anallapse(:,ntime) = (tempg(:,nlevt1)-tempg(:,nlevt2))/&
                         (zg(:,nlevt2)-zg(:,nlevt1))
   
 enddo ! nfhr
   
    !==> perform Benjamin and Miller reduction for each ob, compute ob priors.
 !    also do gross qc checks.
 allocate(src_llpoints(npts,2))
 allocate(tgt_llpoints(1,2))
 src_llpoints(:,1) = lons
 src_llpoints(:,2) = lats
 nn = 0
 do nob=1,nobstot
    tgt_llpoints(1,1) = deg2rad*oblocx(nob)
    tgt_llpoints(1,2) = deg2rad*oblocy(nob)
    if (nob .eq. 1) then
       call bilinear_init(src_llpoints,npts,tgt_llpoints,1)
    else
       call bilinear_update(tgt_llpoints,1)
    end if
    dtob = 1.+((real(fhmin)+obtime(nob))/real(fhout))
    call lintrp3(analtemp, npts, ntimes, dtob, anal_obt)
    call lintrp3(anallapse, npts, ntimes, dtob, anal_oblapse)
    call lintrp3(analpress, npts, ntimes, dtob, anal_obp)
    call interp(analzs,anal_obz(nob))
    
    ! this is ob prior.
    call lintrp3(analps,npts,ntimes,dtob,anal_ob(nob))
    !if (nproc .eq. 0 .AND. nob .lt. 30) write(6,*) oblocx(nob),oblocy(nob),ob(nob),anal_ob(nob)
    ! ? should make sure mean of these perturbations is zero ?
    ob_pert(nob) = ob(nob) + rnorm()*stdevorig(nob)
    zob_pert = zob(nob) + rnorm()*zerr ! gaussian random deviate
    ! adjust obs to model orog.
    ! perturbed ob (with perturbed station elevation).
    ! ob error will be set to variance of adjusted perturbed obs.
    ob_pert(nob) = &
    preduce(ob_pert(nob),anal_obp,anal_obt,anal_obz(nob),zob_pert,rd,grav,anal_oblapse)
    ! unperturbed ob.
    ob(nob) = &
    preduce(ob(nob),anal_obp,anal_obt,anal_obz(nob),zob(nob),rd,grav,anal_oblapse)
    zdiff = zob(nob)-anal_obz(nob)
    if ((obtime(nob) .ge. -3. .and. &
        obtime(nob) .le. 3.) .and. abs(zdiff) .lt. zthresh) then
        iuseob(nob) = 1
    else
        iuseob(nob) = 0
        nn = nn + 1
    end if
! gross error check.
    if (iuseob(nob) .eq. 1) then
       altob = palt(oborig(nob),zob(nob))
       if (altob .lt. 850. .or. altob .gt. 1090.) then
          !if (nproc .eq. 0) print *,oborig(nob),ob(nob),zob(nob),anal_obz(nob),altob
          iuseob(nob)=0
          nn = nn + 1
       end if
    end if   
 enddo

 if (nn .ne. 0) print *,nanal,nn,' failed gross qc check'

! distribute the ob error to all processors.
! first, gather back on last proc.
 if (nproc .eq. nanals) then
    do np=0,nanals-1
       call MPI_Recv(ob_ens(np+1,:),nobstot,MPI_REAL,np, &
                     1,MPI_COMM_WORLD,MPI_Status,ierr)
    enddo
    do nob=1,nobstot
       ob_pert(nob) = sum(ob_ens(1:nanals,nob))/float(nanals)
    enddo
    do nob=1,nobstot
       stdev(nob) = &
       sqrt(sum((ob_ens(1:nanals,nob)-ob_pert(nob))**2)/float(nanals-1))
       !print *,nob,stdevorig(nob),stdev(nob)
    enddo
 else
    call MPI_Send(ob_pert,nobstot,MPI_REAL,nanals, &
                  1,MPI_COMM_WORLD,ierr)
 end if

! now push back out to all other procs.
 call MPI_Bcast(stdev,nobstot,MPI_REAL,nanals, &
               MPI_COMM_WORLD,MPI_Status,ierr)

 do ii=1,nobstot
    cdiagbuf(ii)    = trim(adjustl(statid(ii)))  ! station id

    rdiagbuf(1,ii)  = stattype(ii)       ! observation type
    rdiagbuf(2,ii)  = stattype(ii)       ! observation subtype
    rdiagbuf(3,ii)  = oblocy(ii) ! observation latitude (degrees)
    rdiagbuf(4,ii)  = oblocx(ii) ! observation longitude (degrees)
    rdiagbuf(5,ii)  = zob(ii)            ! station elevation (meters)
    rdiagbuf(6,ii)  = ob(ii)             ! observation pressure (hPa)
    rdiagbuf(7,ii)  = anal_obz(ii)       ! observation height (meters)
    rdiagbuf(8,ii)  = obtime(ii)         ! obs time (hours relative to analysis time)

    rdiagbuf(9,ii)  = 1.                 ! input prepbufr qc or event mark
    rdiagbuf(10,ii) = 1.e30              ! setup qc or event mark
    rdiagbuf(11,ii) = 1.                 ! read_prepbufr data usage flag
    if(iuseob(ii) .eq. 1) then
       rdiagbuf(12,ii) = 1.              ! analysis usage flag (1=use, -1=not used)
    else
       rdiagbuf(12,ii) = -1.                    
    endif
    rdiagbuf(13,ii) = 1.                 ! nonlinear qc relative weight
    rdiagbuf(14,ii) = 1./stdevorig(ii)   ! prepbufr inverse obs error (hPa**-1)
    rdiagbuf(15,ii) = 1./stdevorig(ii)   ! read_prepbufr inverse obs error (hPa**-1)
    rdiagbuf(16,ii) = 1./stdev(ii)       ! final inverse observation error (hPa**-1)

    rdiagbuf(17,ii) = ob(ii)             ! surface pressure observation (hPa)
    rdiagbuf(18,ii) = ob(ii)-anal_ob(ii) ! obs-ges used in analysis (coverted to hPa)
    rdiagbuf(19,ii) = ob(ii)-anal_ob(ii) ! obs-ges w/o adjustment to guess surface pressure (hPa)
 enddo
 read(datestring,'(i10)') idate
 write(iunito) idate
 write(iunito)' ps',nchar,nreal,nobstot,nproc
 write(iunito)cdiagbuf(1:nobstot),rdiagbuf(:,1:nobstot)
 close(iunito)

 call MPI_Barrier(MPI_COMM_WORLD,ierr)
 if (nproc .eq. 0) write(6,*) 'all done!'
 call MPI_Finalize(ierr)
 if (nproc .eq. 0 .and. ierr .ne. 0) then
  print *, 'MPI_Finalize error status = ',ierr
 end if

end program psop

real function preduce(ps,tpress,t,zmodel,zob,rd,grav,rlapse)
! compute MAPS pressure reduction from station to model elevation
! See Benjamin and Miller (1990, MWR, p. 2100)
! uses plus 'effective' surface
! temperature extrapolated from virtual temp (tv) at sig2 (tpress mb)
! using lapse rate between sig2 and sig1.
! ps - surface pressure observation to reduce
! t - virtual temp. at sig2 (pressure tpress).
! zmodel - model orographic height
! zob - station height
! rlapse - lapse rate between sig2 and sig1 (positive for
! temp increasing from sig2 to sig1).
   use kinds, only: r_kind
   implicit none
   real(r_kind), intent(in) :: rd,grav
   real, intent(in) :: t,tpress,zmodel,zob,ps,rlapse
   real t0,alpha
   alpha = rd*rlapse/grav
   t0 = t*(ps/tpress)**alpha
   preduce = ps*((t0 + rlapse*(zob-zmodel))/t0)**(1./alpha)
end function preduce

function palt(ps,zs)
! compute QNH altimeter setting (in mb) given ps (in mb), zs (in m).
! see WAF, Sept 1998, p 833-850 (Pauley) section 2c
   real, parameter :: rd =2.8705e+2
   real, parameter :: g = 9.8
   real, parameter :: rlapse = 0.0065
   real, parameter :: t0 = 288.15
   real, parameter :: p0 = 1013.25
   real, parameter :: alpha = rd*rlapse/g
   real palt,ps,zs
   palt = ps*(1.+(rlapse*zs/t0)*(p0/ps)**alpha)**(1./alpha)
end function palt

subroutine lintrp3(data, npts, ntimes, dtob, data_interp)
   integer, intent(in) :: npts, ntimes
   real, intent(in) :: data(npts,ntimes) 
   real, intent(in) :: dtob
   real, intent(out) :: data_interp
   integer nt,ntp
   real delt, data_interp1, data_interp2
   nt   = dtob
   nt   = max(1,min(nt,ntimes))
   ntp  = nt + 1
   ntp  = min(ntp,ntimes)
   delt = dtob-nt
   call interp(data(:,nt),data_interp1)
   call interp(data(:,ntp),data_interp2)
   data_interp = (1.-delt)*data_interp1 + delt*data_interp2
end subroutine  lintrp3

 subroutine temptoz(npts, nlevs,rgas,cp,grav,pint,pl,zs,tv,z)
! compute z (geopot height) on interfaces, given 
! pint (interface pressure in hPa),
! pl (pressure at mid-layers in hPa), tv (virtual temp at mid-layers) and
! zs (surface orog). rgas,cp,grav are gas constant, specific heat and gravity.
! z does not include surface height (k=1 is 1st level, k=nlevs is model top)
! uses hydrostatic eqn d(phi)/d(pi) = -thetav, where phi is geopot. height,
! pi is exner function and thetav is virtual potential temp.
  implicit none
  integer, intent(in) :: npts,nlevs
  real, dimension(npts, nlevs) :: thetav,pil
  real, dimension(npts, nlevs+1) :: pii
  real, intent(in), dimension(npts,nlevs) :: tv,pl
  real, intent(in), dimension(npts,nlevs+1) :: pint
  real, intent(out), dimension(npts,nlevs) :: z
  real, intent(in), dimension(npts) :: zs
  real, intent(in) :: rgas,cp,grav
  integer i,k
  real dz
 
  pii = cp*(pint/1.e3)**(rgas/cp)
  pil = cp*(pl/1.e3)**(rgas/cp)
  thetav = cp*tv/pil
  do i=1,npts
     dz = -thetav(i,1) * (pii(i,2)-pii(i,1))
     z(i,1) = grav*zs(i) + dz
     do k=3,nlevs+1
        dz = -thetav(i,k-1) * (pii(i,k)-pii(i,k-1))
        z(i,k-1) = z(i,k-2) + dz
     end do
  end do
  z = z/grav
 
 end subroutine temptoz
