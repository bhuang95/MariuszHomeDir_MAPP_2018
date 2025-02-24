program psop
 use constants, only: pi, rd, grav, cp, rearth, deg2rad, rad2deg, init_constants, init_constants_derived
 use random_normal
 implicit none
 character(len=120) filenamein,obsfile,filename,obsfileout,inpath
 character(len=10) datestring
 integer npts,ntrac,k,nt,ierr,nanals,nfhr,nobstot,nlevs,&
         nob,nanal,i,iunit,iunitsig,fhmin,fhmax,fhout,fhanal,ntimes,&
         nchar,nreal,ii,jj,nn,nlevt1,nlevt2,ntime,np,nobsh,izob,iunit_nml,iunito,idate
 integer :: imo,jmo
 real dtob,zerr,anal_obt,anal_oblapse,anal_obp,&
      slpob,bias,preduce,palt,zthresh,zdiff,altob,ptop
 character(len=2) charfhr
 character(len=3) charnanal
 real, dimension(:), allocatable :: lats, lons,tmparr,psinterp
 real, dimension(:), allocatable :: oblocx,oblocy,ob,zob,obtime,stdev,&
                                    anal_ob,anal_obz,oborig,stdevorig
 real, dimension(:), allocatable :: new_oblocx,new_oblocy,new_ob,fim_lon,fim_lat,fim_lonb,fim_latb
 real, dimension(:,:), allocatable :: ob_ens,&
       src_llpoints,tgt_llpoints,analpress,analtemp,anallapse,analps,topo,rdiagbuf
real, allocatable :: fgps(:,:,:)
 integer, allocatable, dimension(:) :: stattype,iuseob,new_zob
 character(8),allocatable,dimension(:):: cdiagbuf
 character (len=1) :: obtype
 character(len=19), allocatable :: statid(:)
 character (len=30) :: statname
 character (len=13) :: obid
 logical :: FOUND
 namelist /nam_psop/nlevt1,nlevt2,zerr,fhmin,fhmax,fhout,fhanal,datestring,&
                    nanals,npts,obsfile,zthresh
 call init_constants(.false.) ! initialize constants.
 call init_constants_derived()
 imo=384
 jmo=190
allocate(fim_lat(jmo),fim_lon(imo))
allocate(fim_latb(jmo-1),fim_lonb(imo-1))
DO i=1,imo
   fim_lon(i)=(i-1)*(360.0/384.0)
!   print*,(i-1)*(360.0/384.0)
ENDDO
call gaulats(fim_lat,jmo)
fim_lat=fim_lat*rad2deg
DO i=1,imo-1
   fim_lonb(i)=fim_lon(i)*0.5+fim_lon(i+1)*0.5
ENDDO
DO i=1,jmo-1
   fim_latb(i)=fim_lat(i)*0.5+fim_lat(i+1)*0.5
ENDDO
 nchar=1; nreal=19
 iunit = 7
 iunitsig = 22
 iunit_nml = 912
 iunito = 9

 zthresh = 9.9e31
 zerr = 0
 open(iunit_nml,file='psop.nml',form='formatted')
 read(iunit_nml,nam_psop)
 close(iunit_nml)
 write(6,nam_psop)

 ntimes = 1+((fhmax-fhmin)/fhout)

 call set_random_seed(0,1) ! set random seed.
 
 print *,trim(adjustl(obsfile))
 open(149,form="formatted",file=trim(adjustl(obsfile)))
 print *, filename
 nobstot = 0
 do 
   read(149,9801,err=199,end=199) obtype
   nobstot = nobstot + 1
 enddo
 199 continue

 

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
 allocate(ob_ens(nanals,nobstot))
 allocate(zob(nobstot))
 allocate(obtime(nobstot))
 allocate(stdev(nobstot))
 allocate(stdevorig(nobstot))
 allocate(cdiagbuf(nobstot),rdiagbuf(nreal,nobstot))
! new arrays for replaced values
 allocate(new_oblocx(nobstot))
 allocate(new_oblocy(nobstot))
 allocate(new_ob(nobstot))
 allocate(new_zob(nobstot))
 
 
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
 
 write(charnanal,'(i3.3)') nanal
 obsfileout = "ps_obs_replaced"
 
 ntime = 0
 do nfhr=fhmin,fhmax,fhout
    ntime = ntime + 1
    write(charfhr,'(i2.2)') nfhr
    filenamein = "gfg_"//datestring//"_fhr"//charfhr//"_ensmean"
    inpath='/lfs1/projects/fim/ppegion/fimenkf/FIMenkf.p/fim_enkf.sig1/G6/2008010106/'
    open(iunit,form="unformatted",file=trim(inpath)//trim(filenamein),convert='native')
    read(iunit) npts,nlevs,ntrac,ptop
    IF (nfhr .eq. fhmin) THEN
        allocate(tmparr(npts))
        allocate(lats(npts))
        allocate(lons(npts))
        allocate(topo(imo,jmo))
        allocate(fgps(imo,jmo,7))
        allocate(psinterp(7))
    ENDIF
    read(iunit) lons
    read(iunit) lats
    read(iunit) tmparr
    IF (nfhr .eq. fhmin) THEN 
       call bilinear_init_i2r_jsw(imo, jmo, lons, lats, npts, .true., 0)
       call bilinear_interp_i2r(1, 1, tmparr, topo)
    ENDIF
    do k=1,nlevs
       read(iunit) 
    enddo
    read(iunit) tmparr
    call bilinear_interp_i2r(1, 1, tmparr, fgps(:,:,ntime))
    close(iunit) 

   ! interpolate psig to latlon
   ! grab closed observation to 
   
 enddo ! nfhr
   
 nn = 0
 do nob=1,nobstot
    FOUND=.FALSE.
       IF (oblocx(nob) .LT. fim_lonb(1)) THEN
          ii=1
          FOUND=.TRUE.
       ENDIF
       IF (oblocx(nob) .GE. fim_lonb(imo-1)) THEN
          ii=imo
          FOUND=.TRUE.
       ENDIF
       i=1
       DO WHILE (.not. FOUND) 
          IF (oblocx(nob) .GE. fim_lonb(i) .AND. oblocx(nob) .LT. fim_lonb(i+1)) THEN
             ii=i
             FOUND=.TRUE.
          ENDIF
          i=i+1
          IF (i .GT. imo) STOP
       ENDDO

    FOUND=.FALSE.
       IF (oblocy(nob) .GE. fim_latb(1)) THEN
          jj=1
          FOUND=.TRUE.
       ENDIF
       IF (oblocy(nob) .LT. fim_latb(jmo-1)) THEN
          jj=jmo
          FOUND=.TRUE.
       ENDIF
       i=1
        DO WHILE (.not. FOUND)
          IF (oblocy(nob) .LT. fim_latb(i) .AND. oblocy(nob) .GE. fim_latb(i+1)) THEN
             jj=i
             FOUND=.TRUE.
          ENDIF
          i=i+1
          IF (i .GT. jmo) STOP
       ENDDO
       psinterp(:)=fgps(ii,jj,:)
!       print*,ii,oblocx(nob),fim_lonb(ii),fim_lonb(ii+1)
!       print*,jj,oblocy(nob),fim_latb(jj),fim_latb(jj+1)
!STOP
       !call timeinterp(psinterp,ntimes,dtob,new_ob(nob))
       !new_zob(nob)=nint(topo(ii,jj))
       !new_oblocx(nob)=fim_lat(ii)
       !new_oblocy(nob)=fim_lon(jj)
       call timeinterp(psinterp,ntimes,dtob,ob(nob))
       zob(nob)=nint(topo(ii,jj))
       oblocy(nob)=fim_lat(jj)
       oblocx(nob)=fim_lon(ii)
 enddo

 open(150,form="formatted",file=trim(obsfileout))
 do nob=1,nobstot
      izob=nint(zob(nob))
      write(150,9801) statid(nob),stattype(nob),obtype,oblocx(nob),oblocy(nob),&
           izob,obtime(nob),ob(nob),slpob,bias,stdevorig(nob),statname,obid
 enddo

 close(150)
end program psop

subroutine timeinterp(data, ntimes, dtob, data_interp)
   integer, intent(in) :: ntimes
   real, intent(in) :: data(ntimes) 
   real, intent(in) :: dtob
   real, intent(out) :: data_interp
   integer nt,ntp
   real delt
   nt   = dtob
   nt   = max(1,min(nt,ntimes))
   ntp  = nt + 1
   ntp  = min(ntp,ntimes)
   delt = dtob-nt
   data_interp = (1.-delt)*data(nt) + delt*data(ntp)
end subroutine  timeinterp
