# define namelist for community GSI release version 3.3

export gsi_namelist="

 &SETUP
   miter=${nummiter},niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=1,
   factqmin=0.0,factqmax=0.0,
   iguess=-1,
   oneobtest=.false.,retrieval=.false.,
   l_foto=.false.,
   use_pbl=.false.,
   offtime_data=.true.,diag_aero=.false.,
   lread_obs_save=${if_read_obs_save},lread_obs_skip=${if_read_obs_skip},
   use_gfs_nemsio=.true.
!   id_bias_viirs=0
!   lsqrtb=.true.,ltlint = .true.
 /
 &GRIDOPTS
   JCAP=${JCAP_ens},JCAP_B=${JCAP_ens},NLAT=${NLAT_ens},NLON=${LONB_ens},nsig=${LEVS},
   regional=.false.,
   nlayers(63)=3,nlayers(64)=6,
   filled_grid=.false.,half_grid=.false.,netcdf=.false.,
 /
 &BKGERR
   vs=${vs_op}
   hzscl=${hzscl_op}
   bw=0.,fstat=.false.,
 /
 &ANBKGERR
 /
 &JCOPTS
 /
 &STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=100.0,dmesh(2)=60.0,dmesh(3)=30,time_window_max=3.0,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   viirsbufr      viirs_aod   viirs     v.viirs-m_npp        1.0     1     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=.true.,
   n_ens=$nanals,
   beta1_inv=0.125,
   s_ens_h=485,
   s_ens_v=-0.485,
   generate_ens=.false.,
   uv_hyb_ens=.true.,
   jcap_ens=${JCAP_ens},
   nlat_ens=386,
   nlon_ens=${LONB_ens},
   aniso_a_en=.false.,
   jcap_ens_test=${JCAP_ens},
   q_hyb_ens=.false.
 /
 &RAPIDREFRESH_CLDSURF
 /
 &CHEM
  laeroana_gocart=.true.
  l_aoderr_table = .false.,
  aod_qa_limit = 3,
  luse_deepblue = .false.,
  aero_ratios = .false.,
  tunable_error=0.5,
  berror_chem=.true.,
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${ANAL_TIME},
   obhourset=0.,
 /
"

