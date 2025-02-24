export prep_chem_sources_inp="

  \$RP_INPUT
!##########################################################################!
!  CATT-BRAMS/MCGA-CPTEC emission model    CPTEC/INPE                      !
!  version 1: 28/feb/2007                                                  !
!  developed/coded by Saulo Freitas and Karla Longo                        !
!  contact: sfreitas@cptec.inpe.br   - www.cptec.inpe.br/meio_ambiente     !
!###########################################################################

 
!---------------- grid_type of the grid output
   grid_type= 'll',	
   		       !'rams' = rams grid 
   		       ! 'polar'  = polar sterog. grid output
                       ! 'gg'     = gaussian grid output
		       ! 'll'     = lat/lon grid output
		       ! 'lambert'  = lambert grid output
		       ! 'mercator' = mercator grid output
 
!---------------- if the output data is for use in CATT-BRAMS model, provide at least one analysis file 
!---------------- of this model
    rams_anal_prefix = './ANL/barca',
   !rams_anal_prefix = '/dados/dados2/mfapel/TESTCASE/ANL/A',
!---------------- date of emission  
!---------------- date of emission
!   ihour=00, iday=2, imon=10, iyear=2008,
    ihour=00, 
    iday=${day}, 
    imon=${month}, 
    iyear=${year},

!---------------- select the sources datasets to be used   
   use_retro =0,  ! 1 = yes, 0 = not
   retro_data_dir='/scratch3/BMC/wrf-chem/Emission_data/RETRO/anthro',

   use_edgar =0,  ! 0 - not,
                  ! 1 - Version 3,
                  ! 2 - Version 4 for some species
                  ! 3 - Version HTAP

   edgar_data_dir='/scratch4/BMC/fim/fimdata_chem/EDGAR-HTAP',
   fim_data_dir='.'  ! FIM grid data in current directory

   use_gocart =0,
   gocart_data_dir='/scratch3/BMC/wrf-chem/Emission_data/GOCART/emissions',

   use_streets =0,
   streets_data_dir='/scratch3/BMC/wrf-chem/Emission_data/STREETS',

   use_seac4rs =0,
   seac4rs_data_dir='/scratch3/BMC/wrf-chem/Emission_data/SEAC4RS',

   use_fwbawb =0,
   fwbawb_data_dir ='/scratch3/BMC/wrf-chem/Emission_data/Emissions_Yevich_Logan',

   use_bioge =2, ! 1 - geia, 2 - megan 
   ! ###### 
   ! # BIOGENIC = 1
   bioge_data_dir ='/home/poluicao/EMISSION_DATA/biogenic_emissions',
   ! # MEGAN = 2
   ! ######   
    bioge_data_dir='/scratch4/BMC/fim/fimdata_chem/EMISSION_DATA/MEGAN/2000',   
   ! ######

   use_gfedv2 =0,
   gfedv2_data_dir ='/home/poluicao/EMISSION_DATA/GFEDv2-8days',

   use_bbem =1,
   use_bbem_plumerise =1,
!---------------- if  the merging of gfedv2 with bbem is desired (=1, yes, 0 = no)  
   merge_GFEDv2_bbem =0,
!---------------- Fire product for BBBEM/BBBEM-plumerise emission models
!   bbem_wfabba_data_dir   ='/scratch4/BMC/public/data/sat/ssec/goes13/wf_abba/f',
!  bbem_wfabba_data_dir   ='/scratch4/BMC/public/data/sat/ssec/goes-west/wf_abba/f',
  bbem_wfabba_data_dir   ="\""${INDIR_wfabba}/f""\"
!  bbem_wfabba_data_dir   ='/scratch4/BMC/fim/lzhang/Fires/goes-west/wf_abba/f',
!   bbem_modis_data_dir    ='/scratch4/BMC/public/data/sat/firms/global/Global_MCD14DL_',
!   bbem_modis_data_dir    ='/scratch4/BMC/fim/lzhang/Fires/Global/Global_MCD14DL_',
   bbem_modis_data_dir    ='/scratch4/BMC/fim/lzhang/Fires/Global/2010/Fir!es.',
   bbem_modis_data_dir    ="\""${INDIR_modis}/Global_MCD14DL_"\"",
   bbem_inpe_data_dir     ='/scratch3/BMC/wrf-chem/Emission_data/fires_data/DSA/Focos',
   bbem_extra_data_dir    ='/scratch3/BMC/wrf-chem/Emission_data/fires_data/BLMALASKA/current.dat',

!---------------- veg type data set (dir + prefix)
! veg_type_data_dir      ='/home/poluicao/SURFACE_DATA/GL_IGBP_AVHRR_INPE/IGBP',	       
  veg_type_data_dir      ='/scratch3/BMC/wrf-chem/Emission_data/surface_data/GL_IGBP_MODIS_INPE/MODIS',	       
  
!---------------- vcf type data set (dir + prefix)
  use_vcf = 0,
  vcf_type_data_dir      ='/dados/dados3/stockler/VCF/data_out/2004/VCF',	       
       

!----------------  New Olson''s data set (dir + prefix)
!  olson_data_dir= '/home/poluicao/EMISSION_DATA/OLSON2/OLSON',   
  olson_data_dir= '/scratch3/BMC/wrf-chem/Emission_data/OLSON2/OLSON',   
     
!---------------- carbon density data set for Amazon (dir + prefix)
  carbon_density_data_dir='/scratch3/BMC/wrf-chem/Emission_data/surface_data/GL_OGE_INPE/OGE',
 
  fuel_data_dir      ='/scratch3/BMC/wrf-chem/Emission_data/Carbon_density',		     
 
!---------------- gocart background
  use_gocart_bg =0,
  gocart_bg_data_dir='/scratch3/BMC/wrf-chem/Emission_data/GOCART',

!---------------- volcanoes emissions
   use_volcanoes =0,
   volcano_index =1143, !Popocatepetl

   use_these_values='NONE',
! define a text file for using external values for INJ_HEIGHT, DURATION,
! MASS ASH (units are meters - seconds - kilograms) and the format for
! a file 'values.txt' is like this:
! 11000. 10800. 1.5e10
! use_these_values='values.txt', 
!  begin_eruption='201204200000',  !begin time UTC of eruption YYYYMMDDhhmm   
   begin_eruption='201407281200',  !begin time UTC of eruption YYYYMMDDhhmm   

!---------------- degassing volcanoes emissions
   use_degass_volcanoes =0,
   degass_volc_data_dir ='/scratch3/BMC/wrf-chem/Emission_data/VOLC_SO2', 

!---------------- user specific  emissions directory
!---------------- Update for South America megacities
!---------------- set 'NONE' if you do not want to use
!  user_data_dir='/home/poluicao/EMISSION_DATA/SouthAmerica_Megacities',
   user_data_dir='NONE',


!--------------------------------------------------------------------------------------------------
   pond=1,   ! mad/mfa  0 -> molar mass weighted 
             !          1 -> Reactivity weighted   

!---------------- for grid type 'll' or 'gg' only
   grid_resolucao_lon=0.5,
   grid_resolucao_lat=0.5,

   nlat=320,          ! if gg (only global grid)
   lon_beg   = -180., ! (-180.:+180.) long-begin of the output file
   lat_beg   = -89.75, ! ( -90.:+90. ) lat -begin of the output file
   delta_lon = 360., ! total long extension of the domain (360 for global)
   delta_lat = 179.5, ! total lat  extension of the domain (180 for global)

!---------------- For regional grids (polar or lambert)

   NGRIDS   = 1,            ! Number of grids to run

   NNXP     = 649,201,86,46,        ! Number of x gridpoints
   NNYP     = 648,311,74,46,        ! Number of y gridpoints
   NXTNEST  = 0,1,2,3,          ! Grid number which is the next coarser grid
   DELTAX   = 13545.087
   DELTAY   = 13545.087       ! X and Y grid spacing

   ! Nest ratios between this grid and the next coarser grid.
   NSTRATX  = 1,5,3,4,           ! x-direction
   NSTRATY  = 1,5,3,4,           ! y-direction

   NINEST = 1,10,0,0,        ! Grid point on the next coarser
   NJNEST = 1,10,0,0,        !  nest where the lower southwest
                             !  corner of this nest will start.
                             !  If NINEST or NJNEST = 0, use CENTLAT/LON
   POLELAT  = 45.,           ! If polar, latitude/longitude of pole point
   POLELON  = -99.18        ! If lambert/mercator, lat/lon of grid origin (x=y=0.) (ref_lat
                             ! /ref_lon from namelist.wps)

   STDLAT1  = 45.,           ! If polar, unused
   STDLAT2  = 45.,           ! If lambert, standard latitudes of projection 
                             !(truelat2/truelat1 from namelist.wps, STDLAT1 < STDLAT2)
                             ! If mercator STDLAT1 = 1st true latitude 

   CENTLAT  = 45.,  -23., 27.5,  27.5,   ! (ref_lat/ref_lon from namelist.wps)
   CENTLON  = -99.18, -46.,-80.5, -80.5,

!---------------- model output domain for each grid (only set up for rams)
   lati =  -90.,  -90.,   -90., 
   latf =  +90.,  +90.,   +90.,  
   loni = -180., -180.,  -180., 
   lonf =  180.,  180.,   180., 

!---------------- project rams grid (polar sterogr) to lat/lon: 'YES' or 'NOT'
   proj_to_ll='NO', 
   
!---------------- output file prefix (may include directory other than the current)
   chem_out_prefix = "\""${prefix}"\"",
   chem_out_format = 'vfm',
!---------------- convert to WRF/CHEM (yes,not)
  special_output_to_wrf = 'no',
   
  \$END

"