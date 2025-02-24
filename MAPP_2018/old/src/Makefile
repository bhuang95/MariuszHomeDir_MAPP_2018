FC =  ifort
#FCFLAGSFREE =   -FR -traceback -O   -g -C
#FCFLAGSFREE =   -FR -O3 # -traceback -O   -g -C
#FCFLAGSFIXED =   -g -C -Mfixed
FCENDIAN = -convert big_endian
#LINKFLAGS =  -FR -traceback -O   -g -C # -O3 
LINKFLAGS =  -O3 
LIB =  -L$(MKL) -lmkl -lmkl_lapack -lpthread
LIBCDF = -L$(NETCDF)/lib -lnetcdff -lnetcdf -I$(NETCDF)/include 
GSI = /scratch3/BMC/chem-var/pagowski/enkf_runs/trunk_r79009
#GSI = /scratch3/BMC/chem-var/pagowski/enkf_runs/gsi_aod_dev
CORLIB=/scratch3/BMC/wrfruc/mhu/code/comGSI/final/v36/release_V3.6/dtc/lib
LIBGSI  = -L$(CORLIB) -lbufr_i4r8 -lw3emc_i4r8 -lw3nco_i4r8
GSIr4=/scratch3/BMC/chem-var/pagowski/gsilibs/libs
LIBGSIr4 =-L$(GSIr4) -lbufr_i4r4 -lbacio_i4r4 -lw3emc_i4r4 -lw3nco_i4r4 -lsp_i4r4
SLINT = /home/Mariusz.Pagowski/codes/src_slint
LIBSLINT = -L$(SLINT) -lslint

INCL= .
RM = /bin/rm -f

.SUFFIXES:

.SUFFIXES: .F .f90 .f .o

.F.o: 
	$(FC) -c $(FCFLAGSFREE)  $*.F

.f90.o: 
	$(FC) -c $(FCFLAGSFREE) $(LIBCDF) $*.f90

.f.o: 
	$(FC) -c $(FCFLAGSFIXED) $*.f


OBJ101 = interp_fcst2obs_enkf.o all_spaces.o julian.o
MODULE101 = constants_module.o \
          module_debug.o queue_module.o bitarray_module.o \
          misc_definitions_module.o module_map_utils.o \
          gridinfo_module.o list_module.o \
          llxy_module.o interp_module.o
interp_fcst2obs_enkf.x: $(MODULE101) $(OBJ101)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE101) $(OBJ101) \
	-o $@

OBJ102 = interp_fcst2obs_enkf_spec.f90 all_spaces.f90 julian.f90
MODULE102 = constants_module.o \
          module_debug.o queue_module.o bitarray_module.o \
          misc_definitions_module.o module_map_utils.o \
          gridinfo_module.o list_module.o \
          llxy_module.o interp_module.o
interp_fcst2obs_enkf_spec.x: $(MODULE102) $(OBJ102)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE102) $(OBJ102) \
	-o $@

OBJ103 = interp_anal2obs_enkf.o all_spaces.o julian.o
MODULE103 = constants_module.o \
          module_debug.o queue_module.o bitarray_module.o \
          misc_definitions_module.o module_map_utils.o \
          gridinfo_module.o list_module.o \
          llxy_module.o interp_module.o
interp_anal2obs_enkf.x: $(MODULE103) $(OBJ103)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE103) $(OBJ103) \
	-o $@

OBJ104 = eval_stats_tseries_enkf.o
MODULE104 =
eval_stats_tseries_enkf.x: $(MODULE104) $(OBJ104)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE104) $(OBJ104) \
	-o $@

OBJ105 = eval_stats_tseries_enkf_spec.o
MODULE105 =
eval_stats_tseries_enkf_spec.x: $(MODULE105) $(OBJ105)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE105) $(OBJ105) \
	-o $@

OBJ106 = eval_stats_tseries_dots.o
MODULE106 =
eval_stats_tseries_dots.x: $(MODULE106) $(OBJ106)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE106) $(OBJ106) \
	-o $@

OBJ107 = iowrf_prep.f90
MODULE107 = 
iowrf_prep.x: $(MODULE107) $(OBJ107)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE107) $(OBJ107) \
	$(LIBCDF) -o $@


OBJ109 = perturb_emissions.f90
MODULE109 =
perturb_emissions.x: $(MODULE109) $(OBJ109)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE109) $(OBJ109) \
	$(LIBCDF) -o $@

OBJ110 = perturb_emissions_varscales.f90 rfdpar.f90
MODULE110 = $(GSI)/kinds.o constants.f90 da_rf_cv3.f90 da_mat_cv3.f90
perturb_emissions_varscales.x: $(MODULE110) $(OBJ110)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE110) $(OBJ110) \
	$(LIBCDF) -o $@


OBJ111 = airnow2gsi_bundle.o
MODULE111 =
airnow2gsi_bundle.x: $(MODULE111) $(OBJ111)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE111) $(OBJ111) -o $@

OBJ112 = rednoise.o
MODULE112 = module_rednoise.o
rednoise.x: $(MODULE112) $(OBJ112)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE112) $(OBJ112) -o $@


OBJ113 = perturb_emissions_varscales_sectors.f90 rfdpar.f90
MODULE113 = $(GSI)/kinds.o constants.f90 da_rf_cv3.f90 da_mat_cv3.f90
perturb_emissions_varscales_sectors.x: $(MODULE113) $(OBJ113)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE113) $(OBJ113) \
	$(LIBCDF) -o $@


OBJ114 = perturb_emissions_varscales_pm10.f90 rfdpar.f90
MODULE114 = $(GSI)/kinds.o constants.f90 da_rf_cv3.f90 da_mat_cv3.f90
perturb_emissions_varscales_pm10.x: $(MODULE114) $(OBJ114)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE114) $(OBJ114) \
	$(LIBCDF) -o $@

OBJ115 = eval_stats_tseries_dots_extended.o
MODULE115 =
eval_stats_tseries_dots_extended.x: $(MODULE115) $(OBJ115)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE115) $(OBJ115) \
	-o $@

OBJ116 = writeout_obs.o indexx_numi.o julian.o
MODULE116 = \
          module_debug.o queue_module.o bitarray_module.o \
          misc_definitions_module.o module_map_utils.o \
          gridinfo_module.o list_module.o \
          llxy_module.o interp_module.o

writeout_obs.x: $(MODULE116) $(OBJ116)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE116) $(OBJ116) \
	-o $@

OBJ117 = perturb_emissions_20km.f90
MODULE117 =
perturb_emissions_20km.x: $(MODULE117) $(OBJ117)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE117) $(OBJ117) \
	$(LIBCDF) -o $@

OBJ118 = 
MODULE118 =
unknown.x: $(MODULE118) $(OBJ118)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE118) $(OBJ118) -o $@


OBJ119 = interp_fcst2obs_enkf_subset.o all_spaces.o julian.o
MODULE119 = constants_module.o \
          module_debug.o queue_module.o bitarray_module.o \
          misc_definitions_module.o module_map_utils.o \
          gridinfo_module.o list_module.o \
          llxy_module.o interp_module.o
interp_fcst2obs_enkf_subset.x: $(MODULE119) $(OBJ119)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE119) $(OBJ119) \
	-o $@

OBJ120 = airnow_txt2bufr.o
MODULE120 = $(GSI)/kinds.o
airnow_txt2bufr.x: $(MODULE120) $(OBJ120)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE120) $(OBJ120) \
	$(LIBGSI) -o $@

OBJ121 = makeberror_wrfchem_pm2_5.o
MODULE121 = $(GSI)/kinds.o
makeberror_wrfchem_pm2_5.x: $(MODULE121) $(OBJ121)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE121) $(OBJ121) \
	-o $@

OBJ122 = interp_6hrfcst2obs_enkf.o all_spaces.o julian.o
MODULE122 = constants_module.o \
          module_debug.o queue_module.o bitarray_module.o \
          misc_definitions_module.o module_map_utils.o \
          gridinfo_module.o list_module.o \
          llxy_module.o interp_module.o
interp_6hrfcst2obs_enkf.x: $(MODULE122) $(OBJ122)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE122) $(OBJ122) \
	-o $@

OBJ123 = eval_stats_6hrtseries_dots.o
MODULE123 =
eval_stats_6hrtseries_dots.x: $(MODULE123) $(OBJ123)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE123) $(OBJ123) \
	-o $@

OBJ124 = convert_aero_be.f90
MODULE124 = $(GSI)/kinds.o
convert_aero_be.x: $(MODULE124) $(OBJ124)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE124) $(OBJ124) \
	-o $@

OBJ125 = scale_emissions.f90 
MODULE125 = $(GSI)/kinds.o constants.f90 
scale_emissions.x: $(MODULE125) $(OBJ125)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE125) $(OBJ125) \
	$(LIBCDF) -o $@

OBJ126 = read_anow_bufr.f90
MODULE126 = $(GSI)/kinds.o
read_anow_bufr.x: $(MODULE126) $(OBJ126)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE126) $(OBJ126) \
	$(LIBGSI) -o $@

OBJ127 = convert_aero_be_pm25.f90
MODULE127 = $(GSI)/kinds.o
convert_aero_be_pm25.x: $(MODULE127) $(OBJ127)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE127) $(OBJ127) \
	-o $@

OBJ128 = combfr.f exit.o
MODULE128 = 
combfr.x: $(MODULE128) $(OBJ128)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE128) $(OBJ128) \
	$(LIBGSI) -o $@


OBJ129 = aerocov.f90
MODULE129 = aero_mod.f90
aerocov.x: $(MODULE129) $(OBJ129)
	$(FC) $(FCFLAGSFREE) $(FCENDIAN) -r8 -i4 -I$(INCL) $(MODULE129) $(OBJ129) \
	-o $@

OBJ130 = partition_increment.f90
MODULE130 =
partition_increment.x: $(MODULE130) $(OBJ130)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE130) $(OBJ130) \
	$(LIBCDF) -o $@

OBJ131 = aeronet2gsi_bundle.o
MODULE131 =
aeronet2gsi_bundle.x: $(MODULE131) $(OBJ131)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE131) $(OBJ131) -o $@

OBJ132 = aeronet_txt2bufr.o
MODULE132 = $(GSI)/kinds.o
aeronet_txt2bufr.x: $(MODULE132) $(OBJ132)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE132) $(OBJ132) \
	$(LIBGSI) -o $@

OBJ133 = factor_emiss.o
MODULE133 = netcdf_io.f90
factor_emiss.x: $(MODULE133) $(OBJ133)
	$(FC) $(LINKFLAGS) $(LIBCDF) $(MODULE133) $(OBJ133) \
	$(LIBCDF) -o $@

OBJ134 = factor_emiss_spline_cubic.o
MODULE134 = netcdf_io.f90 module_splines.o
factor_emiss_spline_cubic.x: $(MODULE134) $(OBJ134)
	$(FC) $(LINKFLAGS) -I$(INCL) $(LIBCDF) $(MODULE134) $(OBJ134) \
	$(LIBCDF) -o $@

OBJ135 = testindexx.o
MODULE135 = module_interpolations.f90 
testindexx.x: $(MODULE135) $(OBJ135)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE135) $(OBJ135) \
	 -o $@

OBJ136 = test_tspack.o
MODULE136 = tension_module.o 
test_tspack.x: $(MODULE136) $(OBJ136)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE136) $(OBJ136) \
	 -o $@

OBJ137 = factor_emiss_spline_tspack.o
MODULE137 = netcdf_io.f90 tension_module.o module_splines.o
factor_emiss_spline_tspack.x: $(MODULE137) $(OBJ137)
	$(FC) $(LINKFLAGS) -I$(INCL) $(LIBCDF) $(MODULE137) $(OBJ137) \
	$(LIBCDF) -o $@

OBJ138 = partition_emission_increment.o
MODULE138 = netcdf_io.f90 
partition_emission_increment.x: $(MODULE138) $(OBJ138)
	$(FC) $(LINKFLAGS) -I$(INCL) $(LIBCDF) $(MODULE138) $(OBJ138) \
	$(LIBCDF) -o $@

OBJ139 = wgne_sfc4met.o
MODULE139 = 
wgne_sfc4met.x: $(MODULE139) $(OBJ139)
	$(FC) $(LINKFLAGS) -I$(INCL) $(LIBCDF) $(MODULE139) $(OBJ139) \
	-o $@

OBJ140 = wgne_radios4met.o
MODULE140 = 
wgne_radios4met.x: $(MODULE140) $(OBJ140)
	$(FC) $(LINKFLAGS) -I$(INCL) $(LIBCDF) $(MODULE140) $(OBJ140) \
	-o $@

OBJ141 = partition_increment_soa.f90
MODULE141 = netcdf_io.f90
partition_increment_soa.x: $(MODULE141) $(OBJ141)
	$(FC) $(LINKFLAGS) $(MODULE141) $(OBJ141) \
	$(LIBCDF) -o $@


OBJ142 = interp_fcst2obs_rt.o all_spaces.o julian.o
MODULE142 = constants_module.o \
          module_debug.o queue_module.o bitarray_module.o \
          misc_definitions_module.o module_map_utils.o \
          gridinfo_module.o list_module.o \
          llxy_module.o interp_module.o
interp_fcst2obs_rt.x: $(MODULE142) $(OBJ142)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE142) $(OBJ142) \
	-o $@

OBJ143 = eval_stats_tseries_dots_rt.o
MODULE143 =
eval_stats_tseries_dots_rt.x: $(MODULE143) $(OBJ143)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE143) $(OBJ143) \
	-o $@

OBJ144 = airnow2gsi_bundle_tmpfix.o
MODULE144 =
airnow2gsi_bundle_tmpfix.x: $(MODULE144) $(OBJ144)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE144) $(OBJ144) -o $@

OBJ145 = wrf_pmsfc_ncf2grib.f90
MODULE145 = netcdf_io.f90
wrf_pmsfc_ncf2grib.x: $(MODULE145) $(OBJ145)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE145) $(OBJ145) \
	$(LIBCDF) $(LIBGSIr4) -o $@ 

OBJ146 = obs_pmsfc_4met.f90 julian.f90
MODULE146 = 
obs_pmsfc_4met.x: $(MODULE146) $(OBJ146)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE146) $(OBJ146) \
	-o $@ 

OBJ147 = test_slint.f90
MODULE147 = 
test_slint.x: $(MODULE147) $(OBJ147)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE147) $(OBJ147) \
	$(LIBSLINT) -o $@ 

OBJ148 = test_akima.f90 toms526.f90
MODULE148 = 
test_akima.x: $(MODULE148) $(OBJ148)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE148) $(OBJ148) \
	-o $@ 

OBJ149 = read_viirsaod_bufr.f90
MODULE149 = $(GSI)/src/kinds.o
read_viirsaod_bufr.x: $(MODULE149) $(OBJ149)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE149) $(OBJ149) \
	$(LIBGSI) -o $@

OBJ1491 = read_modisaod_bufr.f90
MODULE1491 = $(GSI)/src/kinds.o
read_modisaod_bufr.x: $(MODULE1491) $(OBJ1491)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE1491) $(OBJ1491) \
	$(LIBGSI) -o $@

OBJ150 = dust_mask.f90
MODULE150 = netcdf_io.o 
dust_mask.x: $(MODULE150) $(OBJ150)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE150) $(OBJ150) \
	$(LIBSLINT) $(LIBCDF) -o $@


OBJ151 = test_spline.f90
MODULE151 = module_interpolations.o
test_spline.x: $(MODULE151) $(OBJ151)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE151) $(OBJ151) \
	-o $@

OBJ152 = ds_mask2ncdf.f90
MODULE152 = netcdf_io.o $(GSI)/kinds.o
ds_mask2ncdf.x: $(MODULE152) $(OBJ152)
	$(FC) $(LINKFLAGS) -I. -I$(GSI) $(MODULE152) $(OBJ152) \
	$(LIBCDF) -o $@

OBJ153 = convertmask.f90
MODULE153 = $(GSI)/kinds.o 
convertmask.x: $(MODULE153) $(OBJ153)
	$(FC) $(LINKFLAGS) $(MODULE153) $(OBJ153) \
	-o $@

OBJ154 = hrrr_add_gocart.f90
MODULE154 = netcdf_io.o
hrrr_add_gocart.x: $(MODULE154) $(OBJ154)
	$(FC) $(LINKFLAGS) $(MODULE154) $(OBJ154) \
	$(LIBCDF) -o $@

OBJ155 = sample_viirsaod_bufr.f90
MODULE155 = $(GSI)/src/main/kinds.o
sample_viirsaod_bufr.x: $(MODULE155) $(OBJ155)
	$(FC) $(LINKFLAGS) -I$(GSI)/include -I$(INCL) $(MODULE155) $(OBJ155) \
	$(LIBGSI) -o $@

OBJ156 = read_abiaod_bufr.f90
MODULE156 = $(GSI)/src/main/kinds.o
read_abiaod_bufr.x: $(MODULE156) $(OBJ156)
	$(FC) $(LINKFLAGS) -I$(INCL) $(MODULE156) $(OBJ156) \
	$(LIBGSI) -o $@


clean:
	$(RM) *.o *.x *.stb *.mod

