#!/bin/ksh 

npx="193"
npy="193"
npz=63
layout_x="8"
layout_y="8"
io_layout="1,1"
ntiles="6"
make_nh="T"

# blocking factor used for threading and general physics performance
nxblocks="1"
nyblocks="24"
fhcyc=0

export OMP_NUM_THREADS=1

LEVS=$npz
((LEVP = $LEVS + 1))
FHMAX=9
months="0"
days="0"
hours=$FHMAX
dt_atmos=450

ncores_per_node=24,

ident=2015081006
analdate=$ident
year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hour=`echo $analdate | cut -c9-10`
restart_secs=0
levp=LEVP


cat > input.nml <<EOF
&amip_interp_nml
interp_oi_sst=T,
use_ncep_sst=T,
use_ncep_ice=F,
no_anom_sst=F,
data_set='reynolds_oi',
date_out_of_range='climo',
/

&atmos_model_nml
nxblocks=$nxblocks,
nyblocks=$nyblocks,
surface_debug=F,
dycore_only=F,
/

&diag_manager_nml
prepend_date=T,
long_date=T,
/

&fms_io_nml
checksum_required=F,
max_files_r=100,
max_files_w=100,
/

&fms_nml
clock_grain='ROUTINE',
domains_stack_size=115200,
print_memory_usage=F,
/

&fv_core_nml
layout=$layout_x,$layout_y,
io_layout=$io_layout,
npx=$npx,
npy=$npy,
ntiles=$ntiles,
npz=$npz,
grid_type=-1,
make_nh=$make_nh,
fv_debug=F,
range_warn=F,
reset_eta=F,
n_sponge=8,
tau=5.,
rf_cutoff=8.e2,
d2_bg_k1=0.16,
d2_bg_k2=0.02,
kord_tm=-11,
kord_mt=10,
kord_wz=10,
kord_tr=11,
hydrostatic=F,
phys_hydrostatic=F,
use_hydro_pressure=F,
beta=0.,
a_imp=1.0,
p_fac=0.1,
k_split=1,
n_split=6,
nwat=2, 
na_init=0,
d_ext=0.0,
dnats=0,
fv_sg_adj=900,
d2_bg=0.,
nord= 2
dddmp=0.0,
d4_bg=0.15, 
vtdm4=0.04,
ke_bg=0.,
do_vort_damp=F,
external_ic=F,
read_increment=F,
gfs_phil=F,
nggps_ic=T,
mountain=T,
ncep_ic=F,
d_con=0.0,
hord_mt=6,
hord_vt=-5,
hord_tm=-5
hord_dp=-5
hord_tr=-8,
adjust_dry_mass=F,
consv_te=1,
consv_am=F,
fill=T,
dwind_2d=F,
print_freq=6,
warm_start=T,
no_dycore=F,
z_tracer=T,
/

&coupler_nml
calendar='julian',
memuse_verbose=F,
dt_atmos=$dt_atmos,
dt_ocean=$dt_atmos,
atmos_nthreads=$OMP_NUM_THREADS,
ncores_per_node=$ncores_per_node,
use_hyper_thread=F,
current_date=${year}, ${month}, ${day}, ${hour}, 0, 0,
months=0,
days =$days,
hours=$hours,
minutes=0,
seconds=0,
restart_secs=$restart_secs,
/

&external_ic_nml 
filtered_terrain=T,
ncep_plevels=F,
levp=$LEVP,
gfs_dwinds=T,
checker_tr=F,
nt_checker=0,
/

&gfs_physics_nml
debug=F,
ntoz=3,
ntcw=2,
fhswr=3600.,
fhlwr=3600.,
ozcalc=T,
nocnv=F,
fdiag=1,
fhzero=1,
prslrd0=0.
fhcyc=$fhcyc, !need to be 0 if no grib files in fixdir
use_ufo=F,
nst_anl=F,
cdmbgwd=0.25,2.0,  ! changed for DA resolution at direction of Jeff Whitaker
/

&interpolator_nml
interp_method='conserve_great_circle'
/

&namsfc
fnglac="${FIXGLOBAL}/global_glacier.2x2.grb",
fnmxic="${FIXGLOBAL}/global_maxice.2x2.grb",
fntsfc="${FIXGLOBAL}/cfs_oi2sst1x1monclim19822001.grb",
fnsnoc="${FIXGLOBAL}/global_snoclim.1.875.grb",
fnzorc="${FIXGLOBAL}/global_zorclim.1x1.grb",
fnalbc="${FIXGLOBAL}/global_albedo4.1x1.grb",
fnaisc="${FIXGLOBAL}/cfs_ice1x1monclim19822001.grb",
fntg3c="${FIXGLOBAL}/global_tg3clim.2.6x1.5.grb",
fnvegc="${FIXGLOBAL}/global_vegfrac.0.144.decpercent.grb",
fnvetc="${FIXGLOBAL}/global_vegtype.1x1.grb",
fnsotc="${FIXGLOBAL}/global_soiltype.1x1.grb",
fnsmcc="${FIXGLOBAL}/global_soilmcpc.1x1.grb",
fnmskh="${FIXGLOBAL}/seaice_newland.grb",
fntsfa="${OBS_DATAPATH}/bufr_${analdate}/gdas1.t${hour}z.sstgrb",
fnacna="${OBS_DATAPATH}/bufr_${analdate}/gdas1.t${hour}z.engicegrb",
fnsnoa="${OBS_DATAPATH}/bufr_${analdate}/gdas1.t${hour}z.snogrb",
fnvmnc="${FIXGLOBAL}/global_shdmin.0.144x0.144.grb",
fnvmxc="${FIXGLOBAL}/global_shdmax.0.144x0.144.grb",
fnslpc="${FIXGLOBAL}/global_slope.1x1.grb",
fnabsc="${FIXGLOBAL}/global_snoalb.1x1.grb",
ldebug=f,,
fsmcl(2)=99999,
fsmcl(3)=99999,
fsmcl(4)=99999,
ftsfs=90,
faiss=99999,
fsnol=99999,
fsicl=99999,
ftsfl=99999,
faisl=99999,
fvetl=99999,
fsotl=99999,
fvmnl=99999,
fvmxl=99999,
fslpl=99999,
fabsl=99999,
fsnos=99999,
fsics=99999,

/

&fv_grid_nml
grid_file='INPUT/grid_spec.nc'
/

EOF

./domain_split.x
