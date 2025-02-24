#!/bin/ksh 
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/driver_scripts
#PBS -N cold2restart_e
##PBS -q batch
##PBS -q urgent
#PBS -q debug
#PBS -A chem-var
##PBS -l walltime=02:00:00
#PBS -l walltime=00:30:00
##PBS -l nodes=1:ppn=24
#PBS -l nodes=4:ppn=24
##PBS -l nodes=2:ppn=12 #Phil
##PBS -j oe #e and o outputs in the same file

#qsub -v ident=2015080100 qsub_fv3_gfs_c192_cold2restart_ensemble_test.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module purge

. ./environ_old.sh

. ${FV3_BASEDIR}/site/env.kshrc

module load nco

nanals=$NANALS

start_ident=2015080100
end_ident=2015082112

ndate=~/bin/ndate

export FHMIN=0
export FHMAX=0
export FHOUT=0

echo "MODULE LIST"

cycle_frequency=$ANALINC

sim_cold=GFS_cold
sim_restart=GFS_cold_restart

resolution=C${RES}
analdate=$ident
year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hour=`echo $analdate | cut -c9-10`

INDIR=${FV3_RUNS}/${sim_cold}/${analdate}
backdate=`$ndate -${cycle_frequency} ${analdate}`

GFS_INDIR=${INDIR}

grid=${FV3_FIXDIR}/${resolution}
OUTDIR=${FV3_RUNS}/${sim_restart}/${analdate}
WORKDIR=${MAINDIR}/tmp_dirs/workdir_${resolution}_cold2restart_ensemble
GOCART_BCKGDIR=${FV3_DATA}/${resolution}/gocart_bckg_data
EMISS_ANTHRODIR=${FV3_DATA}/${resolution}/emiss_anthro_data
SAND_CLAYDIR=${FV3_DATA}/${resolution}/sand_clay_data
ERODDIR=${FV3_DATA}/${resolution}/erod_data
bbdate=`echo ${analdate} | cut -c1-8`00
EMISS_BIOBURNDIR=${FV3_DATA}/${resolution}/${bbdate}/emiss_bioburn_data

warmstart=F
externalic=T
mountain=F
readincrement=F

increment_file="fv3_increment.nc"

reslatlondynamics="${increment_file}"

# run length
months="0"
days="0"
hours=$FHMAX
dt_atmos=450
((restart_secs = $ANALINC * 3600))
#restart_secs=0

na_init=0

fhcyc="0."
#if set fhcyc > 0 then need grib files in FIX dir

((LEVP = $LEVS + 1))

ncores_per_node=24

npes=$PBS_NP
export OMP_NUM_THREADS=1

npx="193"
npy="193"
npz="63"
#layout_x="2"
#layout_y="2"
layout_x="4"
layout_y="4"
io_layout="1,1"

nxblocks="1"
nyblocks="24"


cmon=`printf %02i $month`
cday=`printf %02i $day`
chour=`printf %02i $hour`

nanal=1

while [[ $nanal -le $nanals ]]
do
    charnanal=mem`printf %03i $nanal`
    
    \rm -rf $WORKDIR
    
    mkdir -p $WORKDIR
    cd $WORKDIR
    
    mkdir -p RESTART
    
    # build the date for curr_date and diag_table from analdate
    

# build the diag_table with the experiment name and date stamp

    /bin/cp ${SCRIPTDIR_DRIVER}/data_table .
    /bin/cp ${SCRIPTDIR_DRIVER}/diag_table_cold diag_table
    /bin/cp ${SCRIPTDIR_DRIVER}/nems.configure nems.configure
    /bin/cp ${SCRIPTDIR_DRIVER}/field_table_default field_table
    /bin/cp ${FV3_EXECDIR}/${FV3_EXEC} .
    
    sed -i -e "s/YYYY MM DD HH/${year} ${cmon} ${cday} ${chour}/g" diag_table
    
    mkdir INPUT

    /bin/cp /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/FV3_RUNS/GFS_cold/${analdate}/gfs_ctrl.nc ./INPUT

    ln -sf ${FIXGLOBAL}/* .
    ln -sf ${FV3_CLIM}/* .
    ln -sf ${FV3_CLIM}/* ./INPUT
    
# Grid and orography data

    ln -sf ${grid}/* ./INPUT

    gfsindir=${GFS_INDIR}/${charnanal}

    ln -sf ${gfsindir}/*.nc ./INPUT

    itile=1
    while [[ $itile -le 6 ]]
    do
	ln -sf ${gfsindir}/gfs_data.tile${itile}_${charnanal}.nc ./INPUT/gfs_data.tile${itile}.nc
	ln -sf ${gfsindir}/sfc_data.tile${itile}_${charnanal}.nc ./INPUT/sfc_data.tile${itile}.nc
	((itile=itile+1))
    done
    
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
ntiles=6,
npz=$npz,
grid_type=-1,
make_nh=T,
fv_debug=F,
range_warn=F,
reset_eta=F,
n_sponge=24,
tau=5.,
rf_cutoff=7.5e2,
d2_bg_k1=0.15,
d2_bg_k2=0.02,
kord_tm=-9,
kord_mt=9,
kord_wz=9,
kord_tr=9,
hydrostatic=F,
phys_hydrostatic=F,
use_hydro_pressure=F,
beta=0.,
a_imp=1.0,
p_fac=0.1,
k_split=2,
n_split=6,
nwat=2, 
na_init=$na_init,
d_ext=0.0,
dnats=0,
fv_sg_adj=900,
d2_bg=0.,
nord= 2
dddmp=0.0,
d4_bg=0.12, 
vtdm4=0.03,
ke_bg=0.,
do_vort_damp=T,
external_ic=$externalic,
gfs_phil=F,
nggps_ic=T,
mountain=${mountain},
ncep_ic=F,
d_con=1.0,
hord_mt=5,
hord_vt=5,
hord_tm=5,
hord_dp=5
hord_tr=8,
adjust_dry_mass=F,
consv_te=0,
consv_am=F,
fill=T,
dwind_2d=F,
print_freq=6,
warm_start=$warmstart,
no_dycore=F,
z_tracer=T,
read_increment=$readincrement
res_latlon_dynamics = "$reslatlondynamics"
!gocart_esrl=T
/

&coupler_nml
calendar='julian',
memuse_verbose=F,
dt_atmos=$dt_atmos,
dt_ocean=$dt_atmos,
atmos_nthreads=$OMP_NUM_THREADS,
ncores_per_node=$ncores_per_node,
use_hyper_thread=F,
current_date=${year}, ${cmon}, ${cday}, ${chour}, 0, 0,
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
fhzero=$FHMIN,
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

&nggps_diag_nml
  fdiag = 3
/

&interpolator_nml
interp_method='conserve_great_circle'
/

&namsfc
fnglac="${FIXGLOBAL}/global_glacier.2x2.grb",
fnmxic="${FIXGLOBAL}/global_maxice.2x2.grb",
fntsfc="${FIXGLOBAL}/RTGSST.1982.2012.monthly.clim.grb",
fnsnoc="${FIXGLOBAL}/global_snoclim.1.875.grb",
fnzorc="${FIXGLOBAL}/igbp",
fnalbc="${FIXGLOBAL}/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb",
fnalbc2="${FIXGLOBAL}/global_albedo4.1x1.grb",
fnaisc="${FIXGLOBAL}/CFSR.SEAICE.1982.2012.monthly.clim.grb",
fntg3c="${FIXGLOBAL}/global_tg3clim.2.6x1.5.grb",
fnvegc="${FIXGLOBAL}/global_vegfrac.0.144.decpercent.grb",
fnvetc="${FIXGLOBAL}/global_vegfrac.0.144.decpercent.grb",
fnsotc="${FIXGLOBAL}/global_soiltype.statsgo.t1534.3072.1536.rg.grb",
fnsmcc="${FIXGLOBAL}/global_soilmgldas.t1534.3072.1536.grb",
fnmskh="${FIXGLOBAL}/seaice_newland.grb",
fntsfa="",
fnacna="",
fnsnoa="",
fnvmnc="${FIXGLOBAL}/global_shdmin.0.144x0.144.grb",
fnvmxc="${FIXGLOBAL}/global_shdmax.0.144x0.144.grb",
fnslpc="${FIXGLOBAL}/global_slope.1x1.grb",
fnabsc="${FIXGLOBAL}/global_snoalb.1x1.grb",
ldebug=F,
fsmcl(2)=99999,
fsmcl(3)=99999,
fsmcl(4)=99999,
ftsfs=90,
faisl=99999,
faiss=99999,
fsnol=99999,
fsnos=99999,
fsicl=99999,
fsics=99999,
ftsfl=99999,
fvetl=99999,
fsotl=99999,
fvmnl=99999,
fvmxl=99999,
fslpl=99999,
fabsl=99999,
/

&fv_grid_nml
grid_file='INPUT/grid_spec.nc'
/

&nam_stochy
/

EOF

cat > model_configure <<EOF
total_member:            1
PE_MEMBER01:             $npes
start_year:              $year
start_month:             $month
start_day:               $day
start_hour:              $hour
start_minute:            0
start_second:            0
nhours_fcst:             $FHMAX
RUN_CONTINUE:            .false.
ENS_SPS:                 .false.
dt_atmos:                450
calendar:                'julian'
memuse_verbose:          .false.
atmos_nthreads:          $OMP_NUM_THREADS
use_hyper_thread:        .false.
ncores_per_node:         $ncores_per_node
restart_interval:        $restart_secs

quilting:                .false.
write_groups:            1
write_tasks_per_group:   6
num_files:               2
filename_base:           'dyn' 'phy'
output_grid:             'cubed_sphere_grid'
write_nemsiofile:        .false.
imo:                     384
jmo:                     190

nfhout:                  $FHOUT
nfhmax_hf:               $FHMAX
nfhout_hf:               1
nsout:                   -1

EOF

    mpirun -n $npes ${FV3_EXEC} | tee fms.out

    if [[ $? != 0 ]]
    then
	echo "FV3 failed"
	exit 1
    fi
    
    outdir=${OUTDIR}/${charnanal}

    /bin/rm -rf ${outdir}
    
    mkdir -p ${outdir}

    /bin/mv diag_table field_table fms.out input.nml logfile.000000.out time_stamp.out $outdir
    
    /bin/mv RESTART ${outdir}

    ((nanal=nanal+1))
    
    cd ${SCRIPTDIR_DRIVER}
    
done

ident=`$ndate ${cycle_frequency} ${analdate}`

if [[ $ident -le $end_ident ]]
then
    cd $SCRIPTDIR_DRIVER
    echo "qsub -v ident=$ident qsub_fv3_gfs_c192_cold2restart_ensemble.sh"
    qsub -v ident=$ident qsub_fv3_gfs_c192_cold2restart_ensemble.sh
    exit 0
else
    echo "End simulation period ident=$ident"
    exit 0
fi
