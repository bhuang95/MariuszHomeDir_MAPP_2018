#!/bin/ksh 
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -d /home/Mariusz.Pagowski/codes/fv3/driver_scripts
#PBS -N c192_cold
##PBS -q batch
#PBS -q urgent
##PBS -q debug
#PBS -A chem-var
##PBS -l walltime=02:00:00
#PBS -l walltime=00:05:00
##PBS -l nodes=1:ppn=24
#PBS -l nodes=4:ppn=24
##PBS -l nodes=2:ppn=12 #Phil
##PBS -j oe #e and o outputs in the same file

#qsub -v ident=2015080100 qsub_fv3_gfs_c192_cold2restart.sh

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module purge

. ./environ.sh

. ${FV3_BUILD}/../site/env.kshrc

. ~/bin/funcs.sh

export FHMIN=0
export FHMAX=0
export FHOUT=0

echo "MODULE LIST"

start_ident=2015080100
end_ident=2015090100
cycle_frequency=$ANALINC

resolution="C192"
analdate=$ident
year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hour=`echo $analdate | cut -c9-10`


DATEDIR=${FV3_DATA}/${resolution}/${analdate}
INDIR=${DATEDIR}/INPUT_cold
#grid is temporary until figure out why not fix/C192
#grid=${FV3_FIXDIR}/${resolution}/grid 
grid=${FV3_DATA}/${resolution}/grid 
clim=${FV3_CLIM}
OUTDIR=${DATEDIR}/RESTART_cold
WORKDIR=${FV3_BASEDIR}/workdir_${resolution}_cold
GOCART_BCKGDIR=${FV3_DATA}/${resolution}/gocart_bckg_data
EMISS_ANTHRODIR=${FV3_DATA}/${resolution}/emiss_anthro_data
SAND_CLAYDIR=${FV3_DATA}/${resolution}/sand_clay_data
ERODDIR=${FV3_DATA}/${resolution}/erod_data
bbdate=`echo ${analdate} | cut -c1-8`00
EMISS_BIOBURNDIR=${FV3_DATA}/${resolution}/${bbdate}/emiss_bioburn_data

# case specific details
type="nh"         # choices:  nh, hydro
mono="non-mono"   # choices:  mono, non-mono
hypt="off"        # choices:  on, off  (controls hyperthreading)

# set various debug options

if [[ ${type} == "nh" ]] 
then
# non-hydrostatic options
    make_nh="T"
    hydrostatic="F"
    phys_hydrostatic="F"     # can be tested
    use_hydro_pressure="F"   # can be tested
    consv_te="1."
# time step parameters in FV3
    k_split="1"
    n_split="6"
else
# hydrostatic options
    make_nh="F"
    hydrostatic="T"
    phys_hydrostatic="F"     # will be ignored in hydro mode
    use_hydro_pressure="T"   # have to be .T. in hydro mode
    consv_te="0."
# time step parameters in FV3
    k_split="1"
    n_split="6"
fi

if [[ ${mono} == "mono" || ${mono} == "monotonic" ]] 
then
      # monotonic options
    d_con="0."
    do_vort_damp=".false."
    if [[ ${type} == "nh" ]]
    then	
        # non-hydrostatic
	hord_mt=" 10"
	hord_xx="-10"
    else
        # hydrostatic
	hord_mt=" 10"
	hord_xx="-10"
    fi
else
      # non-monotonic options
    d_con="1."
    do_vort_damp=".true."
    if [[ ${type} == "nh" ]] 
    then
        # non-hydrostatic
	hord_mt=" 6"
	hord_xx="-5"
    else
        # hydrostatic
	hord_mt=" 10"
	hord_xx="-10"
    fi
fi

#cold start:
#warmstart=F
#externalic=T
#reslatlondynamics=""
#mountain=F
#readincrement=F

#warmstart:
#warmstart=T
#externalic=F
#reslatlondynamics="${increment_file}"
#readincrement=T
#mountain=T

warmstart=F
externalic=T
reslatlondynamics=""
mountain=F
readincrement=F

# run length
months="0"
days="0"
hours=$FHMAX
dt_atmos=450
((restart_secs = $ANALINC * 3600))
restart_secs=0


# set the pre-conditioning of the solution
# =0 implies no pre-conditioning
# >0 means new adiabatic pre-conditioning
# <0 means older adiabatic pre-conditioning
na_init=0

fhcyc="0."
#if set fhcyc > 0 then need grib files in FIX dir

((LEVP = $LEVS + 1))

cores_per_node="24"

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

# blocking factor used for threading and general physics performance
nxblocks="1"
nyblocks="24"

# when running with threads, need to use the following command

\rm -rf $WORKDIR

mkdir -p $WORKDIR
cd $WORKDIR

mkdir -p RESTART

# build the date for curr_date and diag_table from analdate

cmon=`printf %02i $month`
cday=`printf %02i $day`
chour=`printf %02i $hour`

# build the diag_table with the experiment name and date stamp

/bin/cp ${SCRIPTDIR}/diag_table .
/bin/cp ${SCRIPTDIR}/data_table data_table
/bin/cp ${SCRIPTDIR}/field_table field_table
/bin/cp ${FV3_EXECDIR}/${FV3_EXEC} .

sed -i -e "s/YYYY MM DD HH/${year} ${cmon} ${cday} ${chour}/g" diag_table

mkdir INPUT

ln -s ${clim}/* ./INPUT

# Grid and orography data
ln -s ${grid}/* ./INPUT

# Date specific ICs
ln -s ${INDIR}/*.nc ./INPUT

itile=1
while [[ $itile -le 6 ]]
do
    ctile=tile${itile}
    ln -s ${GOCART_BCKGDIR}/${cmon}/gocart_bckg_${cmon}.${ctile}.nc ./INPUT/bckg.${ctile}.nc

    ln -s ${EMISS_ANTHRODIR}/${cmon}/emiss_anthro_${cmon}.${ctile}.nc ./INPUT/emiss_anthro.${ctile}.nc

#bioburn will be daily files at some point

    ln -s ${SAND_CLAYDIR}/sand_clay.${ctile}.nc ./INPUT

    ln -s ${ERODDIR}/erod.${ctile}.nc ./INPUT

    ln -s ${EMISS_BIOBURNDIR}/emiss_bioburn.${ctile}_${bbdate}.nc ./INPUT/emiss_bioburn.${ctile}.nc

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
ntiles=6,
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
hydrostatic=$hydrostatic,
phys_hydrostatic=$phys_hydrostatic,
use_hydro_pressure=$use_hydro_pressure,
beta=0.,
a_imp=1.0,
p_fac=0.1,
k_split=$k_split,
n_split=$n_split,
nwat=2, 
na_init=$na_init,
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
external_ic=$externalic,
res_latlon_dynamics=$reslatlondynamics,
read_increment=$readincrement,
gfs_phil=F,
nggps_ic=T,
mountain=${mountain},
ncep_ic=F,
d_con=0.0,
hord_mt=${hord_mt},
hord_vt=${hord_xx},
hord_tm=${hord_xx},
hord_dp=${hord_xx},
hord_tr=-8,
adjust_dry_mass=F,
consv_te=$consv_te,
consv_am=F,
fill=T,
dwind_2d=F,
print_freq=6,
warm_start=$warmstart,
no_dycore=F,
z_tracer=T,
gocart_esrl=T
/

&coupler_nml
calendar='julian',
memuse_verbose=F,
dt_atmos=$dt_atmos,
dt_ocean=$dt_atmos,
atmos_nthreads=$OMP_NUM_THREADS,
ncores_per_node=$cores_per_node,
use_hyper_thread=F,
current_date=${year}, ${cmon}, ${cday}, ${chour}, 0, 0,
months=0,
days =$days,
hours=$hours,
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
fdiag=$FHOUT,
fhzero=$FHOUT,
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

mpirun -n $npes ${FV3_EXEC} | tee fms.out

if [[ $? != 0 ]]
then
    echo "FV3 failed"
    exit
fi

/bin/rm -rf ${DATEDIR}/RESTART_cold

module load nco

itile=1
while [[ $itile -le 6 ]]
do
    ctile=tile${itile}
    ncks -O -v sphum,liq_wat,o3mr RESTART/fv_tracer.res.${ctile}.nc RESTART/gfs_tracer.res.${ctile}.nc
    ((itile=itile+1))
done

/bin/rm -f RESTART/fv_tracer.res.tile?.nc

/bin/mv RESTART $DATEDIR/RESTART_cold

increment_date $cycle_frequency

ident=${end_year}${end_month}${end_day}${end_hour}

if [[ $ident -le $end_ident ]]
then
    cd $SCRIPTDIR
    echo "qsub -v ident=$ident qsub_fv3_gfs_c192_cold2restart.sh"
    qsub -v ident=$ident qsub_fv3_gfs_c192_cold2restart.sh
    exit 0
else
    echo "End simulation period ident=$ident"
    exit 0
fi
