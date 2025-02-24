#!/bin/ksh 
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -d /home/Mariusz.Pagowski/codes/fv3/driver_scripts
#PBS -N c192_warm_da
##PBS -q batch
##PBS -q urgent
#PBS -q debug
#PBS -A chem-var
##PBS -l walltime=02:00:00
#PBS -l walltime=00:30:00
##PBS -l nodes=1:ppn=24
#PBS -l nodes=16:ppn=24
##PBS -l nodes=24:ppn=12
##PBS -l nodes=2:ppn=12 #Phil
##PBS -j oe #e and o outputs in the same file


#qsub -v ident=2015080100 qsub_fv3_gfs_c192_warm_da_new.sh

#nprocs=6*layout_x*layout_y

set -x

. /etc/profile
. /apps/lmod/lmod/init/sh

module purge

. ./environ_new.sh

. ${FV3_BUILD}/../site/env_new.kshrc

cycle_frequency=$ANALINC
start_ident=2015080100
end_ident=2015081100

echo "MODULE LIST"

. ~/bin/funcs.sh

sim=DA_GSI
sim_cold=no_DA_cold

resolution=C${RES}
analdate=$ident
year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hour=`echo $analdate | cut -c9-10`

INDIR=${FV3_RUNS}/${sim_cold}/${analdate}
increment_date -${cycle_frequency}
backdate=${end_year}${end_month}${end_day}${end_hour}

BACK_INDIR=${FV3_RUNS}/${sim}/${backdate}

if [[ $ident == $start_ident ]]
then
    start='T'
    GFS_INDIR=${INDIR}/INPUT
    TRACER_INDIR=${INDIR}/tracers_realistic_da
    add_increment=0
else
    start='F'
    GFS_INDIR=${INDIR}/RESTART_cold
    TRACER_INDIR=${BACK_INDIR}/RESTART
    add_increment=1
fi

#grid is temporary until figure out why not fix/C192
#grid=${FV3_FIXDIR}/${resolution}/grid 
grid=${FV3_DATA}/${resolution}/grid 
clim=${FV3_CLIM}
OUTDIR=${FV3_RUNS}/${sim}/${analdate}/OUTPUT_FV3
WORKDIR=${BASEDIR}/workdir_${resolution}_warm_da
GOCART_BCKGDIR=${FV3_DATA}/${resolution}/gocart_bckg_data
EMISS_ANTHRODIR=${FV3_DATA}/${resolution}/emiss_anthro_data
SAND_CLAYDIR=${FV3_DATA}/${resolution}/sand_clay_data
ERODDIR=${FV3_DATA}/${resolution}/erod_data
bbdate=`echo ${analdate} | cut -c1-8`00
EMISS_BIOBURNDIR=${FV3_DATA}/${resolution}/${bbdate}/emiss_bioburn_data

# set various debug options

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

warmstart=T
externalic=T
mountain=T
#reslatlondynamics=""
#readincrement=F

# run length
months="0"
days="0"
hours=$FHMAX
dt_atmos=450
ANALINC=$cycle_frequency
((restart_secs = $ANALINC * 3600))
#restart_secs=0


# set the pre-conditioning of the solution
# =0 implies no pre-conditioning
# >0 means new adiabatic pre-conditioning
# <0 means older adiabatic pre-conditioning
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
layout_x="8"
layout_y="8"
io_layout="1,1"

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

#/bin/cp ${SCRIPTDIR}/data_table data_table
#/bin/cp ${SCRIPTDIR}/diag_table_da diag_table
/bin/cp ${SCRIPTDIR}/diag_table_new diag_table
/bin/cp ${SCRIPTDIR}/nems.configure nems.configure
#/bin/cp ${SCRIPTDIR}/field_table_tracers_da field_table
/bin/cp ${SCRIPTDIR}/field_table_new field_table
/bin/cp /scratch3/BMC/chem-var/pagowski/tmp/fv3_gocart/global_shared.v15.0.0/sorc/fv3gfs.fd/NEMS/exe/${FV3_EXEC} .
#/bin/cp ${FV3_EXECDIR}/${FV3_EXEC} .

sed -i -e "s/YYYY MM DD HH/${year} ${cmon} ${cday} ${chour}/g" diag_table

mkdir INPUT

module load nco

ln -s ${clim}/* .

# Grid and orography data
ln -s ${grid}/* ./INPUT

# Date specific ICs
if [[ $start == 'T' || $cycle_frequency == 24 ]]
then
    if [[ $start == 'T' ]]
    then
	ln -sf ${GFS_INDIR}/*.nc ./INPUT
	ln -sf ${TRACER_INDIR}/fv_tracer.res.tile?.nc ./INPUT
    else
	echo "Needs to be tested - exitting"
	exit
	ln -sf ${GFS_INDIR}/*.nc ./INPUT
	itile=1
	while [[ $itile -le 6 ]]
	do
	    ctile=tile${itile}
	    ln -sf ${TRACER_INDIR}/fv_tracer.res.${ctile}.nc ./INPUT/fv_tracer.res.${ctile}.nc
	    ((itile=itile+1))
	done
    fi
else
    ln -sf ${GFS_INDIR}/[c,f,s]*.nc ./INPUT
    itile=1
    while [[ $itile -le 6 ]]
    do
	ctile=tile${itile}
	if [[ $FHMAX -gt 6 ]]
	then
	    ln -sf ${TRACER_INDIR}/${year}${month}${day}.${hour}0000.fv_tracer.res.${ctile}.nc ./INPUT/fv_tracer.res.${ctile}.nc
	else
	    ln -sf ${TRACER_INDIR}/fv_tracer.res.${ctile}.nc ./INPUT/fv_tracer.res.${ctile}.nc
	fi

        ncks -A ${GFS_INDIR}/gfs_tracer.res.${ctile}.nc ./INPUT/fv_tracer.res.${ctile}.nc

	((itile=itile+1))
    done
fi

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

if [[ $add_increment == 1 ]] 
then 
    increment_file="fv3_increment.nc"
    readincrement=T
    reslatlondynamics="${increment_file}"
    ln -s ${FV3_RUNS}/${sim}/${analdate}/OUTPUT_GSI_increment/${increment_file} ./INPUT
fi

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
blocksize = 32,
chksum_debug = F,
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
make_nh=$make_nh,
fv_debug=F,
range_warn=F,
reset_eta=F,
n_sponge=24,
nudge_qv = T,
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
delt_max = 0.002,
ke_bg=0.,
do_vort_damp=T,
external_ic=$externalic,
!external_eta=T,
!res_latlon_dynamics=$reslatlondynamics,
!read_increment=$readincrement,
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
!gocart_esrl=T
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
ldiag3d= F,
fhcyc=$fhcyc, !need to be 0 if no grib files in fixdir
use_ufo        = T,
pre_rad        = F,
ncld        = 1,
zhao_mic       = T,
pdfcld         = F,
fhswr       = 3600.,
fhlwr          = 3600.,
ialb           = 1,
iems           = 1,
iaer        = 111,
ico2        = 2,
isubc_sw    = 2,
isubc_lw    = 2,
isol        = 2,
lwhtr          = T,
swhtr          = T,
cnvgwd         = T,
shal_cnv       = T,
cal_pre        = T,
redrag         = T,
dspheat        = F,
hybedmf        = T,
random_clds    = T,
trans_trac     = T,
cnvcld         = T,
imfshalcnv  = 3,
imfdeepcnv  = 3,
cdmbgwd     = 2.0,0.25,
prslrd0     = 0.,
ivegsrc     = 1,
isot        = 1,
debug          = F,
nstf_name      = 0
nst_anl=F,
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
ldebug=f,,
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
restart_interval:        21600

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

exit

#for file in ./${analdate}*.fv3_history.tile?.nc
#do
#    ncpdq -O -a '-pfull' $file ${file}_p_reversed
#done


for file in ./RESTART/fv_tracer.res.*.nc
do
    ncpdq -O -a '-zaxis_1' $file ${file}_z_reversed
done

if [[ $? != 0 ]]
then
    echo "FV3 failed"
    exit
fi

/bin/rm -rf  $OUTDIR
mkdir -p $OUTDIR

/bin/mv ${analdate}*.nc diag_table field_table fms.out input.nml logfile.000000.out time_stamp.out $OUTDIR

/bin/rm -rf  ${OUTDIR}/../RESTART
/bin/mv RESTART ${OUTDIR}/..

if [[ $ident -le $end_ident ]]
then
#    cd $SCRIPTDIR
#    echo "qsub -v ident=$ident qsub_fv3_gfs_c192_warm_da.sh"
#    qsub -v ident=$ident qsub_fv3_gfs_c192_warm_da.sh
    cd $SCRIPTDIR_UTIL
    echo "qsub -v ident=$ident qsub_run_regrid-nemsio_4da.sh"
    exit
    qsub -v ident=$ident qsub_run_regrid-nemsio_4da.sh
    exit 0
else
    echo "End simulation period ident=$ident"
    exit 0
fi


