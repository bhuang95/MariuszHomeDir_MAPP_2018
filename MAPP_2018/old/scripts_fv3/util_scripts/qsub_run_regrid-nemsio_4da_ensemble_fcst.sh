#!/bin/ksh --login
#PBS -l nodes=3:ppn=24
#PBS -N regrid_ensemble
##PBS -A chem-var
##PBS -A gsd-fv3-dev
#PBS -A wrf-chem
##PBS -l walltime=04:00:00
#PBS -l walltime=00:10:00
##PBS -q debug
#PBS -q batch
##PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#qsub -v ident=2015080312,member=1 qsub_run_regrid-nemsio_4da_ensemble_fcst.sh
#qsub -v ident=2015080312,member=ensmean qsub_run_regrid-nemsio_4da_ensemble_fcst.sh
#to fix for missing gfs data at 2015080312

if [[ $member == 'ensmean' ]]
then
    charnanal='ensmean'
else
    charnanal=mem`printf %03i $member`
fi


analdate=$ident

. /etc/profile
. /apps/lmod/lmod/init/sh

. ./environ.sh

FHMIN=9
FHMAX=15


. ${MAINDIR}/env.kshrc

ndate=~/bin/ndate

set -x

sim=DA_ENKF

start_date=2015080100
end_date=2015090100

if [[ $analdate -gt $end_date ]]
then
    echo "End of modeling period"
    exit 0
fi

year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hr=`echo $analdate | cut -c9-10`


REGRID_EXEC=$BASEDIR/exec/regrid_nemsio

INDIR_FV3=${FV3_RUNS}/${sim}/${analdate}/${charnanal}/OUTPUT_FV3
OUTDIR=${FV3_RUNS}/${sim}/${analdate}/${charnanal}/OUTPUT_FV3_nemsio

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

export JCAP=$JCAP_ens
export LONB=$LONB_ens
export LATB=$LATB_ens
export NTRAC=21

((LEVP = $LEVS + 1))

workdir=${MAINDIR}/tmp_dirs/workdir_nemsio_regrid_${charnanal}
/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

nprocs=$LEVP

longdate=${analdate}0000
adate=`echo ${analdate} | cut -c1-8`

indir=${INDIR_FV3}
outdir=${OUTDIR}

FIXfv3=${FV3_FIXDIR}/C${RES}

cat > regrid-nemsio.input <<EOF
&share
debug=T,nlons=$LONB,nlats=$LATB,ntrunc=$JCAP,ntrac=$NTRAC,
analysis_filename='${indir}/${adate}.fv3_history.tile1.nc','${indir}/${adate}.fv3_history.tile2.nc','${indir}/${adate}.fv3_history.tile3.nc','${indir}/${adate}.fv3_history.tile4.nc','${indir}/${adate}.fv3_history.tile5.nc','${indir}/${adate}.fv3_history.tile6.nc',
forecast_timestamp='${analdate}'
analysis_filename2d='${indir}/${adate}.fv3_history2d.tile1.nc','${indir}/${adate}.fv3_history2d.tile2.nc','${indir}/${adate}.fv3_history2d.tile3.nc','${indir}/${adate}.fv3_history2d.tile4.nc','${indir}/${adate}.fv3_history2d.tile5.nc','${indir}/${adate}.fv3_history2d.tile6.nc',
variable_table='${SCRIPTDIR_DRIVER}/variable_table_da.txt'
nemsio_opt='bin4'
datapathout2d='bfg_${analdate}',
datapathout3d='sfg_${analdate}',
/
&interpio
esmf_bilinear_filename='${FIXfv3}/fv3_SCRIP_C${RES}_GRIDSPEC_lon${LONB}_lat${LATB}.gaussian.bilinear.nc'
esmf_neareststod_filename='${FIXfv3}/fv3_SCRIP_C${RES}_GRIDSPEC_lon${LONB}_lat${LATB}.gaussian.neareststod.nc'
gfs_hyblevs_filename='${FIXGLOBAL}/global_hyblev.l${LEVP}.txt'
/
EOF

mpirun -n $nprocs $REGRID_EXEC

fh=$FHMIN
while [[ $fh -le $FHMAX ]] 
do
    charfhr1="fhr"`printf %03i $fh`
    ((fhm=fh - $ANALINC))
    charfhr2="fhr"`printf %02i $fhm`
    analp06=`$ndate $ANALINC ${analdate}`
    /bin/mv -f sfg_${analdate}.${charfhr1} ${outdir}/sfg_${analp06}_${charfhr2}
    /bin/mv -f bfg_${analdate}.${charfhr1} ${outdir}/bfg_${analp06}_${charfhr2}
    ((fh=fh+1))
done

#analdate already incremented
ident=`$ndate $ANALINC ${analdate}`

#member='ensmean' qsub_run_regrid-nemsio_4da_ensemble.sh 
#submitted by qsub_calc_ensmean_history.sh

if [[ $member == 'ensmean' ]]
then
    cd $SCRIPTDIR_DA
    echo "qsub -v ident=$ident qsub_gsi_fv3_aod_nnr_ensemble.ksh"
    qsub -v ident=$ident qsub_gsi_fv3_aod_nnr_ensemble.ksh
    exit 0
else
    exit 0
fi


