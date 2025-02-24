#!/bin/ksh --login
#PBS -l nodes=3:ppn=24
#PBS -N regrid
#PBS -A chem-var
##PBS -l walltime=04:00:00
#PBS -l walltime=00:10:00
#PBS -q debug
##PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs

#qsub -v ident=2015081006 qsub_run_regrid-nemsio_4da_ensmean.sh

analdate=$ident

. /etc/profile
. /apps/lmod/lmod/init/sh

. ./environ.sh

. ~/bin/funcs.sh

ndate=~/bin/ndate

set -x

start_date=2015081006
end_date=2015081006

if [[ $analdate -gt $end_date ]]
then
    echo "End of modeling period"
    exit
fi

year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hr=`echo $analdate | cut -c9-10`

. ${FV3_BUILD}/../site/env.kshrc

REGRID_EXEC=$BASEDIR/chgres_fv3/exec/regrid_nemsio

INDIR_FV3=${FV3_RUNS}/DA_ENKF/${analdate}/OUTPUT_FV3/ensmean
OUTDIR=${FV3_RUNS}/DA_ENKF/${analdate}/OUTPUT_FV3_nemsio/ensmean

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

export JCAP=$JCAP_ens
export LONB=$LONB_ens
export LATB=$LATB_ens
export NTRAC=21

((LEVP = $LEVS + 1))

workdir=${BASEDIR}/workdir_nemsio_regrid
/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

nprocs=$LEVP

longdate=${analdate}0000
charnanal='control'

indir=${INDIR_FV3}
outdir=${OUTDIR}

FIXfv3=${FV3_FIXDIR}/C${CRES}

cat > regrid-nemsio.input <<EOF
&share
debug=T,nlons=$LONB,nlats=$LATB,ntrunc=$JCAP,ntrac=$NTRAC,
analysis_filename='${indir}/${longdate}.fv3_history.tile1.nc','${indir}/${longdate}.fv3_history.tile2.nc','${indir}/${longdate}.fv3_history.tile3.nc','${indir}/${longdate}.fv3_history.tile4.nc','${indir}/${longdate}.fv3_history.tile5.nc','${indir}/${longdate}.fv3_history.tile6.nc',
forecast_timestamp='${analdate}'
analysis_filename2d='${indir}/${longdate}.fv3_history2d.tile1.nc','${indir}/${longdate}.fv3_history2d.tile2.nc','${indir}/${longdate}.fv3_history2d.tile3.nc','${indir}/${longdate}.fv3_history2d.tile4.nc','${indir}/${longdate}.fv3_history2d.tile5.nc','${indir}/${longdate}.fv3_history2d.tile6.nc',
variable_table='${SCRIPTDIR_UTIL}/variable_table.txt.da'
nemsio_opt='bin4'
datapathout2d='bfg_${analdate}_${charnanal}',
datapathout3d='sfg_${analdate}_${charnanal}',
/
&interpio
esmf_bilinear_filename='${FIXfv3}/fv3_SCRIP_C${CRES}_GRIDSPEC_lon${LONB}_lat${LATB}.bilinear.nc'
esmf_neareststod_filename='${FIXfv3}/fv3_SCRIP_C${CRES}_GRIDSPEC_lon${LONB}_lat${LATB}.neareststod.nc'
/
EOF

mpirun -n $nprocs $REGRID_EXEC

fh=$FHMIN
while [[ $fh -le $FHMAX ]] 
do
    charfhr1="fhr"`printf %03i $fh`
    charfhr2="fhr"`printf %02i $fh`
    analp06=`$ndate $ANALINC $analdate`

    /bin/mv -f sfg_${analdate}_${charnanal}.${charfhr1} ${outdir}/sfg_${analp06}_${charfhr2}_${charnanal}
    /bin/mv -f bfg_${analdate}_${charnanal}.${charfhr1} ${outdir}/bfg_${analp06}_${charfhr2}_${charnanal}
    ((fh=fh+1))
done

analdatep=`$ndate +$ANALINC $analdate`
ident=$analdatep

exit

cd $SCRIPTDIR_DA

echo "qsub -v ident=$ident qsub_gsi_fv3_aod_viirs.ksh"
qsub -v ident=$ident qsub_gsi_fv3_aod_viirs.ksh
