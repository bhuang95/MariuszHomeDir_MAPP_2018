#!/bin/ksh --login
#PBS -l nodes=3:ppn=24
#PBS -N regrid
#PBS -A chem-var
##PBS -l walltime=04:00:00
#PBS -l walltime=00:30:00
#PBS -q debug
##PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/fv3GFS/qslogs

. /etc/profile
. /apps/lmod/lmod/init/sh

. ./environ.sh

FCSTINC=12

. ~/bin/funcs.sh

ndate=~/bin/ndate

set -x

start_date=2015080400
end_date=2015090100

year=`echo $start_date | cut -c1-4`
month=`echo $start_date | cut -c5-6`
day=`echo $start_date | cut -c7-8`
hr=`echo $start_date | cut -c9-10`

. ${FV3_BUILD}/../site/env.kshrc

REGRID_EXEC=$BASEDIR/chgres_fv3/exec/regrid_nemsio

INDIR_FV3=${FV3_BASEDIR}/FV3_RUNS/BCKG

OUTDIR=${FV3_BASEDIR}/FV3_RUNS/BCKG/nemsio

if [[ ! -r $OUTDIR ]]
then
    mkdir -p $OUTDIR
fi

export JCAP=$JCAP_ens
export LONB=$LONB_ens
export LATB=$LATB_ens
export NTRAC=18

((LEVP = $LEVS + 1))

workdir=${FV3_BASEDIR}/workdir_nemsio_regrid_bckg
/bin/rm -rf $workdir
mkdir -p $workdir

nprocs=$LEVP

FIXfv3=${FV3_FIXDIR}/C${CRES}

analdate=$start_date

while [[ $analdate -le $end_date ]]
do
    longdate=${analdate}0000

    indir=${INDIR_FV3}/${analdate}/OUTPUT_FV3
    outdir=${OUTDIR}

    cd $workdir
    /bin/rm -rf regrid-nemsio.input  bfg_* sfg_*

cat > regrid-nemsio.input <<EOF
&share
debug=T,nlons=$LONB,nlats=$LATB,ntrunc=$JCAP,ntrac=$NTRAC,
analysis_filename='${indir}/${longdate}.fv3_history.tile1.nc','${indir}/${longdate}.fv3_history.tile2.nc','${indir}/${longdate}.fv3_history.tile3.nc','${indir}/${longdate}.fv3_history.tile4.nc','${indir}/${longdate}.fv3_history.tile5.nc','${indir}/${longdate}.fv3_history.tile6.nc',
forecast_timestamp='${analdate}'
variable_table='${SCRIPTDIR_UTIL}/variable_table.txt.bckg'
nemsio_opt='bin4'
datapathout3d='sfg_${analdate}_ensmean',
datapathout2d='bfg_${analdate}_ensmean',
/
&interpio
esmf_bilinear_filename='${FIXfv3}/fv3_SCRIP_C${CRES}_GRIDSPEC_lon${LONB}_lat${LATB}.bilinear.nc'
esmf_neareststod_filename='${FIXfv3}/fv3_SCRIP_C${CRES}_GRIDSPEC_lon${LONB}_lat${LATB}.neareststod.nc'
/
EOF

    mpirun -n $nprocs $REGRID_EXEC

    cd ${outdir}

    fh=12
    charfhr1="fhr"`printf %03i $fh`
    charfhr2="fhr"`printf %02i $fh`
    analp12=`$ndate $FCSTINC $analdate`
    /bin/mv -f ${workdir}/sfg_${analdate}_ensmean.${charfhr1} ./sfg_${analp12}_${charfhr2}_ensmean
    
    fh=24
    charfhr1="fhr"`printf %03i $fh`
    charfhr2="fhr"`printf %02i $fh`
    analp24=`$ndate $FCSTINC $analp12`
    /bin/mv -f ${workdir}/sfg_${analdate}_ensmean.${charfhr1} ./sfg_${analp24}_${charfhr2}_ensmean

#    analdate=`$ndate +$ANALINC $analdate`
    analdate=`$ndate +$FCSTINC $analdate`

done

