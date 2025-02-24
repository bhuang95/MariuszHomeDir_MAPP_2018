#!/bin/ksh --login
#PBS -N calc_aod_all
#PBS -A wrf-chem
##PBS -A chem-var
#PBS -l procs=24
#PBS -l walltime=08:00:00
##PBS -q debug
#PBS -q urgent
##PBS -q batch
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#qsub -v ident=2015080100 qsub_aodnc2grib_all.sh

. /etc/profile
. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

. ./environ.sh

sim=DA_ENKF

workdir=${MAINDIR}/tmp_dirs/workdir_aod

FV3INDIR=${FV3_RUNS}/${sim}/${ident}

outdir=${FV3INDIR}/GRIB

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

set -x

SRCDIR=/home/Mariusz.Pagowski/codes/src_fv3_aod

forecast_length=$FHMAX

obstype='modis_aod'
#obstype='viirs_aod'

cycle_frequency=$ANALINC

cd $workdir

ndate=~/bin/ndate

export OMP_NUM_THREADS=1

nprocs=$PBS_NP

year=`echo $ident | cut -c1-4`
month=`echo $ident | cut -c5-6`
day=`echo $ident | cut -c7-8`
hour=`echo $ident | cut -c9-10`


analdate=$ident
adate=`echo $ident | cut -c1-8`

nanals=$NANALS

nanal=0

while [[ $nanal -le $nanals ]]
do

    /bin/rm -f aod*.bin aod*.nc

    if [[ $nanal = 0 ]]
    then
	charnanal="ensmean"
    else
	charnanal=mem`printf %03i $nanal`
    fi    

    GRIBDIR=/home/Mariusz.Pagowski/MAPP_2018/src_grib
    /bin/cp ${GRIBDIR}/aodncdf2grib.x .
    /bin/cp ${GRIBDIR}/fv3_gribtable .

    indir=${FV3INDIR}/${charnanal}/OUTPUT_FV3

    ln -sf ${indir}/aod* .

cat >'aodncdf2grib.nl' <<EOF    
&record
  yyyymmddhh = "${year}${month}${day}${hour}"
  cres = ${RES}
  inputdir = "."
  gridfiledir= "${FV3_FIXDIR}/C${RES}"
  outputdir = "."
  input_filetype = "aod"
  gribtable = "fv3_gribtable"
  grid_id = 4
  numvars = 1
  var_list = "AOD"
  nchan_nnr=6
  nchan_nnr_select=4 !550nm
  var_list = "AOD"
  tbeg = 1
  tend = $forecast_length
  delta_t = 1
/

EOF

    ./aodncdf2grib.x

    /bin/mv aod_${ident}.grib ${outdir}/aod_${charnanal}_${ident}.grib

    ((nanal=nanal+1))
    
done
