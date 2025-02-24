#!/bin/ksh --login
#PBS -N calc_fv3restart_ensmean
##PBS -A chem-var
##PBS -A gsd-fv3-dev
#PBS -A wrf-chem
#PBS -l procs=24
##PBS -q debug
##PBS -q urgent
#PBS -q batch
##PBS -q bigmem 
#PBS -l walltime=00:10:00
#PBS -d /home/Mariusz.Pagowski/codes/scripts_fv3/util_scripts
#PBS -o /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/tmp/FV3GFS/qslogs

#qsub -v ident=2015080106 qsub_calc_ensmean_restart.sh

#ident=2015081006

. /etc/profile

. /home/Mariusz.Pagowski/enkf_runs/scripts/modules/gsi_modules.sh

set -x

. ./environ.sh

nanals=$NANALS

PBS_NP=$nanals

if [[ $nanals -gt $PBS_NP ]]
then
    echo "PBS_NP=$PBS_NP < nanals=$nanals - Stopping"
    exit 1
fi

analdate=$ident
year=`echo $analdate | cut -c1-4`
month=`echo $analdate | cut -c5-6`
day=`echo $analdate | cut -c7-8`
hour=`echo $analdate | cut -c9-10`

sim=DA_ENKF

ndate=~/bin/ndate

EXECDIR=${BASEDIR}/execs

analdate=$ident

EXEC=${MY_EXECDIR}/calc_ensmean_fv3.x

workdir=${MAINDIR}/tmp_dirs/workdir_calc_ensmean

/bin/rm -rf $workdir
mkdir -p $workdir

cd $workdir

cat > ensmean.nml <<EOF
&ensmean_nml
varnames2d = ,
varnames3d =  'bc1','bc2','oc1','oc2','p25','sulf','dust1','dust2','dust3','dust4','dust5','seas1','seas2','seas3','seas4'
/
EOF


analdatep=`ndate +$ANALINC ${analdate}`
adatep=`echo ${analdatep} | cut -c1-8`
hourp=`echo ${analdatep} | cut -c9-10`

INDIR=${FV3_RUNS}/${sim}/${analdate}

itile=1
while [[ $itile -le 6 ]]
do
    echo $itile
    /bin/cp ${INDIR}/mem001/RESTART/${adatep}.${hourp}0000.fv_tracer.res.tile${itile}.nc restart.ensmean

    nanal=1
    while [[ $nanal -le $nanals ]]
    do
	charnanal=mem`printf %03i $nanal`
	indir=${FV3_RUNS}/${sim}/${analdate}/${charnanal}/RESTART
	ln -sf ${indir}/${adatep}.${hourp}0000.fv_tracer.res.tile${itile}.nc restart.${charnanal}
	if [[ ! -s restart.${charnanal} ]]
	then
	    echo "Missing fv3 restart output for member ${charnanal} tile ${itile} - Stopping"
	    exit 1
	fi
	((nanal=nanal+1))
    done
    mpirun -n $nanals ${EXEC} $nanals restart 'F'
    if [[ $? -ne 0 ]]
    then
	echo "calc_ensmean failed - Stopping"
	exit 1
    fi

    if [[ ! -r ${INDIR}/ensmean/RESTART ]]
    then
	mkdir -p ${INDIR}/ensmean/RESTART
    fi

    /bin/mv restart.ensmean ${INDIR}/ensmean/RESTART/${adatep}.${hourp}0000.fv_tracer.res.tile${itile}.nc
    if [[ $itile == 1 ]] then
#	/bin/cp ${INDIR}/mem001/RESTART/${adatep}.${hourp}0000.fv_[c,s]*.res.*.nc ${INDIR}/mem001/RESTART/${adatep}.${hourp}0000.[c,s]*.nc ${INDIR}/mem001/RESTART/${adatep}.${hourp}0000.coupler.res ${INDIR}/ensmean/RESTART/
	/bin/cp ${INDIR}/mem001/RESTART/${adatep}.${hourp}0000.coupler.res ${INDIR}/ensmean/RESTART
    fi
    ((itile=itile+1))
done

echo "Only to run restart to history conversion for ensmean"

((ANALINC2=2*ANALINC))

analdatem2=`$ndate -$ANALINC2 $analdate`

INDIRm2=${FV3_RUNS}/${sim}/${analdatem2}

if [[ -r ${INDIRm2}/OUTPUT_ENKF/increment_${analdatem2}_ensmean.nc && -r ${INDIRm2}/RESTART ]]
then
    rm -rf ${INDIRm2}/mem???
    echo "rm -rf ${INDIRm2}/mem???"
fi

exit 0
