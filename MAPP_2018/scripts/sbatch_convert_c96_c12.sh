#!/bin/ksh
#
# -- Request cores
#SBATCH -n 6
#
# -- Specify a maximum wallclock of 4 hours
#SBATCH -t 00:10:00
#
# -- Specify under which account a job should run
#SBATCH -q debug
#
# -- Set the name of the job, or moab will default to STDIN
#SBATCH -A chem-var

#SBATCH -J convert_state
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_convert_c96_c12.sh

. /etc/profile
. /apps/lmod/lmod/init/sh
. ~/.jedi

ulimit -s unlimited
ulimit -v unlimited

start_date=2018041406
end_date=2018041606

ndate=~/bin/ndate

nanals=8

cycle_frequency=6

export JEDIYAML=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/yamls
export JEDIBUILD=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/fv3-bundle
export JEDIBIN=${JEDIBUILD}/bin
export JEDICODE=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/code/fv3-bundle/fv3-jedi
export JEDIDATA=${JEDICODE}/test/Data

MAINDIR=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/envar_2018

INDIR_C96=${MAINDIR}/C96/RESTART
OUTDIR_C12=${MAINDIR}/C12/RESTART

/bin/cp ${INDIR_C96}/gfs_aero_c96/akbk.nc ${OUTDIR_C12}/gfs_aero_c12


TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_changeres

# Run time debugging options
# --------------------------
export OOPS_TRACE=0
export OOPS_DEBUG=0
export OOPS_LOG=0

rm -rf $TMPDIR
mkdir -p $TMPDIR

cd $TMPDIR

ln -sf ${JEDIDATA}/fieldsets .
ln -sf ${JEDIDATA}/fv3files .

ln -sf ${INDIR_C96} inputs
ln -sf ${OUTDIR_C12} outputs

/bin/cp ${JEDIYAML}/gfs_c96_to_c12_template.yaml .

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    analdate=${year}${month}${day}.${hr}0000

    nanal=0
    
    while [[ $nanal -le $nanals ]]
    do
	if [[ $nanal -eq 0 ]]
	then
	    charnanal="bkg"
	else
	    charnanal=mem`printf %03i $nanal`
	fi

	echo $charnanal
	
	if [[ ! -r ${OUTDIR_C12}/gfs_aero_c12/${charnanal} ]]
	then
	    mkdir -p ${OUTDIR_C12}/gfs_aero_c12/${charnanal}
	fi
	
	sed -e "s/member/${charnanal}/g" -e "s/analdate/${analdate}/g" gfs_c96_to_c12_template.yaml > gfs_c96_to_c12.yaml
	
	export jobtype=gfs_c96_to_c12
	
	srun -n $SLURM_NTASKS  $JEDIBIN/fv3jedi_convertstate.x ${jobtype}.yaml
	
	((nanal=nanal+1))
	
    done
    
    echo $ident

    ident=`$ndate +${cycle_frequency} $ident`
    
done



