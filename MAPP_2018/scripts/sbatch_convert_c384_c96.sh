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

#sbatch --export=ALL sbatch_convert_c384_c96.sh

. /etc/profile
. /apps/lmod/lmod/init/sh
. ~/.jedi

ulimit -s unlimited
ulimit -v unlimited


start_date=2021052500
end_date=2021052500

ndate=~/bin/ndate

cycle_frequency=6

export JEDI=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi
export JEDIYAML=${JEDI}/yamls
export JEDIBUILD=${JEDI}/build/fv3-bundle

export JEDIBIN=${JEDIBUILD}/bin
export JEDICODE=${JEDI}/code/fv3-bundle/fv3-jedi
export JEDIDATA=${JEDICODE}/test/Data
export JEDIFIELDS=${JEDI}/fieldsets

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/nrt/RESTART

TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_changeres

# Run time debugging options
# --------------------------
export OOPS_TRACE=0
export OOPS_DEBUG=0
export OOPS_LOG=0

rm -rf $TMPDIR
mkdir -p $TMPDIR

cd $TMPDIR

ln -sf ${JEDIFIELDS} .
ln -sf ${JEDIDATA}/fv3files .


/bin/cp ${JEDIYAML}/convertstate_gfs_aero_c384_c96_template.yaml .

ident=$start_date

while [[ $ident -le $end_date ]]
do

    echo $ident

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    INDIR=${MAINDIR}/inputs
    OUTDIR=${MAINDIR}/outputs

    rm -rf inputs outputs

    ln -sf ${INDIR} inputs
    ln -sf ${OUTDIR} outputs

    
    if [[ ! -r ${OUTDIR} ]]
    then
	mkdir -p ${OUTDIR}
    fi

    analdate=${year}${month}${day}.${hr}0000
    
    sed -e "s/analdate/${analdate}/g" convertstate_gfs_aero_c384_c96_template.yaml > convertstate_gfs_aero_c384_c96.yaml
	
    export jobtype=convertstate_gfs_aero_c384_c96

    srun -n $SLURM_NTASKS  $JEDIBIN/fv3jedi_convertstate.x ${jobtype}.yaml

    ident=`$ndate +${cycle_frequency} $ident`

done



