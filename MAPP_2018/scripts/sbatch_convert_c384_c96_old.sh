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


start_date=2019073000
end_date=2019080918

start_date=2019072800
end_date=2019073000



ndate=~/bin/ndate

cycle_frequency=6

export JEDI=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi
export JEDIYAML=${JEDI}/yamls
export JEDIBUILD=${JEDI}/build/fv3-bundle

export JEDIBIN=${JEDIBUILD}/bin
export JEDICODE=${JEDI}/code/fv3-bundle/fv3-jedi
export JEDIDATA=${JEDICODE}/test/Data
export JEDIFIELDS_CO=${JEDI}/fieldsets

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/FV3_CHEM_2019/comrot/jpss_report

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/FV3_CHEM_2019/comrot/pagowski_test_gsdchem_c384_6hr


TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_changeres

# Run time debugging options
# --------------------------
export OOPS_TRACE=0
export OOPS_DEBUG=0
export OOPS_LOG=0

rm -rf $TMPDIR
mkdir -p $TMPDIR

cd $TMPDIR

ln -sf ${JEDIFIELDS_CO} .
ln -sf ${JEDIDATA}/fv3files .


/bin/cp ${JEDIYAML}/gfs_c384_to_c96_template.yaml .

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`

    INDIR_C384=${MAINDIR}/gfs.${year}${month}${day}/${hr}/RESTART
    OUTDIR_C96=${MAINDIR}/C96/gfs.${year}${month}${day}/${hr}/RESTART

    rm -rf inputs outputs

    ln -sf ${INDIR_C384} inputs
    ln -sf ${OUTDIR_C96} outputs

    
    if [[ ! -r ${OUTDIR_C96} ]]
    then
	mkdir -p ${OUTDIR_C96}
    fi

    ident=`$ndate +${cycle_frequency} $ident`

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hr=`echo "${ident}" | cut -c9-10`
    analdate=${year}${month}${day}.${hr}0000
    
    sed -e "s/analdate/${analdate}/g" gfs_c384_to_c96_template.yaml > gfs_c384_to_c96.yaml
	
    export jobtype=gfs_c384_to_c96

    srun -n $SLURM_NTASKS  $JEDIBIN/fv3jedi_convertstate.x ${jobtype}.yaml

    /bin/cp ${INDIR_C384}/${analdate}.fv_core.res.nc ${OUTDIR_C96}

    echo $ident

done



