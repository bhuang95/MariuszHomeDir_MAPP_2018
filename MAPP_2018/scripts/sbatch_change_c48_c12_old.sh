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

#SBATCH -J change_res
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL sbatch_change_c48_c12.sh

. /etc/profile
. /apps/lmod/lmod/init/sh
. ~/.jedi

ulimit -s unlimited
ulimit -v unlimited


member=mem008 #bkg

export JEDIYAML=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/yamls
export JEDIBUILD=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/fv3-bundle
export JEDIBIN=${JEDIBUILD}/bin
export INDIR=${JEDIBUILD}/fv3-jedi/test

INDIR_C48=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/fv3-jedi/inputs/gfs_aero_c48

ln -sf ${INDIR_C48} ${INDIR}/Data/inputs

TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_changeres

# Run time debugging options
# --------------------------
export OOPS_TRACE=0
export OOPS_DEBUG=0
export OOPS_LOG=0

if [[ ! -r $TMPDIR ]]
then
    mkdir -p $TMPDIR
fi

cd $TMPDIR

ln -sf ${INDIR}/Data .

mkdir -p outputs/gfs_aero_c12/${member}

export jobtype=gfs_c48_to_c12

srun -n $SLURM_NTASKS  $JEDIBIN/fv3jedi_convertstate.x ${JEDIYAML}/${jobtype}.yaml



