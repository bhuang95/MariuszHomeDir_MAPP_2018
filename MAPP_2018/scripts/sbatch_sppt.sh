#!/bin/ksh
#SBATCH -J sppt
#SBATCH -q batch
#SBATCH -A chem-var
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -t 00:20:00
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

##sbatch --export=ALL sbatch_sppt.sh

set -x

RES=96
NPX=`expr $RES + 1`
NPY=`expr $RES + 1`

STOCHDIR=/home/Mariusz.Pagowski/mapp_2018/src_sppt
EXECDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/exec

TESTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/stochastic_tests

cd ${STOCHDIR}

. ./module-setup.sh
module purge
module use $( pwd -P )
module load modules.hera.intel

cd ${TESTDIR}

if [ ! -r INPUT ]
then
    echo "copy INDIR from Phil's dir"
    exit 1
fi

mkdir -p RESTART

/bin/cp ${STOCHDIR}/input.nml.template input.nml
/bin/cp ${EXECDIR}/standalone_stochy_ocn.x .

#sed -i -e "s/LOX/1/g" input.nml
#sed -i -e "s/LOY/4/g" input.nml
sed -i -e "s/LOX/2/g" input.nml
sed -i -e "s/LOY/2/g" input.nml
sed -i -e "s/NPX/$NPX/g" input.nml
sed -i -e "s/NPY/$NPY/g" input.nml
sed -i -e "s/RES/$RES/g" input.nml
sed -i -e "s/_STOCHINI_/.false./g" input.nml
export OMP_NUM_THREADS=2
module list

time srun --label -n 24 standalone_stochy_ocn.x

if [ ! -r OUTPUT ]
then
    mkdir -p OUTPUT
fi

mv workg* OUTPUT
