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

#sbatch --export=ALL sbatch_convert_c48_c12.sh

. /etc/profile
. /apps/lmod/lmod/init/sh
. ~/.jedi

ulimit -s unlimited
ulimit -v unlimited

start_date=2020121418
end_date=2020121506

ndate=~/bin/ndate

nanals=5

cycle_frequency=6

export JEDIMAIN=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi

export JEDIYAML=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/yamls
export JEDIBUILD=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/fv3-bundle
export JEDIBIN=${JEDIBUILD}/bin
export JEDICODE=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/code/fv3-bundle/fv3-jedi
export JEDIDATA=${JEDICODE}/test/Data

INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/model

INDIR_C48=${INDIR}/C48
OUTDIR_C12=${INDIR}/C12

TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/workdir_changeres

# Run time debugging options
# --------------------------
export OOPS_TRACE=0
export OOPS_DEBUG=0
export OOPS_LOG=0

rm -rf $TMPDIR
mkdir -p $TMPDIR

cd $TMPDIR

ln -sf ${JEDIDATA}/fv3files .
ln -sf ${JEDIDATA}/fieldmetadata .

ln -sf ${INDIR_C48} inputs
ln -sf ${OUTDIR_C12} outputs

/bin/cp ${JEDIYAML}/gfs_c48_to_c12_template.yaml .

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    datestd=$ident
    datecf=${year}-${month}-${day}T${hour}:00:00Z
    datefv3=${year}${month}${day}.${hour}0000

    if [[ $hour == 18 ]]
    then
	datestd=`$ndate +3 $ident`
    fi
   
    if [[ $hour == 06 ]]
    then
	datestd=`$ndate -3 $ident`
    fi	

    year=`echo "${datestd}" | cut -c1-4`
    month=`echo "${datestd}" | cut -c5-6`
    day=`echo "${datestd}" | cut -c7-8`
    hour=`echo "${datestd}" | cut -c9-10`

    datefv3fix3=${year}${month}${day}.${hour}0000

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
	
	if [[ ! -r ${OUTDIR_C12}/${charnanal} ]]
	then
	    mkdir -p ${OUTDIR_C12}/${charnanal}
	fi
	
	sed -e "s/member/${charnanal}/g"  -e "s/datefv3fix3/${datefv3fix3}/g" -e "s/datefv3/${datefv3}/g" -e "s/datecf/${datecf}/g" gfs_c48_to_c12_template.yaml > gfs_c48_to_c12.yaml
	
	export jobtype=gfs_c48_to_c12
	
	srun -n $SLURM_NTASKS  $JEDIBIN/fv3jedi_convertstate.x ${jobtype}.yaml

	((nanal=nanal+1))
	
    done
    
    echo $ident

    ident=`$ndate +${cycle_frequency} $ident`
    
done



