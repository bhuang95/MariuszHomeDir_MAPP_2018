#!/bin/ksh
#SBATCH --ntasks=40
#SBATCH --cpus-per-task=1
#SBATCH --exclusive
#SBATCH -t 00:30:00
#SBATCH -q debug
#SBATCH -A ap-fc
#SBATCH -J unzip_viirs
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#assume 10 mins to unzip one day -> can perform three cycles
#in a single submission 3 cycles i.e. 3*40=120 =~4 months
#use debug queue to fit 30 mins

set -x

ndays=$SLURM_NTASKS
np=$SLURM_CPUS_PER_TASK
ncycles=3

start_date=2019010200
((nhours=ndays*24))
((nhoursm=nhours-24))

indir=/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/DATA/OBS/VIIRS/AOT
indir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/OBS/VIIRS/AOT

ndate=~/bin/ndate
sd1=$start_date
sd2=`$ndate $nhours $sd1`
sd3=`$ndate $nhours $sd2`

cycle_frequency=24

ident=$start_date

for sd in {$sd1,$sd2,$sd3}
do
    ed=`$ndate $nhoursm $sd`
    echo "new cycle: $sd"

    ident=$sd

    while [[ $ident -le $ed ]]
    do
	
	id=`echo "${ident}" | cut -c1-8`
	
	if [[ -r ${indir}/${id} ]]
	then
	    cd ${indir}/${id}
	    srun -n ${np} unzip ${ident}00.zip '*_npp_s*.nc' &
	#    /bin/rm ${ident}00.zip
	    echo "within cycle $sd: $id"
	    idlast=$id
	fi

	ident=`$ndate $cycle_frequency $ident`
	
    done

    wait
    
done

echo "final date processed $idlast"

exit
