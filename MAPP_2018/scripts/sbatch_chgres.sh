#!/bin/ksh
#SBATCH -J chgres
##SBATCH -q batch
#SBATCH -q debug
#SBATCH -p bigmem
#SBATCH -A gsd-fv3-dev
##SBATCH -A chem-var
##SBATCH -A wrf-chem
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -n 1
#SBATCH -t 0:30:00
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

##sbatch --export=ALL sbatch_chgres.sh

WORKFLOWDIR="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/workflows/global-workflow"

export machine=HERA
export machine_lc=hera
export CASE=C96

. /etc/profile

. ${WORKFLOWDIR}/parm/config/config.resources ecen

. ${WORKFLOWDIR}/env/${machine}.env ecen

. ${WORKFLOWDIR}/modulefiles/module_base.${machine_lc}

#ulimit  -s unlimited

METDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/MET_ANALYSES

start_date=2019072000
end_date=2019073018

cycle_frequency=6

nanals=20
nanals=0

ndate=~/bin/ndate

lon_s=${LONB:-$((`echo $CASE | cut -c 2-`*4))}
lat_s=${LATB:-$((`echo $CASE | cut -c 2-`*2))}

export OMP_NUM_THREADS=1

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    nanal=0    
    
    while [[ $nanal -le $nanals ]]
    do

	if [[ $nanal -eq 0 ]]
	then
	    charnanal=control
	    metdir=${METDIR}/ncdf/${charnanal}_${CASE}/${ident}
	    metfile=${METDIR}/ncdf/${charnanal}/${ident}/gdas.t${hour}z.atmanl.nc
	else
	    charnanal=mem`printf %03i $nanal`
	    metdir=${METDIR}/ncdf/ensemble/${ident}/${charnanal}
	    metfile=${METDIR}/ncdf/ensemble/${charnanal}/${ident}/gdas.t${hour}z.atmanl.nc
	fi

	echo "ident = $ident" $charnanal

	if [[ ! -r $metdir ]]
	then
	    mkdir -p $metdir
	fi

	cd $metdir 

	ln -sf ${metfile} gdas.t${hour}z.orig.atmanl.nc

	ln -sf ${METDIR}/ncdf/atmanl_terrain_${CASE} .


cat > enkf_chgres_recenter_nc.nl <<EOF
&chgres_setup
 i_output=$lon_s
 j_output=$lat_s
 input_file="gdas.t${hour}z.orig.atmanl.nc"
 output_file="gdas.t${hour}z.atmanl.nc"
 terrain_file="./atmanl_terrain_${CASE}"
 ref_file="./atmanl_terrain_${CASE}"
/
EOF


        srun -n 1 ${WORKFLOWDIR}/exec/enkf_chgres_recenter_nc.x enkf_chgres_recenter_nc.nl

#	${WORKFLOWDIR}/exec/enkf_chgres_recenter_nc.x enkf_chgres_recenter_nc.nl


        /bin/rm -f enkf_chgres_recenter_nc.nl gdas.t${hour}z.orig.atmanl.nc atmanl_terrain_C96

	((nanal=nanal+1))    

    done

    ident=`$ndate +${cycle_frequency} $ident`

done
