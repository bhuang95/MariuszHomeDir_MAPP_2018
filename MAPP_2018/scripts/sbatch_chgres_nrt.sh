#!/bin/ksh
#SBATCH -J chgres
#SBATCH -q batch
#SBATCH -p bigmem
##SBATCH -q debug
#SBATCH -A gsd-fv3-dev
##SBATCH -A chem-var
##SBATCH -A wrf-chem
#SBATCH -D /home/Mariusz.Pagowski/mapp_2018/scripts
#SBATCH -n 1
#SBATCH -t 1:30:00
#SBATCH -o /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.o%j
#SBATCH -e /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/qslogs/%x.e%j

#sbatch --export=ALL,start=2019072000,end=2019072018 sbatch_chgres_nrt.sh

WORKFLOWDIR="/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/workflows/global-workflow"

export machine=HERA
export machine_lc=hera
export CASE=C96

. /etc/profile

. ${WORKFLOWDIR}/parm/config/config.resources ecen

. ${WORKFLOWDIR}/env/${machine}.env ecen

. ${WORKFLOWDIR}/modulefiles/module_base.${machine_lc}

ulimit  -s unlimited

MAINDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata

cycle_frequency=6

nanals=20

ndate=~/bin/ndate

lon_s=${LONB:-$((`echo $CASE | cut -c 2-`*4))}
lat_s=${LATB:-$((`echo $CASE | cut -c 2-`*2))}

INDIR=${MAINDIR}/gdasAna/${CASE}

export OMP_NUM_THREADS=1

start_date=$start
end_date=$end

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    datestamp=${year}${month}${day}/${hour}

    indir=${INDIR}/gdas.${datestamp}
    infile=${indir}/gdas.t${hour}z.atmanl_orig.nc

    outfile=${indir}/gdas.t${hour}z.atmanl.nc

    cd ${indir}

    ln -sf ${INDIR}/atmanl_terrain_${CASE} .


cat > enkf_chgres_recenter_nc.nl <<EOF
&chgres_setup
 i_output=$lon_s
 j_output=$lat_s
 input_file="$infile"
 output_file="$outfile"
 terrain_file="./atmanl_terrain_${CASE}"
 ref_file="./atmanl_terrain_${CASE}"
/
EOF

    srun -n 1 ${WORKFLOWDIR}/exec/enkf_chgres_recenter_nc.x enkf_chgres_recenter_nc.nl

    /bin/rm -f enkf_chgres_recenter_nc.nl atmanl_terrain_${CASE}

    nanal=1

    while [[ $nanal -le $nanals ]]
    do
    
	charnanal=mem`printf %03i $nanal`
        indir=${INDIR}/enkfgdas.${datestamp}/${charnanal}
        infile=${indir}/gdas.t${hour}z.ratmanl_orig.nc
        outfile=${indir}/gdas.t${hour}z.ratmanl.nc

	cd ${indir}

	ln -sf ${INDIR}/atmanl_terrain_${CASE} .

cat > enkf_chgres_recenter_nc.nl <<EOF
&chgres_setup
 i_output=$lon_s
 j_output=$lat_s
 input_file="$infile"
 output_file="$outfile"
 terrain_file="./atmanl_terrain_${CASE}"
 ref_file="./atmanl_terrain_${CASE}"
/
EOF

	srun -n 1 ${WORKFLOWDIR}/exec/enkf_chgres_recenter_nc.x enkf_chgres_recenter_nc.nl

	/bin/rm -f enkf_chgres_recenter_nc.nl atmanl_terrain_${CASE}

	((nanal=nanal+1))    
    
    done

    ident=`$ndate +${cycle_frequency} $ident`

done
