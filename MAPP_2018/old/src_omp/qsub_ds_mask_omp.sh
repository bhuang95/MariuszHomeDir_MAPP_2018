#!/bin/ksh --login
#PBS -N ds_mask
#PBS -A chem-var
#PBS -l procs=24
#PBS -l walltime=00:30:00
##PBS -l walltime=08:00:00
#PBS -q debug
##PBS -q urgent
#PBS -d /home/Mariusz.Pagowski/codes/src_omp
#PBS -o /scratch3/BMC/chem-var/pagowski/dust_smoke/qslogs
#PBS -e /scratch3/BMC/chem-var/pagowski/dust_smoke/qslogs

smoke=1
dust=0

#starting with hrrr_smoke binary files  are r_kind and big endian;  before that regular real

#arizona
inwrffile=/scratch3/BMC/chem-var/pagowski/crtm_work/save/2016_04_12_00/wrfinput_d01

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/arizona
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/dust_mask/outdata_domain/arizona

startdate=2014050500
enddate=2014051500

#africa
inwrffile=/scratch3/BMC/chem-var/pagowski/dust_smoke/africa_runs/wrf_run_all/test_000/2014_03_29_00/wrfinput_d01

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/africa
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/dust_mask/outdata_domain/africa

startdate=2014032800
enddate=2014040400


#crtm
inwrffile=/scratch3/BMC/chem-var/pagowski/crtm_work/save/2016_04_12_00/wrfinput_d01

INDIR=/scratch3/BMC/chem-var/pagowski/crtm_work/viirs/outdata
OUTDIR=/scratch3/BMC/chem-var/pagowski/crtm_work/viirs/outdata

startdate=2016041300
enddate=2016041300

#fireseason
inwrffile=/scratch3/BMC/chem-var/pagowski/crtm_work/save/2016_04_12_00/wrfinput_d01

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/fireseason
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/smoke_mask/outdata_domain/fireseason

startdate=2016062800
enddate=2016073000

#fireseason_hrrr_smoke

inwrffile=/scratch3/BMC/wrf-chem/pagowski/hrrr_smoke/wrfout/wrfout_d01_2016-10-12_18_00_00_slim

INDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/fireseason
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/smoke_mask/outdata_domain/fireseason_hrrr_smoke

inwrffile=/scratch3/BMC/chem-var/pagowski/crtm_work/save/2016_04_12_00/wrfinput_d01
OUTDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/smoke_mask/outdata_domain/fireseason

startdate=2016082600
enddate=2016090500


ndate="~/bin/ndate"

nprocs=$PBS_NP

export OMP_NUM_THREADS=$nprocs

echo $OMP_NUM_THREADS

ident=$startdate

while [[ $ident -le $enddate ]]
do
    year=`echo $ident | cut -c1-4`
    month=`echo $ident | cut -c5-6`
    day=`echo $ident | cut -c7-8`
    hour=`echo $ident | cut -c9-10`
    
    if [[ $dust == 1 ]] 
    then
	inmaskfile=${INDIR}/dust_mask_${year}${month}${day}.txt
	outmaskfile=${OUTDIR}/dust_mask_domain_${year}${month}${day}.bin
    else
	inmaskfile=${INDIR}/smoke_mask_${year}${month}${day}.txt
	outmaskfile=${OUTDIR}/smoke_mask_domain_${year}${month}${day}.bin
    fi

    echo  $inmaskfile $inwrffile $outmaskfile

    time ./ds_mask_omp.x $inmaskfile $inwrffile $outmaskfile 

    exit

    ident=`ndate +24 ${year}${month}${day}${hour}`

done




