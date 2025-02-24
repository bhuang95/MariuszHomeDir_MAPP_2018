#!/bin/ksh

. /etc/profile

. ~/.nc

TMPDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/stochastic_tests/INPUT

OUTDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/stochastic_tests/OUTPUT

infile_prefix=FENGSHA_2022_NESDIS_inputs_10km_v3.2

year=2018

cd $TMPDIR

/bin/cp ${infile_prefix}_orig.nc ${infile_prefix}.nc 

ncatted -O -a _FillValue,albedo_drag,o,f,-1.e-18  -a _FillValue,clayfrac,o,f,-1.e-18 -a _FillValue,sandfrac,o,f,-1.e-18 -a _FillValue,sep,o,f,-1.e-18 -a _FillValue,uthres,o,f,-1.e-18 -a _FillValue,uthres_sg,o,f,-1.e-18 ${infile_prefix}.nc

ncatted -O -a _FillValue,albedo_drag,d,, -a _FillValue,clayfrac,d,, -a _FillValue,sandfrac,d,, -a _FillValue,sep,d,, -a _FillValue,uthres,d,, -a _FillValue,uthres_sg,d,, ${infile_prefix}.nc

imonth=1

while [[ $imonth -le 12 ]]
do

    if [[ ! -r ${OUTDIR} ]] 
    then
	mkdir -p ${OUTDIR}
    fi

    ((i=imonth-1))

    cmonth=`printf %02i $imonth`

    ncks -O -d time,$i,$i ${infile_prefix}.nc ${infile_prefix}.${year}${cmonth}01.nc

    echo $imonth

    ((imonth=imonth+1))

done
