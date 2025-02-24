#!/bin/ksh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl

datestring=20180415.000000
indir=./indata_incr

ntiles=6

itile=1
while [[ $itile -le $ntiles ]]
do 
    file_in=${indir}/${datestring}.fv_tracer.res.tile${itile}.nc
    file_out=${indir}/${datestring}.getkf.fv_tracer.res.tile${itile}.nc
    ncdiff -O ${file_out} ${file_in} ${indir}/${datestring}.fv_tracer.increment.tile${itile}.nc
    ncap2 -O -S add_species.nco ${indir}/${datestring}.fv_tracer.increment.tile${itile}.nc ${indir}/${datestring}.fv_tracer.increment.total.tile${itile}.nc
    ((itile=itile+1))
done
    
