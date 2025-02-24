#!/bin/ksh

. /etc/profile

. ../.environ.ksh

#viirs
#cd /scratch1/BMC/wrf-chem/pagowski/MAPP_2018/OBS/VIIRS/AOT/20200710
#cd /scratch1/BMC/wrf-chem/pagowski/MAPP_2018/OBS/VIIRS/AOT/viirs_aug_2019

#nucaps
cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/DATA/OBS/NUCAPS/201908

#sfc restart/analysis
cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/WORKDATA/NRTdata/rawdata/gdasAna

for file in sfc_enkf_*.tar 
do
    tar -xvf $file
done 



