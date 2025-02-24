#!/bin/ksh

#combine solar and lunar VIIRS

cd /scratch1/BMC/wrf-chem/pagowski/MAPP_2018/DATA/OBS/VIIRS_lunar/thinned_debiased_C192/202009/v1_lunar

s_dir='/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/DATA/OBS/VIIRS/thinned_debiased_C192/202009/v1_solar'

s_l_dir='/scratch1/BMC/wrf-chem/pagowski/MAPP_2018/DATA/OBS/VIIRS/thinned_debiased_C192/202009/v1_solar+lunar'

. ~/.nc

for infile in viirs_aod_lunar_snpp.*_v1.nc
do
    echo $infile
    yyyymmddhh=`echo $infile | cut -c22-31`
    ncks -O --mk_rec_dmn nlocs $infile tmp_l.nc
    ncks -O --mk_rec_dmn nlocs ${s_dir}/viirs_aod_snpp.${yyyymmddhh}_v1.nc tmp_s.nc
    ncrcat -O tmp_s.nc tmp_l.nc tmp_sl.nc
    ncks -O --fix_rec_dmn all tmp_sl.nc ${s_l_dir}/viirs_aod_snpp.${yyyymmddhh}_v1.nc

done

/bin/rm tmp*.nc
