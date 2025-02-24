#!/bin/ksh


INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/obs

HOFX_DIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/hofx_test/Data/obs

cd $INDIR

/bin/cp aod_viirs_obs_2018041500_sf6.nc4 f0.nc4

ncrename -O -v time@MetaData,time_at_MetaData f0.nc4 f00.nc4
ncks -O --mk_rec_dmn nlocs f00.nc4 f000.nc4

#42 in defdim hardcoded - need to figure how to do it

istart=0
iend=3

i=$istart

while [[ $i -lt $iend ]]
do
    ((i=i+1))
    echo $i
    ncap2 -O -s "time_at_MetaData=float(time_at_MetaData - $i + 0.1)" f000.nc4 f-${i}.nc4 
    ncap2 -O -s "time_at_MetaData=float(time_at_MetaData + $i - 0.1)" f000.nc4 f+${i}.nc4 
done

ncrcat -O f-*.nc4 f000.nc4 f+*.nc4 f000_all.nc4

ncks -O --fix_rec_dmn nlocs f000_all.nc4 f00_all.nc4

ncrename -O -v time_at_MetaData,time@MetaData f00_all.nc4 

ncap2 -O -s 'defdim ("nrecs",$nlocs.size)' f00_all.nc4 f000_all.nc4

ncatted -O -a history,global,d,, f000_all.nc4 f0_all.nc4


/bin/mv f0_all.nc4 aod_viirs_obs_2018041500_sf42.nc4

/bin/cp aod_viirs_obs_2018041500_sf42.nc4 ${HOFX_DIR}

rm -f f+*.nc4 f-*.nc4 f0*.nc4
