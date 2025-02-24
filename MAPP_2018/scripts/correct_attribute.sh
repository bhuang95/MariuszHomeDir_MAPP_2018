#!/bin/ksh
cd /scratch3/BMC/fim/MAPP_2018/jedi/code/fv3-bundle/ioda/test/testinput/atmosphere
ncatted -O -a date_time,global,m,i,2016100400 aod_obs_2016100400_m.nc4 test.nc
