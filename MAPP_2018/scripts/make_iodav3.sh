#!/bin/ksh

. /etc/profile

. ~/.nc

JEDIBIN=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/fv3-bundle/bin

JEDICODE=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/code/fv3-bundle

upgradev1v2=ioda-upgrade-v1-to-v2.x
upgradev2v3=ioda-upgrade-v2-to-v3.x

cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/obs/2020121500

infile=aod_viirs_obs_2018041500_sf42.nc4
outfile=aod_viirs_obs_2020121500_sf42.nc4

infile=aod_viirs_obs_2018041500_sf6.nc4
outfile=aod_viirs_obs_2020121500_sf6.nc4

#for sf42
#ncks -O -x -v modis_deep_blue_flag@MetaData,polarization@VarMetaData,aerosol_optical_depth_4@ObsBias $infile tmp1.nc

#for sf6
ncks -O -x -v modis_deep_blue_flag@MetaData,polarization@VarMetaData,aerosol_optical_depth_4@KnownObsBias $infile tmp1.nc

ncrename -O -v frequency@VarMetaData,frequency@MetaData -v sensor_channel@VarMetaData,sensor_channel@MetaData -v wavenumber@VarMetaData,wavenumber@MetaData tmp1.nc tmp2.nc

ncatted -O -a date_time,global,m,i,2020121500 tmp2.nc tmp3.nc
ncatted -O -a history,global,d,, tmp3.nc 

. ~/.jedi

${JEDIBIN}/${upgradev1v2} tmp3.nc tmp3_v2.nc

${JEDIBIN}/${upgradev2v3} tmp3_v2.nc $outfile ${JEDICODE}/ioda/share/ioda/yaml/validation/ObsSpace.yaml


