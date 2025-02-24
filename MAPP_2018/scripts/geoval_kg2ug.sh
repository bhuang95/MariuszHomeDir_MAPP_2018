#!/bin/ksh

#to add  benchmark for luts

. /etc/profile

. ~/mapp_2018/.environ.ksh

infile=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/geoval/aod_geoval_2018041500_s.nc4

outfile=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/geoval/aod_geoval_2018041500_s_cf_names.nc4

ncrename -O -v dust1,mass_fraction_of_dust001_in_air -v dust2,mass_fraction_of_dust002_in_air -v dust3,mass_fraction_of_dust003_in_air -v dust4,mass_fraction_of_dust004_in_air -v dust5,mass_fraction_of_dust005_in_air -v seas1,mass_fraction_of_sea_salt001_in_air -v seas2,mass_fraction_of_sea_salt002_in_air -v seas3,mass_fraction_of_sea_salt003_in_air -v seas4,mass_fraction_of_sea_salt004_in_air -v seas5,mass_fraction_of_sea_salt005_in_air -v sulf,mass_fraction_of_sulfate_in_air -v bc1,mass_fraction_of_hydrophobic_black_carbon_in_air -v bc2,mass_fraction_of_hydrophilic_black_carbon_in_air -v oc1,mass_fraction_of_hydrophobic_organic_carbon_in_air -v oc2,mass_fraction_of_hydrophilic_organic_carbon_in_air $infile $outfile

ncatted -O -a history,global,d,, $outfile
