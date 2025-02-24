#!/bin/ksh

cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/geoval

fnamein="aod_geoval_2018041500_s.nc4"
fnameout="aod_geoval_2018041500_s_new.nc4"

#fnamein="aod_geoval_2018041500_m.nc4"
#fnameout="aod_geoval_2018041500_m_new.nc4"


ncap2 -O -v -s 'latitude=latitude;longitude=longitude;time=time;sfc_height=sfc_height;air_temperature=air_temperature;specific_humidity=specific_humidity;relative_humidity=relative_humidity;humidity_mixing_ratio=humidity_mixing_ratio;air_pressure=air_pressure;air_pressure_levels=air_pressure_levels;sfc_height=sfc_height;mass_fraction_of_dust001_in_air=float(1.e-9*dust1);mass_fraction_of_dust002_in_air=float(1.e-9*dust2);mass_fraction_of_dust003_in_air=float(1.e-9*dust3);mass_fraction_of_dust004_in_air=float(1.e-9*dust4);mass_fraction_of_dust005_in_air=float(1.e-9*dust5);mass_fraction_of_sea_salt001_in_air=float(1.e-9*seas1);mass_fraction_of_sea_salt002_in_air=float(1.e-9*seas2);mass_fraction_of_sea_salt003_in_air=float(1.e-9*seas3);mass_fraction_of_sea_salt004_in_air=float(1.e-9*seas4);mass_fraction_of_sea_salt005_in_air=float(1.e-9*seas5);mass_fraction_of_hydrophobic_black_carbon_in_air=float(1.e-9*bc1);mass_fraction_of_hydrophilic_black_carbon_in_air=float(1.e-9*bc2);mass_fraction_of_hydrophobic_organic_carbon_in_air=float(1.e-9*oc1);mass_fraction_of_hydrophilic_organic_carbon_in_air=float(1.e-9*oc2);mass_fraction_of_sulfate_in_air=float(1.e-9*sulf)' $fnamein $fnameout

ncatted -O -a history,global,d,, $fnameout
