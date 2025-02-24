#!/bin/ksh

cd /scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data

fnamein="aod_geoval_2018041500_m.nc4"
fnameout="aod_geoval_2018041500_m_new.nc4"


ncap2 -O -s 'mass_fraction_of_nitrate001_in_air=float(0.2*mass_fraction_of_sulfate_in_air);mass_fraction_of_nitrate002_in_air=float(0.25*mass_fraction_of_sulfate_in_air);mass_fraction_of_nitrate003_in_air=float(0.015*mass_fraction_of_sulfate_in_air)' $fnamein $fnameout

ncatted -O -a history,global,d,, $fnameout
