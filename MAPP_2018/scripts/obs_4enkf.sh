#!/bin/ksh

#to add  total seas and dust to CAMS species

. /etc/profile

. ../.environ.ksh

start_date=2018041418
end_date=2018041418

nanals=10

cycle_frequency=6

ident=$start_date

INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/data/fv3-jedi/results_c48/${ident}

nanal=1

while [[ $nanal -le $nanals ]]
do

    echo $nanal

    charnanal=mem`printf %03i $nanal`
    indir=${INDIR}/${charnanal}

    cd $indir

    /bin/cp aod_viirs_hofx.nc4 aod_viirs_hofx_4envar.nc4

    ncrename -O  -v aerosol_optical_depth_4@KnownObsBias,aerosol_optical_depth_4_KnownObsBias aod_viirs_hofx_4envar.nc4

    ncap2 -O -s "aerosol_optical_depth_4_KnownObsBias=float(aerosol_optical_depth_4_KnownObsBias-aerosol_optical_depth_4_KnownObsBias)" aod_viirs_hofx_4envar.nc4 tmp.nc

    ncrename -O -v aerosol_optical_depth_4_KnownObsBias,aerosol_optical_depth_4@KnownObsBias tmp.nc 

    /bin/mv tmp.nc aod_viirs_hofx_4envar.nc4

    ((nanal=nanal+1))

done


