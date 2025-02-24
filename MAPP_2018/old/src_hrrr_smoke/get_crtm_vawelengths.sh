#!/bin/ksh

RUNDIR=/scratch3/BMC/chem-var/pagowski/dust_smoke/fireseason_runs/crtmrun

/bin/cp get_crtm_vawelengths.x $RUNDIR

cd $RUNDIR

./get_crtm_vawelengths.x modis

./get_crtm_vawelengths.x viirs

./get_crtm_vawelengths.x abi
