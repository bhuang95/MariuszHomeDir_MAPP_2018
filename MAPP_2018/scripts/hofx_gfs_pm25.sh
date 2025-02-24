#!/bin/ksh

. /etc/profile

start_date=2016050100
end_date=2016123118

sim=anal
#sim=noda

cycle_frequency=6
interval=1

ndate=~/bin/ndate
maindir=/work/noaa/gsd-fv3-dev/pagowski
jedidir=/work/noaa/gsd-fv3-dev/pagowski/jedi
jediddir=${jedidir}/code/fv3-bundle/fv3-jedi/test/Data
jedibdir=${jedidir}/build/fv3-bundle/bin

scriptdir=~/mapp_2018/scripts

workdir=${maindir}/tmpdir/workdir_pm25_hofx

obsdir=${maindir}/DATA/OBS/openaq/pm25
modeldir=${maindir}/DATA/MODEL/fv3/akbk_${sim}
modelpm25dir=${maindir}/DATA/MODEL/fv3/pm25_${sim}

outdir=${maindir}/pm25_hofx/${sim}_pm25

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

ndate=~/bin/ndate

((cycle_frequency_half=cycle_frequency/2))

if [[ ! -r $workdir ]]
then
    mkdir -p ${workdir}
fi

mkdir -p ${workdir}/Data/fieldmetadata

/bin/cp ufs-pm25.yaml $workdir/Data/fieldmetadata
/bin/cp hofx_gfs_pm25_template.yaml $workdir

cd $workdir

ln -sf ${jediddir}/fv3files ./Data

. ~/.jedi

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    identm=`$ndate -$interval $ident`
    yearm=`echo "${identm}" | cut -c1-4`
    monthm=`echo "${identm}" | cut -c5-6`
    daym=`echo "${identm}" | cut -c7-8`
    hourm=`echo "${identm}" | cut -c9-10`

    datestd=$ident
    datecf=${year}-${month}-${day}T${hour}:00:00Z
    datefv3=${year}${month}${day}.${hour}0000

    datestdm=$identm
    datemcf=${yearm}-${monthm}-${daym}T${hourm}:00:00Z
    datemfv3=${yearm}${monthm}${daym}.${hourm}0000

    sed -e "s/datestd/${datestd}/g" -e "s/datecf/${datecf}/g" -e "s/datefv3/${datefv3}/g" -e "s/datemstd/${datemstd}/g" -e "s/datemcf/${datemcf}/g" -e "s/datemfv3/${datemfv3}/g" hofx_gfs_pm25_template.yaml > hofx_gfs_pm25.yaml

    obsfile=${obsdir}/openaq_pm25.${ident}.nc4

    mkdir -p Data/obs Data/inputs Data/hofx

    /bin/rm -f Data/obs/*.nc  Data/inputs/*.nc Data/inputs/*.nc.ges  Data/hofx/*nc

    ln -sf $obsfile  Data/obs

    ln -sf ${modeldir}/${datefv3}.coupler.res.ges Data/inputs/${datefv3}.coupler.res

    itile=1
    while [[ $itile -le 6 ]]
    do
        ln -sf ${modelpm25dir}/${datefv3}.fv_pm25.res.tile${itile}.nc Da\
ta/inputs/${datefv3}.fv_pm25.res.tile${itile}.nc
        ((itile=itile+1))
    done

    srun -n 6 ${jedibdir}/fv3jedi_hofx_nomodel.x hofx_gfs_pm25.yaml hofx_gfs_pm25.run

    /bin/mv Data/hofx/openaq_pm25_hofx.${ident}.nc4 ${outdir}

    echo "finished $ident"

    ident=`$ndate +${cycle_frequency} $ident`

done
