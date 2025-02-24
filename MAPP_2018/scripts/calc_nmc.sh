#!/bin/ksh

. /etc/profile

. ~/.nc

start_date=2016052000
end_date=2016063012

cycle_frequency=6

INDIR=/scratch2/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/expRuns/NODA_C96_C96_M20_CCPP2_cntlFcst_6h_12h_201605/dr-data

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski
tmpdir=${maindir}/tmpdir/workdir_nmc
outdir=${maindir}/nmc


if [[ ! -r $tmpdir ]]
then
    mkdir -p $tmpdir
fi

ndate=~/bin/ndate

cd $tmpdir

ident=$start_date

nens=0

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    indir=${INDIR}/gdas.${year}${month}${day}/${hour}/RESTART

    identp6=`$ndate +${cycle_frequency} ${ident}`

    yearp6=`echo "${identp6}" | cut -c1-4`
    monthp6=`echo "${identp6}" | cut -c5-6`
    dayp6=`echo "${identp6}" | cut -c7-8`
    hourp6=`echo "${identp6}" | cut -c9-10`

    indirp6=${INDIR}/gdas.${yearp6}${monthp6}${dayp6}/${hourp6}/RESTART

    identp12=`$ndate +${cycle_frequency} ${identp6}`

    yearp12=`echo "${identp12}" | cut -c1-4`
    monthp12=`echo "${identp12}" | cut -c5-6`
    dayp12=`echo "${identp12}" | cut -c7-8`
    hourp12=`echo "${identp12}" | cut -c9-10`

    itile=1

    while [[ $itile -le 6 ]]
    do

	echo $ident $itile

	fcst_12hr=${indir}/${yearp12}${monthp12}${dayp12}.${hourp12}0000.fv_tracer.res.tile${itile}.nc.ges
	fcst_6hr=${indirp6}/${yearp12}${monthp12}${dayp12}.${hourp12}0000.fv_tracer.res.tile${itile}.nc.ges

	ncdiff -O ${fcst_12hr} ${fcst_6hr} tmp.nc

	ncap2 -O -v -s "bc1=bc1*bc1;bc2=bc2*bc2;oc1=oc1*oc1;oc2=oc2*oc2;dust1=dust1*dust1;dust2=dust2*dust2;dust3=dust3*dust3;dust4=dust4*dust4;dust5=dust5*dust5;seas1=seas1*seas1;seas2=seas2*seas2;seas3=seas3*seas3;seas4=seas4*seas4;seas5=seas5*seas5;sulf=sulf*sulf" tmp.nc nmc_${ident}.tile${itile}.nc	

	((itile=itile+1))

    done

    ident=`$ndate +${cycle_frequency} $ident`

    ((nens=nens+1))

done

nens=167
float rnens rnensm fraction
rnens=$nens

echo $nens
((rnensm=rnens-1))
((fraction=rnens/rnensm))

itile=1

while [[ $itile -le 6 ]]
do
    
    ncea -O nmc_*.tile${itile}.nc tmp.nc 

    ncap2 -O -v -s "bc1=sqrt($fraction*bc1);bc2=sqrt($fraction*bc2);oc1=sqrt($fraction*oc1);oc2=sqrt($fraction*oc2);dust1=sqrt($fraction*dust1);dust2=sqrt($fraction*dust2);dust3=sqrt($fraction*dust3);dust4=sqrt($fraction*dust4);dust5=sqrt($fraction*dust5);seas1=sqrt($fraction*seas1);seas2=sqrt($fraction*seas2);seas3=sqrt($fraction*seas3);seas4=sqrt($fraction*seas4);seas5=sqrt($fraction*seas5);sulf=sqrt($fraction*sulf)" tmp.nc ${outdir}/nmc_${start_date}_${end_date}.tile${itile}.nc

    ((itile=itile+1))

done


