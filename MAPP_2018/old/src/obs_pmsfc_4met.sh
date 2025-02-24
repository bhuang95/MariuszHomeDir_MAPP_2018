#!/bin/ksh

indir=/scratch3/BMC/chem-var/pagowski/chemdata/outdata/2013
infile=PM2_5_DRY_data_bundle

outdir=/scratch3/BMC/chem-var/pagowski/chemdata/outdata/obs_pmsfc_4met

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

METEXEC=/scratch3/BMC/chem-var/pagowski/met/bin

start_ident=2013061000
end_ident=2013072000

cycle_frequency=6

ident=$start_ident

ndate="~/bin/ndate"

while [[ $ident -le $end_ident ]]
do
    outfile=pmsfc_4met_${ident}.txt
    echo ${indir}/${infile}
    echo ${outdir}/obs_pmsfc_4met_${ident}.txt
    echo $ident
    obs_pmsfc_4met.x ${indir}/${infile} ${outdir}/obs_pmsfc_4met_${ident}.txt $ident
    ${METEXEC}/ascii2nc ${outdir}/obs_pmsfc_4met_${ident}.txt ${outdir}/obs_pmsfc_4met_${ident}.nc
    ident=`ndate +$cycle_frequency $ident`
    echo $ident
done
