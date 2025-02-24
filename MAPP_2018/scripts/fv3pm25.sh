#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2016050100
end_date=2016123118

cycle_frequency=6
ntiles=6
grid=C96
sim=noda
#sim=anal

if [[ $sim == "anal" ]]
then
    suffix=''
else
    suffix='.ges'
fi

maindir=/work/noaa/gsd-fv3-dev/pagowski
execdir=${maindir}/exec

workdir=${maindir}/tmpdir/workdir_fv3pm25
fv3dir=${maindir}/DATA/MODEL/fv3
indir=${fv3dir}/akbk_${sim}
outdir=${fv3dir}/pm25_${sim}

ndate=~/bin/ndate

if [[ ! -r $workdir ]]
then
    mkdir -p $workdir
fi

if [[ ! -r $outdir ]]
then
    mkdir -p $outdir
fi

/bin/cp ${execdir}/fv3pm25.x $workdir

cd $workdir

ndate=~/bin/ndate

ident=$start_date

while [[ $ident -le $end_date ]]
do

    year=`echo "${ident}" | cut -c1-4`
    month=`echo "${ident}" | cut -c5-6`
    day=`echo "${ident}" | cut -c7-8`
    hour=`echo "${ident}" | cut -c9-10`

    itile=1
    while [[ $itile -le $ntiles ]]
    do
	outfile="${year}${month}${day}.${hour}0000.fv_pm25.res.tile${itile}.nc"
	echo $outfile

cat > fv3pm25.nl <<EOF
&record_input
 date="${year}${month}${day}${hour}"
 input_fv3_dir="${indir}"
 fname_fv3_core="${year}${month}${day}.${hour}0000.fv_core.res.tile${itile}.nc.ges"
 fname_fv3_tracer="${year}${month}${day}.${hour}0000.fv_tracer.res.tile${itile}.nc${suffix}"
 fname_fv3_sfc="${year}${month}${day}.${hour}0000.sfc_data.tile${itile}.nc.ges"
 fname_akbk="${year}${month}${day}.${hour}0000.fv_core.res.nc.ges"
/
&record_vars
 varlist_in="bc1","bc2","dust1","dust2","oc1","oc2","seas1","seas2","seas3","sulf"
 coeffs=     1.,   1.,   1.,     0.38,   1.,   1.,   1.,     1.,     0.83,   1.
 varlist_out="pm25"
/

&record_output
 output_dir="${outdir}"
 fname_pm25="${outfile}"
/
EOF

    if [[ -r ${outdir}/${outfile} ]]
    then
        /bin/rm ${outdir}/${outfile}
    fi

    ./fv3pm25.x

    ((itile=itile+1))

    done

    ident=`$ndate +${cycle_frequency} $ident`

done
