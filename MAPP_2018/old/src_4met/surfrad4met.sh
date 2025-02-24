#!/bin/ksh

indir=/scratch3/BMC/chem-var/pagowski/gmac_2018/surfrad
year=2015

set -A stations 'bon' 'dra' 'fpk' 'gwn' 'psu' 'sxf' 'tbl'

ndate=~/bin/ndate

outfile=${indir}/aod550_4met.txt

rm -f $outfile

start_ident=2015080100
end_ident=2015083100

for station in ${stations[*]}
do

    ident=$start_ident

    echo $station

    infileconst=${indir}/${station}.txt 

    while [[ ${ident} -le ${end_ident} ]]
    do
	year=`echo $ident | cut -c1-4`
	month=`echo $ident | cut -c5-6`
	day=`echo $ident | cut -c7-8`
	hour=`echo $ident | cut -c9-10`

	infile=${indir}/${station}/${year}/${station}_${year}${month}${day}.aod

	surfrad4met.x ${infileconst} ${year}${month}${day} ${infile} ${outfile}
	ident=`$ndate +24 $ident`
    done
done
