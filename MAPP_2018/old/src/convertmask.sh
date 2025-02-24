#!/bin/ksh

datadir=/scratch3/BMC/chem-var/pagowski/dust_smoke/dust_smoke_mask/outdata_domain/fireseason

ident_start=2016062800
ident_end=2016073000

ident=$ident_start

ndate="~/bin/ndate"

while [[ $ident -le $ident_end ]]
do
    indate=`echo $ident | cut -c1-8`
    infile=${datadir}/smoke_mask_domain_${indate}.bin
    outfile=${datadir}/smoke_mask_domain_r_kind_${indate}.bin
    convertmask.x $infile $outfile
    infile=${datadir}/dust_mask_domain_${indate}.bin
    outfile=${datadir}/dust_mask_domain_r_kind_${indate}.bin
    convertmask.x $infile $outfile
    ident=`ndate +24 $ident`
    echo $ident
done    



