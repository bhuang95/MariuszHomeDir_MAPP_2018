#!/bin/ksh

. /etc/profile

. ../.environ.ksh

start_date=2016082000

cycle_frequency=24

grid=C192

maindir=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/GBBEPx
indir=${maindir}/${grid}_nofrp

ndate=~/bin/ndate

ident=$start_date

year=`echo "${ident}" | cut -c1-4`
month=`echo "${ident}" | cut -c5-6`
day=`echo "${ident}" | cut -c7-8`
hour=`echo "${ident}" | cut -c9-10`

yymmdd=${year}${month}${day}

identm=`$ndate -${cycle_frequency} $ident`

year=`echo "${identm}" | cut -c1-4`
month=`echo "${identm}" | cut -c5-6`
day=`echo "${identm}" | cut -c7-8`
hour=`echo "${identm}" | cut -c9-10`

yymmddm=${year}${month}${day}

echo $yymmdd $yymmddm

cd ${indir}/${yymmdd}

for file in ../${yymmddm}/*.bin
do
    newname=`echo $file | sed "s/${yymmddm}/${yymmdd}/g"`
    newfile=`basename $newname`
    echo $file $newfile
    ln -sf $file $newfile
done
