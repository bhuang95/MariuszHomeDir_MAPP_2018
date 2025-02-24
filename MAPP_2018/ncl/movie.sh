#!/bin/sh

NCLDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/ncl
cd $NCLDIR


cd pics_4movie
convert -set delay 60 -loop 5 seas_dust_*png  seas_dust.gif
