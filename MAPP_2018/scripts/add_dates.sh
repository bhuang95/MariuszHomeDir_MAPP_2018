#!/bin/ksh
cd /scratch3/BMC/fim/MAPP_2018/jedi/code/fv3-bundle/fv3-jedi/test/Data/INPUTS/FV3-GOCART_c96/ENSEMBLE/mem001

add_day=20161004
add_time=000000

for file in *
do
    newfile=${add_day}.${add_time}.${file}
    /bin/mv ${file} ${newfile}
done
