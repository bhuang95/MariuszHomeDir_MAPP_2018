#!/bin/ksh

inwrffile=/scratch3/BMC/chem-var/pagowski/crtm_work/save/wrfout_gocart/2016_04_12_00/wrfinput_gsi_d01_2016-04-13_18:00:00

inmaskfile=/scratch3/BMC/chem-var/pagowski/dust_smoke/outdata/fireseason/smoke_mask_20160706.txt
outfile=smoke_mask_domain_20160706.bin

./dust_mask.x $inmaskfile $inwrffile $outfile
