#!/bin/ksh
cd /scratch3/BMC/chem-var/pagowski/gmac_2018/aeronet
cut -c 7-16,35-52 aod550_4met.txt | sort -u > aeronet_locs.txt
