#!/bin/sh

cd /scratch1/BMC/chem-var/pagowski/junk_scp/from_lelek

cd pics_all
convert -set delay 60 -loop 5 fig_*png  movie_wcrp_1.gif

#cd pics_201601/
#convert -set delay 60 -loop 5 fig_*png  movie_201601.gif
