#!/bin/ksh 

. /etc/profile

. ~/MAPP_2018/.environ.ksh

#set -x

nanals=20

INDIR=/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/from_sam/canned-test-case

OUTDIR=${INDIR}/HISTORY_000

rm -rf $OUTDIR
mkdir -p ${OUTDIR}

cd ${OUTDIR}

itile=1
while [[ $itile -le 6 ]]
do

    echo tile${itile}

    ncecat -O -v bcembb,bceman,ocembb,oceman,so2embb,so2eman,duem001,duem002,duem003,duem004,duem005,ssem001,ssem002,ssem003,ssem004,ssem005,aecmass,bccmass,occmass,ducmass,sscmass,sucmass  ${INDIR}/HISTORY_???/fv3_history2d.tile${itile}.nc fv3_history2d.tile${itile}.nc

    ncap2 -O -v -s 'duem_std=((duem001+duem002+duem003+duem004+duem005)-(duem001+duem002+duem003+duem004+duem005).avg($record)).rmssdn($record);ssem_std=((ssem001+ssem002+ssem003+ssem004+ssem005)-(ssem001+ssem002+ssem003+ssem004+ssem005).avg($record)).rmssdn($record);bcembb_std=(bcembb-bcembb.avg($record)).rmssdn($record);bceman_std=(bceman-bceman.avg($record)).rmssdn($record);ocembb_std=(ocembb-ocembb.avg($record)).rmssdn($record);oceman_std=(oceman-oceman.avg($record)).rmssdn($record);so2embb_std=(so2embb-so2embb.avg($record)).rmssdn($record);so2eman_std=(so2eman-so2eman.avg($record)).rmssdn($record);bccmass_std=(bccmass-bccmass.avg($record)).rmssdn($record);occmass_std=(occmass-occmass.avg($record)).rmssdn($record);ducmass_std=(ducmass-ducmass.avg($record)).rmssdn($record);sscmass_std=(sscmass-sscmass.avg($record)).rmssdn($record);sucmass_std=(sucmass-sucmass.avg($record)).rmssdn($record);aecmass_std=((ducmass+sscmass+bccmass+occmass+sucmass)-(ducmass+sscmass+bccmass+occmass+sucmass+bccmass+occmass+sucmass).avg($record)).rmssdn($record)' fv3_history2d.tile${itile}.nc fv3_history2d_spread.tile${itile}.nc
    
    ((itile=itile+1))

done

