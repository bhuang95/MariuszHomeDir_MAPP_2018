#!/bin/ksh
#set -x

target_dir='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/frp_issue/WFABBA_data'
cd $target_dir

for file in f*filt
do
  echo $file
  mv $file bak.$file
  sed -e "/\*\*/d" bak.$file >> $file
done
