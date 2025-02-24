#!/bin/ksh

#for fname in `seq -f '%03.f' 0 6 120`
#do
#    echo abc$fname
#done

#exit
#seq -f '%03.f' 0 6 120
#exit

echo 3 > seed.txt

random=`shuf --random-source=seed.txt -i0-10000 -n1`
random=`shuf -i0-10000 -n1`

echo $random

exit

#RANDOM=13
imax=1

i=1
while [[ $i -le $imax ]]
do
    alphal=`echo "scale=2;$((RANDOM%1000)) / 200 " | bc`

    alphaf=`echo "scale=2;$((RANDOM%1000)) / 200 " | bc`

    echo "$alphal $alphaf"

    ((i=i+1))
done

exit

float nens nensm fraction

nens=50
echo $nens
((nensm=nens-1))
((fraction=nens/nensm))

echo $fraction
