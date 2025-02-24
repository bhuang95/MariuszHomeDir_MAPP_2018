#utility funcs for ksh

increment_date( ) {

#requires positive or negative increment in hours as input

#year=2010
#month=07
#day=30
#hr=12

hr=${hr:=$hour}

if [ $1 -lt 0 ]
then
    sign="-"
    incr_hr=`expr -1 \* $1`
else
    sign="+"
    incr_hr=$1
fi
    
indate=`~/util/bin/mktime -z -y $year -m $month -d $day -H $hr`

outdate=`~/util/bin/mktime -z -D $indate -H $hr -H ${sign}$incr_hr`

#echo $indate $outdate

end_year=`~/util/bin/mktime -F %Y -D $indate -H $hr -H ${sign}$incr_hr`
end_month=`~/util/bin/mktime -F %m -D $indate -H $hr -H ${sign}$incr_hr`
end_day=`~/util/bin/mktime -F %d -D $indate -H $hr -H ${sign}$incr_hr`
end_hr=`~/util/bin/mktime -F %H -D $indate -H $hr -H ${sign}$incr_hr`
end_hour=$end_hr

#echo $end_year $end_month $end_day $end_hr

}

get_JD () {

bc << MSG

# calculation below is all on one line.
$1-32075+1461*($3+4800+($2-14)/12)/4+367*($2-2-($2-14)/12*12)/12-3*(($3+4900+($2-14)/12)/100)/4

MSG

}


julian () {

/bin/date --date=${year}'/'${month}'/'${day} +%j

}

get_julian () {

jan=1
firstday=1

#date0=`bc << MSG
#$firstday-32075+1461*($3+4800+($jan-14)/12)/4+367*($jan-2-($jan-14)/12*12#)/12-3*(($3+4900+($jan-14)/12)/100)/4-1
#MSG`

date0=`echo $firstday-32075+1461*($3+4800+($jan-14)/12)/4+367*($jan-2-($jan-14)/12*12)/12-3*(($3+4900+($jan-14)/12)/100)/4-1 | bc`

#date1=`bc << MSG
#$1-32075+1461*($3+4800+($2-14)/12)/4+367*($2-2-($2-14)/12*12)/12-3*(($3+4#900+($2-14)/12)/100)/4
#MSG`

date1=`echo $1-32075+1461*($3+4800+($2-14)/12)/4+367*($2-2-($2-14)/12*12)/12-3*(($3+4900+($2-14)/12)/100)/4 | bc`

julian_day=`expr $date1 - $date0`

}

