#!/usr/bin/env python3

import datetime
import pandas as pd
import netCDF4 as nc
import requests

base_url='https://api.openaq.org/v2/measurements'

#cycle in 6 hours

year_start=2016
month_start=4
day_start=30
hour_start=23
minute_start=30

datestart=datetime.datetime(year_start,month_start,day_start,
                            hour_start,minute_start)
dateend=datestart+datetime.timedelta(days=1)

datenow=datestart
while datenow <= dateend:

    date_from=datenow.strftime("%Y-%m-%dT%X")
    datenowp=datenow+datetime.timedelta(hours=1)
    date_to=datenowp.strftime("%Y-%m-%dT%X")

    params = {
        'date_from':date_from,
        'date_to':date_to,
        'limit':'10000',
        'parameter':'pm25',
        'order_by':'datetime',
        'sensorType':'reference grade',
        'has_geo':'true',
        'value_from':"0",
    }
    
    response = requests.get(base_url,params=params)
    global_data = response.json()

    #measurements = [{"date": row['date']['utc'], 
    #                 "location": row['locationId'],
    #                 "latitude": row['coordinates']['latitude'],
    #                 "longitude": row['coordinates']['longitude'],
    #                 "value": row['value']}
    #for row in global_data['results']]

    measurements = [{"date": row['date']['utc'],
                     "locationid": row['locationId'],
                     "location": row['location'],
                     "coords": row['coordinates'],
                     "value": row['value']}
                    for row in global_data['results']]

    print(measurements[10])
    datecenter=datenow+datetime.timedelta(minutes=30)
    print(datecenter)
    #sort out obs that have no lat/lon or other unrealistic values
    #write in IODA v3 at datecenter
    
    datenow = datenow + datetime.timedelta(hours=6)
