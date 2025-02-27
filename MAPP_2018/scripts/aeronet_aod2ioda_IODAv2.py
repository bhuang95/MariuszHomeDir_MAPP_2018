#!/usr/bin/env python3
### Run command
### Load JEDI moudle or even not use and just use the below 
#/scratch1/NCEPDEV/da/python/hpc-stack/miniconda3/core/miniconda3/4.6.14/envs/iodaconv/bin/python3.7 aeronet_aod2ioda_IODAv2.py -t 2022092512 -w 6 -o AOD_IODAv2_2022092512.nc



# read/interpolate online aeronet AOD data and convert to netcdf
#import netCDF4 as nc
import numpy as np
import inspect, sys, os, argparse
import pandas as pd
from datetime import datetime, timedelta
from builtins import object, str
from numpy import NaN
from pathlib import Path

#IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
#if not IODA_CONV_PATH.is_dir():
#    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
#sys.path.append(str(IODA_CONV_PATH.resolve()))
sys.path.append('/home/Bo.Huang/JEDI-2020/miscScripts-home/JEDI-Support/aeronetScript/readAeronet/lib-python/')
#sys.path.append('/scratch2/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/MISC/codeDev/JEDI/iodaSprint-20220308/build/lib/pyiodaconv')
#sys.path.append('/scratch2/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/MISC/codeDev/JEDI/iodaSprint-20220308/build/lib/python3.7/pyioda')
sys.path.append('/scratch2/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/MISC/codeDev/JEDI/iodaSprint-20211025/build/lib/pyiodaconv')
sys.path.append('/scratch2/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/MISC/codeDev/JEDI/iodaSprint-20211025/build/lib/python3.7/pyioda')
#sys.path.append('/scratch2/BMC/gsd-fv3-dev/MAPP_2018/bhuang/JEDI-2020/JEDI-FV3/MISC/codeDev/JEDI/iodaSprint-20220308/build/lib')
#import pytspack as pts
import meteo_utils
import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

def dateparse(x):
    return datetime.strptime(x, '%d:%m:%Y %H:%M:%S')

def add_data(dates=None,
             product='AOD15',
             latlonbox=None,
             daily=False,
             interp_to_aod_values=None,
             inv_type=None,
             freq=None,
             siteid=None,
             detect_dust=False, n_procs=1, verbose=10):
    a = AERONET()
    df = a.add_data(dates=dates,
                    product=product,
                    latlonbox=latlonbox,
                    daily=daily,
                    interp_to_aod_values=interp_to_aod_values,
                    inv_type=inv_type,
                    siteid=siteid,
                    freq=freq,
                    detect_dust=detect_dust)
    return df.reset_index(drop=True)


class AERONET(object):
    def __init__(self):
        from numpy import concatenate, arange
        self.baseurl = 'https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_v3?'
        self.dates = [
            datetime.strptime('2016-06-06 12:00:00', '%Y-%m-%d %H:%M:%S'),
            datetime.strptime('2016-06-10 13:00:00', '%Y-%m-%d %H:%M:%S')
        ]
        self.datestr = []
        self.df = pd.DataFrame()
        self.daily = None
        self.prod = None
        self.inv_type = None
        self.siteid = None
        self.objtype = 'AERONET'
        self.usecols = concatenate((arange(30), arange(65, 83)))
        self.latlonbox = None
        self.url = None
        self.new_aod_values = None

    def build_url(self):
        sy = self.dates.min().strftime('%Y')
        sm = self.dates.min().strftime('%m').zfill(2)
        sd = self.dates.min().strftime('%d').zfill(2)
        sh = self.dates.min().strftime('%H').zfill(2)
        ey = self.dates.max().strftime('%Y').zfill(2)
        em = self.dates.max().strftime('%m').zfill(2)
        ed = self.dates.max().strftime('%d').zfill(2)
        eh = self.dates.max().strftime('%H').zfill(2)
        if self.prod in [
                'AOD10', 'AOD15', 'AOD20', 'SDA10', 'SDA15', 'SDA20', 'TOT10',
                'TOT15', 'TOT20'
        ]:
            base_url = 'https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_v3?'
            inv_type = None
        else:
            base_url = 'https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_inv_v3?'
            if self.inv_type == 'ALM15':
                inv_type = '&ALM15=1'
            else:
                inv_type = '&AML20=1'
        date_portion = 'year=' + sy + '&month=' + sm + '&day=' + sd + \
            '&hour=' + sh + '&year2=' + ey + '&month2=' + em + '&day2=' + ed +\
            '&hour2=' + eh
        if self.inv_type is not None:
            product = '&product=' + self.prod
        else:
            product = '&' + self.prod + '=1'
            self.inv_type = ''
        time = '&AVG=' + str(self.daily)
        if self.siteid is not None:
            latlonbox = '&site={}'.format(self.siteid)
        elif self.latlonbox is None:
            latlonbox = ''
        else:
            lat1 = str(float(self.latlonbox[0]))
            lon1 = str(float(self.latlonbox[1]))
            lat2 = str(float(self.latlonbox[2]))
            lon2 = str(float(self.latlonbox[3]))
            latlonbox = '&lat1=' + lat1 + '&lat2=' + \
                lat2 + '&lon1=' + lon1 + '&lon2=' + lon2
        print(base_url)
        print(date_portion)
        print(product)
        print(inv_type)
        print(time)
        print(latlonbox)
        if inv_type is None:
            inv_type = ''
        self.url = base_url + date_portion + product + \
            inv_type + time + latlonbox + '&if_no_html=1'

    def read_aeronet(self):
        print('Reading Aeronet Data...')
        df = pd.read_csv(self.url,
                         engine='python',
                         header=None,
                         skiprows=6,
                         parse_dates={'time': [1, 2]},
                         date_parser=dateparse,
                         na_values=-999)
        columns = self.get_columns()
        df.columns = columns
        df.index = df.time
        df.rename(columns={
            'site_latitude(degrees)': 'latitude',
            'site_longitude(degrees)': 'longitude',
            'site_elevation(m)': 'elevation',
            'aeronet_site': 'siteid'
        },
            inplace=True)
        df.dropna(subset=['latitude', 'longitude'], inplace=True)
        df.dropna(axis=1, how='all', inplace=True)
        self.df = df

    def get_columns(self):
        header = pd.read_csv(self.url, skiprows=5, header=None,
                             nrows=1).values.flatten()
        final = ['time']
        for i in header:
            if "Date(" in i or 'Time(' in i:
                pass
            else:
                final.append(i.lower())
        return final

    def add_data(self,
                 dates=None,
                 product='AOD15',
                 latlonbox=None,
                 daily=False,
                 interp_to_aod_values=None,
                 inv_type=None,
                 freq=None,
                 siteid=None,
                 detect_dust=False):
        self.latlonbox = latlonbox
        self.siteid = siteid
        if dates is None:  # get the current day
            self.dates = pd.date_range(start=pd.to_datetime('today'),
                                       end=pd.to_datetime('now'),
                                       freq='H')
        else:
            self.dates = dates
        self.prod = product.upper()
        if daily:
            self.daily = 20  # daily data
        else:
            self.daily = 10  # all points
        if inv_type is not None:
            self.inv_type = 'ALM15'
        else:
            self.inv_type = inv_type
        if 'AOD' in self.prod:
            self.new_aod_values = interp_to_aod_values
        self.build_url()
        try:
            self.read_aeronet()
            print(self.url)
        except:
            print(self.url)
        if freq is not None:
            self.df = self.df.groupby('siteid').resample(
                freq).mean().reset_index()
        if detect_dust:
            self.dust_detect()
        if self.new_aod_values is not None:
            self.calc_new_aod_values()
        return self.df

    def calc_new_aod_values(self):

        def _tspack_aod_interp(row, new_wv=[440., 470., 550., 670., 870., 1240.]):
            try:
                import pytspack
            except ImportError:
                print('You must install pytspack before using this function')
            aod_columns = [aod_column for aod_column in row.index if 'aod_' in aod_column]
            aods = row[aod_columns]
            wv = [float(aod_column.replace('aod_', '').replace('nm', '')) for aod_column in aod_columns]
            a = pd.DataFrame({'aod': aods}).reset_index()
	    # Interpolate AOD based on log(wv)
            wv_log=np.log(wv, dtype='float64')
            a['wv'] = wv_log
            new_wv_log=np.log(new_wv, dtype='float64')
            df_aod_nu = a.dropna()
            df_aod_nu_sorted = df_aod_nu.sort_values(by='wv').dropna()
            if len(df_aod_nu_sorted) < 2:
                return new_wv_log * NaN
            else:
                x, y, yp, sigma = pytspack.tspsi(df_aod_nu_sorted.wv.values, df_aod_nu_sorted.aod.values)
                yi = pytspack.hval(new_wv_log, x, y, yp, sigma)
                return yi

        out = self.df.apply(_tspack_aod_interp, axis=1, result_type='expand', new_wv=self.new_aod_values)
        names = 'aod_int_' + pd.Series(self.new_aod_values.astype(int).astype(str)) + 'nm'
        out.columns = names.values
        self.df = pd.concat([self.df, out], axis=1)

    def dust_detect(self):
        """Detect dust from AERONET. See [Dubovik et al., 2002].

        AOD_1020 > 0.3 and AE(440,870) < 0.6

        Returns
        -------
        type
            Description of returned object.

        """
        self.df['dust'] = (self.df['aod_1020nm'] >
                           0.3) & (self.df['440-870_angstrom_exponent'] < 0.6)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
	     description=(
		          'Reads online AERONET data from NASA website '
			  ' and converts into IODA formatted output files')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
		    '-t', '--time',
		    help="time (YYYYMMDDTHH) of AERONET AOD files to be downloaded from NASA website",
		    type=str, required=True)
    required.add_argument(
		    '-w', '--window',
		    help="An integer/float number defines a time window centered at time argument in hours within which AERONET AOD data will be downloaded",
		    type=float, required=True)
    required.add_argument(
		    '-o', '--output',
		    help="path of AERONET AOD IODA file",
		    type=str, required=True)

    args = parser.parse_args()
    date_center1 = args.time
    hwindow=args.window
    hwindow=hwindow/2.0
    outfile = args.output
    date_center = datetime.strptime(date_center1, '%Y%m%d%H') 
    date_start = date_center + timedelta(hours=-1.*hwindow)
    date_end = date_center + timedelta(hours=hwindow)

    print('Download AERONET AOD within +/- ' + str(hwindow) + ' hours at: ')
    print(date_center)

    dates = pd.date_range(start=date_start,end=date_end,freq='H')

    # Define AOD wavelengths, channels and frequencies
    aod_wav = np.array([340., 380., 440., 500., 675, 870., 1020., 1640.,], dtype=np.float32)
    aod_chan = np.array([1,   2,    3,    4,    5,   6,    7,     8 ], dtype=np.intc)

    # If AOD at other wavelenths (e.g., 550nm) are needed, please define/calculate aod_new_wav, aod_new_chan (other than 1-8),
    # frequency_new, concatanate aod_new_chan to aod_chan, and frequency_new to frequency and modify outcols and obsvars. 
    # In the current, AOD at aod_new_wav is not written out in the IODA files (e.g, obsvars).
    aod_new_wav=np.array([550.,], dtype=np.float32)
    aod_new_chan=None
    frequency_new=None
    speed_light = 2.99792458E8
    frequency = speed_light*1.0E9/aod_wav
    print('Calculate AERONET AOD at wavelengths/channels/frequencies: ')
    print(aod_wav)
    print(aod_chan)
    print(frequency)

    # Read and extract online AERONET AOD
    print('Read and extract online AERONET AOD')
    outcols=['time', 'siteid', 'longitude', 'latitude', 'elevation', 'aod_int_550nm', 'aod_340nm', 'aod_380nm', 'aod_440nm', 'aod_500nm', 'aod_675nm','aod_870nm', 'aod_1020nm','aod_1640nm']

    f3 = add_data(dates=dates, product='AOD15',interp_to_aod_values=aod_new_wav)
    #f3 = f3_tmp.fillna(NaN)
    #tfile1=open("df_header_new_aod.txt", "w")
    #for line in f3:
    #    tfile1.write(line)
    #    tfile1.write("\n")
    #tfile1.close()
    #
    #f3.to_csv('df_output_new_aod_all_na_rep.txt', sep=' ', na_rep='NaN', index=False)
    #f3.to_csv('df_output_new_aod_interp_na_rep.txt', sep=' ', na_rep='NaN', index=False, columns=outcols)
    #f3.to_csv('df_output_new_aod_all.txt', sep=' ', index=False)
    #f3.to_csv('df_output_new_aod_interp.txt', sep=' ',  index=False, columns=outcols)

    # Define AOD varname that match with those in f3
    nlocs, columns = f3.shape
    nchans = len(aod_chan)
    if nlocs==0:
        print('No avaiable AERONET AOD at ' + date_center1 + '  and exit')
        exit(0)

    locationKeyList = [("latitude", "float"), ("longitude", "float"), ("datetime", "string")]
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    obsvars = {'aerosol_optical_depth': ['aod_340nm', 'aod_380nm',
                                         'aod_440nm', 'aod_500nm',
                                         'aod_675nm', 'aod_870nm',
                                         'aod_1020nm', 'aod_1640nm']}

    AttrData = {
        'converter': os.path.basename(__file__),
    }
 
    DimDict = {
    }

    VarDims = {
        'aerosol_optical_depth': ['nlocs', 'nchans'],
        'frequency': ['nchans'],
        'wavelength': ['nchans'],
        'sensor_channel': ['nchans']
    }

    # Get the group names we use the most.
    obsValName = iconv.OvalName()
    obsErrName = iconv.OerrName()
    qcName = iconv.OqcName()
    
    for key, value in obsvars.items():
        varDict[key]['valKey'] = key, obsValName
        varDict[key]['errKey'] = key, obsErrName
        varDict[key]['qcKey'] = key, qcName
        varAttrs[key, obsValName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, obsErrName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, qcName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, obsValName]['_FillValue'] = -9999.
        varAttrs[key, obsErrName]['_FillValue'] = -9999.
        varAttrs[key, qcName]['_FillValue'] = -9999
        varAttrs[key, obsValName]['units'] = '1'
        varAttrs[key, obsErrName]['units'] = '1'
        varAttrs[key, iconv.OqcName()]['units'] = ''

    for key, value in obsvars.items():
        outdata[varDict[key]['valKey']] = np.array(np.float32(f3[value].fillna(np.float32(-9999.))))
        outdata[varDict[key]['qcKey']] = np.where(outdata[varDict[key]['valKey']] == np.float32(-9999.),
                                                  1, 0)
        outdata[varDict[key]['errKey']] = np.where(outdata[varDict[key]['valKey']] == np.float32(-9999.),
                                                   np.float32(-9999.), np.float32(0.02))
    outdata[varDict['aerosol_optical_depth']['qcKey']][:,7]=outdata[varDict['aerosol_optical_depth']['qcKey']][:,3]

    # Add metadata variables
    metaDataName='MetaData'
    outdata[('latitude', metaDataName)] = np.array(np.float32(f3['latitude']))
    outdata[('longitude', metaDataName)] = np.array(np.float32(f3['longitude']))
    outdata[('station_elevation', metaDataName)] = np.array(np.float32(f3['elevation']))
    varAttrs[('station_elevation', metaDataName)]['units'] = 'm'
    outdata[('surface_type', metaDataName)] = np.full((nlocs), 1)
    varAttrs[('surface_type', metaDataName)]['units'] = ''

    c = np.empty([nlocs], dtype=object)
    c[:] = np.array(f3.siteid)
    outdata[('station_id', metaDataName)] = c
    varAttrs[('station_id', metaDataName)]['units'] = ''

    d = np.empty([nlocs], dtype=object)
    for i in range(nlocs):
        d[i] = f3.time[i].strftime('%Y-%m-%dT%H:%M:%SZ')
    outdata[('datetime', metaDataName)] = d
    varAttrs[('datetime', metaDataName)]['units'] = ''

    outdata[('frequency', metaDataName)] = np.float32(frequency)
    varAttrs[('frequency', metaDataName)]['units'] = 'Hz'
    outdata[('wavelength', metaDataName)] = np.float32(aod_wav*1.0E-9)
    varAttrs[('wavelength', metaDataName)]['units'] = 'm'
    outdata[('sensor_channel', metaDataName)] = np.int32(aod_chan)
    varAttrs[('sensor_channel', metaDataName)]['units'] = ''

    # Add global atrributes
    DimDict['nlocs'] = nlocs
    DimDict['nchans'] = aod_chan
    AttrData['nlocs'] = np.int32(DimDict['nlocs'])
    AttrData['nchans'] = np.int32(nchans)
    AttrData['observation_type'] = 'AOD'
    AttrData['sensor'] = 'aeronet'
    AttrData['surface_type'] = 'ocean=0,land=1,costal=2'

    # Setup the IODA writer
    writer = iconv.IodaWriter(outfile, locationKeyList, DimDict)

    # Write out IODA NC files
    writer.BuildIoda(outdata, VarDims, varAttrs, AttrData)
