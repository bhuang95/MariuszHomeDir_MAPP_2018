from datetime import datetime
from datetime import timedelta

def ndate(cdate,hinc):
    yy=int(str(cdate)[:4])
    mm=int(str(cdate)[4:6])
    dd=int(str(cdate)[6:8])
    hh=int(str(cdate)[8:10])
    dstart=datetime(yy,mm,dd,hh)
    dnew=dstart+timedelta(hours=hinc)
    dnewint=int(str('%4.4d' %dnew.year)+str('%2.2d' %dnew.month)+
                str('%2.2d' %dnew.day)+str('%2.2d' %dnew.hour))
    return dnewint
