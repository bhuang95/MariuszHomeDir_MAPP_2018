PROGRAM test

  USE iso_fortran_env, ONLY: real64
  USE datetime_module, ONLY : datetime, timedelta

  IMPLICIT NONE

  TYPE(datetime)  :: utctime
  TYPE(timedelta)  :: dt

  INTEGER :: i,j

  utctime=datetime(year=2013,month=1,day=1)

  dt=timedelta(hours=5)

  PRINT *,utctime%getyear(),utctime%getmonth(),utctime%getday(),utctime%gethour()

  utctime=utctime-dt
  PRINT *,utctime%getyear(),utctime%getmonth(),utctime%getday(),utctime%gethour()


END PROGRAM test
