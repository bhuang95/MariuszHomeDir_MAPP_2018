PROGRAM test

!to test datetime library

  USE datetime_mod
  USE timedelta_mod

  TYPE(datetime_type) a, b
  TYPE(timedelta_type) dt

  a = create_datetime(year=2017, month=10, day=6, hour=14)
  b = create_datetime(year=2017, month=10, day=6, hour=12)

  WRITE(6, *) a%isoformat() ! => 2017-10-06T14:00:00Z

  dt = timedelta(minutes=6)
  b = a + dt

  WRITE(6, *) a%isoformat() ! => 2017-10-06T14:06:00Z

  b = create_datetime(year=2018, month=4, day=16, hour=23, minute=51)

  dt = b - a

  WRITE(6, *) dt%total_seconds() ! => 16624260.0
  WRITE(6, *) dt%total_minutes() ! => 277071.0
  WRITE(6, *) dt%total_hours()   ! => 4617.85
  WRITE(6, *) dt%total_days()    ! => 192.4104166666667

END PROGRAM test
