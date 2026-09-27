      SUBROUTINE get_date (date_str)
!
!svn $Id: get_date.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine gets todays date, day of the week and time called      !
!  (default month & weekday are December & Saturday respectively).     !
!  It uses SUN intrinsic date routine by default.                      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     date_str   Concatenated string for the day of the week, date     !
!                (month,day,year), and time (12hr clock) of day        !
!                (hour:min:sec).                                       !
!                                                                      !
#ifdef cray
!     CALLs:  day_code                                                 !
# else
!     CALLs:  none                                                     !
#endif
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer dstat, half, hour, iday, imon, len1, len2, len3, min,     &
     &        nday, sec, tstat, year
      integer lday(31), lmonth(12)
      integer lenstr
#if defined cray
      integer century
      parameter (century=1900)
      character*8  tstring
#elif defined AIX
      character*3  day3, mon
      character*28 tmpday
#else
      character*3  day3, mon
      character*28 fdate, tmpday                 ! Sun, SGI, DECALPHA, ...
#endif
      character*3  ampm(0:1)
      character*9  day(0:6), month(12)
      character*11 ctime
      character*18 today
      character*20 fmt
      character*44 wkday
      character*(*) date_str
      data ampm /' AM',' PM'/
      data day /'Sunday','Monday','Tuesday','Wednesday','Thursday',     &
     &          'Friday','Saturday'/
      data lmonth, lday /7,8,5,5,3,4,4,6,9,7,8,8,9*1,22*2/
      data month /'January','February','March','April','May','June',    &
     &            'July','August','September','October','November',     &
     &            'December'/
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Get weekday, date and time in short format, then extract this
!  information.
!-----------------------------------------------------------------------
!
#if defined vax
      dstat=0
      CALL idate (imon,nday,year)
      year=year+century
      CALL time (tstring)
      READ (tstring,'(i2,1x,i2,1x,i2)',iostat=tstat) hour, min, sec
      IF (tstat.ne.0) ctime=tstring
#elif defined cray
      WRITE (tstring,'(a8)') date()
      READ (tstring,'(i2,1x,i2,1x,i2)',iostat=dstat) imon, nday, year
      year=year+century
      IF (dstat.ne.0) THEN
        wkday=tstring
        today=' '
      END IF
      WRITE (tstring,'(a8)') clock()
      READ (tstring,'(i2,1x,i2,1x,i2)',iostat=tstat) hour, min, sec
      IF (tstat.ne.0) ctime=tstring
#elif defined AIX
      CALL fdate_(tmpday)
      READ (tmpday,'(a3,1x,a3,1x,i2)',iostat=dstat) day3, mon, nday
      READ (tmpday,'(11x,i2,1x,i2,1x,i2)',iostat=tstat) hour, min, sec
      tstat=MAX(ABS(dstat),ABS(tstat))
      READ (tmpday,'(20x,i4)',iostat=dstat) year
      IF ((dstat.ne.0).or.(tstat.ne.0)) THEN
        dstat=1
        tstat=1
        wkday=tmpday
        today=' '
        ctime=' '
      END IF
#else
      tmpday=fdate()                             ! Sun, SGI, DECALPHA, ...
      READ (tmpday,'(a3,1x,a3,1x,i2)',iostat=dstat) day3, mon, nday
      READ (tmpday,'(11x,i2,1x,i2,1x,i2)',iostat=tstat) hour, min, sec
      tstat=MAX(ABS(dstat),ABS(tstat))
      READ (tmpday,'(20x,i4)',iostat=dstat) year
      IF ((dstat.ne.0).or.(tstat.ne.0)) THEN
        dstat=1
        tstat=1
        wkday=tmpday
        today=' '
        ctime=' '
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Convert from 24 hour clock to 12 hour AM/PM clock.
!-----------------------------------------------------------------------
!
      IF (tstat.eq.0) THEN
        half=hour/12
        hour=hour-half*12
        IF (hour.eq.0) hour=12
        IF (half.eq.2) half=0
      END IF
!
      IF (dstat.eq.0) THEN
!
#if defined vax || defined cray
!-----------------------------------------------------------------------
!  Get index for the day of the week.
!-----------------------------------------------------------------------
!
        CALL day_code (imon,nday,year,iday)
#else
!-----------------------------------------------------------------------
!  Loop to find full day name by comparing DAY3 with first 3 letters
!  of day.
!-----------------------------------------------------------------------
!
        iday=0
        DO WHILE ((day3.ne.day(iday)(1:3)).and.(iday.lt.6))
          iday=iday+1
        END DO
!
!-----------------------------------------------------------------------
!  Loop to find full month name by comparing MON with first 3 letters
!  of month.
!-----------------------------------------------------------------------
!
        imon=1
        DO WHILE ((mon.ne.month(imon)(1:3)).and.(imon.lt.12))
          imon=imon+1
        END DO
#endif
!
!-----------------------------------------------------------------------
!  Construct date, time and day of the week output string.
!-----------------------------------------------------------------------
!
        WRITE (fmt,10) lmonth(imon), lday(nday)
  10    FORMAT ('(a',i1,',1x,i',i1,',1h,,1x,i4)')
        WRITE (today,fmt) month(imon),nday,year
        wkday=day(iday)
      END IF
      IF (tstat.eq.0) THEN
        WRITE (ctime,20) hour, min, sec, ampm(half)
  20    FORMAT (i2,':',i2.2,':',i2.2,a3)
      END IF
!
!  Concatenate date string.
!
      len1=lenstr(wkday)
      len2=lenstr(today)
      len3=lenstr(ctime)
      date_str=wkday(1:len1)
      IF (len2.gt.0) THEN
        len1=lenstr(date_str)
        date_str=date_str(1:len1)//' - '//today(1:len2)
      END IF
      IF (len3.gt.0) THEN
        len1=lenstr(date_str)
        date_str=date_str(1:len1)//' - '//ctime(1:len3)
      END IF

      RETURN
      END
