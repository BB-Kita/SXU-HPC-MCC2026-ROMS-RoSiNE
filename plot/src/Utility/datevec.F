      SUBROUTINE datevec (datenum, year, month, iday,                   &
     &                    hour, minute, second)

!
!svn $Id: datevec.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Given the datenum (number of days since 01-Jan-0000), this routine  !
!  returns it is associated year, month, day, hour, minute, second.    !
!                                                                      !
!    datevec(1)      = [0000 01 01 00 00 00]     01-Jan-0000 00:00:00  !
!    datevec(693962) = [1900 01 01 00 00 00]     01-Jan-1900 00:00:00  !
!                                                                      !
!=======================================================================
!
      implicit none
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      integer year, month, iday, hour, minute, second
      real*8  datenum
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer dpm (12)                   ! days per month
      integer dpml(12)                   ! days per month (leap year)
      integer days                       ! elapsed days from reference
      integer daytest

      data dpm   /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      data dpml  /31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!
!-----------------------------------------------------------------------
!  Compute date vector from date number.
!-----------------------------------------------------------------------
!
!  If time is a fraction of a day, compute hour, minute, and seconds.
!
      second=NINT((datenum-FLOOR(datenum))*86400.d0)
      hour  =INT(second/3600)
      second=second-hour*3600
      minute=INT(second/60)
      second=second-minute*60
!
!  Compute day, month, and year.    
!
      IF (datenum.lt.1.0d0) THEN
        year =0
        month=1
        iday =0
      ELSE
        year =0
        month=1
        iday =1
        days =1
!
!  If datenum is greater or equal to the number of days in 400 years,
!  update "year" and "days" values.
!
        DO WHILE ((INT(datenum)-days).ge.146097)
          year=year+400
          days=days+146097
        END DO
!
!  Check for leap years.
!  
        DO WHILE ((INT(datenum)-days).ge.366)
          IF (MOD(year,400).eq.0) THEN
            days=days+366                             ! leap year
          ELSE IF (MOD(year,100).eq.0) THEN
            days=days+365
          ELSE IF (MOD(year,4).eq.0) THEN
            days=days+366                             ! leap year
          ELSE
            days=days+365
          END IF
          year=year+1
        END DO
!
!  Compute day and month.
!
        DO WHILE (days.lt.INT(datenum))
          iday=iday+1
          IF (MOD(year,400).eq.0) THEN
            daytest=dpml(month)                      ! leap year
          ELSE IF (MOD(year,100).eq.0) THEN
            daytest=dpm (month)
          ELSE IF (MOD(year,4).eq.0) THEN
            daytest=dpml(month)                      ! leap year
          ELSE
            daytest=dpm (month)
          END IF
          IF (iday.gt.daytest) THEN
            iday=1
            month=month+1
            IF (month.gt.12) THEN
              month=1
              year=year+1
            END IF
          END IF
          days=days+1
        END DO

      END IF

      RETURN
      END
