      FUNCTION yearday (year, month, iday)
!
!svn $Id: yearday.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Given the year, month, and day, this function computes the day of   !
!  the year number.                                                    !
!                                                                      !
!=======================================================================
!
      implicit none
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      integer year, month, iday
      integer yearday
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical leapyear

      integer iyd (13)      ! beginning of the month yearday
      integer iydl(13)      ! beginning of the month yearday (leap)

      data iyd  /1,32,60,91,121,152,182,213,244,274,305,335,366/
      data iydl /1,32,61,92,122,153,183,214,245,275,306,336,367/
!    
!-----------------------------------------------------------------------
!  Determine day of the year.
!-----------------------------------------------------------------------
!
!  Set leap year switch.
!
      IF (MOD(year,400).eq.0) THEN
        leapyear=.true.
      ELSE IF (MOD(year,100).eq.0) THEN
        leapyear=.false.
      ELSE IF (MOD(year,4).eq.0) THEN
        leapyear=.true.
      ELSE
        leapyear=.false.
      END IF
!
!  Set day of the year.
!
      IF (leapyear) THEN
        yearday=iydl(month)+iday-1
      ELSE
        yearday=iyd (month)+iday-1
      END IF

      RETURN
      END
