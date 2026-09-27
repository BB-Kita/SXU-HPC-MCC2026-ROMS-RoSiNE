      FUNCTION datenum (iday,month,year)
!
!svn $Id: datenum.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Given the day, month, and year, this routine computes the date      !
!  number (in days) similar to the one computed in Matlab. That is,    !
!  it calculates the number of days since 01-Jan-0000.                 !
!                                                                      !
!         datenum(1,1,0000) = 1           01-Jan-0000                  !
!         datenum(1,1,1900) = 693962      01-Jan-1900                  !
!                                                                      !
!=======================================================================
!
      implicit none
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      integer iday, month, year
      integer datenum
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer daypermonth (12)

      data daypermonth  /0,31,59,90,120,151,181,212,243,273,304,334/
!
!-----------------------------------------------------------------------
!  Compute date number
!-----------------------------------------------------------------------
!
      datenum=INT(year*365+                                             &
     &            CEILING(year/4.0)-                                    &
     &            CEILING(year/100.0)+                                  &
     &            CEILING(year/400.0)+                                  &
     &            daypermonth(month)+                                   &
     &            iday)
!
!  Correct for leap years.
!
      IF (((month.gt.2).and.                                            &
     &    (((MOD(year,4).eq.0).and.(MOD(year,100).ne.0)).or.            &
     &      (MOD(year,400).eq.0)))) THEN
        datenum=datenum+1
      END IF

      RETURN
      END
