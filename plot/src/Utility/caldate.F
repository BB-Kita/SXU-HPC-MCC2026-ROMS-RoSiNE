       SUBROUTINE caldate (julian,month,iday,iyear)
!
!svn $Id: caldate.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine converts  Julian Day  number to calendar date.  Here   !
!  JULIAN is input as the Julian day number, and outputs MONTH, IDAY,  !
!  IYEAR as the month, day, and year.                                  !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer gregorian, iday, iyear, ja, jalpha, jb, jc, jd, je,       &
     &        julian, month
      parameter (gregorian=2299161)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      IF (julian.ge.gregorian) THEN
        jalpha=INT(((julian-1867216)-p25)/36524.25)
        ja=julian+1+jalpha-INT(p25*FLOAT(jalpha))
      ELSE
        ja=julian
      END IF
      jb=ja+1524
      jc=INT(6680.0+(FLOAT(jb-2439870)-122.1)/365.25)
      jd=365*jc+INT(p25*FLOAT(jc))
      je=INT(FLOAT(jb-jd)/30.6001)
      iday=jb-jd-INT(30.6001*FLOAT(je))
      month=je-1
      IF (month.gt.12) month=month-12
      iyear=jc-4715
      IF (month.gt.2) iyear=iyear-1
      IF (iyear.le.0) iyear=iyear-1

      RETURN
      END
