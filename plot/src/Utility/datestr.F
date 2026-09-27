      SUBROUTINE datestr (iyear,yday,hour,datelab)
!
!svn $Id: datestr.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets a date label given the year IYEAR and the         !
!  year day YDAY.                                                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer ileap, imonth, ihour, it, iyear, jyr, itmax
      integer iyd(13), iydl(13)
      real hour, minute, yday
      character*3   month(12)
      character*19  datelab
      data iyd  /1,32,60,91,121,152,182,213,244,274,305,335,366/
      data iydl /1,32,61,92,122,153,183,214,245,275,306,336,367/
!!    data iyd  /0,31,59,90,120,151,181,212,243,273,304,334,365/
!!    data iydl /0,31,60,91,121,152,182,213,244,274,305,337,366/
      data month /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',             &
     &            'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!     it=nint(yday)
      it=int(yday)
      jyr=iyear
      IF ((it.le.0).or.(it.ge.600)) THEN
        WRITE (datelab,1) it
  1     FORMAT (10a1,i4)
        RETURN
      END IF
      imonth=0
!
  100 continue
      ileap=MOD(jyr,4)
      IF (ileap.eq.0) THEN
  105   imonth=imonth+1
        IF (imonth.eq.14) THEN
          jyr=jyr+1
          it=it-366
          imonth=0
          GO TO 100
        END IF
        IF (it.ge.iydl(imonth)) GO TO 105
        imonth=imonth-1
        it=it-iydl(imonth)+1
      ELSE
  107   imonth=imonth+1
        IF (imonth.eq.14) THEN
          jyr=jyr+1
          it=it-365
          imonth=0
          GO TO 100
        END IF
        IF (it.ge.iyd(imonth)) GO TO 107
        imonth=imonth-1
        it=it-iyd(imonth)+1
      END IF
!
!  Build date label.
!
      IF ((1.le.imonth).and.(imonth.le.12)) THEN
        ihour=int(hour)
        minute=(hour-aint(hour))*60.0
        IF (minute.ge.59.5) THEN
          ihour=ihour+1
          minute=0.0
        END IF
        IF (ihour.gt.24) THEN
          ihour=0
          it=it+1
        END IF
        WRITE (datelab,2) it, month(imonth), jyr, ihour, int(minute)
 2      FORMAT (i2,1x,a3,1x,i4,1x,'-',1x,i2.2,':',i2.2)
      END IF

      RETURN
      END
