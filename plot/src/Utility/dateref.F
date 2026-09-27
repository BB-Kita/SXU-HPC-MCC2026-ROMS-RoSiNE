      SUBROUTINE dateref (time_att, time_ref, julday)
!
!svn $Id: dateref.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine decode the time reference from the time attribute of   !
!  the form, for example:                                              !
!                                                                      !
!     seconds since 1900-01-01 00:00:00                                !
!  or                                                                  !
!     days since 1968-05-23 00:00:00                                   !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     time_att   Time attribute (string)                               !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     time_ref   Time reference (date number: days since 01-Jan-0000)  !
!     julday     Switch indicating a modified Julian day number        !
!                                                                      !
!=======================================================================
!
      implicit none
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      logical julday
      real*8  time_ref

      character*(*) time_att
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer year, month, iday, hour, minute, second
      integer date, lstr, lend

      integer datenum
!
!-----------------------------------------------------------------------
!  Decode time reference from time attribute string.
!-----------------------------------------------------------------------
!
!  Determine if modified Julian day number attribute.
!
      IF ((time_att(1:24).eq.'seconds since 1968-05-23').or.            &
     &    (time_att(1:21).eq.'days since 1968-05-23')) THEN
        julday=.true.
      ELSE
        julday=.false.
      END IF
!
!  Decode time reference.
!
      lend=LEN_TRIM(time_att)
      IF ((time_att(1:13).eq.'seconds since').or.                       &
     &    (time_att(1:10).eq.'days since')) THEN
        lstr=INDEX(time_att(1:lend),'since')+6
        READ (time_att(lstr:lend),10) year, month, iday,                &
     &                                hour, minute, second
 10     FORMAT (i4.4,5(1x,i2.2))
        date=datenum(iday,month,year)
        time_ref=DBLE(date)+                                            &
     &           DBLE(hour)/24.0d0+                                     &
     &           DBLE(minute)/1440.0d0+                                 &
     &           DBLE(second)/86400.0d0
      ELSE
        time_ref=0.d0
      END IF        

      RETURN
      END

