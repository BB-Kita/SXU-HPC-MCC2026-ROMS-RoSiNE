      SUBROUTINE maptitles (day,cint,fmin1,fmax1,fmin2,fmax2,frms,      &
     &                      icolor,idfld,level)
!
!svn $Id: maptitles.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine writes all map titles which depend on the number of    !
!  plots per page, and number of fields per page.                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "fldsid.h"
#include "forecast.h"
#include "ncinfo.h"
#include "pltfld.h"
#include "pltio.h"
#include "pltlab.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, iadd, icolor, iday, idfld, ie, is, ihour, julian,      &
     &        lenbot, lstr, lstr1, lstr2, lstr3, minute, month, second
      integer lenstr
      integer yearday

      real cint, day, dayofyear, fmax1, fmax2, fmin1, fmin2, frms,      &
     &     fx, fy, hour, level, pday, rday, scle, xlab, ylab
      real yd(13)
      real*8 datenumber

      character*5  Cscle
      character*9  smonth(0:12)
      character*19 datelab
      character*80 toplab
      character*80 text, text1, text2, text3

      data yd  /1.,32.,60.,91.,121.,152.,182.,213.,244.,274.,305.,335., &
     &          366./
      data smonth /'Annual','January','February','March','April','May', &
     &             'June','July','August','September','October', &
     &             'November','December'/
!
!=======================================================================
!  Begin executable code
!=======================================================================
!
!-----------------------------------------------------------------------
!  Get data date label.
!-----------------------------------------------------------------------
!
!  Determine the units of reporting time label.
!
      scle=1.0                              ! days
      Cscle=' Day '
      IF (dt.gt.c0) THEN
        IF (dt.lt.60.0) THEN
          scle=86400.0                      ! days to seconds
          Cscle=' Sec '
        ELSE IF (dt.lt.3600.0) THEN
          scle=1440.0                       ! days to minutes
          Cscle=' Min '
        ELSE IF (dt.lt.86400.0) THEN
          scle=24.0                         ! days to hours
          Cscle=' Hour'
        END IF
      END IF
!
!  If appropriate, add forecast interval to issued time.
!
      IF ((nfcst1.gt.0).and.(findx.gt.0)) THEN
        pday=day+fcst1(findx)
      ELSE
        pday=day
      END IF
      IF (var_time1.and.(wdate.or.julday1)) THEN
        IF (julday1) THEN
          julian=2440000+INT(pday)
          hour=(pday-INT(pday))*24.0
          CALL caldate (julian,month,iday,iyear)
          dayofyear=FLOAT(yearday(iyear,month,iday))
          CALL datestr (iyear,dayofyear,hour,datelab)
        ELSE IF (timeref1.gt.0.d0) THEN
          datenumber=timeref1+DBLE(day)
          CALL datevec (datenumber,iyear,month,iday,ihour,minute,second)
          dayofyear=FLOAT(yearday(iyear,month,iday))
          hour=FLOAT(ihour)+FLOAT(minute)/60.0+FLOAT(second)/3600.0
          CALL datestr (iyear,dayofyear,hour,datelab)
        ELSE
          rday=pday+yday
          hour=(pday-int(pday))*24.0
          IF (rday.gt.365.0) THEN
            iadd=INT(rday/365.25)
            dayofyear=MOD(rday,365.0)
          ELSE
            iadd=0
            dayofyear=rday
          END IF
          CALL datestr (iyear+iadd,dayofyear,hour,datelab)
        END IF
      ELSE
        datelab='           '
      END IF
!
!  Set first part of the date label.
!
      IF (var_time1) THEN
        IF ((type1(1:5).eq.'SCRUM').or.(type1(1:4).eq.'ROMS').or. &
     &      (type1(1:4).eq.'TOMS').or.(type1(1:3).eq.'ICE')) THEN
          IF ((day-dstart).ge.c0) THEN
            WRITE (daylab,10) (day-dstart)*scle, Cscle
  10        FORMAT (f8.2,a)
          ELSE
            WRITE (daylab,10) day*scle, Cscle
          END IF
          IF (.not.lmodday) daylab=' '
        ELSE IF (type1(1:11).eq.'CLIMATOLOGY') THEN
          month=0
          DO i=1,12
            IF ((yd(i).le.day).and.(day.lt.yd(i+1))) THEN
              month=i
              GO TO 20
            END IF
          END DO
  20      CONTINUE
          IF ((0.le.month).and.(month.le.12)) THEN
            lstr=lenstr(smonth(month))
            daylab=smonth(month)(1:lstr)
          ELSE
            WRITE (daylab,'(f8.2)') day
          END IF
        ELSE IF (julday1.and. &
     &          (type1(1:2).eq.'OA').or.(type1(1:4).eq.'DATA')) THEN
          julian=2440000+int(pday)
          hour=(pday-int(pday))*24.0
          CALL caldate (julian,month,iday,iyear)
          dayofyear=FLOAT(yearday(iyear,month,iday))
          CALL datestr (iyear,dayofyear,hour,daylab)
        ELSE
          WRITE (daylab,10) day*scle, Cscle
          IF (.not.lmodday) daylab=' '
        END IF
      ELSE
        daylab=' '
      END IF
!
!-----------------------------------------------------------------------
!  Write top titles.
!-----------------------------------------------------------------------
!
      IF (ipltpg.eq.1) THEN
        CALL set (c0,c1,c0,c1,c0,c1,c0,c1,1)
        IF (wrthdr) THEN
          CALL checkstrm (toplab,lstr,pltitle(1))
          CALL plchhq (0.23,0.975,toplab(1:lstr),FontSize1,c0,-c1)
          CALL checkstrm (toplab,lstr,pltitle(2))
          CALL plchhq (0.23,0.945,toplab(1:lstr),FontSize2,c0,-c1)
          CALL checkstrm (toplab,lstr,pltitle(3))
          CALL plchhq (0.23,0.915,toplab(1:lstr),FontSize2,c0,-c1)
        END IF
!
!  If needed, over-write fourth title and use bottom date title instead.
!
        IF ((nfields.gt.1).or.(nlevels.gt.1)) THEN
          IF (var_time1) THEN
            IF (wdate) THEN
              IF (lmodday) THEN
                text=daylab//' : '//datelab
              ELSE
                text=datelab
              END IF
            ELSE
              text=daylab
            END IF
            lstr=lenstr(text)
            DO i=1,lstr
              IF (text(i:i).ne.' ') GO TO 30
            END DO
  30        is=i
            CALL checkstrm (toplab,lstr,text(is:lstr))
          ELSE
            CALL checkstrm (toplab,lstr,pltitle(4))
          END IF
        ELSE
          CALL checkstrm (toplab,lstr,pltitle(4))
        END IF
        IF (wrthdr) THEN
          CALL plchhq (0.23,0.885,toplab(1:lstr),FontSize2,c0,-c1)
        END IF
!
!-----------------------------------------------------------------------
!  Write current date label.
!-----------------------------------------------------------------------
!
        IF (wrtdate) THEN
          lstr=lenstr(date_str)
          IF (plttyp.eq.cntplt) THEN
            CALL plchmq (0.96,0.02,date_str(1:lstr),FontSize6,c0,c1)
          ELSE IF (plttyp.eq.secplt) THEN
            CALL plchmq (0.96,0.99,date_str(1:lstr),FontSize6,c0,c1)
          END IF
        END IF
!
!-----------------------------------------------------------------------
!  Write input filename label.
!-----------------------------------------------------------------------
!
        IF (wrtfnam) THEN
          IF (icolor.eq.0) text=fname(2)
          IF (icolor.eq.1) text=fname(3)
          lstr=lenstr(text)
          DO i=lstr,1,-1
            IF (text(i:i).eq.'/') GO TO 40
          END DO
  40      is=i+1
          ie=lstr-i
          IF (plttyp.eq.cntplt) THEN
            xlab=0.96
            ylab=0.01
          ELSE IF (plttyp.eq.secplt) THEN
            xlab=0.96
            ylab=0.98
          END IF
          CALL plchmq (xlab,ylab,text(is:lstr),FontSize6,c0,c1)
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Write bottom label.
!-----------------------------------------------------------------------
!
!  Field name and depth (or level) is the bottom label.
!
      IF ((nfields.gt.1).or.(nlevels.gt.1)) THEN
        IF (plttyp.eq.cntplt) THEN
          CALL depth_lab (idfld,level,botlab)
          lenbot=lenstr(botlab)
        ELSE IF (plttyp.eq.secplt) THEN
          botlab=vlabel(idfld)
          lenbot=lenstr(botlab)
        END IF
!
!  The date is the bottom label.
!
      ELSE
        IF (var_time1.and.wdate) THEN
          IF (lmodday) THEN
            botlab=daylab//' : '//datelab
          ELSE
            botlab=datelab
          END IF
        ELSE
          botlab=daylab
        END IF
        lenbot=lenstr(botlab)
      END IF
!
!  Write bottom label.
!
      CALL checkstrm (text,lstr,botlab)
      CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
      fx=fl+p5*(fr-fl)
      IF (plttyp.eq.cntplt) THEN
        fy=fb-0.035
      ELSE IF (plttyp.eq.secplt) THEN
        fy=fb-0.06
      END IF
      xlab=ul+(fx-fl)*(ur-ul)/(fr-fl)
      ylab=ub+(fy-fb)*(ut-ub)/(ft-fb)
      IF (wrtblab.and.(lstr.gt.0)) THEN
        CALL plchhq (xlab,ylab,text(1:lstr),FontSize2,c0,c0)
!!    ELSE
!!      READ (100,'(a)') text
!!      CLOSE (100)
!!      lstr=lenstr(text)
!!      CALL plchhq (xlab,ylab,text(1:lstr),FontSize2,c0,c0)
      END IF
!
!-----------------------------------------------------------------------
!  Write MIN, MAX and CI label.
!-----------------------------------------------------------------------
!
      lstr1=0
      lstr2=0
      lstr3=0
      IF (scalar1) THEN
        IF (icolor.eq.0) THEN
          WRITE (text1,50) fmin1, fmax2, abs(cint)
          lstr1=lenstr(text1)
  50      FORMAT (SP,'Min=',1pe11.4,2x,'Max=',1pe11.4,2x,'CI=',1pe11.4)
        ELSE IF (icolor.eq.1) THEN
          WRITE (text1,60) fmin1, fmax1
          lstr1=lenstr(text1)
  60      FORMAT (SP,'Min=',1pe11.4,2x,'Max=',1pe11.4)
        END IF
      ELSE IF ((vector1.or.wbarbs1).and.                                &
     &         ((iref.eq.3).or.(iref.eq.4))) THEN
        WRITE (text1,70) fmax1, frms
        lstr1=lenstr(text1)
  70    FORMAT (SP,'Max=',1pe11.4,2x,'RMS=',1pe11.4)
      END IF
!
      IF (iref.gt.0) THEN
        IF (scalar2) THEN
          WRITE (text2,50) fmin2, fmax2, abs(cint)
          lstr2=lenstr(text2)
        ELSE IF (vector2) THEN
          WRITE (text2,80) fmin2, fmax2
          lstr2=lenstr(text2)        
  80      FORMAT (SP,'Min=',1pe11.4,2x,'Max=',1pe11.4)
        END IF
      END IF
!
      IF (isosurf) THEN
        WRITE (text3,90) isoval
  90    FORMAT (SP,'ISO=',1pe11.4)
        lstr3=lenstr(text3)
      ELSE IF (level.gt.c1000) THEN
        WRITE (text3,80) level-c1000
        lstr3=lenstr(text3)
      END IF
      IF (lstr3.gt.0) THEN
        text=text1(1:lstr1) // '  ' // text3(1:lstr3)
      ELSE
        text=text1(1:lstr1)
      END IF
!
!  Write primary field statistics.
!
      lstr=lenstr(text)
      IF (plttyp.eq.cntplt) THEN
        fx=fl
        IF (spherical) THEN
          fy=fb-0.007
        ELSE
          fy=fb-0.0125
        END IF
      ELSE IF (plttyp.eq.secplt) THEN
        fx=fl
        fy=fb-0.025
      END IF
      xlab=ul+(fx-fl)*(ur-ul)/(fr-fl)
      ylab=ub+(fy-fb)*(ut-ub)/(ft-fb)
      IF (wrtrang1) THEN
        CALL plchhq (xlab,ylab,text(1:lstr),FontSize5,c0,-c1)
      END IF
!
!  Write secondary (overlay) field statistics.
!
      IF ((lstr2.gt.0).and.wrtrang2) THEN
        IF (plttyp.eq.cntplt) THEN
          fx=fl
          IF (spherical) THEN
            fy=fb-0.02
          ELSE
            fy=fb-0.0225
          END IF
        ELSE IF (plttyp.eq.secplt) THEN
          fx=fl
          fy=fb-0.038
        END IF
        xlab=ul+(fx-fl)*(ur-ul)/(fr-fl)
        ylab=ub+(fy-fb)*(ut-ub)/(ft-fb)
        CALL plchhq (xlab,ylab,text2(1:lstr2),FontSize5,c0,-c1)
      END IF

      RETURN
      END
