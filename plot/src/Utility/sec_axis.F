      SUBROUTINE sec_axis
!
!svn $Id: sec_axis.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets and draws the axis for the cross-section box.     !
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
#include "fields.h"
#include "ndimen.h"
#include "pltncar.h"
#include "pltfld.h"
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, lstr
      integer lenstr
      real bearing, d, dbot, delmaj, delmin, dend, dist, dlef, dmax,    &
     &     dmin, drig, dstr, dtop, fbs, fls, frs, ftc1, ftc2, fts, fxl, &
     &     fzl, sxl, sxr, syl, syr, wl, wr, xlab, ylab1, ylab2, zlab
      character*3 Cscle
      character*5 dlab
      character*6 text
      character*8 sxlab, sylab
!
!=======================================================================
!  Begin executive code.
!=======================================================================
!
!  If spherical configuration, label axis in metric or degree units.
!
      IF (spherical) THEN
        IF (secaxis.eq.0) THEN
          wl=c0
          wr=wl
          DO i=1,ims-1
            CALL geodesic_dist (xsec(i),ysec(i),xsec(i+1),ysec(i+1),4,  &
     &                          dist,bearing)
            wr=wr+dist
          END DO
        ELSE
          wl=x(1)
          wr=x(ims)
        END IF
      ELSE
        wl=ul
        wr=ur
      END IF
!
!  Set plotting frame.
!
      CALL set (fl,fr,fb,ft,wl,wr,ub,ut,1)
!
!  Draw section box.
!
      CALL gslwsc (c2)
      CALL gridal (0,0,0,0,0,0,5,c0,c0)
      CALL gslwsc (c1)
!
!  Draw horizontal axis minor tick marks.
!
      IF (ltmarks) THEN
        IF (.not.spherical.or.(secaxis.eq.0)) THEN
          dmin=wl
          dmax=wr
          IF (INT((dmax-dmin)/c5).le.5) THEN
            delmin=c1
            delmaj=c5
          ELSE IF (INT((dmax-dmin)/c5).le.10) THEN
            delmin=c5
            delmaj=c25
          ELSE IF (INT((dmax-dmin)/c10).le.10) THEN
            delmin=c10
            delmaj=c50
          ELSE IF (INT((dmax-dmin)/c100).le.5) THEN
            delmin=c20
            delmaj=c100
          ELSE IF (INT((dmax-dmin)/c200).le.5) THEN
            delmin=c50
            delmaj=c200
          ELSE IF (INT((dmax-dmin)/c500).le.5) THEN
            delmin=c100
            delmaj=c500
          ELSE IF (INT((dmax-dmin)/c1000).le.5) THEN
            delmin=c200
            delmaj=c1000
          ELSE
            delmin=c500
            delmaj=c5000
          END IF
          dstr=c0
          dend=dmax
        ELSE
          dmin=wl
          dmax=wr
          IF (INT(ABS(dmax-dmin)/c5).le.1) THEN
            delmin=0.25
            delmaj=c1
          ELSE IF (INT(ABS(dmax-dmin)/c5).le.5) THEN
            delmin=c1
            delmaj=c5
          ELSE IF (INT(ABS(dmax-dmin)/c10).le.10) THEN
            delmin=c2
            delmaj=c10
          ELSE IF (INT(ABS(dmax-dmin)/c20).le.50) THEN
            delmin=c5
            delmaj=c10
          ELSE
            delmin=c4
            delmaj=c20
          END IF
          IF (dmin.lt.dmax) THEN
            dstr=dmin-MOD(dmin,delmaj)
            dend=dmax
          ELSE
            dstr=dmin-MOD(dmin,delmaj)+delmaj
            dend=dmax
          END IF
        END IF
        fts=ft-r100
        fbs=fb+r100
        dtop=ub+(ut-ub)*(fts-fb)/(ft-fb)
        dbot=ub+(ut-ub)*(fbs-fb)/(ft-fb)
        IF (dstr.lt.dend) THEN
          d=dstr
          DO WHILE ((d+delmin).le.dend)
            d=d+delmin
            IF (d.ge.dmin) THEN
              CALL line (d,ut,d,dtop)
              CALL line (d,ub,d,dbot)
            END IF
          END DO
        ELSE
          d=dstr
          DO WHILE ((d-delmin).ge.dend)
            d=d-delmin
            IF (d.le.dmin) THEN
              CALL line (d,ut,d,dtop)
              CALL line (d,ub,d,dbot)
            END IF
          END DO
        END IF
!
!  Label horizontal axis and draw its associated major tick marks.
!
        fts=ft-r50
        fbs=fb+r50
        fxl=fb-r100
        dtop=ub+(ut-ub)*(fts-fb)/(ft-fb)
        dbot=ub+(ut-ub)*(fbs-fb)/(ft-fb)
        xlab=ub+(ut-ub)*(fxl-fb)/(ft-fb)
        IF (dstr.lt.dend) THEN
          d=dstr
          DO WHILE (d.le.dend)
            IF (d.ge.dmin) THEN
              IF (.not.spherical.or.(secaxis.eq.0)) THEN
                IF (d.eq.dstart) THEN
                  IF (coorscl.eq.0) THEN
                    dlab='0 km'
                  ELSE IF (coorscl.eq.1) THEN
                    dlab='0 m '
                  ELSE IF (coorscl.eq.2) THEN
                    dlab='0 cm'
                  ELSE IF (coorscl.eq.3) THEN
                    dlab='0 mm'
                  END IF
                  lstr=lenstr(dlab)
                  CALL plchhq (d,xlab,dlab(1:lstr),FontSize4,c0,-c1)
                ELSE
                  WRITE (dlab,'(i5)') INT(d)
                  lstr=lenstr(dlab)
                  CALL plchhq (d,xlab,dlab(1:lstr),FontSize4,c0,c0)
                END IF
              ELSE
                WRITE (dlab,'(i5)') ABS(INT(d))
                lstr=lenstr(dlab)
                IF (lataxis) THEN
                  IF (d.lt.c0) THEN
                    text=dlab(1:lstr)//'S'
                  ELSE IF (d.gt.c0) THEN
                    text=dlab(1:lstr)//'N'
                  ELSE
                    text=dlab(1:lstr)
                  END IF
                ELSE
                  IF (d.lt.c0) THEN
                    text=dlab(1:lstr)//'W'
                  ELSE IF (d.gt.c0) THEN
                    text=dlab(1:lstr)//'E'
                  ELSE
                    text=dlab(1:lstr)
                  END IF
                END IF
                lstr=lenstr(text)
                CALL plchhq (d,xlab,text(1:lstr),FontSize4,c0,c0)
              END IF
              CALL line (d,ut,d,dtop)
              CALL line (d,ub,d,dbot)
            END IF
            d=d+delmaj
          END DO
        ELSE
          d=dstr
          DO WHILE (d.gt.dend)
            IF (d.le.dmin) THEN
              IF ((secaxis.eq.0).and.(d.eq.dstart)) THEN
                WRITE (dlab,'(i5)') INT(d)
                lstr=lenstr(dlab)
                IF (coorscl.eq.0) THEN
                  dlab='0 km'
                ELSE IF (coorscl.eq.1) THEN
                  dlab='0 m '
                ELSE IF (coorscl.eq.2) THEN
                  dlab='0 cm'
                ELSE IF (coorscl.eq.3) THEN
                  dlab='0 mm'
                END IF
                lstr=lenstr(dlab)
                CALL plchhq (d,xlab,dlab(1:lstr),FontSize4,c0,-c1)
              ELSE
                WRITE (dlab,'(i5)') ABS(INT(d))
                lstr=lenstr(dlab)
                IF (lataxis) THEN
                  IF (d.lt.c0) THEN
                    text=dlab(1:lstr)//'S'
                  ELSE IF (d.gt.c0) THEN
                    text=dlab(1:lstr)//'N'
                  ELSE
                    text=dlab(1:lstr)
                  END IF
                ELSE
                  IF (d.lt.c0) THEN
                    text=dlab(1:lstr)//'W'
                  ELSE IF (d.gt.c0) THEN
                    text=dlab(1:lstr)//'E'
                  ELSE
                    text=dlab(1:lstr)
                  END IF
                END IF
                lstr=lenstr(text)
                CALL plchhq (d,xlab,text(1:lstr),FontSize4,c0,c0)
              END IF
              CALL line (d,ut,d,dtop)
              CALL line (d,ub,d,dbot)
            END IF
            d=d-delmaj
          END DO
        END IF
!
!  Draw vertical axis minor tick marks.
!
        dmin=ABS(ut)
        dmax=ABS(ub)
        IF (INT((dmax-dmin)/c5).le.5) THEN
          delmin=c1
          delmaj=c5
        ELSE IF (INT((dmax-dmin)/c5).le.10) THEN
          delmin=c5
          delmaj=c25
        ELSE IF (INT((dmax-dmin)/c10).le.10) THEN
          delmin=c10
          delmaj=c50
        ELSE IF (INT((dmax-dmin)/c100).le.5) THEN
          delmin=c20
          delmaj=c100
        ELSE IF (INT((dmax-dmin)/c200).le.5) THEN
          delmin=c50
          delmaj=c200
        ELSE IF (INT((dmax-dmin)/c500).le.5) THEN
          delmin=c100
          delmaj=c500
        ELSE
          delmin=c200
          delmaj=c1000
        END IF
        fls=fl+r100
        frs=fr-r100
        dlef=wl+(wr-wl)*(fls-fl)/(fr-fl)
        drig=wl+(wr-wl)*(frs-fl)/(fr-fl)
        d=c0
        DO WHILE ((d+delmin).le.dmax)
          d=d+delmin
          IF (d.ge.dmin) THEN
            CALL line (wl,-d,dlef,-d)
            CALL line (wr,-d,drig,-d)
          END IF
        END DO
!
!  Label vertical axis and draw its associated major tick marks.
!
        fls=fl+r40
        frs=fr-r40
        fzl=fl-r33
        dlef=wl+(wr-wl)*(fls-fl)/(fr-fl)
        drig=wl+(wr-wl)*(frs-fl)/(fr-fl)
        zlab=wl+(wr-wl)*(fzl-fl)/(fr-fl)
        d=dmin
        Cscle=' m '
        IF (coorscl.eq.2) THEN
          Cscle=' cm'
        ELSE IF (coorscl.eq.3) THEN
          Cscle=' mm'
        END IF
        WRITE (dlab,'(i4,a)') INT(d)
        CALL plchhq (zlab,-d,dlab,FontSize4,c0,c0)
        CALL plchhq (zlab,-(d+delmin),Cscle,FontSize4,c0,c0)
        CALL line (wl,-d,dlef,-d)
        CALL line (wr,-d,drig,-d)
        d=c0
        DO WHILE (d.le.dmax)
          IF (d.gt.dmin) THEN
            WRITE (dlab,'(i4)') INT(d)
            CALL plchhq (zlab,-d,dlab,FontSize4,c0,c0)
            CALL line (wl,-d,dlef,-d)
            CALL line (wr,-d,drig,-d)
          END IF
          d=d+delmaj
        END DO
!
!  Write starting and ending section points label.
!
        IF (.not.spherical.or.(secaxis.eq.0)) THEN
          sxl=xsec1
          sxr=xsec2
          IF (isecpos.eq.2) THEN
            IF (eastlon.and.(xsec1.gt.c180)) sxl=xsec1-c360
            IF (eastlon.and.(xsec2.gt.c180)) sxr=xsec2-c360
          END IF
          syl=ysec1
          syr=ysec2
          IF (isecpos.eq.0) THEN
            IF (INT(xsec1).lt.1) sxl=c1
            IF (INT(ysec1).lt.1) syl=c1
            IF (((xsec1-AINT(xsec1)).eq.0).and.                         &
     &          ((ysec1-AINT(ysec1)).eq.0)) THEN
              IF (INT(xsec1).gt.im-1) sxl=FLOAT(im-1)
              IF (INT(ysec1).gt.jm-1) syl=FLOAT(jm-1)
            ELSE
              IF (INT(xsec1).gt.im) sxl=FLOAT(im)
              IF (INT(ysec1).gt.jm) syl=FLOAT(jm)
            END IF
            IF (INT(xsec2).lt.1) sxr=c1
            IF (INT(ysec2).lt.1) syr=c1
            IF (((xsec2-AINT(xsec2)).eq.0).and.                         &
     &          ((ysec2-AINT(ysec2)).eq.0)) THEN
              IF (INT(xsec2).gt.im-1) sxr=FLOAT(im-1)
              IF (INT(ysec2).gt.jm-1) syr=FLOAT(jm-1)
            ELSE
              IF (INT(xsec2).gt.im) sxr=FLOAT(im)
              IF (INT(ysec2).gt.jm) syr=FLOAT(jm)
            END IF
          END IF
          ftc1=ft+r100
          ftc2=ft+r40
          ylab1=ub+(ut-ub)*(ftc1-fb)/(ft-fb)
          ylab2=ub+(ut-ub)*(ftc2-fb)/(ft-fb)
          IF (isecpos.eq.0) THEN
            WRITE (sxlab,'(''I='',i3.3)') INT(sxl)
            WRITE (sylab,'(''J='',i3.3)') INT(syl)
            CALL plchhq (wl,ylab2,sxlab(1:5),FontSize4,c0,-c1)
            CALL plchhq (wl,ylab1,sylab(1:5),FontSize4,c0,-c1)
          ELSE IF (isecpos.eq.1) THEN
            WRITE (sxlab,'(''X='',f6.2)') sxl
            WRITE (sylab,'(''Y='',f6.2)') syl
            CALL plchhq (wl,ylab2,sxlab,FontSize4,c0,-c1)
            CALL plchhq (wl,ylab1,sylab,FontSize4,c0,-c1)
          ELSE IF (isecpos.eq.2) THEN
            IF (sxl.lt.c0) THEN
              WRITE (sxlab,'(f6.2,'' W'')') ABS(sxl)
            ELSE
              WRITE (sxlab,'(f6.2,'' E'')') ABS(sxl)
            END IF
            IF (syl.gt.c0) THEN
              WRITE (sylab,'(f6.2,'' N'')') ABS(syl)
            ELSE
              WRITE (sylab,'(f6.2,'' S'')') ABS(syl)
            END IF
            CALL plchhq (wl,ylab1,sxlab,FontSize4,c0,-c1)
            CALL plchhq (wl,ylab2,sylab,FontSize4,c0,-c1)
          END IF
!
          IF (isecpos.eq.0) THEN
            WRITE (sxlab,'(''I='',i3.3)') INT(sxr)
            WRITE (sylab,'(''J='',i3.3)') INT(syr)
            CALL plchhq (wr,ylab2,sxlab(1:5),FontSize4,c0,c1)
            CALL plchhq (wr,ylab1,sylab(1:5),FontSize4,c0,c1)
          ELSE IF (isecpos.eq.1) THEN
            WRITE (sxlab,'(''X='',f6.2)') sxr
            WRITE (sylab,'(''Y='',f6.2)') syr
            CALL plchhq (wr,ylab2,sxlab,FontSize4,c0,c1)
            CALL plchhq (wr,ylab1,sylab,FontSize4,c0,c1)
          ELSE IF (isecpos.eq.2) THEN
            IF (sxr.lt.c0) THEN
              WRITE (sxlab,'(f6.2,'' W'')') ABS(sxr)
            ELSE
              WRITE (sxlab,'(f6.2,'' E'')') ABS(sxr)
            END IF
            IF (syr.gt.c0) THEN
              WRITE (sylab,'(f6.2,'' N'')') ABS(syr)
            ELSE
              WRITE (sylab,'(f6.2,'' S'')') ABS(syr)
            END IF
            CALL plchhq (wr,ylab1,sxlab,FontSize4,c0,c1)
            CALL plchhq (wr,ylab2,sylab,FontSize4,c0,c1)
          END IF
        END IF
      END IF

      RETURN
      END
