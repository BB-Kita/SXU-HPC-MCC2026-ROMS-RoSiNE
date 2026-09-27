      SUBROUTINE colorbar (nclev,cbarlab,fmin,fmax,ipos)
!
!svn $Id: colorbar.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws color bar at the bottom (IPOS=0) or at the       !
!  right (IPOS=1) of the map.                                          !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
#include "colpal.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical outline,out_size
      integer i,ipos,cbarlab,nclev,lenlab,lnlg,pwr
      integer great_int
      real bb,bl,br,bt,cmin,cmax,delta,fmax,fmin,fval,max_abs,scl,wb,   &
     &     wl,wr,wt,xlab0,xlab,ylab0,ylab
      real xb(5),yb(5)
      character*10 lab
      character*15 scl_str
      character*22 fmt
      parameter (lnlg=1)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Set parameters for horizontal (IPOS=0) or vertical (IPOS=1)
!  color bar.
!-----------------------------------------------------------------------
!
      delta=c1/FLOAT(nclev)
      cmin=fmin
      cmax=fmax
      IF (ipos.eq.0) THEN
        bl=fl
        br=fr
        bb=p06*sclmap
        bt=bb+p08*sclmap
        wl=c0
        wr=c1
        wb=c0
        wt=c1
        xb(1)=wl
        xb(2)=wl
        xb(3)=wl+delta
        xb(4)=wl+delta
        xb(5)=xb(1)
        yb(1)=wt
        yb(2)=wb
        yb(3)=wb
        yb(4)=wt
        yb(5)=yb(1)
      ELSE IF (ipos.eq.1) THEN
        bl=fr+r50*sclmap
        br=fr+p08*sclmap
        bb=fb
        bt=ft
        wl=c0
        wr=c1
        wb=c0
        wt=c1
        xb(1)=wl
        xb(2)=wl
        xb(3)=wr
        xb(4)=wr
        xb(5)=xb(1)
        yb(1)=wb+delta
        yb(2)=wb
        yb(3)=wb
        yb(4)=wb+delta
        yb(5)=yb(1)
      END IF
!
!-----------------------------------------------------------------------
!  Set color bar frame.
!-----------------------------------------------------------------------
!
      CALL set (bl,br,bb,bt,wl,wr,wb,wt,lnlg)
!
!-----------------------------------------------------------------------
!  Determine scaling for labelling.
!-----------------------------------------------------------------------
!
      max_abs=MAX(ABS(cmin),ABS(cmax))
      out_size=((max_abs.lt.r10).and.(max_abs.gt.c0)).or.               &
     &          (max_abs.ge.c10000)
      IF (out_size) THEN
        pwr=great_int(LOG10(max_abs))
        scl=c10**(-pwr)
      ELSE
        scl=c1
      END IF
!
!-----------------------------------------------------------------------
!  Draw color bar.
!-----------------------------------------------------------------------
!
      IF (nclev.le.50) THEN
        outline=.true.
      ELSE
        outline=.false.
      END IF
      DO 20 i=1,nclev
        CALL clrbox (xb,yb,5,i+1,outline)
        IF (ipos.eq.0) THEN
          xlab0=xb(2)
          ylab0=wb-r100
          xlab=xb(3)
          ylab=wb-r100
          xb(1)=xb(4)
          xb(2)=xb(1)
          xb(3)=xb(1)+delta
          xb(4)=xb(3)
          xb(5)=xb(1)
        ELSE IF (ipos.eq.1) THEN
          xlab0=wr+r10
          ylab0=yb(2)
          xlab=wr+r10
          ylab=yb(1)
          yb(1)=yb(1)+delta
          yb(2)=yb(1)-delta
          yb(3)=yb(2)
          yb(4)=yb(1)
          yb(5)=yb(1)
        ELSE
          WRITE (6,10) ipos
  10      FORMAT (/' COLORBAR: illegal position, IPOS=',i4)
        END IF
!
!  Label bottom color band.
!
        IF (i.eq.1) THEN
          IF (log_cnt) THEN
            IF (cmin.le.0) THEN
              cmin=c0
              fval=c0
            ELSE
              fval=10**(LOG10(cmin))
            END IF
          ELSE IF (c_levels) THEN
            fval=cmin
          ELSE
            fval=cmin
          END IF
          CALL barlab (lab,lenlab,fval,cmin,cmax,scl)
          CALL plchhq (xlab0,ylab0,lab(1:lenlab),FontSize3,c0,cm1)
        END IF
!
!  Label every CBARLAB color bands.
!
        IF (MOD(i,cbarlab).eq.0) THEN
          IF (log_cnt) THEN
            IF (cmin.le.c0) THEN
              fval=10**(LOG10(cmax)/FLOAT(nclev)*FLOAT(i))
            ELSE
              fval=10**(LOG10(cmin)+(LOG10(cmax)-LOG10(cmin))/          &
     &                               FLOAT(nclev)*FLOAT(i))
            END IF
          ELSE IF (c_levels) THEN
            fval=clevels(i)
          ELSE
            fval=cmin+FLOAT(i)*(cmax-cmin)/FLOAT(nclev)
          END IF
          CALL barlab (lab,lenlab,fval,cmin,cmax,scl)
          CALL plchhq (xlab,ylab,lab(1:lenlab),FontSize3,c0,cm1)
        END IF
  20  CONTINUE
!
!-----------------------------------------------------------------------
!  Write label scaling (if needed).
!-----------------------------------------------------------------------
!
      IF (out_size) THEN
        IF ((pwr.lt.100).and.(pwr.gt.9)) THEN
          WRITE (fmt,30) 2
  30      FORMAT ('(',1h',':R:x10:S:',1h',',i',i1,',',1h',':N:',1h',')')
        ELSE IF (pwr.ge.0) THEN
          WRITE (fmt,30) 1
        ELSE IF (pwr.gt.-10) THEN
          WRITE (fmt,30) 2
        ELSE
          WRITE (fmt,30) 3
        END IF
        WRITE (scl_str,fmt) pwr
        IF (ipos.eq.0) THEN
          xlab=xb(3)
          ylab=p5*(wb+wt)
        ELSE IF (ipos.eq.1) THEN
          xlab=wl
          ylab=wb-r20
        END IF
        CALL plchhq (xlab,ylab,scl_str,FontSize3,c0,cm1)
      END IF
!
!-----------------------------------------------------------------------
!  Draw color bar box.
!-----------------------------------------------------------------------
!
      CALL gslwsc (c2)
      CALL box (1)
      CALL gslwsc (c1)

      RETURN
      END
