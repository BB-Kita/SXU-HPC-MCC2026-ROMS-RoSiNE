      SUBROUTINE wind_barbs (u,v,p,mm,m,n,smin,smax,icolor,ipos,overlay)
!
!svn $Id: wind_barbs.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws a wind barbs field.                              !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     u        Vector component in the first dimension.                !
!     v        Vector component in the second dimension.               !
!     p        Array of scalar data that may used to color the         !
!              vectors (usually, vector magnitude).                    !
!     mm       Size of the first dimension in the calling program.     !
!     m        Number of elements in the first dimension.              !
!     n        Number of elements in the second dimension.             !
!     smin     Minimum vector magnitude to consider.                   !
!     smax     Maximum vector magnitude to consider.                   !
!     icolor   Vector color switch:                                    !
!                icolor=0  foreground color: black or white.           !
!                icolor=1  Use NCL colors in palette.                  !
!                icolor=2  one color overlay.                          !
!     ipos     Position of color bar (only when icolor=1):             !
!                ipos=0    at the bottom of viewport.                  !
!                ipos=1    at the right of viewport.                   !
!     overlay  Switch indicating drawing barbs as an overlay.          !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "cntwkspa.h"
#include "colpal.h"
#include "fields.h"
#include "fmask.h"
#include "ndimen.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical bounded, overlay
      integer fill, i, ip, j, icolor, ipos, m, mm, n
      real smax, smin, wbsize, speed
      real p(mm,n), u(mm,n), v(mm,n), xc(5), yc(5), xpt, ypt, x2, y2
      external colram, vvudmv
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Initialize area map.
!-----------------------------------------------------------------------
!
      IF (.not.overlay) THEN
        CALL arinam (iam,lam)
      END IF
!
!-----------------------------------------------------------------------
!  If land masking, paint the entire plot window with blue water.
!-----------------------------------------------------------------------
!
      IF (.not.overlay) THEN
        lmask=lmsk
        IF (lmsk.gt.0) THEN
          fill=0
          CALL landmask (fill)
          fill=1
          CALL landmask (fill)
          CALL arpram (iam,0,0,0)
          CALL arscam (iam,xcs,ycs,mcs,iai,iag,mai,colram)
        ELSE
          IF (icolor.eq.1) THEN
            xc(1)=ul
            yc(1)=ub
            xc(2)=ur
            yc(2)=ub
            xc(3)=ur
            yc(3)=ut
            xc(4)=ul
            yc(4)=ut
            xc(5)=xc(1)
            yc(5)=yc(1)
            CALL clrbox (xc,yc,5,ncl+3,.false.)
          END IF
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Set VECTORS drawing options.
!-----------------------------------------------------------------------
!
!  Set size of barbs.
!
      wbsize=0.035*vlscl
      CALL wmsetr ('WBS - wind barb size', wbsize)
!
!  Set mapping to that provided by the User. Set up data coordinate
!  boundaries.
!
      CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
!
!  Turn on clipping
!
      CALL gsclip (1)
!
!  Set up color of wind barbs.
!
      IF (icolor .eq. 2) CALL wmseti('COL - color', ncl+4)
!
!  Plot wind barbs at specified sampling.
!
      DO j=1,n,jvinc
        DO i=1,m,ivinc
          ip=i+(j-1)*im
          IF (spherical) THEN
            CALL maptrn (y(ip),x(ip),xpt,ypt)
          ELSE
            xpt=x(ip)
            ypt=y(ip)
          END IF
          bounded=.true.
          IF ((xpt.lt.ul).or.(xpt.gt.ur).or.                            &
     &        (ypt.lt.ub).or.(ypt.gt.ut)) bounded=.false.
          speed=SQRT(u(i,j)**2+v(i,j)**2)
          x2=fl+(xpt-ul)*(fr-fl)/(ur-ul)
          x2=x2+u(i,j)*wbsize/speed
          y2=fb+(ypt-ub)*(ft-fb)/(ut-ub)
          y2=y2+v(i,j)*wbsize/speed
          IF ((x2.lt.fl).or.(x2.gt.fr).or.                              &
     &        (y2.lt.fb).or.(y2.gt.ft)) bounded=.false.
          IF (bounded) CALL wmbarb (xpt,ypt,u(i,j),v(i,j))
        END DO
      END DO
!
!  Set color back to the foreground color.
!
      IF (icolor .eq. 2) CALL wmseti ('COL - color', 1)

      RETURN
      END
