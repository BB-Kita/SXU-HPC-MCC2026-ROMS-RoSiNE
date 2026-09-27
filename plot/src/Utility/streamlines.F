      SUBROUTINE streamlines (u,v,p,mm,m,n,smin,smax,icolor,ipos,       &
     &                        overlay)
!
!svn $Id: streamlines.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws a stremlines from a vector field.                !
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
!     overlay  Switch indicating drawing streamlines as an overlay.    !
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
#include "fmask.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical overlay
      integer fill, icolor, ipos, m, mm, n
      real smax, smin
      real p(mm,n), u(mm,n), v(mm,n), xc(5), yc(5)
      external colram, stumsl
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Reset all STREAM parameters to their default values to facilitate
!  overlays.
!
      CALL strset
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
!  Prevent STREAM from setting the viewport.
!
      CALL stseti ('SET - do-set-call flag',0)
!!    CALL stseti ('SST - streamline statistics output flag',1)
!
!  Set mapping to that provided by the User. Set up data coordinate
!  boundaries.
!
      IF (spherical) THEN
        CALL stseti ('MAP - mapping flag',3)
        CALL stsetr ('XC1 - Lower X Bound',c1)
        CALL stsetr ('XCM - Upper X Bound',FLOAT(m))
        CALL stsetr ('YC1 - Lower Y Bound',c1)
        CALL stsetr ('YCN - Upper Y Bound',FLOAT(n))
      ELSE
        CALL stseti ('MAP - mapping flag',4)
        CALL stsetr ('XC1 - Lower X Bound',ul)
        CALL stsetr ('XCM - Upper X Bound',ur)
        CALL stsetr ('YC1 - Lower Y Bound',ub)
        CALL stsetr ('YCN - Upper Y Bound',ut)
      END IF
!
!  Set streamline parameters.
!
      CALL stsetr ('LWD - Streamline Line Width',1.0)
      CALL stseti ('TRP - Interpolation Method', 1)
      CALL stsetr ('SSP - Stream Spacing', 0.005)
      CALL stsetr ('DFM - Differential Magnitude', 0.005)
      CALL stseti ('AGD - Arrow Grid Increment', 3)
      CALL stseti ('SGD - Stream Grid Increment', 3)
!
!  Set special value processing appropriately for the vector field.
!
      CALL stseti ('SVF - Special Values Flag', 3)
      CALL stsetr ('USV - U Special Value',spval1)
      CALL stsetr ('VSV - V Special Value',spval1)
!
!  Set up masking of streamlines to an existing area map.
!
      IF (lmsk.gt.0) THEN
        CALL stseti ('MSK - Mask to area map flag',1)
      ELSE
        CALL stseti ('MSK - Mask to area map flag',0)
      END IF
!
!  Set up streamline color processing.
!
      IF (icolor.gt.1) THEN
        CALL gsplci (ncl+4)
      END IF
!
!  Initialize streamlines.
!
      CALL stinit (u,mm,v,mm,p,mm,m,n,rwrk,lrwk)
!
!  Draw streamlines
!
      CALL stream (u,v,p,iam,stumsl,rwrk)
!
!  Set polyline color back to its foreground color.
!
      IF (icolor.gt.0) CALL gsplci (1)

      RETURN
      END
