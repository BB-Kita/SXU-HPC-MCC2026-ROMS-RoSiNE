      SUBROUTINE vecpack (u,v,p,mm,m,n,smin,smax,icolor,ipos,overlay)
!
!svn $Id: vecpack.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws a vector field.                                  !
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
!     overlay  Switch indicating drawing vectors as an overlay.        !
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
      integer fill, i, icolor, ipos, ll, m, mm, n
      real dmx, smax, smin, pval, vb, vl, vmin, vmax, vr, vt, wb, wl,   &
     &     wr, wt
      real p(mm,n), u(mm,n), v(mm,n), xc(5), yc(5)
      external colram, vvudmv
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Reset all VECPACK parameters to their default values to facilitate
!  overlays.
!
      CALL vvrset
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
!  Prevent VECTORS from setting the viewport.
!
      CALL vvseti ('SET - do-set-call flag',0)
!!    CALL vvseti ('VST - vector statistics output flag',1)
!
!  Set vector positioning mode.
!
      CALL vvseti('VPO - vector positioning mode',vec_vpo)
!
!  Prevent "zero field" text from being displayed.
!
      CALL vvsetc ('ZFT - zero field text block',' ')
!
!  Set mapping to that provided by the User. Set up data coordinate
!  boundaries.
!
      IF (spherical) THEN
        CALL vvseti ('MAP - mapping flag',3)
        CALL vvsetr ('XC1 - Lower X Bound',c1)
        CALL vvsetr ('XCM - Upper X Bound',FLOAT(m))
        CALL vvsetr ('YC1 - Lower Y Bound',c1)
        CALL vvsetr ('YCN - Upper Y Bound',FLOAT(n))
      ELSE
        CALL vvseti ('MAP - mapping flag',4)
        CALL vvsetr ('XC1 - Lower X Bound',c1)
        CALL vvsetr ('XCM - Upper X Bound',FLOAT(m))
        CALL vvsetr ('YC1 - Lower Y Bound',c1)
        CALL vvsetr ('YCN - Upper Y Bound',FLOAT(n))
      END IF
!
!  Set vector grid sampling.
!
      CALL vvseti ('XIN - X-axis array increment',ivinc)
      CALL vvseti ('YIN - Y-axis array increment',jvinc)
!
!  Set minimum and maximum vector to Draw. Vectors below and above
!  this values are not drawn.
!
      CALL vvsetr ('VLC - Vector low cutoff value',smin)
      CALL vvsetr ('VHC - Vector high cutoff value',smax)
!
!  Set-up vector text block.
!
      IF ((icolor.eq.0).or.(icolor.eq.2)) THEN
        CALL vvsetc ('MNT - Minimum vector text block',' ')
        CALL vvseti ('MXP - Maximum vector text position mode',-4)
        CALL vvsetr ('MXS - Maximum vector text block size',0.0095)
        CALL vvsetr ('MXX - Maximum vector X-coordinate',vec_mxx)
        CALL vvsetr ('MXY - Maximum vector Y-coordinate',vec_mxy)
        IF (vec_vrm.gt.c0) THEN
!!        CALL vvsetr ('VHC - Vector reference magnitude',-vec_vrm)
          CALL vvsetr ('VRM - Vector reference magnitude',vec_vrm)
        ELSE
!!        CALL vvsetr ('VHC - Vector reference magnitude',-smax)
          CALL vvsetr ('VRM - Vector reference magnitude',smax)
        END IF
      ELSE IF (icolor.eq.1) THEN
        CALL vvsetc ('MNT - Minimum vector text block',' ')
        CALL vvsetc ('MXT - Maximum vector text block',' ')
      END IF
      CALL vvseti ('DPF - Vector Label Decimal Point Control Flag',1)
!
!  Set special value processing appropriately for the vector field.
!
      CALL vvseti ('SVF - Special Values Flag', 3)
      CALL vvsetr ('USV - U Special Value',spval1)
      CALL vvsetr ('VSV - V Special Value',spval1)
      CALL vvsetr ('PSV - P Special Value',spval1)
      CALL vvseti ('SPC - P Special Color',1)
!
!  Set up masking of vectors to an existing area map.
!
      IF (lmsk.gt.0) THEN
        CALL vvseti ('MSK - Vector Masking',1)
      ELSE
        CALL vvseti ('MSK - Vector Masking',0)
      END IF
!
!  Set up vector color processing.
!
      IF (icolor.eq.1) THEN
        CALL vvseti ('CTV - Color Thresholds Value',-2)
        CALL vvseti ('NLV - Number Of Levels',ncl)
        DO i=1,ncl
          pval=smin+FLOAT(i-1)*(smax-smin)/FLOAT(ncl-1)
          CALL vvseti ('PAI -- Parameter Array Index',i)
          CALL vvseti ('CLR -- GKS Color Index',i+1)
          CALL vvsetr ('TVL -- Treshold Value',pval)
        END DO
      ELSE IF (icolor.eq.2) THEN
        CALL gsplci (ncl+4)
      END IF
!
!  Initialize vectors.
!
      CALL vvinit (u,mm,v,mm,p,mm,m,n,rwrk,lrwk)
!
!  Adjust vector rendering options.
!
      CALL vvsetr ('AMN - Arrow Minimum Size',vec_amn)
      CALL vvsetr ('AMX - Arrow Maximum Size',vec_amx)
      CALL vvsetr ('LWD - Vector Line Width',vlwd)
      CALL vvgetr ('VMN - Minimum Vector',vmin)
      CALL vvgetr ('VMX - Maximum Vector',vmax)
      CALL vvsetr ('VLC - Vector Low Cutoff',vec_vlc*vmax)
      CALL vvgetr ('DMX - Device Maximum Vector Length',dmx)
      CALL getset (vl,vr,vb,vt,wl,wr,wb,wt,ll)
      CALL vvsetr ('VRL - Vector Realized Length',vlscl*dmx/(vr-vl))
      CALL vvsetr ('VFR - Vector Fractional Minimum',vec_vfr)
!
!  Draw the vector field plot.
!
      CALL vvectr (u,v,p,iam,vvudmv,rwrk)
!
!  Set polyline color back to its foreground color.
!
      IF (icolor.eq.2) CALL gsplci (1)
!
!-----------------------------------------------------------------------
!  Draw color bar (turn off and on clipping).
!-----------------------------------------------------------------------
!
      IF (lclrbar.and.(icolor.eq.1)) THEN
        CALL gsclip (0)
        CALL colorbar (ncl,ibarlab,smin,smax,ipos)
        CALL gsclip (1)
      END IF

      RETURN
      END
