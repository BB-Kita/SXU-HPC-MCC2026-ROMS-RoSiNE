      SUBROUTINE stumxy (xda,yda,xus,yus,ist)
!
!svn $Id: stumxy.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine is used to locate streamlines in the map defined by    !
!  the user.                                                           !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xda,yda   Point in data coordinate space.                        !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     xus,yus   Point in user coordinate space.                        !
!     ist       Status code indicating success or failure.             !
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
!
! The mapping common block: made available to user mapping routines.
!
      common /stmap/                                                    &
     &                imap       ,lnlg       ,invx       ,invy       ,  &
     &                xlov       ,xhiv       ,ylov       ,yhiv       ,  &
     &                wxmn       ,wxmx       ,wymn       ,wymx       ,  &
     &                xvpl       ,xvpr       ,yvpb       ,yvpt       ,  &
     &                xgds       ,ygds       ,nxct       ,nyct       ,  &
     &                itrt       ,fw2w       ,fh2h       ,              &
     &                dfmg       ,vnml       ,rbig       ,ibig
      integer ibig, imap, invx, invy, itrt, lnlg, nxct, nyct
      real dfmg, fh2h, fw2w, rbig, vnml, wxmn, wymn, wxmx, wymx, xgds,  &
     &     xhiv, xlov, xvpl, xvpr, ygds, yhiv, ylov, yvpb, yvpt
      save /stmap/
!
!-----------------------------------------------------------------------
!  Description of STMAP contents:
!
!  imap                - value of the internal parameter 'MAP'
!  xvpl,xvpr,yvpb,yvpt - the currently set viewport values. (GETSET
!                        arguments 1, 2, 3, and 4)
!  wxmn,wxmx,wymn,wymx - the min and max boundaries of user coordinate
!                        space, (usually but not always equivalent to
!                        window coordinates). WXMN and WYMN are true
!                        minimum values even one or both axes is
!                        inverted. (i.e. they are equivalent to GETSET
!                        arguments 5,6,7, and 8 sorted numerically)
!  xlov,xhiv,ylov,yhiv - min and max boundaries of the data space, by
!                        default equivalent to the array grid space.
!                        XLOV and YLOV are not necessarily less than
!                        XHIV and YHIV.
!  nxct,nyct           - Length of each dimension of the U and V
!                        component arrays.
!  lnlg                - The linear/log mode (GETSET argument 9)
!  invx,invy           - User coordinates inversion flags:
!                        0 - not inverted, 1 - inverted
!  itrt                - value of the internal parameter TRT
!  iwct                - not currently used
!  fw2w,fh2h           - scale factors for converting from fraction of
!                        viewport width/height to NDC width/height
!  rbig,ibig           - machine dependent maximum REAL/INTEGER values
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer i, ip, ist, j
      real u, v, xda, xus, x1, x2, x3, x4, yda, yus, y1, y2, y3, y4
!
!  Math constants:
!
      real pdtor, prtod, p1xpi, p2xpi, p1d2pi, p5d2pi
      parameter (pdtor=0.017453292519943, prtod=57.2957795130823,       &
     &           p1xpi=3.14159265358979, p2xpi=6.28318530717959,        &
     &           p1d2pi=1.57079632679489, p5d2pi=7.85398163397448)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      ist=0
!
!-----------------------------------------------------------------------
!  Spherical EZMAP mapping projections.
!-----------------------------------------------------------------------
!
      IF (imap.eq.1) THEN
        CALL maptra (yda,xda,xus,yus)
!
!-----------------------------------------------------------------------
!  Polar coordinates mapping.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.2) THEN
        xus=xda*COS(pdtor*yda)
        yus=xda*SIN(pdtor*yda)
!
!-----------------------------------------------------------------------
!  Spherical curvilinear grid mapping projections.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.3) THEN
        i=INT(xda)
        j=INT(yda)
        u=xda-AINT(xda)
        v=yda-AINT(yda)
        ip=i+(j-1)*im
        CALL maptrn (y(ip),x(ip),x1,y1)
        CALL maptrn (y(ip+1),x(ip+1),x2,y2)
        CALL maptrn (y(ip+im+1),x(ip+im+1),x3,y3)
        CALL maptrn (y(ip+im),x(ip+im),x4,y4)
        xus=(c1-u)*(c1-v)*x1+u*(c1-v)*x2+u*v*x3+v*(c1-u)*x4
        yus=(c1-u)*(c1-v)*y1+u*(c1-v)*y2+u*v*y3+v*(c1-u)*y4
!
!-----------------------------------------------------------------------
!  Cartesian curvilinear grid mapping.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.4) THEN
        i=INT(xda)
        j=INT(yda)
        u=xda-AINT(xda)
        v=yda-AINT(yda)
        ip=i+(j-1)*im
        x1=x(ip)
        x2=x(ip+1)
        x3=x(ip+im+1)
        x4=x(ip+im)
        y1=y(ip)
        y2=y(ip+1)
        y3=y(ip+im+1)
        y4=y(ip+im)
        xus=(c1-u)*(c1-v)*x1+u*(c1-v)*x2+u*v*x3+v*(c1-u)*x4
        yus=(c1-u)*(c1-v)*y1+u*(c1-v)*y2+u*v*y3+v*(c1-u)*y4
!
!-----------------------------------------------------------------------
!  No mapping.
!-----------------------------------------------------------------------
!
      ELSE
        xus=xda
        yus=yda
      END IF
!
!-----------------------------------------------------------------------
!  Check for out of bound conditions.
!-----------------------------------------------------------------------
!
      IF ((xus.lt.wxmn).or.(xus.gt.wxmx).or.                            &
     &    (yus.lt.wymn).or.(yus.gt.wymx)) THEN
        ist=-1
      END IF

      RETURN
      END

      SUBROUTINE stuixy (xus,yus,xda,yda,ist)
!
!=======================================================================
!                                                                      !
!  This routine is used by streamlines for inversely transforming a    !
!  point in user coordinate space to data space.                       !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xus,yus   Point in user coordinate space.                        !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     xda,yda   Point in data coordinate space.                        !
!     ist       Status code indicating success or failure.             !
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
!
! The mapping common block: made available to user mapping routines.
!
      common /stmap/                                                    &
     &                imap       ,lnlg       ,invx       ,invy       ,  &
     &                xlov       ,xhiv       ,ylov       ,yhiv       ,  &
     &                wxmn       ,wxmx       ,wymn       ,wymx       ,  &
     &                xvpl       ,xvpr       ,yvpb       ,yvpt       ,  &
     &                xgds       ,ygds       ,nxct       ,nyct       ,  &
     &                itrt       ,fw2w       ,fh2h       ,              &
     &                dfmg       ,vnml       ,rbig       ,ibig
      integer ibig, imap, invx, invy, itrt, lnlg, nxct, nyct
      real dfmg, fh2h, fw2w, rbig, vnml, wxmn, wymn, wxmx, wymx, xgds,  &
     &     xhiv, xlov, xvpl, xvpr, ygds, yhiv, ylov, yvpb, yvpt
      save /stmap/
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer i, ip, ist, j
      real u, v, x1, x2, x3, x4, xda, xus, y1, y2, y3, y4, yda, yus
!
!  Math constants:
!
      real pdtor, prtod, p1xpi, p2xpi, p1d2pi, p5d2pi
      parameter (pdtor=0.017453292519943, prtod=57.2957795130823,       &
     &           p1xpi=3.14159265358979, p2xpi=6.28318530717959,        &
     &           p1d2pi=1.57079632679489, p5d2pi=7.85398163397448)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      ist=0
!
!-----------------------------------------------------------------------
!  Spherical EZMAP mapping projections.
!-----------------------------------------------------------------------
!
      IF (imap.eq.1) THEN
        CALL maptri (xus,yus,yda,xda)
        IF ((xda.eq.1.e12).or.(yda.eq.1.e12)) THEN
          ist=-1
          RETURN
        END IF
!
!-----------------------------------------------------------------------
!  Polar coordinates mapping.
!-----------------------------------------------------------------------
!
!  This mapping has special bounds checking requirements. If YDA is less
!  than YLOV, it is possible the user intends to treat some or all of
!  quandrant 3 and 4 as positive angles (unlike ATAN2). Add 360 in this
!  case.
!
      ELSE IF (imap.eq.2) THEN
        xda=SQRT(xus*xus+yus*yus)
        yda=prtod*ATAN2(yus,xus)
        IF (yda.lt.ylov) yda=yda+360.0
!
!-----------------------------------------------------------------------
!  Curvilinear coordinates.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.3) THEN
        CALL maptri (xus,yus,yda,xda)
        IF ((xda.eq.1.e12).or.(yda.eq.1.e12)) THEN
          ist=-1
          RETURN
        END IF
!
!  Set the (i,j) indices of the 3D grid cell cointaining the (x,y) to
!  horizontally interpolate.
!
        CALL hindices (i,j,xda,yda,1,x,y,im,jm)
        IF (i.eq.0) THEN
          ist=-1
          RETURN
        END IF
!
!  Interpolate.
!
        ip=i+(j-1)*im
        x1=x(ip)
        x2=x(ip+1)
        x3=x(ip+im+1)
        x4=x(ip+im)
        y1=y(ip)
        y2=y(ip+1)
        y3=y(ip+im+1)
        y4=y(ip+im)
        u=xda-AINT(xda)
        v=yda-AINT(yda)
!!      xda=(c1-u)*(c1-v)*x1+u*(c1-v)*x2+u*v*x3+v*(c1-u)*x4
!!      yda=(c1-u)*(c1-v)*y1+u*(c1-v)*y2+u*v*y3+v*(c1-u)*y4
        xda=FLOAT(i)+(xda-x1)/(x2-x1)
        yda=FLOAT(j)+(yda-y1)/(y4-y1)
!
!-----------------------------------------------------------------------
!  No maping.
!-----------------------------------------------------------------------
!
      ELSE
        xda=xus
        yda=yus
      END IF
!
!-----------------------------------------------------------------------
!  Check for out of bound conditions.
!-----------------------------------------------------------------------
!
      IF ((xda.lt.MIN(xlov,xhiv)).or.(xda.gt.MAX(xhiv,xlov)).or.        &
     &    (yda.lt.MIN(ylov,yhiv)).or.(yda.gt.MAX(yhiv,ylov))) THEN
        ist=-1
      END IF

      RETURN
      END

      SUBROUTINE stumta (xda,yda,xus,yus,xnd,ynd,du,dv,ta,ist)
!
!=======================================================================
!                                                                      !
!  This routine is used by streamlines for inversely transforming a    !
!  point in user coordinate space to data space.                       !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xda,yda   Point in data coordinate space.                        !
!     xus,yus   Point in user coordinate space.                        !
!     xnd,ynd   Point in NDC space.                                    !
!     du,dv     Differential vector components in data space.          !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     ta        Streamline tangent angle in NDC space.                 !
!     ist       Status code indicating success or failure.             !
!                                                                      !
!  In general the distance over which the angle is measured is         !
!  iteratively adjusted until it is becomes a small (but not too       !
!  small) fraction of the fixed (in NDC space) increment distance.     !
!  The transformation type parameter is implemented for the polar      !
!  mapping only.                                                       !
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
!
! The mapping common block: made available to user mapping routines.
!
      common /stmap/                                                    &
     &                imap       ,lnlg       ,invx       ,invy       ,  &
     &                xlov       ,xhiv       ,ylov       ,yhiv       ,  &
     &                wxmn       ,wxmx       ,wymn       ,wymx       ,  &
     &                xvpl       ,xvpr       ,yvpb       ,yvpt       ,  &
     &                xgds       ,ygds       ,nxct       ,nyct       ,  &
     &                itrt       ,fw2w       ,fh2h       ,              &
     &                dfmg       ,vnml       ,rbig       ,ibig
      integer ibig, imap, invx, invy, itrt, lnlg, nxct, nyct
      real dfmg, fh2h, fw2w, rbig, vnml, wxmn, wymn, wxmx, wymx, xgds,  &
     &     xhiv, xlov, xvpl, xvpr, ygds, yhiv, ylov, yvpb, yvpt
      save /stmap/
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer ist
      real du, dv, ta, xda, xnd, xus, yda, ynd, yus
!
!  Math constants:
!
      real pdtor, prtod, p1xpi, p2xpi, p1d2pi, p5d2pi
      parameter (pdtor=0.017453292519943, prtod=57.2957795130823,       &
     &           p1xpi=3.14159265358979, p2xpi=6.28318530717959,        &
     &           p1d2pi=1.57079632679489, p5d2pi=7.85398163397448)
!
!  Local parameters:
!
!  PRCFAC - Precision factor used to resolve float equality within
!            the precision of a 4 byte REAL.
!  PVFRAC - Initial fraction of the vector magnitude used to
!            determine the differential increment.
!  IPMXCT - Number of times to allow the differential to increase.
!  PDUVML - Multiplier when the differential is increased.
!  PCSTST - Test value for closeness to 90 degree latitude.
!
      integer ipmxct
      real pcstst, pduvml, prcfac, pvfrac
      parameter (ipmxct=40, prcfac=1e5, pvfrac=0.1, pduvml=2.0,         &
     &           pcstst=prcfac*90.0)
!
!  Local variables:
!
!  XE,YE   - Current end of the temporary vector (data space).
!  XU2,YU2 - Current end of the temporary vector (user space).
!  DU2,DV2 - Differential vector components in user space.
!  TH      - Angle theta
!
      real xu2, yu2, xe, ye, du2, dv2, th
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      ist=0
!
!-----------------------------------------------------------------------
!  No mapping.
!-----------------------------------------------------------------------
!
      IF ((imap.eq.0).or.(imap.eq.4)) THEN
!
!  Calculate the angle
!
        ta=ATAN2(dv,du)
!
!-----------------------------------------------------------------------
!  Spherical mapping projections.
!-----------------------------------------------------------------------
!
      ELSE IF ((imap.eq.1).or.(imap.eq.3)) THEN
!
!  X is longitude, Y is latitude. If Y is 90 degrees, cannot compute a
!  direction
!
        xe=xda+du
        ye=yda+dv
        CALL stumxy (xe,ye,xu2,yu2,ist)
        IF (ist.ne.0) RETURN
        du2=xu2-xus
        dv2=yu2-yus
        ta=ATAN2(dv2,du2)
!
!-----------------------------------------------------------------------
!  Polar coordinates mapping.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.2) THEN
        ta=ATAN2(dv,du)
        IF (itrt.ge.1) THEN
          th=pdtor*yda
          ta=th+ta
        END IF
      END IF

      RETURN
      END
