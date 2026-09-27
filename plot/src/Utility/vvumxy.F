      SUBROUTINE vvumxy (xp,yp,u,v,uvm,xb,yb,xe,ye,ist)
!
!svn $Id: vvumxy.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This is a user modifiable routine that allows custom projections    !
!  of the vector space. X and Y give the vector position within the    !
!  domain of the data space.  By default,  this space is coincident    !
!  with the grid space (that is, 1 through dimension lengths of the    !
!  U and V arrays).  The vector end-points are output in fractional    !
!  coordinates (NDC space).                                            !
!  Note that this is different from the old  MXF and  MYF routines,    !
!  which output in 'plotter coordinate' space. It also differs from    !
!  the Conpack routine CPMPXY, which returns values in user space.     !
!                                                                      !
!  VVUMXY (Velocity Vector -- User Map X, Y) is called whenever the    !
!  internal parameter MAP is set to a value other than 0 to 4.         !
!                                                                      !
!  Based on the magnitude and direction of the vector the start and    !
!  ending points of the vector are RETURNed in NDC space.              !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xp,yp    Vector position in the user coordinate system           !
!     u,v      Vector components from the U,V arrays for this          !
!              position.                                               !
!     uvm      Magnitude of the U,V components (supplied for           !
!              convenience and efficiency - but note that many         !
!              mappings do not need yhis value).                       !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     xb,yb    Starting point of the vector in fractional              !
!              coordinates (NDC space).                                !
!     xe,ye    ENDing point of the vector in fractional                !
!              coordinates (NDC space).                                !
!     ist      Status results of the mapping: 0 indicates success;     !
!              any non-zero value causes  VVECTR  to discard the       !
!              vector at this location.                                !
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
#include "fields.h"
#include "ndimen.h"
!
!  The mapping common block: made available to user mapping routines.
!  NOTE: all these variables should be considered read-only by VVUMXY.
!
      common /vvmap/                                                    &
     &                imap       ,                                      &
     &                xvpl       ,xvpr       ,yvpb       ,yvpt       ,  &
     &                wxmn       ,wxmx       ,wymn       ,wymx       ,  &
     &                xlov       ,xhiv       ,ylov       ,yhiv       ,  &
     &                sxdc       ,sydc       ,nxct       ,nyct       ,  &
     &                rlen       ,lnlg       ,invx       ,invy       ,  &
     &                itrt       ,iwct       ,fw2w       ,fh2h       ,  &
     &                dvmn       ,dvmx       ,rbig       ,ibig
      integer ibig, imap, invx, invy, itrt, iwct, lnlg, nxct, nyct
      real dvmn, dvmx, fw2w, fh2h, rbig, rlen, sxdc, sydc, wxmn, wxmx,  &
     &     wymn, wymx, xlov, xhiv, xvpl, xvpr, ylov, yhiv, yvpb, yvpt
      save /vvmap/
!
!  Description of VVMAP contents:
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
!  sxdc,sydc           - Scaling factors for converting vector component
!                        values into lengths in NDC space.
!  nxct,nyct           - Length of each dimension of the U and V
!                        component arrays.
!  rlen                - Length of the maximum vector in user
!                        coordinates.
!  lnlg                - The linear/log mode (GETSET argument 9)
!  invx,invy           - User coordinates inversion flags:
!                        0 - not inverted, 1 - inverted
!  itrt                - value of the internal parameter TRT
!  iwct                - not currently used
!  fw2w,fh2h           - scale factors for converting from fraction of
!                        viewport width/height to NDC width/height
!  dvmn,dvmx           - min/max vector lengths in NDC
!  rbig,ibig           - machine dependent maximum REAL/INTEGER values
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer i, ict, ip, ist, j
      real clt, duv, dv1, s, sgn, t, u, uvm, v, xb, xe, xp, xt, xvt, x1,&
     &     x2, x3, x4, yb, ye, yp, yt, yvt, y1, y2, y3, y4
      real cufx, cufy
!
!  Math constants:
!
      real pdtor, prtod, p1xpi, p2xpi, p1d2pi, p5d2pi
      parameter (pdtor=0.017453292519943, prtod=57.2957795130823,       &
     &           p1xpi=3.14159265358979, p2xpi=6.28318530717959,        &
     &           p1d2pi=1.57079632679489, p5d2pi=7.85398163397448)
!
!  Local parameters (used for the Ezmap projection only):
!
!  prcfac - Precision factor used to resolve float equality within
!           the precision of a 4 byte REAL.
!  pvfrac - Initial fraction of the vector magnitude used to
!           determine the differential increment.
!  pfovfl - Floating point overflow value.
!  ipmxct - Number of times to allow the differential to increase.
!  pduvml - Multiplier when the differential is increased.
!  pcstst - Test value for closeness to 90 degree latitude.
!
      integer ipmxct, ipctst
      real prcfac, pvfrac, pfovfl, pduvml
      parameter (prcfac=1E5, pvfrac=0.001, pfovfl=1E12, ipmxct=40,      &
     &           pduvml=2.0, ipctst=prcfac*90)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      ist=0
!
!-----------------------------------------------------------------------
!  User spherical coordinate mapping.  Interpolate from User grid.
!-----------------------------------------------------------------------
!
      IF (imap.eq.3) THEN
        i=int(xp)
        j=int(yp)
!
!  IM and JM contain the number of elements along each coordinate
!  axis. Therefore the following test ensures that I and J are within
!  the domain of the array dimensions.
!
        IF ((i.lt.1).or.(i.gt.im).or.(j.lt.1).or.(j.gt.jm)) THEN
          ist=-1
          RETURN
        END IF
!
!  Interpolate vector starting position (xb,yb).
!
        s=xp-aint(xp)
        t=yp-aint(yp)
        ip=i+(j-1)*im
        x1=x(ip)
        x2=x(ip+1)
        x3=x(ip+im+1)
        x4=x(ip+im)
        y1=y(ip)
        y2=y(ip+1)
        y3=y(ip+im+1)
        y4=y(ip+im)
        xvt=(c1-s)*(c1-t)*x1+s*(c1-t)*x2+s*t*x3+t*(c1-s)*x4
        yvt=(c1-s)*(c1-t)*y1+s*(c1-t)*y2+s*t*y3+t*(c1-s)*y4
!
!  The following code is adapted from the Ezmap projection code in
!  VVMPXY. An iterative technique is used that handles most vectors
!  arbitrarily close to the projection limb.  XT is longitude,
!  YT is latitude.
!
!  Test for 90 degree latitude.
!
        IF (ifix(abs(yvt)*prcfac+0.5).eq.ipctst) THEN
          ist=-1
          RETURN
        END IF
!
!  Project the starting value: bail out if outside the window.
!
        CALL maptra (yvt,xvt,xb,yb)
        IF ((xb.lt.wxmn).or.(xb.gt.wxmx).or.                            &
     &      (yb.lt.wymn).or.(yb.gt.wymx)) THEN
          ist=-5
          RETURN
        END IF
!
!  Check the vector magnitude.
!
        IF (IFIX(uvm*prcfac+0.5).eq.0) THEN
          ist=-2
          RETURN
        END IF
!
!  The incremental distance is proportional to a small fraction
!  of the vector magnitude.
!
        duv=pvfrac/uvm
        clt=COS(yvt*pdtor)
!
!  Project the incremental distance. If the positive difference does not
!  work, try the negative difference. If the difference results in a
!  zero length vector, try a number of progressively larger increments.
!
        ict=0
        sgn=1.0
  10    CONTINUE
        CALL maptra (yvt+sgn*v*duv,xvt+sgn*u*duv/clt,xt,yt)
        dv1=SQRT((xt-xb)*(xt-xb)+(yt-yb)*(yt-yb))
        IF (dv1.gt.rlen) THEN
          IF (sgn.eq.-1.0) THEN
            ist=-4
            RETURN
          ELSE
            sgn=-1.0
            GO TO 10
          END IF
        END IF
        IF (IFIX(dv1*prcfac).eq.0) THEN
          IF (ict.lt.ipmxct) THEN
            ict=ict+1
            duv=duv*pduvml
            GO TO 10
          ELSE
            ist=-3
            RETURN
          END IF
        END IF
        IF ((ABS(xt).ge.pfovfl).or.(ABS(yt).ge.pfovfl)) THEN
          ist=-6
          RETURN
        END IF
!
!  Find vector ending position (xe,ye).  Convert starting and ending
!  positions to fractional coordinates.
!
        t=sgn*((xt-xb)/dv1)*uvm
        xb=cufx(xb)
        xe=xb+t*sxdc
        t=sgn*((yt-yb)/dv1)*uvm
        yb=cufy(yb)
        ye=yb+t*sydc
!
!-----------------------------------------------------------------------
!  Curvilinear coordinates mapping.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.4) THEN
        i=INT(xp)
        j=INT(yp)
!
!  IM and JM contain the number of elements along each coordinate
!  axis. Therefore the following test ensures that I and J are within
!  the domain of the array dimensions.
!
        IF ((i.lt.1).or.(i.gt.im).or.(j.lt.1).or.(j.gt.jm)) THEN
          ist=-1
          RETURN
        END IF
!
!  Interpolate vector starting position (xb,yb).
!
        s=xp-AINT(xp)
        t=yp-AINT(yp)
        ip=i+(j-1)*im
        x1=x(ip)
        x2=x(ip+1)
        x3=x(ip+im+1)
        x4=x(ip+im)
        y1=y(ip)
        y2=y(ip+1)
        y3=y(ip+im+1)
        y4=y(ip+im)
        xb=(c1-s)*(c1-t)*x1+s*(c1-t)*x2+s*t*x3+t*(c1-s)*x4
        yb=(c1-s)*(c1-t)*y1+s*(c1-t)*y2+s*t*y3+t*(c1-s)*y4
!
!  Find vector ending position (xe,ye).  Convert starting and ending
!  positions to fractional coordinates.
!
        xb=cufx(xb)
        yb=cufy(yb)
        xe=xb+u*sxdc
        ye=yb+v*sydc
!
!-----------------------------------------------------------------------
!  Default mapping.
!-----------------------------------------------------------------------
!
      ELSE
!
!  WXMN, WXMX, WYMN, and WYMX contain the minimum and maximum values of
!  the user coordinate space. Somewhat inaccurately, the mmenomic 'W'
!  implies window coordinate space, which is usually (but not always)
!  the same as user coordinate space. But note that even when the
!  coordinates are reversed, you are guaranteed that WXMN .lt. WXMX
!  and WYMN .lt. WYMX. This eliminates the need to invoke MIN and MAX.
!
        IF ((xp.lt.wxmn).or.(xp.gt.wxmx).or.                            &
     &      (yp.lt.wymn).or.(yp.gt.wymx)) THEN
          ist=-1
          RETURN
        END IF
        xb=cufx(xp)
        yb=cufy(yp)
        xe=xb+u*sxdc
        ye=yb+v*sydc
      END IF

      RETURN
      END
