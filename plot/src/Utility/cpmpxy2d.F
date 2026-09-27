      SUBROUTINE cpmpxy(imap,xinp,yinp,xotp,yotp)
!
!svn $Id: cpmpxy2d.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine is used to locate contours in the map defined by       !
!  the user.                                                           !
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
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i,ip,j,imap
      real u,v,xinp,xotp,x1,x2,x3,x4,yinp,yotp,y1,y2,y3,y4
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  The incoming XINP and YINP coordinates are assumed to represent
!  longitude and latitude, respectively.  The EZMAP routine MAPTRN
!  is called to find the X- and Y-coordinates of the projection of
!  the specified point on the globe,  and those coordinates are
!  RETURNed as the outgoing XOTP and YOTP.
!-----------------------------------------------------------------------
!
      IF (imap.eq.1) THEN
        CALL maptrn (yinp,xinp,xotp,yotp)
!
!-----------------------------------------------------------------------
!  The incoming XINP and YINP coordinates are assumed to represent
!  RHO and THETA (in degrees) in polar coordinates. The output XINP
!  and YINP coordinates are computed from these.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.2) THEN
        xotp=xinp*COS(0.017453292519943*yinp)
        yotp=xinp*SIN(0.017453292519943*yinp)
!
!-----------------------------------------------------------------------
!  User spherical coordinate mapping.  Interpolate from User grid.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.3) THEN
        i=int(xinp)
        j=int(yinp)
        u=xinp-aint(xinp)
        v=yinp-aint(yinp)
        ip=i+(j-1)*im
        CALL maptrn (y(ip),x(ip),x1,y1)
        CALL maptrn (y(ip+1),x(ip+1),x2,y2)
        CALL maptrn (y(ip+im+1),x(ip+im+1),x3,y3)
        CALL maptrn (y(ip+im),x(ip+im),x4,y4)
        xotp=(c1-u)*(c1-v)*x1+u*(c1-v)*x2+u*v*x3+v*(c1-u)*x4
        yotp=(c1-u)*(c1-v)*y1+u*(c1-v)*y2+u*v*y3+v*(c1-u)*y4
!
!-----------------------------------------------------------------------
!  User Cartesian coordinate mapping.  Interpolate from User grid.
!-----------------------------------------------------------------------
!
      ELSE IF (imap.eq.4) THEN
        i=int(xinp)
        j=int(yinp)
        u=xinp-aint(xinp)
        v=yinp-aint(yinp)
        ip=i+(j-1)*im
        x1=x(ip)
        x2=x(ip+1)
        x3=x(ip+im+1)
        x4=x(ip+im)
        y1=y(ip)
        y2=y(ip+1)
        y3=y(ip+im+1)
        y4=y(ip+im)
        xotp=(c1-u)*(c1-v)*x1+u*(c1-v)*x2+u*v*x3+v*(c1-u)*x4
        yotp=(c1-u)*(c1-v)*y1+u*(c1-v)*y2+u*v*y3+v*(c1-u)*y4
!
!-----------------------------------------------------------------------
!  If IMAP is anything else, the input XINP and YINP coordinates are
!  simply RETURNed.
!-----------------------------------------------------------------------
!
      ELSE
        xotp=xinp
        yotp=yinp
      END IF

      RETURN
      END
