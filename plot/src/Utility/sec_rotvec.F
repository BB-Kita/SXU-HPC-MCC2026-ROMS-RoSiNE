      SUBROUTINE sec_rotvec (sec,u,v,im,km,idfld,spval)
!
!svn $Id: sec_rotvec.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine rotates vector components to the requested section     !
!  coordinate.                                                         !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     u       U-vector component (real array).                         !
!     v       V-vector component (real array).                         !
!     im      Number of section points in the XY-plane (integer).      !
!     kms     Number of section points in the Z-direction (integer).   !
!     idfld   Identification of field to read (integer)                !
!     spval   Special value to flag missing data (real).               !
!                                                                      !
!  On Onput:                                                           !
!                                                                      !
!     sec     Requested rotated vector component (real array).         !
!     u       Rotated U-vector component (real array).                 !
!     v       Rotated V-vector component (real array).                 !
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
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, idfld, im, k, km
      real bearing, dist, spval, urot, vrot
      real angle(NX), u(im,km), v(im,km), sec(im,km)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Get rotation angle.
!
      IF (isecpos.eq.2) THEN
        IF ((xsec1.eq.xsec2).or.(ysec1.eq.ysec2)) THEN
          DO i=1,im
            angle(i)=c0
          END DO
        ELSE
          DO i=1,im-1
            CALL geodesic_dist (xsec(i),ysec(i),xsec(i+1),ysec(i+1),4,  &
     &                          dist,bearing)
            angle(i)=(bearing-90)*deg2rad
          END DO
          angle(im)=angle(im-1)
        END IF
      ELSE
        DO i=1,im
          angle(i)=c0
        END DO
      END IF
!
!  Rotate components.
!
      DO k=1,km
        DO i=1,im
          IF ((u(i,k).ne.spval).and.(v(i,k).ne.spval)) THEN
            urot=u(i,k)*COS(angle(i))-v(i,k)*SIN(angle(i))
            vrot=u(i,k)*SIN(angle(i))+v(i,k)*COS(angle(i))
            u(i,k)=urot
            v(i,k)=vrot
          END IF
        END DO
      END DO
!
!  Load requested rotated component.
!
      IF ((vcode(idfld).eq.'IDUTOT').or.                                &
     &    (vcode(idfld).eq.'IDU3DD').or.                                &
     &    (vcode(idfld).eq.'IDUCLI')) THEN
        DO k=1,km
          DO i=1,im
            sec(i,k)=u(i,k)
          END DO
        END DO
      ELSE
        DO k=1,km
          DO i=1,im
            sec(i,k)=v(i,k)
          END DO
        END DO
      END IF

      RETURN
      END
