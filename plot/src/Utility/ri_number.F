      SUBROUTINE ri_number (Rig,bvf,u,v,z_r,im,jm,km)
!
!svn $Id: ri_number.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the gradient Richardson number for vertical   !
!  mixing schemes.                                                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Rig      gradient Richardson number at horizontal RHO-points     !
!              and vertical W-points (nondimensional).                 !
!  On Input:                                                           !
!                                                                      !
!     bvf      Brunt-Vaisala frequency (1/s^2).                        !
!     u        velocity component in the XI-direction (m/s).           !
!     v        velocity component in the ETA-direction (m/s).          !
!     z_r      Depth (m) at RHO-points.                                !
!     im       Number of points in the XI-direction.                   !
!     jm       Number of points in the ETA-direction.                  !
!     km       Number of points in the S-direction.                    !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer i, im, j, jm, k, km
      real Ri_big, Ri_low, dummy1, dummy2, dudz, dvdz, shear2
      real Rig(im-1,jm-1,km-1), bvf(0:im,0:jm,km-1), u(im,0:jm,km),     &
     &     v(0:im,jm,km), z_r(0:im,0:jm,km)
      parameter (Ri_low=c0, Ri_big=c2)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      DO k=1,km-1
        DO j=1,jm-1
          DO i=1,im-1
!
!-----------------------------------------------------------------------
!  Compute horizontal velocity shear squared, (du/dz)^2 + (dv/dz)^2, at
!  horizontal RHO-points and vertical W-points.
!-----------------------------------------------------------------------
!
            dudz=p5*((u(i,j,k+1)-u(i,j,k))+(u(i+1,j,k+1)-u(i+1,j,k)))/  &
     &           (z_r(i,j,k+1)-z_r(i,j,k))
            dvdz=p5*((v(i,j,k+1)-v(i,j,k))+(v(i,j+1,k+1)-v(i,j+1,k)))/  &
     &           (z_r(i,j,k+1)-z_r(i,j,k))
            shear2=dudz*dudz+dvdz*dvdz
!
!-----------------------------------------------------------------------
!  Compute gradient Richardson number at horizontal RHO-points and
!  vertical W-points.  Mask values if zero velocity shear squared.
!  Bound Richardson number by a low a high threshold values.
!-----------------------------------------------------------------------
!
            dummy1=MAX(c1em12,bvf(i,j,k)-c1em9)
            dummy2=MAX(c1em10,shear2)
            Rig(i,j,k)=MAX(Ri_low,MIN(Ri_big,dummy1/dummy2))
          END DO
        END DO
      END DO

      RETURN
      END
