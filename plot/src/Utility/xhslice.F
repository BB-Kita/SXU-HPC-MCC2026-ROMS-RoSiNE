      SUBROUTINE xhslice (f2d,depth,z,f3d,im,jm,km,vintrp,spval)
!
!svn $Id: xhslice.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine extract a horizontal slice from a 3D field at the      !
!  requested depth via interpolation.                                  !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     depth    Depth to interpolate.                                   !
!     f3d      3D field.                                               !
!     z        Depths of 3D field.                                     !
!     im       Size of inner dimension.                                !
!     jm       Size of outter dimension.                               !
!     km       Number of vertical levels.                              !
!     vintrp   Interpolation scheme:                                   !
!                vintrp = 0  linear                                    !
!                vintrp = 1  cubic splines                             !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     f2d      Interpolated field.                                     !
!                                                                      !
!  Calls:                                                              !
!                                                                      !
!     linterp                                                          !
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
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer i, im, j, jm, k, km, vintrp
      real der1, derkm, frstd, spval
      real depth(im,jm), f2d(im,jm), f3d(im,jm,km), fk(NK), z(im,jm,km),&
     &     zk(NK), wk(NK)
      parameter (der1=c1ep30,derkm=c1ep30)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Linear Interpolation.
!
      IF (vintrp.eq.0) THEN
        DO j=1,jm
          DO i=1,im
            DO k=1,km
              fk(k)=f3d(i,j,k)
              zk(k)=z(i,j,k)
            END DO
            IF ((zk(1).le.depth(i,j)).and.(depth(i,j).le.zk(km))) THEN
              CALL lintrp (km,zk,fk,1,depth(i,j),f2d(i,j))
            ELSE
              f2d(i,j)=spval
            END IF
          END DO
        END DO
!
!  Cubic spline interpolation.
!
      ELSE IF (vintrp.eq.1) THEN
        DO j=1,jm
          DO i=1,im
            DO k=1,km
              fk(k)=f3d(i,j,k)
              zk(k)=z(i,j,k)
            END DO
            CALL spline (zk,fk,km,der1,derkm,wk)
            IF ((zk(1).le.depth(i,j)).and.(depth(i,j).le.zk(km))) THEN
              CALL splint (zk,fk,wk,km,depth(i,j),f2d(i,j),frstd)
            ELSE
              f2d(i,j)=spval
            END IF
          END DO
        END DO
      END IF

      RETURN
      END
