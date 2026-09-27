      SUBROUTINE cvector (u,v,im,jm,km,uu,imu,jmu,vv,imv,jmv,igrid,     &
     &                    ncid,gotangle)
!
!svn $Id: cvector.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes vector components at density-points from      !
!  their staggered locations.                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     km       Number of vertical levels.                              !
!     uu       Vector component in the XI-direction at U-points.       !
!     imu      Size of "uu" inner dimension.                           !
!     jmu      Size of "uu" outer dimension.                           !
!     vv       Vector component in the ETA-direction at V-points.      !
!     imv      Size of "vv" inner dimension.                           !
!     jmv      Size of "vv" outer dimension.                           !
!     igrid    Staggered C-grid location:                              !
!              igrid=1  => density points.                             !
!              igrid=2  => streamfunction points.                      !
!              igrid=7  => interior density points.                    !
!     ncid     NetCDF ID for input file (integer).                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     u        Vector component in the XI-direction at RHO-points.     !
!     v        Vector component in the ETA-direction at RHO-points.    !
!     im       Size of "u" and "v" inner dimension.                    !
!     jm       Size of "u" and "v" outer dimension.                    !
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
#include "pltfld.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      logical gotangle
      integer angid, i, igrid, ij, im, imr, imu, imv, irgrid, j, jm,    &
     &        jmr, jmu, jmv, k, km, n, ncid
      real angle, spv, unew, vnew
      real u(*), uu(imu,jmu,km), v(*), vv(imv,jmv,km)
      character*60  angnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Vector component in the XI-direction.
!
      IF (igrid.eq.1) THEN
        im=imu
        jm=jmu
        n=0
        DO k=1,km
          DO j=1,jmu
            DO i=1,imu
              n=n+1
              u(n)=uu(i,j,k)
            END DO
          END DO
        END DO
      ELSE IF (igrid.eq.2) THEN
        im=imu
        jm=jmv
        n=0
        DO k=1,km
          DO j=2,jmu
            DO i=1,imu
              n=n+1
              u(n)=p5*(uu(i,j-1,k)+uu(i,j,k))
            END DO
          END DO
        END DO
      ELSE IF (igrid.eq.7) THEN
        im=imu-1
        jm=jmv-1
        n=0
        DO k=1,km
          DO j=2,jmu-1
            DO i=1,imu-1
              n=n+1
              u(n)=p5*(uu(i,j,k)+uu(i+1,j,k))
            END DO
          END DO
        END DO
      END IF
!
!  Vector component in the ETA-direction.
!
      IF (igrid.eq.1) THEN
        n=0
        DO k=1,km
          DO j=1,jmv
            DO i=1,imv
              n=n+1
              v(n)=vv(i,j,k)
            END DO
          END DO
        END DO
      ELSE IF (igrid.eq.2) THEN
        n=0
        DO k=1,km
          DO j=1,jmv
            DO i=2,imv
              n=n+1
              v(n)=p5*(vv(i-1,j,k)+vv(i,j,k))
            END DO
          END DO
        END DO
      ELSE IF (igrid.eq.7) THEN
        n=0
        DO k=1,km
          DO j=1,jmv-1
            DO i=2,imv-1
              n=n+1
              v(n)=p5*(vv(i,j,k)+vv(i,j+1,k))
            END DO
          END DO
        END DO
      END IF
!
!  If appropriate, rotate vectors from (XI,ETA) to (lon,lat).
!
      IF (spherical.and.gotangle) THEN
        angnam='angle'
        CALL get_nc2dat (theta,NH,imr,jmr,ncid,ncid,0,angnam,0,irgrid,  &
     &                   angid,spv)
        IF (igrid.eq.1) THEN
          n=0
          DO k=1,km
            DO j=1,jmr
              DO i=1,imr
                n=n+1
                IF ((u(n).ne.spval1).and.(v(n).ne.spval1)) THEN
                  ij=i+(j-1)*imr
                  unew=u(n)*COS(theta(ij))-v(n)*SIN(theta(ij))
                  vnew=u(n)*SIN(theta(ij))+v(n)*COS(theta(ij))
                  u(n)=unew
                  v(n)=vnew
                END IF
              END DO
            END DO
          END DO
        ELSE IF (igrid.eq.2) THEN
          n=0
          DO k=1,km
            DO j=2,jmr
              DO i=2,imr
                n=n+1
                ij=i+(j-1)*imr
                angle=p25*(theta(ij-1)+theta(ij)+                       &
     &                     theta(ij-1+imr)+theta(ij+imr))
                unew=u(n)*COS(angle)-v(n)*SIN(angle)
                vnew=u(n)*SIN(angle)+v(n)*COS(angle)
                u(n)=unew
                v(n)=vnew
              END DO
            END DO
          END DO
        ELSE IF (igrid.eq.7) THEN
          n=0
          DO k=1,km
            DO j=2,jmr-1
              DO i=2,imr-1
                n=n+1
                IF ((u(n).ne.spval1).and.(v(n).ne.spval1)) THEN
                  ij=i+(j-1)*imr
                  unew=u(n)*COS(theta(ij))-v(n)*SIN(theta(ij))
                  vnew=u(n)*SIN(theta(ij))+v(n)*COS(theta(ij))
                  u(n)=unew
                  v(n)=vnew
                END IF
              END DO
            END DO
          END DO
        END IF
      END IF

      RETURN
      END
