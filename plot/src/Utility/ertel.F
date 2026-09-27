      SUBROUTINE ertel (pv,L,M,N,u,v,rho,f,pm,pn,z_r,wk1,wk2)
!
!svn $Id: ertel.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes Ertel potential vorticity.                    !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     L        Number of grid points in the XI-direction.              !
!     M        Number of grid points in the ETA-direction.             !
!     N        Number of vertical levels.                              !
!     u        Total velocity component (m/s) in the XI-direction.     !
!     v        Total velocity component (m/s) in the ETA-direction.    !
!     rho      Density anomaly perturbation (kg/m^3).                  !
!     f        Coriolis parameter (1/s).                               !
!     pm       Coordinate transformation metric "m" (1/meters)         !
!              associated with the differential distances in XI.       !
!     pn       Coordinate transformation metric "n" (1/meters)         !
!              associated with the differential distances in ETA.      !
!     z_r      Actual depths (m) at RHO-points.                        !
!     wk1      Utility work array.                                     !
!     wk2      Utility work array.                                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     pv       Ertel potential vorticity (1/s/m).                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global variables.
!-----------------------------------------------------------------------
!
      implicit none
#include "domdat.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer L, Lm, M, Mm, N, Nm, i, j, k
      real c1, p5, p25, p125
      real f(0:L,0:M), pm(0:L,0:M), pn(0:L,0:M), pv(L-1,M-1,N-1),       &
     &     rho(0:L,0:M,N), u(L,0:M,N), v(0:L,M,N), wk1(0:L,0:M,N),      &
     &     wk2(0:L,0:M,N), z_r(0:L,0:M,N)
      parameter (c1=1.0, p5=0.5, p25=0.25, p125=0.125)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      Lm=L-1
      Mm=M-1
      Nm=N-1
!
!-----------------------------------------------------------------------
!  Ertel potential vorticity, term 1: [f + (dv/dx - du/dy)]*drho/dz
!-----------------------------------------------------------------------
!
!  Compute d(v)/d(xi) at PSI-points and store in "wk1".
!
      DO k=1,N
        DO j=1,M
          DO i=1,L
            wk1(i,j,k)=(v(i,j,k)-v(i-1,j,k))*                           &
     &                 p25*(pm(i,j)+pm(i-1,j)+pm(i,j-1)+pm(i-1,j-1))
          END DO
        END DO
      END DO
!
!  Compute d(u)/d(eta) at PSI-points and store in "wk2".
!
      DO k=1,N
        DO j=1,M
          DO i=1,L
            wk2(i,j,k)=(u(i,j,k)-u(i,j-1,k))*                           &
     &                 p25*(pn(i,j)+pn(i-1,j)+pn(i,j-1)+pn(i-1,j-1))
            END DO
         END DO
      END DO
!
!  Compute Ertel potential vorticity at horizontal RHO-points and
!  vertical W-points.
!
      DO k=1,Nm
        DO j=1,Mm
          DO i=1,Lm
            pv(i,j,k)=(f(i,j)+                                          &
     &                 p125*(wk1(i  ,j  ,k  )-wk2(i  ,j  ,k  )+         &
     &                       wk1(i  ,j  ,k+1)-wk2(i  ,j  ,k+1)+         &
     &                       wk1(i+1,j  ,k  )-wk2(i+1,j  ,k  )+         &
     &                       wk1(i+1,j  ,k+1)-wk2(i+1,j  ,k+1)+         &
     &                       wk1(i  ,j+1,k  )-wk2(i  ,j+1,k  )+         &
     &                       wk1(i  ,j+1,k+1)-wk2(i  ,j+1,k+1)+         &
     &                       wk1(i+1,j+1,k  )-wk2(i+1,j+1,k  )+         &
     &                       wk1(i+1,j+1,k+1)-wk2(i+1,j+1,k+1)))*       &
     &                (rho(i,j,k+1)-rho(i,j,k))/                        &
     &                (z_r(i,j,k+1)-z_r(i,j,k))
          END DO
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Ertel potential vorticity, term 2: (dv/dz)*(drho/dx)
!-----------------------------------------------------------------------
!
!  Compute d(v)/d(z) at horizontal V-points and vertical W-points
!  and store in "wk1".
!
      DO k=1,Nm
        DO j=1,M
          DO i=0,L
            wk1(i,j,k)=(v(i,j,k+1)-v(i,j,k))/                           &
     &                 (p5*((z_r(i,j,k+1)+z_r(i,j-1,k+1))-              &
     &                      (z_r(i,j,k  )+z_r(i,j-1,k  ))))
          END DO
        END DO
      END DO
!
!  Compute d(rho)/d(xi) at horizontal U-points and vertical RHO-points
!  and store in "wk2".
!
      DO k=1,N
        DO j=0,M
          DO i=1,L
            wk2(i,j,k)=(rho(i,j,k)-rho(i-1,j,k))*                       &
     &                 p5*(pm(i,j)+pm(i-1,j))
          END DO
        END DO
      END DO
!
!  Add in term 2 contribution to Ertel potential vorticity.
!
      DO k=1,Nm
        DO j=1,Mm
          DO i=1,Lm
            pv(i,j,k)=pv(i,j,k)-                                        &
     &                p5*(wk1(i,j,k)+wk1(i,j+1,k))*                     &
     &                p25*(wk2(i,j,k  )+wk2(i+1,j,k  )+                 &
     &                     wk2(i,j,k+1)+wk2(i+1,j,k+1))
          END DO
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Ertel potential vorticity, term 3: (du/dz)*(drho/dy)
!-----------------------------------------------------------------------
!
!  Compute d(u)/d(z) at horizontal U-points and vertical W-points
!  and store in "wk1".
!
      DO k=1,Nm
        DO j=0,M
          DO i=1,L
            wk1(i,j,k)=(u(i,j,k+1)-u(i,j,k))/                           &
     &                 (p5*((z_r(i,j,k+1)+z_r(i-1,j,k+1))-              &
     &                      (z_r(i,j,k  )+z_r(i-1,j,k  ))))
          END DO
        END DO
      END DO
!
!  Compute d(rho)/d(eta) at horizontal V-points and vertical RHO-points
!  and store in "wk2".
!
      DO k=1,N
        DO j=1,M
          DO i=0,L
            wk2(i,j,k)=(rho(i,j,k)-rho(i,j-1,k))*                       &
     &                 p5*(pn(i,j)+pn(i,j-1))
          END DO
        END DO
      END DO
!
!  Add in term 3 contribution to Ertel potential vorticity.
!
      DO k=1,Nm
        DO j=1,Mm
          DO i=1,Lm
            pv(i,j,k)=pv(i,j,k)+                                        &
     &                p5*(wk1(i,j,k)+wk1(i+1,j,k))*                     &
     &                p25*(wk2(i,j,k  )+wk2(i,j+1,k  )+                 &
     &                     wk2(i,j,k+1)+wk2(i,j+1,k+1))
          END DO
        END DO
      END DO
!
!  Divide by RHO0. Set depths positions.
!
      DO k=1,Nm
        DO j=1,Mm
          DO i=1,Lm
            pv(i,j,k)=pv(i,j,k)/rho0
          END DO
        END DO
      END DO

      RETURN
      END
