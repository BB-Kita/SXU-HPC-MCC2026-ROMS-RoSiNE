      SUBROUTINE bernoulli (B,rho,u,v,zeta,z_w,z_r,ziso,L,M,N,level)
!
!svn $Id: bernoulli.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the Bernoulli function.  It integrates the    !
!  density anomaly from the bottom  or  from the depth of isopycnic    !
!  the surface.                                                        !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     rho      Density anomaly.                                        !
!     zeta     Free-surface (m).                                       !
!     z_w      Depths at W-points (m).                                 !
!     z_w      Depths at RHO-points (m).                               !
!     ziso     Depths of requested isopycnal (m), if any.              !
!     L        Number of points in the XI-direction.                   !
!     M        Number of points in the ETA-direction.                  !
!     N        Number of points in the S-direction.                    !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real).                                         !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     B        Bernoulli function (Pa).                                !
!                                                                      !
!              B = rho0*g*zeta + g * INT[(rho - rho_ref)]              !
!                                    from z(rho_ref) to surface        !
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
#include "domdat.h"
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer L, M, N, i, j, k
      real Riso, Uiso, Viso, d1, d2, dz1, dz2, level, ri, rref, sum
      real rk(NK), uk(NK), vk(NK), zk(NK)
      real B(0:L,0:M), rho(0:L,0:M,N), u(1:L,0:M,N), v(0:L,1:M,N),      &
     &     zeta(0:L,0:M), ziso(0:L,0:M), z_r(0:L,0:M,N),                &
     &     z_w(0:L,0:M,0:N)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Compute free-surface contribution (kg/s2/m)
!
      DO j=0,M
        DO i=0,L
          B(i,j)=rho0*grav*zeta(i,j)
        END DO
      END DO
!
!  Compute term rho*(u*u+v*v)/2 at interior RHO-points (kg/s2/m).
!
      IF (level.ge.c1000) THEN
        DO j=1,M-1
          DO i=1,L-1
            DO k=1,N
              rk(k)=rho(i,j,k)
              uk(k)=p5*(u(i,j,k)+u(i+1,j,k))
              vk(k)=p5*(v(i,j,k)+v(i,j+1,k))
              zk(k)=z_r(i,j,k)
            END DO
            IF ((zk(1).le.ziso(i,j)).and.(ziso(i,j).le.zk(N))) THEN
              CALL lintrp (N,zk,rk,1,ziso(i,j),Riso)
              CALL lintrp (N,zk,uk,1,ziso(i,j),Uiso)
              CALL lintrp (N,zk,vk,1,ziso(i,j),Viso)
              B(i,j)=B(i,j)+p5*(Riso+c1000)*(Uiso*Uiso+Viso*Viso)
            END IF
          END DO
        END DO
      END IF
!
!  Compute pressure term (kg/s2/m). Integerage density from requested
!  isopycnal depths.
!
      IF (level.ge.c1000) THEN
        rref=level-c1000
        DO j=0,M
          DO i=0,L
            IF (ziso(i,j).lt.z_r(i,j,N)) THEN
              dz2=z_w(i,j,N)-z_r(i,j,N)
              sum=(rho(i,j,N)-rref)*dz2
              DO k=N-1,1,-1
                IF (ziso(i,j).lt.z_r(i,j,k)) THEN
                  dz1=z_r(i,j,k+1)-z_w(i,j,k)
                  dz2=z_w(i,j,k  )-z_r(i,j,k)
                  sum=sum+(rho(i,j,k+1)-rref)*dz1+                      &
     &                    (rho(i,j,k  )-rref)*dz2
                ELSE
                  d1=z_r(i,j,k+1)-ziso(i,j)
                  d2=ziso(i,j)-z_r(i,j,k)
                  ri=(d1*rho(i,j,k)+d2*rho(i,j,k+1))/(d1+d2)
                  dz1=z_r(i,j,k)-ziso(i,j)
                  sum=sum+(ri-rref)*dz1
                  GO TO 10
                END IF
              END DO
  10          B(i,j)=B(i,j)+grav*sum
            ELSE
              B(i,j)=spval1
            END IF
          END DO
        END DO
!
!  Integerage density from requested depth.
!
      ELSE IF (level.lt.c0) THEN
        rref=c0
        DO j=0,M
          DO i=0,L
            IF (level.lt.z_r(i,j,N)) THEN
              sum=(z_w(i,j,N)-z_r(i,j,N))*(rho(i,j,N)-rref)
              DO k=N-1,1,-1
                IF (level.lt.z_r(i,j,k)) THEN
                  sum=sum+                                              &
     &                (z_r(i,j,k+1)-z_w(i,j,k))*(rho(i,j,k+1)-rref)+    &
     &                (z_w(i,j,k  )-z_r(i,j,k))*(rho(i,j,k  )-rref)
                ELSE
                  d1=z_r(i,j,k+1)-level
                  d2=level-z_r(i,j,k)
                  ri=(d1*rho(i,j,k)+d2*rho(i,j,k+1))/(d1+d2)
                  sum=sum+                                              &
     &                (z_r(i,j,k)-ziso(i,j))*(ri-rref)
                  GO TO 20
                END IF
              END DO
  20          B(i,j)=B(i,j)+grav*sum
            ELSE
              B(i,j)=spval1
            END IF
          END DO
        END DO
      END IF

      RETURN
      END
