      SUBROUTINE wvelocity (w,L,M,N,u,v,omega,DUon,DVom,z_r,z_w,pm,pn)
!
!svn $Id: wvelocity.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes "true" vertical velocity from terrain-     !
!  following coordinates "omega" vertical velocity.                    !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     L        Number of grid points in the XI-direction.              !
!     M        Number of grid points in the ETA-direction.             !
!     N        Number of vertical levels.                              !
!     u        Total velocity component (m/s) in the XI-direction.     !
!     v        Total velocity component (m/s) in the ETA-direction.    !
!     omega    Omega vertical velocity (m3/s).                         !
!     DUon     Barotropic mass flux (m3/s) in the XI-direction.        !
!     DVom     Barotropic mass flux (m3/s) in the ETA-direction.       !
!     z_r      depths (m) at RHO-points.                               !
!     z_w      depths (m) at W-points.                                 !
!     pm       Coordinate transformation metric "m" (1/meters)         !
!              associated with the differential distances in XI.       !
!     pn       Coordinate transformation metric "n" (1/meters)         !
!              associated with the differential distances in ETA.      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     w        Vertical velocity (m/s).                                !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer L, Lm, M, Mm, N, i, j, k
      real cff1, cff2, cff3
      real DUon(0:L,0:M), DVom(0:L,M), omega(0:L,0:M,0:N), pm(0:L,0:M), &
     &     pn(0:L,0:M), u(L,0:M,N), v(0:L,M,N), w(0:L,0:M,0:N), &
     &     z_r(0:L,0:M,N), z_w(0:L,0:M,0:N)
!
!-----------------------------------------------------------------------
!  Compute vertical velocity (m/s) from omega vertical velocity.
!-----------------------------------------------------------------------
!
!  Compute contribution due to quasi-horizontal motions along
!  S-coordinate surfaces:  (Ui +Vj)*GRADs(z).
!
      Lm=L-1
      Mm=M-1
!
      DO k=1,N
        DO j=Jstr,Jend
          DO i=1,L
            wrk(i,j)=u(i,j,k)*(z_r(i,j,k)-z_r(i-1,j,k))*                &
     &                        (pm(i-1,j)+pm(i,j))
          END DO
          DO i=1,Lm
            vert(i,j,k)=0.25*(wrk(i,j)+wrk(i+1,j))
          END DO
        END DO
        DO j=1,M
          DO i=1,Lm
            wrk(i,j)=v(i,j,k)*(z_r(i,j,k)-z_r(i,j-1,k))*                &
     &                        (pn(i,j-1)+pn(i,j))
          END DO
        END DO
        DO j=1,Mm
          DO i=1,Lm
            vert(i,j,k)=vert(i,j,k)+0.25_r8*(wrk(i,j)+wrk(i,j+1))
          END DO
        END DO
      END DO
!
!  Compute contribution due to time tendency of the free-surface,
!  d(zeta)/d(t), which is the vertical velocity at the free-surface
!  and it is expressed in terms of barotropic mass flux divergence.
!  Notice that it is divided by the total depth of the water column.
!  This is needed because this contribution is linearly distributed
!  throughout the water column by multiplying it by the distance from
!  the bottom to the depth at which the vertical velocity is computed.
!
      DO j=1,Mm
        DO i=1,Lm
          wrk(i,j)=(DUon(i,j)-DUon(i+1,j)+                              &
     &              DVom(i,j)-DVom(i,j+1))/                             &
     &             (z_w(i,j,N)-z_w(i,j,0))
        END DO
!
!  Notice that a cubic interpolation is used to shift the "vert"
!  contribution from vertical RHO- to W-points.
!
        cff1=3.0/4.0
        cff2=0.5
        cff3=1.0/20.0
        DO i=1,Lm
          w(i,j,0)=0.0
          w(i,j,1)=pm(i,j)*pn(i,j)*                                     &
     &             (omega(i,j,1)+wrk(i,j)*(z_w(i,j,1)-                  &
     &                                     z_w(i,j,0)))+                &
     &             cff1*vert(i,j,1)+                                    &
     &             cff2*vert(i,j,2)-                                    &
     &             cff3*vert(i,j,3)
        END DO
        cff1=9.0/16.0
        cff2=1.0/16.0
        DO k=2,N-2
          DO i=1,Lm
            w(i,j,k)=pm(i,j)*pn(i,j)*                                   &
     &               (omega(i,j,k)+wrk(i,j)*(z_w(i,j,k)-                &
     &                                       z_w(i,j,0)))+              &
     &               cff1*(vert(i,j,k  )+vert(i,j,k+1))-                &
     &               cff2*(vert(i,j,k-1)+vert(i,j,k+2))
          END DO
        END DO
        cff1=3.0/8.0
        cff2=3.0/4.0
        cff3=1.0/8.0
        DO i=1,Lm
          w(i,j,N)=wrk(i,j)
          w(i,j,N-1)=pm(i,j)*pn(i,j)*                                   &
     &               (omega(i,j,Nm)+wrk(i,j)*(z_w(i,j,N-1)-             &
     &                                        z_w(i,j,0  )))+           &
     &               cff1*vert(i,j,N  )+                                &
     &               cff2*vert(i,j,N-1)-                                &
     &               cff3*vert(i,j,N-2)
        END DO
      END DO

      RETURN
      END
