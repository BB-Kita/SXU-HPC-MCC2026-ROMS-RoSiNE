      SUBROUTINE hordiv (hdiv,L,M,N,u,v,Hz,pm,pn,spval)
!
!svn $Id: hordiv.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes horizontal divergence.                        !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     L        Number of grid points in the XI-direction.              !
!     M        Number of grid points in the ETA-direction.             !
!     N        Number of vertical levels.                              !
!     u        vector component (m/s) in the XI-direction.             !
!     v        vector component (m/s) in the ETA-direction.            !
!     Hz       total depth or vertical thicknesses (m).                !
!     pm       Coordinate transformation metric "m" (1/meters)         !
!              associated with the differential distances in XI.       !
!     pn       Coordinate transformation metric "n" (1/meters)         !
!              associated with the differential distances in ETA.      !
!     spval    Masked special value.                                   !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     hdiv     Horizontal divergence (1/s).                            !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      implicit none
      integer L, M, N, i, j, k
      real spval
      real Hz(0:L,0:M,N), pm(0:L,0:M), pn(0:L,0:M), hdiv(L-1,M-1,N),    &
     &     u(L,0:M,N), v(0:L,M,N)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Compute horizontal divergence (flux form):
!           (m*n/Hz)*[d(Hz*u/n)/d(xi) + d(Hz*v/m)d(eta)]
!-----------------------------------------------------------------------
!
      DO k=1,N
        DO j=1,M-1
          DO i=1,L-1
            IF ((u(i,j,k).eq.spval).or.(u(i+1,j,k).eq.spval).or.        &
     &          (v(i,j,k).eq.spval).or.(v(i,j+1,k).eq.spval)) THEN
              hdiv(i,j,k)=spval
            ELSE
              hdiv(i,j,k)=(pm(i,j)*pn(i,j)/Hz(i,j,k))*                  &
     &                    ((u(i+1,j,k)*(Hz(i,j,k)+Hz(i+1,j,k))/         &
     &                      (pn(i,j)+pn(i+1,j))-                        &
     &                      u(i  ,j,k)*(Hz(i-1,j,k)+Hz(i,j,k))/         &
     &                      (pn(i-1,j)+pn(i,j)))+                       &
     &                     (v(i,j+1,k)*(Hz(i,j,k)+Hz(i,j+1,k))/         &
     &                      (pm(i,j)+pm(i,j+1))-                        &
     &                      v(i,j  ,k)*(Hz(i,j-1,k)+Hz(i,j,k))/         &
     &                      (pm(i,j-1)+pm(i,j))))
            END IF
          END DO
        END DO
      END DO

      RETURN
      END
