      SUBROUTINE relvor (rvor,L,M,N,u,v,Hz,pm,pn,spval)
!
!svn $Id: relvor.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes relative vorticity.                           !
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
!     rvor     Relative vorticity (1/s).                               !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      implicit none
      integer L, M, N, i, j, k
      real p25, spval
      real Hz(0:L,0:M,N), pm(0:L,0:M), pn(0:L,0:M), rvor(L,M,N),        &
     &     u(L,0:M,N), v(0:L,M,N)
      parameter (p25=0.25)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Compute relative vorticity (flux form):
!           (m*n/Hz)*[d(Hz*v/n)d(xi) - d(Hz*u/m)d(eta)]
!-----------------------------------------------------------------------
!
      DO k=1,N
        DO j=1,M
          DO i=1,L
            IF ((u(i,j,k).eq.spval).or.(u(i,j-1,k).eq.spval).or.        &
     &          (v(i,j,k).eq.spval).or.(v(i-1,j,k).eq.spval)) THEN
              rvor(i,j,k)=spval
            ELSE
              rvor(i,j,k)=p25*(pm(i-1,j-1)+pm(i  ,j-1)+                 &
     &                         pm(i  ,j  )+pm(i-1,j  ))*                &
     &                        (pn(i-1,j-1)+pn(i  ,j-1)+                 &
     &                         pn(i  ,j  )+pn(i-1,j  ))/                &
     &                        (Hz(i-1,j-1,k)+Hz(i  ,j-1,k)+             &
     &                         Hz(i  ,j  ,k)+Hz(i-1,j  ,k))*            &
     &                    ((v(i  ,j,k)*(Hz(i,j-1,k)+Hz(i,j,k))/         &
     &                                 (pn(i,j-1)+pn(i,j))-             &
     &                      v(i-1,j,k)*(Hz(i-1,j,k)+Hz(i-1,j-1,k))/     &
     &                                 (pn(i-1,j)+pn(i-1,j-1)))-        &
     &                     (u(i,j  ,k)*(Hz(i-1,j,k)+Hz(i,j,k))/         &
     &                                 (pm(i-1,j)+pm(i,j))-             &
     &                      u(i,j-1,k)*(Hz(i-1,j-1,k)+Hz(i,j-1,k))/     &
     &                                 (pm(i-1,j-1)+pm(i,j-1))))
            END IF
          END DO
        END DO
      END DO

      RETURN
      END
