      SUBROUTINE velocity (u,v,L,M,psi,D,pm,pn)
!
!svn $Id: velocity.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes velocity components from transport         !
!  streamfunction.                                                     !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     L        Number of grid points in the XI-direction.              !
!     M        Number of grid points in the ETA-direction.             !
!     psi      transport streamfunction (meters^3/s).                  !
!     D        total depth of water column (meters; positive).         !
!     pm       Coordinate transformation metric "m" (1/meters)         !
!              associated with the differential distances in XI.       !
!     pn       Coordinate transformation metric "n" (1/meters)         !
!              associated with the differential distances in ETA.      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     u        velocity component in the XI-direction (m/s).           !
!     v        velocity component in the ETA-direction (m/s).          !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local variables.
!-----------------------------------------------------------------------
!
      integer L, Lm, M, Mm, i, j
      real D(0:L,0:M), psi(L,M), pm(0:L,0:M), pn(0:L,0:M),              &
     &     u(L,0:M), v(0:L,M)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      Lm=L-1
      Mm=M-1
!
!  Compute velocity components from transport streamfunction.
!
      DO j=1,Mm
        DO i=1,L
          u(i,j)=-(psi(i,j+1)-psi(i,j))*                                &
     &            (pn(i-1,j)+pn(i,j))/(D(i-1,j)+D(i,j))
        END DO
      END DO
      DO j=1,M
        DO i=1,Lm
          v(i,j)=(psi(i+1,j)-psi(i,j))*                                 &
     &           (pm(i,j-1)+pm(i,j))/(D(i,j-1)+D(i,j))
        END DO
      END DO
!
!  Pad out vaules at the edges.
!
      DO i=1,L
        u(i,0)=u(i,1 )
        u(i,M)=u(i,Mm)
      END DO
      DO j=1,M
        v(0,j)=v(1 ,j)
        v(L,j)=v(Lm,j)
      END DO

      RETURN
      END
