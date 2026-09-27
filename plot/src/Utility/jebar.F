      SUBROUTINE jebar (ujbar,vjbar,L,M,phi,h,pm,pn)
!
!svn $Id: jebar.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine computes the Joint Effect of Baroclinicity and      !
!  bottom Relief (JEBAR) at PSI-points.                                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     L        Number of grid points in the XI-direction.              !
!     M        Number of grid points in the ETA-direction.             !
!     phi      Bottom dynamic pressure (Pa).                           !
!     h        Bathymetry (m).                                         !
!     pm       Coordinate transformation metric "m" (1/meters)         !
!              associated with the differential distances in XI.       !
!     pn       Coordinate transformation metric "n" (1/meters)         !
!              associated with the differential distances in ETA.      !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     ujbar    JEBAR component in the XI-direction (m^2/s^2).          !
!     vjbar    JEBAR component in the ETA-direction (m^2/s^2).         !
!                                                                      !
!=======================================================================
!
      implicit none
      integer L, M, i, j
      real p25
      real h(0:L,0:M), phi(0:L,0:M), pm(0:L,0:M), pn(0:L,0:M),          &
     &     ujbar(L,0:M), vjbar(0:L,M)
      parameter (p25=0.25)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Compute JEBAR vector components.
!
      DO j=0,M
        DO i=1,L
          ujbar(i,j)=p25*(phi(i-1,j)+phi(i,j))*                         &
     &                   (pm (i-1,j)+pm (i,j))*(h(i,j)-h(i-1,j))
        END DO
      END DO
      DO j=1,M
        DO i=0,L
          vjbar(i,j)=p25*(phi(i,j-1)+phi(i,j))*                         &
     &                   (pn (i,j-1)+pn (i,j))*(h(i,j)-h(i,j-1))
        END DO
      END DO

      RETURN
      END
