      SUBROUTINE curlz (curl,L,M,u,v,f,pm,pn,ekman)
!
!svn $Id: curlz.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the vertical curl of a 2d vector field.       !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     L        Number of grid points in the XI-direction.              !
!     M        Number of grid points in the ETA-direction.             !
!     u        Field component in the XI-direction.                    !
!     v        Field component in the ETA-direction.                   !
!     f        Coriolis parameter.                                     !
!     pm       Coordinate transformation metric "m" (1/meters)         !
!              associated with the differential distances in XI.       !
!     pn       Coordinate transformation metric "n" (1/meters)         !
!              associated with the differential distances in ETA.      !
!     ekman    Switch to compute Ekman pumping.                        !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     curl     Vertical curl of field.                                 !
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
      logical ekman
      integer L, Lm, M, Mm, i, j
      real p5
      real curl(L-1,M-1), f(0:L,0:M), pm(0:L,0:M), pn(0:L,0:M),         &
     &     u(L,0:M), v(0:L,M)
      parameter (p5=0.5)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      Lm=L-1
      Mm=M-1
!
!-----------------------------------------------------------------------
!  Compute vertical curl.
!-----------------------------------------------------------------------
!
      IF (ekman) THEN
        DO j=2,Mm
          DO i=2,Lm
            curl(i,j)=pm(i,j)*pn(i,j)*                                  &
     &                ((v(i  ,j)/(p5*(pn(i  ,j)+pn(i  ,j-1))*           &
     &                            p5*(f (i  ,j)+f (i  ,j-1)))-          &
     &                  v(i-1,j)/(p5*(pn(i-1,j)+pn(i-1,j-1))*           &
     &                            p5*(f (i-1,j)+f (i-1,j-1))))-         &
     &                 (u(i,j  )/(p5*(pm(i,j  )+pm(i-1,j  ))*           &
     &                            p5*(f (i,j  )+f (i-1,j  )))-          &
     &                  u(i,j-1)/(p5*(pm(i,j-1)+pm(i-1,j-1))*           &
     &                            p5*(f (i,j-1)+f (i-1,j-1)))))
          END DO
        END DO
      ELSE
        DO j=2,Mm
          DO i=2,Lm
            curl(i,j)=pm(i,j)*pn(i,j)*                                  &
     &                ((v(i  ,j)/(p5*(pn(i  ,j)+pn(i  ,j-1)))-          &
     &                  v(i-1,j)/(p5*(pn(i-1,j)+pn(i-1,j-1))))-         &
     &                 (u(i,j  )/(p5*(pm(i,j  )+pm(i-1,j  )))-          &
     &                  u(i,j-1)/(p5*(pm(i,j-1)+pm(i-1,j-1)))))
          END DO
        END DO
      END IF
!
!  Pad out vaules at the edges.
!
      DO j=2,Mm
        curl(1,j)=curl(2,j)
      END DO
      DO i=2,Lm
        curl(i,1)=curl(i,2)
      END DO
      curl(1,1)=p5*(curl(2,1 )+curl(1,2))

      RETURN
      END
