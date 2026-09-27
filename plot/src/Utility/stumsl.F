      SUBROUTINE stumsl (xcs,ycs,ncs,iai,iag,nai)
!
!svn $Id: stumsl.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2003 ROMS/TOMS Adjoint Group                          !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Area masking routine used by the STREAM package.                    !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     xcs      Streamline X-coordinate.                                !
!     ycs      Streamline Y-coordinate.                                !
!     ncs      Number of points.                                       !
!     iai      Area identifier array.                                  !
!     iag      Area group array.                                       !
!     nai      Number of area/group identifiers.                       !
!                                                                      !
!=======================================================================
!
      integer i, nai, ncs
      integer iag(nai), iai(nai)
      real xcs(ncs), ycs(ncs)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      DO i=1,nai
        IF (iai(i).lt.0) RETURN
      END DO
!
!  If drawing is turned on, draw the polyline.
!
      CALL curve (xcs,ycs,ncs)

      RETURN
      END
