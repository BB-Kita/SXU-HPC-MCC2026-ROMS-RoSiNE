      SUBROUTINE setcolpal
!
!svn $Id: setcolpal.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets the color indices that contain the desired        !
!  color palette:                                                      !
!                                                                      !
!            INDX = 0              background color                    !
!                 = 1              foregound color                     !
!                 = 2,...,ncl+1    NCL color levels to apply           !
!                 = ncl+2          color for land areas                !
!                 = ncl+3          color for water areas               !
!                 = ncl+4          color for polyline overlays         !
!                                                                      !
!  These indices are selected according to the value of the flag       !
!  IPAL specified in the input color palette file:                     !
!                                                                      !
!            IPAL = 1    (R,G,B) values ranging between 0 and 255      !
!                 = 2    (R,G,B) values ranging between 0.0 and 1.0    !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
#include "colpal.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer indx, n
      real b, g, r
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Load color indices.
!-----------------------------------------------------------------------
!
      DO n=1,ncl+5
        indx=n-1
        IF (ipal.eq.1) THEN
          r=rgbpal(1,n)/c255
          g=rgbpal(2,n)/c255
          b=rgbpal(3,n)/c255
        ELSE IF (ipal.eq.2) THEN
          r=rgbpal(1,n)
          g=rgbpal(2,n)
          b=rgbpal(3,n)
        ELSE IF (ipal.eq.3) THEN
          r=rgbpal(1,n)/c255
          g=rgbpal(2,n)/c255
          b=rgbpal(3,n)/c255
        END IF
        CALL gscr (1,indx,r,g,b)
      END DO

      RETURN
      END
