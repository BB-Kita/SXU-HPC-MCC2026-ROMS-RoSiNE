      SUBROUTINE mapeod (nout,nsec,idls,idrs,npts,pnts)
!
!svn $Id: mapeod.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This version of MAPEOD suppresses all EZMAP outlines when the       !
!  user provides a digitized outline. Used for color masking the       !
!  land.                                                               !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer idls,idrs,nout,npts,nsec
      real pnts(*)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  If the User provide the land boundaries, zero-out the number of
!  points extracted from EZMAP boundary data.
!
      IF (add.or.(.not.addezmap)) npts=0

      RETURN
      END
