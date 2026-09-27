      SUBROUTINE draw_user
!
!svn $Id: draw_user.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine is used to draw any User overlay.                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "pconst.h"
#include "colpal.h"
#include "fields.h"
#include "ndimen.h"
#include "pltncar.h"
#include "pltfld.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      real x1, x2, y1, y2
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Initialize use of polylines.
!
      IF (ldrwusr.or.(iref.eq.-2)) THEN
        CALL sflush
        CALL gsplci (ncl+4)
      END IF
!
!  Draw specified user polylines data from Fortran unit 35. Usually,
!  the file is named fort.35 for mostly all compilers.
!
      IF (iref.eq.-2) THEN
        CALL gslwsc (thckusr)
        CALL draw_line (35,3)
        CALL sflush
        CALL gsplci (1)
        CALL gslwsc (c1)
      END IF

      RETURN
      END
