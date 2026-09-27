      SUBROUTINE vbackground (day,cint,fmin1,fmax1,fmin2,fmax2,frms,    &
     &                        icolor,idfld)
!
!svn $Id: vbackground.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws background plot for a cross-section.             !
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
      integer icolor, idfld
      real cint, day, fmax1, fmax2, fmin1, fmin2, frms, level
      parameter (level=0.0)
!
!=======================================================================
!  Begin executive code.
!=======================================================================
!
!  Turn off clipping.
!
      CALL gsclip (0)
!
!-----------------------------------------------------------------------
!  Draw domain box, grid, and labels.
!-----------------------------------------------------------------------
!
      CALL sec_axis
!
!-----------------------------------------------------------------------
!  Write map titles.
!-----------------------------------------------------------------------
!
      CALL maptitles (day,cint,fmin1,fmax1,fmin2,fmax2,frms,            &
     &                icolor,idfld,level)
!
!  Turn on clipping
!
      CALL gsclip (1)

      RETURN
      END
