      SUBROUTINE clrbox (xcs,ycs,ncs,indx,outline)
!
!svn $Id: clrbox.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine color fill the polygon described in XCS and YCS        !
!  using color index INDX.                                             !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      logical outline
      integer indx,ncs
      real xcs(ncs),ycs(ncs)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Color the polygon.
!
      CALL gsfaci (indx)
      CALL gfa (ncs-1,xcs,ycs)
!
!  Outline the polygon, using a double-width line.
!
      IF (outline) THEN
        CALL gslwsc (c2)
        CALL gpl (ncs,xcs,ycs)
        CALL gslwsc (c1)
      END IF

      RETURN
      END
