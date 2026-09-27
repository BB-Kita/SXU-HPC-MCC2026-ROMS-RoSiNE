      FUNCTION great_int (x)
!
!svn $Id: great_int.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This function returns the greatest integer less than or equal to    !
!  X.                                                                  !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     X        The value whose greatest integer is sought (real).      !
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
      integer ix
      integer great_int
      real x
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Truncate the real value, x.  If x is nonnegative or an integer,
!  then the greatest integer is the truncated value.  Otherwise, the
!  greatest integer less than or equal to x is one less than the
!  truncated value.
!-----------------------------------------------------------------------
!
      ix=INT(x)
      IF ((x.eq.FLOAT(ix)).or.(x.ge.c0)) THEN
        great_int=ix
      ELSE
        great_int=ix-1
      END IF

      RETURN
      END
