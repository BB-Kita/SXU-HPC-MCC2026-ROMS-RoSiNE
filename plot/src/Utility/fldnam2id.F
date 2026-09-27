      FUNCTION fldnam2id (varnam)
!
!svn $Id: fldnam2id.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine returns the field identification of classification     !
!  CLASS for NetCDF Variable VARNAM.                                   !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     varnam     NetCDF variable name (character).                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     fldnam2id  Field identification (integer).                       !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "fldsid.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, idfld, lvar, lvar1
      integer fldnam2id, lenstr
      character*(*) varnam
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      idfld=-99
      lvar=lenstr(varnam)
      IF (lvar.eq.0) THEN
        fldnam2id=idfld
        RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Get field identification.
!-----------------------------------------------------------------------
!
      DO i=0,nvar
        lvar1=lenstr(vname1(i))
        IF (varnam(1:lvar).eq.vname1(i)(1:lvar1)) THEN
          idfld=vid(i)
          GO TO 10
        END IF
      END DO
 10   fldnam2id=idfld

      RETURN
      END
