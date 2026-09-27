      FUNCTION cntint (ifield,level,fmin,fmax,km,cont,iflds,ilevs)
!
!svn $Id: cntint.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine determines contour interval.                           !
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
      integer ifield, iflds, ilevs, km, level, ncl, nl
      real cint, cintsgn, fint, fmax, fmin, p
      real cont(0:iflds,ilevs)
      real cntint
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Compute contour interval: about NCL contour lines.
!
      ncl=12
      fint=(fmax-fmin)/FLOAT(ncl)
      IF (fint.ne.c0) THEN
        p=10**(IFIX(ALOG10(fint)+c5000)-c5000)
        fint=MAX(AINT(fint/p),c1)*p
      ELSE
        IF ((fmax.eq.fmin).and.(fmax.ne.c0)) THEN
          fint=c1          ! Draw just requested single contour
        ELSE
          fint=c0          ! Select countour intevals internally
        END IF
      END IF
!
!  Extract contour interval from provided plot data file.
!
      IF ((1.le.level).and.(level.le.ilevs)) THEN
        cint=cont(ifield,level)
      ELSE
        cint=cont(ifield,1)
      END IF
!
!  If the specified contour interval needs to be modified, activate the
!  no drawing of zero-contour lines (set negative contour interval),
!  if requested.
!
      cintsgn=c1
      IF (cint.lt.c0) cintsgn=cm1
!
!  Select contour interval: computed or provided.
!
      IF (cint.ne.c0) THEN
        nl=INT((fmax-fmin)/ABS(cint))+1
      ELSE
        nl=2*ncl
      END IF
      IF (nl.ge.2*ncl) THEN
        cntint=fint
!!      cntint=fint*cintsgn
      ELSE
        cntint=cint
      END IF

      RETURN
      END
