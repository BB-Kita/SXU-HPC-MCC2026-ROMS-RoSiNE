      SUBROUTINE areamask (f,n)
!
!svn $Id: areamask.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine builds the coordinates of the area to mask due to   !
!  flagged data values  or the imposition of the provided bathymetry   !
!  data.  These coordinates will be used in routine AREAFILL.          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     f        Section field to process (real array).                  !
!     n        Number of points in F (integer).                        !
!                                                                      !
!  On Output: (common block "cntwkspa")                                !
!                                                                      !
!     nra     Number of points in arrays XRA and YRA.                  !
!     xra     X-coordinate of section area masking (real array).       !
!     yra     Y-coordinate of section area masking (real array).       !
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
#include "cntwkspa.h"
#include "fields.h"
#include "pltncar.h"
#include "secbath.h"
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer i, ic, n
      real scle, f(n)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Set vertical coordinate units.
!
      scle=c1                                  ! meters
      IF (coorscl.eq.2) THEN
        scle=m2cm                              ! meters to centimeters
      ELSE IF (coorscl.eq.3) THEN
        scle=m2mm                              ! meters to milimeters
      END IF
!
!  Get the coordinates of the polygon to fill from the masked field or
!  impose provided bathymetry data.
!
      ic=1
      DO i=1,ims
        ic=ic+1
        xra(ic)=x(i)
        IF (abs(sbth(i)).le.abs(sztop*scle)) THEN
          yra(ic)=-abs(sztop*scle)
        ELSE IF (abs(sbth(i)).ge.abs(szbot*scle)) THEN
          yra(ic)=-abs(szbot*scle)
        ELSE
          yra(ic)=-sbth(i)
        END IF
      END DO
      nra=ic+2
      xra(1)=xra(2)
      yra(1)=-abs(szbot*scle)
      xra(nra-1)=xra(ic)
      yra(nra-1)=-abs(szbot*scle)
      xra(nra)=xra(1)
      yra(nra)=yra(1)
!
!  Check dimensions of work arrays.
!
      IF (nra.gt.mcs) THEN
        PRINT 20, nra, mcs
  20    FORMAT (/' AREAMASK - underdimension array, NRA,MCS = ',2i6)
        CALL crash ('AREAMASK',1)
      END IF

      RETURN
      END
