      SUBROUTINE sec_frame (icolor)
!
!svn $Id: sec_frame.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets viewport space for the current section map on     !
!  the page.                                                           !
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
#include "pltio.h"
#include "pltncar.h"
#include "secpos.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer icolor
      real scle, xb(5), yb(5)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Set viewport according to the number of maps per page.
!-----------------------------------------------------------------------
!
      CALL viewport (fl,fr,fb,ft,sclmap,ipltpg,npage,icolor,wrthdr,     &
     &               pltlogo)
!
!-----------------------------------------------------------------------
!  Set plotting frame.
!-----------------------------------------------------------------------
!
      scle=c1                                  ! meters
      IF (coorscl.eq.2) THEN
        scle=m2cm                              ! meters to centimeters
      ELSE IF (coorscl.eq.3) THEN
        scle=m2mm                              ! meters to milimeters
      END IF
      ul=x(1)
      ur=x(ims)
      ut=-abs(sztop*scle)
      ub=-abs(szbot*scle)
      CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
!
!-----------------------------------------------------------------------
!  Fill the entire section with water color.
!-----------------------------------------------------------------------
!
      IF (icolor.eq.1) THEN
        xb(1)=ul
        xb(2)=ul
        xb(3)=ur
        xb(4)=ur
        xb(5)=xb(1)
        yb(1)=ub
        yb(2)=ut
        yb(3)=ut
        yb(4)=ub
        yb(5)=yb(1)
        CALL clrbox (xb,yb,5,ncl+3,.false.)
      END IF
!
!-----------------------------------------------------------------------
!  Report viewport information.
!-----------------------------------------------------------------------
!
      IF (lvpinfo) THEN
        WRITE (6,"(/' SEC_FRAME - plots/page   = ',i4)") npage
        WRITE (6,"( ' SEC_FRAME - current plot = ',i4)") ipltpg
        WRITE (6,"( ' SEC_FRAME - frame left   = ',f9.4)") fl
        WRITE (6,"( ' SEC_FRAME - frame right  = ',f9.4)") fr
        WRITE (6,"( ' SEC_FRAME - frame bottom = ',f9.4)") fb
        WRITE (6,"( ' SEC_FRAME - frame top    = ',f9.4)") ft
        WRITE (6,"( ' SEC_FRAME - box Xmin     = ',f9.4)") ul
        WRITE (6,"( ' SEC_FRAME - box Xmax     = ',f9.4)") ur
        WRITE (6,"( ' SEC_FRAME - box Zmin     = ',f9.4)") ub
        WRITE (6,"( ' SEC_FRAME - box Zmax     = ',f9.4/)") ut
      END IF

      RETURN
      END
