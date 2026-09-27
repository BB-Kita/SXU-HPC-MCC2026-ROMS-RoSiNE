      SUBROUTINE readpal (iunit)
!
!svn $Id: readpal.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads color palette and sets common block COLPAL.      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "colpal.h"
#include "pltio.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer ib, ig, indx, ir, iunit, n
      real b, clv, g, r
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
      read(iunit,*) ncl, ipal, ibarlab
!
!  Check input values.
!
      ipal=ABS(ipal)
      ibarlab=MIN(ncl,MAX(1,ABS(ibarlab)))
!
      IF (ncl.gt.255) THEN
        WRITE (stdout,10) ncl
  10    FORMAT (' READPAL - only 300 colors allowed, NCL = ',i5)
        STOP
      END IF
      IF ((ipal.lt.1).or.(ipal.gt.3)) THEN
        WRITE (stdout,20) ipal
  20    FORMAT (' READPAL - illegal color palette type, IPAL = ',i5)
        STOP
      END IF
!
!  Set non-uniform contour levels and read them from palette file.
!
      IF (ipal.eq.3) THEN
        c_levels=.true.
      END IF
!
!  Read backgound,foreground,and NCL color bands.
!  (Notice reading NCL+2 records)
!
      DO n=1,ncl+5
        IF ((ipal.eq.1).or.(ipal.eq.2)) THEN
          READ (iunit,*) indx, ir, ig, ib, r, g, b
        ELSE IF (ipal.eq.3) THEN
          READ (iunit,*) indx, ir, ig, ib, r, g, b, clv
        END IF
        IF (ipal.eq.1) THEN
          rgbpal(1,n)=FLOAT(ir)
          rgbpal(2,n)=FLOAT(ig)
          rgbpal(3,n)=FLOAT(ib)
        ELSE IF (ipal.eq.2) THEN
          rgbpal(1,n)=r
          rgbpal(2,n)=g
          rgbpal(3,n)=b
        ELSE IF (ipal.eq.3) THEN
          rgbpal(1,n)=FLOAT(ir)
          rgbpal(2,n)=FLOAT(ig)
          rgbpal(3,n)=FLOAT(ib)
          IF (n.gt.2) THEN        ! Ignore bacground and foreground
            clevels(n-2)=clv
          END IF
        END IF
      END DO

      RETURN
      END
