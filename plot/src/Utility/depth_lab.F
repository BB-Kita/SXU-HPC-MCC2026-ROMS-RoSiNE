      SUBROUTINE depth_lab (idfld,level,botlab)
!
!svn $Id: depth_lab.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine creates the bottom label of the current field.  The    !
!  bottom label consists of the field name and the depth (or level)    !
!  of the field.                                                       !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     idfld    Identification of current field (integer).              !
!     level    Depth (>0), level (>0), or isopycnal (>1000) to         !
!              process (real).                                         !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     botlab   Bottom label (character).                               !
!                                                                      !
!  Calls:   ncvid   (NetCDF library)                                   !
!           crash, lenstr, ncvget                                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define global data.
!-----------------------------------------------------------------------
!
      implicit none
#include "pconst.h"
#include "fldsid.h"
#include "pltfld.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer idfld, lenfld, lstr
      integer lenstr
      real level, scle
      character*3   Cscle
      character*60  dpthlab, fldlab
      character*(*) botlab
!
!-----------------------------------------------------------------------
!  Get field label.
!-----------------------------------------------------------------------
!
      levless=is2d(idfld)
      fldlab=vlabel(idfld)
      lenfld=lenstr(fldlab)
!
!  PE classification field.
!
      IF (.not.levless) THEN
        IF ((c0.lt.level).and.(level.lt.c1000)) THEN
          IF (level.lt.c10) THEN
            WRITE (dpthlab,10) int(level)
  10        FORMAT ('at Level ',i1)
          ELSE
            WRITE (dpthlab,20) int(level)
  20        FORMAT ('at Level ',i2)
          END IF
        ELSE IF (level.gt.c1000) THEN
          WRITE (dpthlab,30) level-c1000
  30      FORMAT ('at Isopycnal ',f5.2)
        ELSE
          scle=c1
          Cscle=' m '
          IF (coorscl.eq.2) THEN
            scle=m2cm
            Cscle=' cm'
          ELSE IF (coorscl.eq.3) THEN
            scle=m2mm
            Cscle=' mm'
          END IF
          IF ((level*scle).gt.-c100) THEN
            WRITE (dpthlab,40) abs(level)*scle, Cscle
  40        FORMAT ('at ',f4.1,a)
          ELSE IF ((level*scle).gt.-c1000) THEN
            WRITE (dpthlab,50) abs(level)*scle, Cscle
  50        FORMAT (' at ',f5.1,a)
          ELSE
            WRITE (dpthlab,60) abs(level)*scle, Cscle
  60        FORMAT (' at ',f6.1,a)
          END IF
        END IF
      ELSE
        dpthlab='                    '
      END IF
!
!  Generate bottom label.
!
      lstr=lenstr(dpthlab)
      IF (lstr.ne.0) THEN
        botlab=fldlab(1:lenfld)//' '//dpthlab(1:lstr)
      ELSE
        botlab=fldlab(1:lenfld)
      END IF

      RETURN
      END
