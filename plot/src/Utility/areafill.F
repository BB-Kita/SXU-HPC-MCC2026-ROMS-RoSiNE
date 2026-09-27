      SUBROUTINE areafill (icolor,fill)
!
!svn $Id: areafill.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine fills with color or with the selected pattern the      !
!  section area masked by routine AREAMASK.                            !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     icolor   switch indicating type of section:                      !
!               icolor=0    noncolor section                           !
!               icolor=1    color section                              !
!     fill     switch indicating color of area file:                   !
!              Noncolor Section:                                       !
!                 fill=1  default dot pattern.                         !
!                 fill=2  dots, square and cross patterns.             !
!                 fill=3  dots, slanted square pattern.                !
!                 fill=4  vertical lines and dots.                     !
!                 fill=5  vertical lines.                              !
!                 fill=6  tree pattern.                                !
!              Color Section:                                          !
!                 fill=1  grey fill.                                   !
!                 fill=2  tan fill.                                    !
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
#include "colpal.h"
#include "pltio.h"
#include "pltncar.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer fill, ici, icolor, ity, nst, nnd
      integer id1(8,8), id2(8,8), id3(8,8), ind(mcs)
      real dst(mcs)
      data id1 /1,1,0,0,0,0,1,1,                                        &
     &          1,1,0,1,1,0,1,1,                                        &
     &          0,0,0,1,1,0,0,0,                                        &
     &          0,1,1,1,1,1,1,0,                                        &
     &          0,1,1,1,1,1,1,0,                                        &
     &          0,0,0,1,1,0,0,0,                                        &
     &          1,1,0,1,1,0,1,1,                                        &
     &          1,1,0,0,0,0,1,1/
      data id2 /0,0,0,0,0,0,0,0,                                        &
     &          0,1,1,1,1,1,1,0,                                        &
     &          0,1,1,1,1,1,1,0,                                        &
     &          0,1,1,0,0,1,1,0,                                        &
     &          0,1,1,0,0,1,1,0,                                        &
     &          0,1,1,1,1,1,1,0,                                        &
     &          0,1,1,1,1,1,1,0,                                        &
     &          0,0,0,0,0,0,0,0/
      data id3 /0,0,0,0,0,0,0,0,                                        &
     &          0,0,0,0,1,0,0,0,                                        &
     &          0,0,0,1,1,1,0,0,                                        &
     &          0,1,0,0,1,0,0,1,                                        &
     &          0,0,1,1,1,1,1,0,                                        &
     &          0,0,0,0,1,0,0,0,                                        &
     &          0,0,0,1,0,1,0,0,                                        &
     &          0,1,1,0,0,0,1,1/
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Check size of work space arrays.
!
      nst=mcs
      nnd=mcs
      IF (nst.lt.2*nra) THEN
        WRITE (stdout,10) 2*nra, mcs
  10    FORMAT (/' AREAFILL - array size too small, NST: ',2i6)
        CALL crash ('AREAFILL',1)
      ELSE IF (nnd.lt.nra+2*nra) THEN
        WRITE (stdout,20) nra+2*nra, mcs
  20    FORMAT (/' AREAFILL - array size too small, NND: ',2i6)
        CALL crash ('AREAFILL',1)
      END IF
!
!----------------------------------------------------------------------
!  Fill below water column with the default dot pattern, increasing
!  the inter-dot spacing considerably for noncolor section.  Use color
!  fill on color sections.
!----------------------------------------------------------------------
!
!  Noncolor sections.
!
      IF (icolor.eq.0) THEN
!
!  Draw topography isoline.
!
        CALL curve (xra(2),yra(2),nra-1)
!
!  Fill blank and white plot.
!
        IF (fill.eq.1) THEN
          CALL sfsetr ('SP - spacing of fill lines',r200)
          CALL sfseti ('AN - angle of fill lines',0)
          CALL sfseti ('DO - dot-fill flag',1)
          CALL sfwrld (xra,yra,nra,dst,nst,ind,nnd)
        ELSE IF (fill.eq.2) THEN
          CALL sfsetr ('SP - spacing of fill lines',r250)
          CALL sfseti ('DO - dot-fill flag',1)
          CALL sfsetp (id1)
          CALL sfwrld (xra,yra,nra,dst,nst,ind,nnd)
        ELSE IF (fill.eq.3) THEN
          CALL sfseti ('AN - angle of fill lines',45)
          CALL sfseti ('DO - dot-fill flag',1)
          CALL sfsetp (id2)
          CALL sfwrld (xra,yra,nra,dst,nst,ind,nnd)
        ELSE IF (fill.eq.4) THEN
          CALL sfsetr ('SP - spacing of fill lines',p012)
          CALL sfseti ('AN - angle of fill lines',90)
          CALL sfseti ('DO - dot-fill flag',0)
          CALL sfwrld (xra,yra,nra,dst,nst,ind,nnd)
          CALL sfsetr ('SP - spacing of fill lines',p006)
          CALL sfseti ('AN - angle of fill lines',90)
          CALL sfseti ('DO - dot-fill flag',1)
          CALL sfnorm (xra,yra,nra,dst,nst,ind,nnd)
        ELSE IF (fill.eq.5) THEN
          CALL sfsetr ('SP - spacing of fill lines',r200)
          CALL sfseti ('AN - angle of fill lines',90)
          CALL sfwrld (xra,yra,nra,dst,nst,ind,nnd)
        ELSE IF (fill.eq.6) THEN
          CALL gschh (r250)
          CALL sfsetr ('SP - spacing of fill lines',r250)
          CALL sfseti ('AN - angle of fill lines',0)
          CALL sfseti ('DO - dot-fill flag',1)
          CALL sfsetc ('CH - character specifier','o')
          CALL sfsetp (id3)
          CALL sfwrld (xra,yra,nra,dst,nst,ind,nnd)
        END IF
!
!-----------------------------------------------------------------------
!  Color section.
!-----------------------------------------------------------------------
!
      ELSE IF (icolor.eq.1) THEN
!
!  Fill area with color indicated in FILL.
!
        ity=0
        ici=ncl+2
        IF (fill.eq.1) THEN
          CALL gscr (1,ici,0.4,0.4,0.4)
        ELSE IF (fill.eq.2) THEN
          CALL gscr (1,ici,0.86,0.58,0.44)
        END IF
        CALL set (fl,fr,fb,ft,ul,ur,ub,ut,1)
        CALL sfseti ('TY - type of fill',ity)
        CALL sfsgfa (xra,yra,nra,dst,nst,ind,nnd,ici)
!!      CALL sfnorm (xra,yra,nra,dst,nst,ind,nnd)
      END IF

      RETURN
      END
