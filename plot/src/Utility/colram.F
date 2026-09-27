      SUBROUTINE colram(xam,yam,nam,ai,ag,nai)
!
!svn $Id: colram.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  The arrays XAM and YAM,  for indices 1 to NAM,  contain the         !
!  X and Y coordinates of points defining a polygon.  The area         !
!  identifiers in the array AI,  each with an associated group         !
!  identifier in the array AG,  tell us whether the polygon is         !
!  to be color-filled or not.                                          !
!                                                                      !
!=======================================================================
!
      implicit none
#include "cntwkspa.h"
#include "colpal.h"
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      integer cindx,i,ai1,ai3,ici,mapaci,nai,nam
      integer ai(nai),ag(nai)
      real xam(nam),yam(nam)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Check if scrach array space is appropriate.
!
      IF (nai.gt.mai) THEN
        WRITE (6,10) mai,nai
  10    FORMAT (' COLRAM - underdimensioned arrays: AI, AG: ',2i6)
      ELSE IF (nam.gt.mcs) THEN
        WRITE (6,20) mcs,nam
  20    FORMAT (' COLRAM - underdimensioned arrays: XAM, YAM: ',2i6)
      END IF
!
!-----------------------------------------------------------------------
!  Find the area identifiers for the area realtive to groups 1 and 3.
!  The first of these tell us whether the area is over land or water,
!  and the second tell us what contour band the area is in.
!-----------------------------------------------------------------------
!
      ai1=-1
      ai3=-1
      DO 40 i=1,nai
        IF (ag(i).eq.1) ai1=ai(i)
        IF (ag(i).eq.3) ai3=ai(i)
!!      PRINT 30, i,ag(i),ai(i),mapaci(ai1),nam
  30    FORMAT (i4,' Group = ',i5,'  Area = ',i5,' Land Index = ',i5,   &
     &          ' Npts = ',i5)
  40  CONTINUE
      ici=mapaci(ai1)
      IF (((1.le.ai3).and.(ai3.le.ncl)).and.(ici.ne.1)) ici=1
!
!-----------------------------------------------------------------------
!  Reset flags if not color masking land and water.
!-----------------------------------------------------------------------
!
      IF (lmask.eq.0) ai1=-1
!
!-----------------------------------------------------------------------
!  Color-fill the area, using brown for any area over land, a color
!  depending on the contour level, and blue for any area over water
!  elsewhere.
!-----------------------------------------------------------------------
!
      IF (ai3.gt.0) THEN
        cindx=ai3+1
        CALL gsfaci (cindx)
        CALL gfa (nam-1,xam,yam)
!!      PRINT 50, nam,ici,ai1,ai3
  50    FORMAT ('Water: ',5x,' npts = ',i5,3x,'ici = ',i5,3x,'ai1 = ',  &
     &          i5,3x,'ai3 = ',i5/)
      ELSE IF ((ai1.gt.0).and.(ici.ne.1)) THEN
        cindx=ncl+2
        CALL gsfaci (cindx)
        CALL gfa (nam-1,xam,yam)
!!      PRINT 60, nam,ici,ai1,ai3
  60    FORMAT ('Land: ',5x,' npts = ',i5,3x,'ici = ',i5,3x,'ai1 = ',i5,&
     &          3x,'ai3 = ',i5/)
      END IF

      RETURN
      END
