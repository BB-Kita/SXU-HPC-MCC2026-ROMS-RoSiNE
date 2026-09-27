      SUBROUTINE drawcl (xcs,ycs,ncs,iai,iag,nai)
!
!svn $Id: drawcl.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine draws the polyline defined by the arrays XCS and       !
!  YCS,  for indices 1 to NCS,  if and only if none  of the area       !
!  identifiers for the area containing the polyline are negtive.       !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer i, idr, nai, ncs
      integer iai(nai), iag(nai)
      real xcs(ncs), ycs(ncs)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
!  Turn on drawing.
!
      idr=1
!
!  If any area identifier is negative, turn off drawing.
!
      DO i=1,nai
        IF (iai(i).lt.0) idr=0
      END DO
!
!  If drawing is turned on, draw the polyline.
!
      IF (idr.ne.0) THEN
        CALL curved (xcs,ycs,ncs)
      END IF

      RETURN
      END
