      SUBROUTINE box(ics)
!
!svn $Id: box.F 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Draw a white box around current window.                             !
!                                                                      !
!           ICS = 0    draw in viewport coordinate system              !
!           ICS = 1    draw in world coordinate system                 !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Define local data.
!-----------------------------------------------------------------------
!
      implicit none
      integer ics,ll
      real ub,ul,ur,ut,vb,vl,vr,vt
      real x(5),y(5)
!
!=======================================================================
!  Begin executable code.
!=======================================================================
!
       CALL getset (vl,vr,vb,vt,ul,ur,ub,ut,ll)
!
!  Viewport coordinates.
!
       IF (ics.eq.0) THEN
         x(1)=vl
         x(2)=vl
         x(3)=vr
         x(4)=vr
         x(5)=vl
         y(1)=vb
         y(2)=vt
         y(3)=vt
         y(4)=vb
         y(5)=vb
!
!  World coordinates.
!
       ELSE
         x(1)=ul
         x(2)=ul
         x(3)=ur
         x(4)=ur
         x(5)=ul
         y(1)=ub
         y(2)=ut
         y(3)=ut
         y(4)=ub
         y(5)=ub
       END IF
!
!  Draw box.
!
       CALL gsfais (1)
       CALL gsplci (1)
       CALL gpl (5,x,y)

       RETURN
       END
