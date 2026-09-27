#ifndef rmdocinc
!
!svn $Id: domdat.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  CLATD      Domain centroid latitude (degrees; North positive).      !
!  CLNGD      Domain centroid longitude (degrees; West negative).      !
!  DX         Domain grid spacing in the x-direction (meters).         !
!  DY         Domain grid spacing in the y-direction (meters).         !
!  Lr         Number of RHO-points in the x-direction.                 !
!  Lu         Number of U-points in the x-direction.                   !
!  Lv         Number of V-points in the x-direction.                   !
!  Mr         Number of RHO-points in the y-direction.                 !
!  Mu         Number of U-points in the y-direction.                   !
!  Mv         Number of V-points in the y-direction.                   !
!  NL         Number of history veritical levels.                      !
!  Nbed       Number of sediment bed layers.                           !
!  Nr         Number of vertical RHO-points.                           !
!  Nw         Number of vertical W-points.                             !
!  RHO0       Mean density used in Boussinesq approximation (kg/m^3).  !
!  XBASIN     Domain length in the x-direction (meters).               !
!  YBASIN     Domain length in the y-direction (meters).               !
!                                                                      !
!=======================================================================
!
#endif
      common /idomdat/ Lr, Lu, Lv, Mr, Mu, Mv, NL, Nbed, Nr, Nw
      integer Lr, Lu, Lv, Mr, Mu, Mv, NL, Nbed, Nr, Nw

      common /fdomdat/ clatd, clngd, dx, dy, rho0, xbasin, ybasin
      real clatd, clngd, dx, dy, rho0, xbasin, ybasin
