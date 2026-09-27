#ifndef rmdocinc
!
!svn $Id: param.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  NH    Maximum number of horizontal points (product of x,y)          !
!  NK    Maximum number of vertical levels                             !
!  NMSK  Maximum number of points for storage array MASK               !
!  NV    Maximum number of volume points (product of x,y,z)            !
!  NX    Maximum number of points in the x or y directions             !
!                                                                      !
!=======================================================================
!
#endif
      integer NH, NK, NMSK, NV, NX
      parameter (NH=6000000,NK=100,NMSK=100000,NV=7000000,NX=10000)
!     parameter (NH=1400000,NK=100,NMSK=100000,NV=31000000,NX=10000)
