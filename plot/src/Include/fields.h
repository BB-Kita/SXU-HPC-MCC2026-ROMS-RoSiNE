#ifndef rmdocinc
!
!svn $Id: fields.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  depth  Depths to interpolate.                                       !
!  f1     Utility field array.                                         !
!  f2     Utility field array.                                         !
!  f3     Utility field array.                                         !
!  f4     Utility field array.                                         !
!  f5     Utility field array.                                         !
!  f6     Utility field array.                                         !
!  f7     Utility field array.                                         !
!  theta  Utility coordinate tranformation angle array.                !
!  x      Utility x-position array.                                    !
!  y      Utility y-position array.                                    !
!  z      Utility z-position array.                                    !
!  ziso   Depth of requested isopycnal.                                !
!                                                                      !
!=======================================================================
!
#endif
      common /fields_f1/ f1
      common /fields_f2/ f2
      common /fields_f3/ f3
      common /fields_f4/ f4
      common /fields_f5/ f5
      common /fields_f6/ f6
      common /fields_f7/ f7
      common /fields_x/  x
      common /fields_y/  y
      common /fields_z/  z
      common /fields/    theta, depth, ziso
!
      real depth(NH), f1(NV), f2(NV), f3(NV), f4(NV), f5(NV), f6(NV),   &
     &     f7(NV), theta(NH), x(NV), y(NV), z(NV), ziso(NH)
