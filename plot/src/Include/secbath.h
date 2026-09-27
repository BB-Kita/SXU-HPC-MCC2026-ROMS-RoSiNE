#ifndef rmdocinc
!
!svn $Id: secbath.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  bath    bathymetry (meters).                                        !
!  sbth    Bathymetry for the section (meters).                        !
!                                                                      !
!=======================================================================
!
#endif
      common /secbath/ bath, sbth
      real bath(NH), sbth(NX)
