#ifndef rmdocinc
!
!svn $Id: secpos.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  dels    Horizontal spacing for section extraction (km or degrees).  !
!  ims     Number of section points in the XY-plane.                   !
!  isecpos flag indicating section extraction coordinates:             !
!          [0] grid units, [1] Cartesian (km), [2] Spherical (degrees).!
!  kms     Number of section points in the Z-direction.                !
!  lataxis flag indicating section having latitude axis when true and  !
!          longitude axis when false.  Only used when isecpos=2.       !
!  sbth    Bathymetry for the section (meters).                        !
!  slat    Latitudes of sections points (degrees).                     !
!  slon    Longitudes of sections points (degrees).                    !
!  xsec    Section x-positions with respect grided data (km).          !
!  ysec    Section y-positions with respect grided data (km).          !
!  szbot   Deepest section depth to consider (meters).                 !
!  zsec    Section z-positions (meters).                               !
!  sztop   Shallowest section depth to xonsider (meters).              !
!  xsec1   Starting section X-position (grid units, km, degrees).      !
!  xsec2   Ending section X-position (grid units, km, degrees).        !
!  ysec1   Starting section Y-position (grid units, km, degrees).      !
!  ysec2   Ending section Y-position (grid units, km, degrees).        !
!                                                                      !
!=======================================================================
!
#endif
      common /isecpos/ lataxis, ims, isecpos, kms
      logical lataxis
      integer ims, isecpos, kms
!
      common /fsecpos/ dels, sztop, szbot, xsec1, xsec2, ysec1, ysec2,  &
     &                 slon, slat, xsec, ysec, zsec
      real dels, szbot, sztop, xsec1, xsec2, ysec1, ysec2
      real slat(NX), slon(NX), xsec(NX), ysec(NX), zsec(NX)
