#ifndef rmdocinc
!
!svn $Id: fmask.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  MASK     current mask field indices (integer array).                !
!  MSK      switch that activates masking of current field (logical).  !
!  MSKINP1  switch that activates masking of primary fields (logical). !
!  MSKINP2  switch that activates masking of secondary fields (logical)!
!  MSK3d    switch that activates masking of 3D fields (logical).      !
!  MSKVAL   land mask value (integer).                                 !
!                                                                      !
!=======================================================================
!
#endif
      common /imask/ msk, msk3d, mskinp1, mskinp2, mskval
      logical msk, mskinp1, mskinp2, msk3d
      integer mskval
!
      common /fmask/ mask
      real mask(NH)
