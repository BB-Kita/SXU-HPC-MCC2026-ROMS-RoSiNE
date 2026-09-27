#ifndef rmdocinc
!
!svn $Id: fdiagn.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  FADV     average deviation.                                         !
!  FAVG     mean.                                                      !
!  FKUR     kurtosis.                                                  !
!  FMAX     maximum value.                                             !
!  FMIN     minimum value.                                             !
!  FRMS     root mean square.                                          !
!  FSDV     standard deviation.                                        !
!  FSKW     skewness.                                                  !
!  FVAR     variance.                                                  !
!                                                                      !
!=======================================================================
!
#endif
      common /fdiagn/ fmin, fmax, favg, frms, fadv, fvar, fsdv, fskw,   &
     &                fkur
      real fadv, favg, fkur, fmax, fmin, frms, fsdv, fskw, fvar
