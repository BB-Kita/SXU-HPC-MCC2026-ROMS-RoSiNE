#ifndef rmdocinc
!
!svn $Id: cntwkspa.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  IAG     map group identifiers (integer array)                       !
!  IAI     map area identifiers (integer array)                        !
!  IAM     area map working storage (integer array)                    !
!  IWRK    work space used when adding contour lines to the area map   !
!          (integer array)                                             !
!  LAM     length of array IAM (integer)                               !
!  LIWK    length of array IWRK (integer)                              !
!  LMASK   switch to activate land masking (integer)                   !
!  LRWK    length of array RWRK (integer)                              !
!  MAI     length of arrays IAG and IAI (integer)                      !
!  MCS     length of arrays XCS and YCS (integer)                      !
!  NXY     current number of points in processing arrays XCS and YCS.  !
!  NRA     current number of points in processing arrays XRA and YRA.  !
!  RWRK    work space used when adding contour lines to the area map   !
!          (real array)                                                !
!  XCS     x-coordinate of the current processing area (real array)    !
!  XRA     x-coordinate of water column area masking (real array)      !
!  YCS     y-coordinate of the current processing area (real array)    !
!  YRA     y-coordinate of water column area masking (real array)      !
!                                                                      !
!=======================================================================
!
#endif
      integer lam, liwk, lrwk, mai, mcs
      parameter (lam=5000000, liwk=5000000, lrwk=1000000, mai=10,       &
     &           mcs=500000)
!
      common /cntwkspa_int/  lmask, nra, nxy, iag, iai
      common /cntwkspa_iam/  iam
      common /cntwkspa_iwrk/ iwrk
      common /cntwkspa_rwrk/ rwrk
      common /cntwkspa_xy/   xcs, xra, ycs, yra
!
      integer lmask, nra, nxy
      integer iag(mai), iam(lam), iai(mai), iwrk(liwk)
      real rwrk(lrwk), xcs(mcs), xra(mcs), ycs(mcs), yra(mcs)



