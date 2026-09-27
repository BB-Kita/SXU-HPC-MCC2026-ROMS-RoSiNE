#ifndef rmdocinc
!
!svn $Id: pltio.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  CST        switch to draw coastlines (logical)                      !
!  FNAME      input file names (character)                             !
!  INPCNT     input logical unit for contour interval values (integer) !
!  INPCST     input logical unit for coaslines (integer)               !
!  INPPAL     input logical unit for color palette (integer)           !
!  INPVID     input logical unit for variable IDs (integer)            !
!                                                                      !
!=======================================================================
!
#endif
      common /ipltio/ cst, stdinp, stdout, inppal, inpcnt, inpcst,      &
     &                inpvid
      logical cst
      integer inpcnt, inpcst, inppal, inpvid, stdinp, stdout
!
      common /cpltio/ fname
      character*160 fname(0:6)
