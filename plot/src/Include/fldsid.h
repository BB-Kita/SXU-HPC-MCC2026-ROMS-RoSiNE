#ifndef rmdocinc
!
!svn $Id: fldsid.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  is2d      Switch indicating spatial 2D or 3D variable (logical).    !
!  nvar      Current number of variables in ID file.                   !
!  vcode     Variable text code identification (six character string). !
!  vid       Variable to plot identification (integer).                !
!  vlabel    Variable plot label (character string).                   !
!  vname1    NetCDF name for variable 1 (character string).            !
!  vname2    NetCDF name for variable 2 (character string).            !
!  vtype     Variable type switch (integer):                           !
!               vtype = 1, scalar variable.                            !
!               vtype = 2, vector variable.                            !
!               vtype = 3, derived from two scalar variables.          !
!               vtype = 4, special scalar variable.                    !
!               vtype = 5, special vector variable.                    !
!               vtype = 6, streamlines from vector variables.          !
!                                                                      !
!=======================================================================
!
#endif
      integer mvar
      parameter (mvar=1000)
!
      common /ifldsid/ nvar, is2d, vid, vtype
      logical is2d(0:mvar)
      integer nvar
      integer vid(0:mvar), vtype(0:mvar)
!
      common /cfldsid/ vcode, vlabel, vname1, vname2
      character*6  vcode(0:mvar)
      character*25 vname1(0:mvar), vname2(0:mvar)
      character*80 vlabel(0:mvar)
