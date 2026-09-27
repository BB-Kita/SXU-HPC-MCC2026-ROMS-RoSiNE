#define SASHA
!!
!!svn $Id$
!!================================================== Hernan G. Arango ==
!! Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!!======================================================================
!!                                                                     !
!! This routine interpolates cross-sections from curvilinear grid      !
!! using bicubic interpolation.                                        !
!!                                                                     !
!!======================================================================
!!

#ifdef SASHA
# include "bcintrp_new.h"
#else
# include "bcintrp.h"
#endif 
