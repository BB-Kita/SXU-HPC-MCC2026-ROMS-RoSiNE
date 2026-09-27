#include "cppdefs.h"
#ifdef BIOLOGY
!!
!!svn $Id: read_biopar.F 830 2017-01-24 21:21:11Z arango $
!!================================================= Hernan G. Arango ===
!! Copyright (c) 2002-2017 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!!======================================================================
!!                                                                     !
!!  This routine reads and reports selected biological model input     !
!!  parameters.                                                        !
!!                                                                     !
!!======================================================================
!!
# if defined BIO_FENNEL
#  include <fennel_inp.h>
# elif defined ECOSIM
#  include <ecosim_inp.h>
# elif defined NEMURO
#  include <nemuro_inp.h>
# elif defined NPZD_FRANKS
#  include <npzd_Franks_inp.h>
# elif defined NPZD_IRON
#  include <npzd_iron_inp.h>
# elif defined NPZD_POWELL
#  include <npzd_Powell_inp.h>
# elif defined RED_TIDE
#  include <red_tide_inp.h>
# elif defined BIO_UMAINE31
#  include <bio_UMAINE31_inp.h>
# elif defined BIO_UMAINE13
#  include <bio_UMAINE13_inp.h>
# elif defined BIO_UMAINE15
#  include <bio_UMAINE15_inp.h>
# endif
#else
      SUBROUTINE read_BioPar
      END SUBROUTINE read_BioPar
#endif
