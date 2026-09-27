#ifndef rmdocinc
!
!svn $Id: colpal.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  CLEVELS  Contour levels (real vector)                               !
!  IBARLAB  Labeling interval for color bands in the colorbar (integer)!
!           If IBARLAB=1, all color will be labeled                    !
!  IPAL     Color palette type (integer):                              !
!           IPAL = 1   only the (iR,iG,iB) values are considered by    !
!                      the application program.                        !
!           IPAL = 2   only the (R,G,B) values are considered by the   !
!                      application program.                            !
!  NCL      Number of color levels not including the background and    !
!           foreground colors (integer).                               !
!  RGBPAL   Read, green and blue color palette.  If IPAL=1, the colors !
!           are taken as values ranging from FLOAT(0) to FLOAT(255), or!
!           if IPAL=2, the colors are taken as values ranging from 0.0 !
!           to 1.0 (real array).                                       !
!                                                                      !
!=======================================================================
!
#endif
      common /colpal/ ncl, ibarlab, ipal, clevels, rgbpal
      integer ibarlab, ipal, ncl
      real clevels(300), rgbpal(3,300)
