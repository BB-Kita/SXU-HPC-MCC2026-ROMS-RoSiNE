#ifndef rmdocinc
!
!svn $Id: pltlab.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  botlab    Bottom box title (character).                             !
!  date      Date of program execution (character).                    !
!  daylab    Bottom title specifying the date of the map (character).  !
!  ffile     Data file name label for map (character).                 !
!  fldlab    Current field label (character).                          !
!  julday1   Switch indicating Julian Day time for primary file.       !
!  julday1   Switch indicating Julian Day time for secondary file.     !
!  pltitle   Map titles (character array).                             !
!  wdate     Switch to write out date in botton title (logical).       !
!                                                                      !
!=======================================================================
!
#endif
      common /lpltlab/ julday1, julday2, wdate
      logical julday1, julday2, wdate
!
      common /pltlab/ fldlab, pltitle, daylab, botlab, ffile, date_str
      character*19 daylab
      character*24 ffile
      character*44 date_str
      character*60 fldlab
      character*80 botlab, pltitle(4)
