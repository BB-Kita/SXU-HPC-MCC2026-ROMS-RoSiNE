#ifndef rmdocinc
!
!svn $Id: pltfld.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  Lrecord      Logical switch to process data by record number        !
!                 instead of time values (logical).                    !
!  a_ratio      Logical switch used to preserve aspect ratio.          !
!  dskip        Plotting day interval (real).                          !
!  dstart       Time stamp assigned to nitilization (real).            !
!  fieldid      Current field id (integer).                            !
!  fldid        Identification of field(s) to plot (integer array).    !
!  fldlev       Depths/levels/isopycnals values of the field to plot   !
!                 (real array).                                        !
!  fldnam1      Current field name component 1 (character).            !
!  fldnam2      Current field name component 2 (character).            !
!  frstd        First day to plot (real).                              !
!  frstr        First record to plot (integer).                        !
!  idover       Identification of field field (real).                  !
!  isosurf      Switch activated when plotting isosurfaces (logical).  !
!  iref         Switch for reference fields (integer):                 !
!                 [0] do not plot reference field                      !
!                 [1] plot overlay field                               !
!                 [2] primary - secondary (field subtraction)          !
!                 [3] day0 - dayN (field subtraction)                  !
!  interpolate  Switch to interpolate field to requested depths        !
!                 (logical).                                           !
!  iyear        Starting year for plot labeling purposes (integer).    !
!  lastd        Last day to plot (real).                               !
!  lastr        Last record to plot (integer).                         !
!  levless      Switch to indicate that the current field has no       !
!                  vertical levels (logical).                          !
!  levover      Depth/level/isopycnic value of field overlay (real).   !
!  nesting      Switch to overlay nesting in color (logical).          !
!  nlevels      Number of different levels to plot (integer).          !
!  nord         Order of Shapiro filter.                               !
!  nsappl       Number of applications of smoothing filter.            !
!  plotit       Switch to plot current field (logical).                !
!  ptype        Type of plot for current variable(s):                  !
!                  ptype = 1, scalar plot.                             !
!                  ptype = 2, streamlines plot.                        !
!                  ptype = 3, vector plot.                             !
!  rskip        Plotting record interval (integer).                    !
!  scalar1      Switch for current primary field type (logical).       !
!  scalar2      Switch for current secondary field type (logical).     !
!  smthout      Flag to smooth output data:                            !
!                 smthout = 0   => no smoothing.                       !
!                 smthout = 1   => use Bleck filter                    !
!                 smthout = 2   => use Shapiro filter                  !
!  spherical    Switch for current field coordinates (logical):        !
!                 True:  spherical                                     !
!                 False: Cartesian                                     !
!  stream1      Switch for current primary field type (logical).       !
!  stream2      Switch for current secondary field type (logical).     !
!  trecord      Current time record to process (integer).              !
!  vector1      Switch for current primary field type (logical).       !
!  vector2      Switch for current secondary field type (logical).     !
!  vintrp       Vertical interpolation scheme: [0] linear, [1] splines !
!  wbarbs1      Switch for current primary field type (logical).       !
!  wbarbs2      Switch for current secondary field type (logical).     !
!  yday         Starting year-day for plot labeling purposes (real).   !
!                                                                      !
!=======================================================================
!
#endif
      integer pscalar, pstream, pvector, pwbarbs
      parameter (pscalar=1, pstream=2, pvector=3, pwbarbs=4)
!
      common /lpltfld/ Lrecord, a_ratio, interpolate, isosurf, levless, &
     &                 nesting, plotit, scalar1, scalar2, spherical,    &
     &                 stream1, stream2, vector1, vector2, wbarbs1,     &
     &                 wbarbs2
      logical Lrecord, a_ratio, interpolate, isosurf, levless, nesting, &
     &        plotit, scalar1, scalar2, spherical, stream1, stream2,    &
     &        vector1, vector2, wbarbs1, wbarbs2
!
      common /ipltfld/ fieldid, frstr, idover, iref, iyear, lastr,      &
     &                 nfields, nlevels, nord, nsappl, ptype, rskip,    &
     &                 smthout, trecord, vintrp, fldid
      integer fieldid, frstr, idover, iref, iyear, lastr, nfields,      &
     &        nlevels, nord, nsappl, ptype, rskip, smthout, trecord,    &
     &        vintrp
      integer fldid(50)
!
      common /fpltfld/ dskip, dstart, frstd, lastd, yday, levover,      &
     &                 fldlev
      real dskip, dstart, frstd, lastd, levover, yday
      real fldlev(50)
!
      common /cpltfld/ fldnam1, fldnam2
      character*20 fldnam1, fldnam2
