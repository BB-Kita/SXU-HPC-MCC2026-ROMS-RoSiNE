#ifndef rmdocinc
!
!svn $Id: pltncar.h 833 2017-01-24 22:20:16Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2017 The ROMS/TOMS Group                       ===
!    Licensed under a MIT/X style license                            ===
!    See License_ROMS.txt                                            ===
!=======================================================================
!                                                                      !
!  FontSize1 Font size for main header.                                !
!              (default: 20.0 plotter units)                           !
!  FontSize2 Font size for secondary top/bottom labels.                !
!              (default: 10.0 plotter units)                           !
!  FontSize3 Font size for color bar labels.                           !
!              (default: 10.0 plotter units)                           !
!  FontSize4 Font size for horizontal and vertical grid labels.        !
!              (default: 10.0 plotter units)                           !
!  FontSize5 Font size for Min, Max, and CI labels.                    !
!              (default: 7.0 plotter units)                            !
!  FontSize6 Font size for current date and filename labels.           !
!              (default: 5.0 plotter units)                            !
!  XLmap     map left edge coordinate (real; metric).                  !
!  XRmap     map right edge coordinate (real; metric).                 !
!  YBmap     map bottom edge coordinate (real; metric).                !
!  YTmap     map top edge coordinate (real; metric).                   !
!  add       logical switch to a add User coastlines to area map.      !
!  addezmap  logical switch to a add EZMAP coastlines to area map.     !
!  c_levels  logical switch to specify non-uniform contour levels.     !
!  cont      countour interval per field per level (real array)        !
!  cntint    input script contour interval.                            !
!  cnt_cll   width of labeled contour lines: [0] default, [2] double.  !
!  coorscl   Cartesian Coordinate units: [0] km, [1] m, [2] cm, [3] mm !
!  ds        grid spacing in inches (real).                            !
!  eastlon   Locigal switch to convert to EAST-longitudes.             !
!  geo_lab   switch to label geographic axis as degrees and minutes.   !
!            Otherwise, labels are fractional degrees (T/F).           !
!  hvelscl   horizontal velocity units: [0] m/s, [1] cm/s, [2] mm/s.   !
!  icint     contour level selection switch (integer):                 !
!            ICINT = 0  use contour interval provided by the USER      !
!            ICINT = 1  select contour interval internally             !
!  ifill     switch to mask area below water column: [0] no, [1] yes.  !
!  ipltpg    counter for the current map in the page (integer).        !
!  isoval    iso-surface value to process.                             !
!  ivinc     vector sampling in the x-direction (integer).             !
!  jvinc     vector sampling in the y-direction (integer).             !
!  lclrbar   switch to draw color bar (T/F).                           !
!  lcntlab   switch to label contour lines (T/F).                      !
!  ldomain   switch to draw domain box (T/F).                          !
!  ldrwusr   switch to draw user lines (T/F).                          !
!  lframe    switch to draw NCAR frame box (T/F).                      !
!  lgrid     map longitude/latitude grid separation.                   !
!  lhiglow   switch to label High/Low contour labels (T/F).            !
!  lmodday   switch to write model dday (forecast) in date label (T/F) !
!  lmsk      logical switch to activate land masking.                  !
!  lnozero   switch to omit drawing of zero-contour line (T/F).        !
!  log_cnt   switch for logarithmic contour interval.                  !
!  lrotvec   Switch to rotate vector components normal to the          !
!              cross-section coordinates (T/F)                         !
!  ltmarks   switch to draw box thick marks and labels (T/F).          !
!  lvcolor   switch to paint vectors by their magnitude (T/F)          !
!  lvecrho   switch to draw vectors at RHO-points (F: at PSI-points).  !
!  lvpinfo   switch to print out viewport information (T/F).           !
!  lzerozn   switch to ser surface depth to zero in cross-sections.    !
!  npage     number of plots per page: one, two, or four (integer).    !
!  pblat     map bottom latitude (real; negative SOUTH).               !
!  pframe    counter for number of frames plotted.                     !
!  plat      projection Pole latitude (real; negative SOUTH).          !
!  pllon     map left longitude (real; negative WEST).                 !
!  plon      projection Pole longitude (real; negative WEST).          !
!  pltlogo   logical switch to activate drawing of Harvard Logo.       !
!  plttyp    flag indicating plot type:                                !
!              [0] horizontal plot.                                    !
!              [1] vertical plots, cross-section.                      !
!  prlon     map right longitude (real; negative WEST).                !
!  ptlat     map top latitude (real; negative SOUTH).                  !
!  readgrd   logical switch to read in position from grid NetCDF.      !
!  rmax      overlay maximum value to consider.                        !
!  rmin      overlay minimum value to consider.                        !
!  rota      projection rotation angle (real; clockwise, degrees).     !
!  sclmap    scale factor for map labels (real).                       !
!  sdv_fac   standard deviation factor from mean for field range to    !
!            plot.                                                     !
!  secaxis   section axis coordinate labels: [0] metric, [1] spherical !
!  thckusr   User polylines thickness.                                 !
!  vlscl     vector length scale.                                      !
!  vper      percentage of maximum vector magnitude below which the    !
!            drawing of vectors are ommited because too small (real).  !
!  vscle     maximum vector magnitude for arrow scaling (real).        !
!  vec_amn   vector arrow minimum size (fraction of viewport width).   !
!  vec_amx   vector arrow maximum size (fraction of viewport width).   !
!  vec_mxx   Maximum vector text block X-coordinate.                   !
!  vec_mxy   Maximum vector text block Y-coordinate.                   !
!  vec_vrm   Vector reference magnitude.                               !
!  vec_vfr   vector fractional minimum.                                !
!  vec_vlc   vector low cutoff percentage.                             !
!  vec_vpo   vector positioning mode: [<0] head, [0] middle, [>0] tail !
!  wrtblab   logical switch to activate writing of bottom title.       !
!  wrtdate   logical switch to activate writing of current date.       !
!  wrtfnam   logical switch to activate writing of input filename.     !
!  wrthdr    logical switch to activate writing of header titles.      !
!  wrtrang1  logical switch to activate writing of range of field.     !
!  wrtrang2  logical switch to activate writing of range of overlay.   !
!  wvelscl   vertical velocity units: [0] m/s, [1] m/day, [2] cm/day.  !
!  xvpos     X-position to place velocity vector scale (km or degrees) !
!  yvpos     Y-position to place velocity vector scale (km or degrees) !
!  zref      Reference depth used to compute several quantities like   !
!            potential density, dynamic height and others.             !
!                                                                      !
!=======================================================================
!
#endif
       integer cfld, clev
       parameter (cfld=300,clev=50)
!
      common /lpltncar/ add, addezmap, c_levels, eastlon, geo_lab,      &
     &                  lclrbar, lcntlab, ldomain, ldrwusr, lframe,     &
     &                  lhiglow, lmodday, lnozero, log_cnt, lrotvec,    &
     &                  ltmarks, lvcolor, lvecrho, lvpinfo, lzerozn,    &
     &                  pltlogo, readgrd, wrtblab, wrtdate, wrtfnam,    &
     &                  wrthdr, wrtrang1, wrtrang2
      logical add, addezmap, c_levels, eastlon, geo_lab, lclrbar,       &
     &        lcntlab, ldomain, ldrwusr, lframe, lhiglow, lmodday,      &
     &        lnozero, log_cnt, lrotvec, ltmarks, lvcolor, lvecrho,     &
     &        lvpinfo, lzerozn, pltlogo, readgrd, wrtblab, wrtdate,     &
     &        wrtfnam, wrthdr, wrtrang1, wrtrang2
!
      common /ipltncar/ coorscl, cnt_cll, hvelscl, icnt, ifill,         &
     &                  ipltpg, iproj, ivinc, jvinc, lmsk, nframe,      &
     &                  npage, pframe, plttyp, secaxis, vec_vpo,        &
     &                  wvelscl
      integer coorscl, cnt_cll, hvelscl, icnt, ifill, ipltpg, iproj,    &
     &        ivinc, jvinc, lmsk, nframe, npage, pframe, plttyp,        &
     &        secaxis, vec_vpo, wvelscl
!
      common /spltncar/ FontSize1, FontSize2, FontSize3, FontSize4,     &
     &                  FontSize5, FontSize6, XLmap, XRmap, YBmap,      &
     &                  YTmap, asize, cntval, delx, dely, ds, fb, fl,   &
     &                  fr, ft, isoval, lgrid, pblat, plat, pllon,      &
     &                  plon, pmax, pmin, prlon, ptlat, rota, rmax,     &
     &                  rmin, sclmap, sdv_fac, thckusr, ub, ul, ur, ut, &
     &                  vec_amn, vec_amx, vec_mxx, vec_mxy, vec_vfr,    &
     &                  vec_vlc, vec_vrm, vlscl, vlwd, vper, xvpos,     &
     &                  yvpos, zref
      real FontSize1, FontSize2, FontSize3, FontSize4, FontSize5,       &
     &     FontSize6,XLmap, XRmap, YBmap, YTmap, asize, cntval, delx,   &
     &     dely, ds, fb, fl, fr, ft, isoval, lgrid, pblat, plat, pllon, &
     &     plon, pmax, pmin, prlon, ptlat, rota, rmax, rmin, sclmap,    &
     &     sdv_fac, thckusr, ub, ul, ur, ut, vec_amn, vec_amx, vec_mxx, &
     &     vec_mxy,  vec_vfr, vec_vlc, vec_vrm, vlscl, vlwd, vper,      &
     &     xvpos, yvpos, zref
!
      common /fpltncar/ cont
      real cont(0:cfld,clev)
