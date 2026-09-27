#!/bin/csh -f 

# svn $Id: plt_html.sh 833 2017-01-24 22:20:16Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
# Copyright (c) 2002-2017 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                                                                       :::
# This script generates several plots from history NetCDF files.  It    :::
# was used during the 2006 Shallow Water Acustics Experiments to create :::
# all the forecast fields shown on ROMS website.                        :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Set application prefix.

set PREFIX="sw06c"

# Set starting and ending year day of assimilation cycle.

set STR_DAY=197
set END_DAY=225

# Set data assimilation cycle time window (days).

set DayStep=2

# Set NCAR plotting source, 4DVAR and storage directories.

set PLTSRC="/p/arango/ocean/plot"
set MYROOT="/p/arango/ocean/toms/adjoint/Test/SW06c"
set STORAGE="/p/arango/ocean/toms/adjoint/Test/SW06c/Storage"

# Set directory to store all plots.

set PLTDIR=${MYROOT}/Plots

# Set various plotting set-up files.

set var_file=${PLTSRC}/Data/varid.dat
set pa1_file=${PLTSRC}/Palettes/natlan1.pal
set pa2_file=${PLTSRC}/Palettes/natlan2.pal
set bth_file=${PLTSRC}/Palettes/topo2.pal
set vel_file=${PLTSRC}/Palettes/vel.pal
set par_file=${MYROOT}/Data/default.cnt
set cst_file=${PLTSRC}/Data/NAeast_full.cst

# Set plot standard output file name.

set LOG="html.log"

# Set output NCAR filename.

set NCGM="ncgm"
setenv NCARG_GKS_OUTPUT $NCGM

# Set Application grid NetCDF file

set grd_file=${MYROOT}/Data/sw06_grid_2.nc

# Set application title.

set TITLE1="ROMS 3.0"
set TITLE2="Shallow Water Acoustics 2006 Experiment"
set TITLE3="Incremental, Strong Constraint 4DVAR"
set TITLE4="Sequential Assimilation"

# Set various plotting standard input parameters.

set YEAR=2006
set YDAY=0

set GRID=1.0            # Full Grid
set BLAT=37.2
set TLAT=41.0
set LLON=-76.0
set RLON=-70.5

set grid=0.1            # SW06 intensive observations Grid
set blat=39.1           # we need to get the correct values
set tlat=39.8
set llon=-74.5
set rlon=-73.7

set VINT=0              # Vertical interpolation scheme

 set ISEC=0             # Model (i,j) indices
#set ISEC=2             # Spherical coordinates

if ($ISEC == 2) then
 set DELS=0.01
else
 set DELS=1.0
endif

if ($ISEC == 2) then    # I=20 cross-section positions
  set XS1=-74.1700
  set XE1=-73.9917
  set YS1=39.6367
  set YE1=39.5165
else
  set XS1=31
  set XE1=31
  set YS1=45
  set YE1=1
endif

# Set ROMS plotting package executables and scripts to convert NCAR
# CGM files to GIF,

if ($HOST == "moby.marine.rutgers.edu") then
 set CCNT="/home/arango/bin/ccnt"
 set CSEC="/home/arango/bin/csec"
 set NCGM2GIF="/home/arango/bin/ncgm2www"
 set MED="/opt/gnu/ncarg/bin/med"
 set CONVERT=0
 setenv NCARG_ROOT /opt/gnu/ncarg
else if ($HOST == "nopp") then
 set CCNT="/home/arango/bin/ccnt_solaris"
 set CSEC="/home/arango/bin/csec_solaris"
 set NCGM2GIF="/home/arango/bin/ncgm2www"
 set MED="/usr/local/bin/med"
 set CONVERT=1
 setenv NCARG_ROOT /usr/local
endif

#  Set output GIF files resolution

set RESOLUTION=512x512

#---------------------------------------------------------------------
# Loop over each assimilation cycle.
#---------------------------------------------------------------------

if ( -e $LOG ) then
  /bin/rm $LOG
endif

echo " "

date >> $LOG

set DT=0        # plotting interval (plot all records
set REC=0       # Starting time record (minus 1) to process

set DAY=$STR_DAY

set N=0

while ($DAY <= $END_DAY)

  @ N += 1

# Set primary and secondary files to plot.

  set pri_file=${STORAGE}/${PREFIX}_his_${DAY}.nc
  set sec_file=${STORAGE}/${PREFIX}_his_${DAY}.nc


  echo "Processing assimilation cycle starting on: $DAY"
  
  set SREC=-$REC
  set EREC=-$REC

#=============================================================================

  $CCNT >> & $LOG << EOF
$YEAR $YDAY
$TITLE1
$TITLE2
$TITLE3
Surface Temperature (C) and Currents (m/s)
1     NFIELDS: number of fields to plot. Line below, field(s) types:
23             field identification: FLDID(1:NFIELDS)
1     NLEVELS: number of depths/levels/isopycnals to plot (0 for all levels)
30             depths (<0), levels (>0) or isopycnals (>1000) to plot
$SREC FRSTD  : first day to plot
$EREC LASTD  : last day to plot
$DT   DSKIP  : plot every other DSKIP days (see below)
0     FINDX  : forecast record to process, if any (atmospheric files only).
$VINT VINTRP : vertical interpolation scheme: 0=linear, 1:cubic splines
11    PMIN   : field minimum value for color palette (0.0 for default)
26    PMAX   : field maximum value for color palette (0.0 for default)
0     ICNT   : draw contours between color bands: 0=no, 1=yes
0.0   ISOVAL : iso-surface value to process (see below)
1.2   VLWD   : vector line width (1.0 for default)
2.0   VLSCL  : vector length scale (1.0 for default)
2     IVINC  : vector grid sampling in the X-direction (1 for default)
2     JVINC  : vector grid sampling in the Y-direction (1 for default)
1     IREF   : secondary or reference field option (see below)
3     IDOVER : overlay field identification (for IREF=1,2 only)
0     LEVOVER: level of the overlay field (set to 0 if same as current FLDLEV)
0.0   RMIN   : overlay field minimum value to consider (0.0 for default)
0.0   RMAX   : overlay field maximum value to consider (0.0 for default)
$GRID LGRID  : Desired longitude/latitude grid spacing (degrees)
2     IPROJ  : map projection (see below).
0.0   PLON   : projection Pole longitude (west values are negative).
0.0   PLAT   : projection Pole latitude (south values are negative).
0.0   ROTA   : projection rotation angle (clockwise; degrees).
1     LMSK   : flag to color mask land: [0] no, [1] yes
-1    NPAGE  : number of plots per page (currently 1, 2, or 4)  
T     READGRD: logical switch to read in positions from grid NetCDF file.
F     PLTLOGO: logical switch draw Logo.
F     WRTHDR : logical switch to write out the plot header titles.
T     WRTBLAB: logical switch to write out the plot bottom title.
T     WRTRANG: logical switch to write out data range values and CI.
F     WRTFNAM: logical switch to write out input primary filename.
F     WRTDATE: logical switch to write out current date.
T     CST    : logical switch to read and plot coastlines and islands.
$BLAT $TLAT  : bottom and top map latitudes (south values are negative).
$LLON $RLON  : left and right map longitudes (west values are negative).
$var_file
$pa1_file
$par_file
$pri_file
$sec_file
$grd_file
$cst_file
EOF

mv -f $NCGM TVsur_${DAY}.ncgm
if (${CONVERT} == 1) then
  $NCGM2GIF TVsur_${DAY}.ncgm $RESOLUTION TVsur_${DAY} >> & $LOG
endif

#=============================================================================

  $CCNT >> & $LOG << EOF
$YEAR $YDAY
$TITLE1
$TITLE2
$TITLE3
Temperature (C) at 5m
1     NFIELDS: number of fields to plot. Line below, field(s) types:
23             field identification: FLDID(1:NFIELDS)
1     NLEVELS: number of depths/levels/isopycnals to plot (0 for all levels)
-5             depths (<0), levels (>0) or isopycnals (>1000) to plot
$SREC FRSTD  : first day to plot
$EREC LASTD  : last day to plot
$DT   DSKIP  : plot every other DSKIP days (see below)
0     FINDX  : forecast record to process, if any (atmospheric files only).
$VINT VINTRP : vertical interpolation scheme: 0=linear, 1:cubic splines
11    PMIN   : field minimum value for color palette (0.0 for default)
26    PMAX   : field maximum value for color palette (0.0 for default)
0     ICNT   : draw contours between color bands: 0=no, 1=yes
0.0   ISOVAL : iso-surface value to process (see below)
1.2   VLWD   : vector line width (1.0 for default)
4.0   VLSCL  : vector length scale (1.0 for default)
2     IVINC  : vector grid sampling in the X-direction (1 for default)
2     JVINC  : vector grid sampling in the Y-direction (1 for default)
0     IREF   : secondary or reference field option (see below)
0     IDOVER : overlay field identification (for IREF=1,2 only)
-5    LEVOVER: level of the overlay field (set to 0 if same as current FLDLEV)
0.0   RMIN   : overlay field minimum value to consider (0.0 for default)
0.0   RMAX   : overlay field maximum value to consider (0.0 for default)
$GRID LGRID  : Desired longitude/latitude grid spacing (degrees)
2     IPROJ  : map projection (see below).
0.0   PLON   : projection Pole longitude (west values are negative).
0.0   PLAT   : projection Pole latitude (south values are negative).
0.0   ROTA   : projection rotation angle (clockwise; degrees).
1     LMSK   : flag to color mask land: [0] no, [1] yes
-1    NPAGE  : number of plots per page (currently 1, 2, or 4)  
T     READGRD: logical switch to read in positions from grid NetCDF file.
F     PLTLOGO: logical switch draw Logo.
F     WRTHDR : logical switch to write out the plot header titles.
T     WRTBLAB: logical switch to write out the plot bottom title.
T     WRTRANG: logical switch to write out data range values and CI.
F     WRTFNAM: logical switch to write out input primary filename.
F     WRTDATE: logical switch to write out current date.
T     CST    : logical switch to read and plot coastlines and islands.
$BLAT $TLAT  : bottom and top map latitudes (south values are negative).
$LLON $RLON  : left and right map longitudes (west values are negative).
$var_file
$pa1_file
$par_file
$pri_file
$sec_file
$grd_file
$cst_file
EOF

mv -f $NCGM Temp5m_${DAY}.ncgm
if (${CONVERT} == 1) then
  $NCGM2GIF Temp5m_${DAY}.ncgm $RESOLUTION Temp5m_${DAY} >> & $LOG
endif

#=============================================================================

  $CCNT >> & $LOG << EOF
$YEAR $YDAY
$TITLE1
$TITLE2
$TITLE3
Salinity at 5m
1     NFIELDS: number of fields to plot. Line below, field(s) types:
24             field identification: FLDID(1:NFIELDS)
1     NLEVELS: number of depths/levels/isopycnals to plot (0 for all levels)
-5             depths (<0), levels (>0) or isopycnals (>1000) to plot
$SREC FRSTD  : first day to plot
$EREC LASTD  : last day to plot
$DT   DSKIP  : plot every other DSKIP days (see below)
0     FINDX  : forecast record to process, if any (atmospheric files only).
$VINT VINTRP : vertical interpolation scheme: 0=linear, 1:cubic splines
30    PMIN   : field minimum value for color palette (0.0 for default)
34.5  PMAX   : field maximum value for color palette (0.0 for default)
0     ICNT   : draw contours between color bands: 0=no, 1=yes
0.0   ISOVAL : iso-surface value to process (see below)
1.2   VLWD   : vector line width (1.0 for default)
4.0   VLSCL  : vector length scale (1.0 for default)
2     IVINC  : vector grid sampling in the X-direction (1 for default)
2     JVINC  : vector grid sampling in the Y-direction (1 for default)
0     IREF   : secondary or reference field option (see below)
3     IDOVER : overlay field identification (for IREF=1,2 only)
-5    LEVOVER: level of the overlay field (set to 0 if same as current FLDLEV)
0.0   RMIN   : overlay field minimum value to consider (0.0 for default)
0.0   RMAX   : overlay field maximum value to consider (0.0 for default)
$GRID LGRID  : Desired longitude/latitude grid spacing (degrees)
2     IPROJ  : map projection (see below).
0.0   PLON   : projection Pole longitude (west values are negative).
0.0   PLAT   : projection Pole latitude (south values are negative).
0.0   ROTA   : projection rotation angle (clockwise; degrees).
1     LMSK   : flag to color mask land: [0] no, [1] yes
-1    NPAGE  : number of plots per page (currently 1, 2, or 4)  
T     READGRD: logical switch to read in positions from grid NetCDF file.
F     PLTLOGO: logical switch draw Logo.
F     WRTHDR : logical switch to write out the plot header titles.
T     WRTBLAB: logical switch to write out the plot bottom title.
T     WRTRANG: logical switch to write out data range values and CI.
F     WRTFNAM: logical switch to write out input primary filename.
F     WRTDATE: logical switch to write out current date.
T     CST    : logical switch to read and plot coastlines and islands.
$BLAT $TLAT  : bottom and top map latitudes (south values are negative).
$LLON $RLON  : left and right map longitudes (west values are negative).
$var_file
$pa1_file
$par_file
$pri_file
$sec_file
$grd_file
$cst_file
EOF

mv -f $NCGM Salt5m_${DAY}.ncgm
if (${CONVERT} == 1) then
  $NCGM2GIF Salt5m_${DAY}.ncgm $RESOLUTION Salt5m_${DAY} >> & $LOG
endif

#=============================================================================

  $CCNT >> & $LOG << EOF
$YEAR $YDAY
$TITLE1
$TITLE2
$TITLE3
Temperature (C) at 30m
1     NFIELDS: number of fields to plot. Line below, field(s) types:
23             field identification: FLDID(1:NFIELDS)
1     NLEVELS: number of depths/levels/isopycnals to plot (0 for all levels)
-30             depths (<0), levels (>0) or isopycnals (>1000) to plot
$SREC FRSTD  : first day to plot
$EREC LASTD  : last day to plot
$DT   DSKIP  : plot every other DSKIP days (see below)
0     FINDX  : forecast record to process, if any (atmospheric files only).
$VINT VINTRP : vertical interpolation scheme: 0=linear, 1:cubic splines
11    PMIN   : field minimum value for color palette (0.0 for default)
26    PMAX   : field maximum value for color palette (0.0 for default)
0     ICNT   : draw contours between color bands: 0=no, 1=yes
0.0   ISOVAL : iso-surface value to process (see below)
1.2   VLWD   : vector line width (1.0 for default)
4.0   VLSCL  : vector length scale (1.0 for default)
2     IVINC  : vector grid sampling in the X-direction (1 for default)
2     JVINC  : vector grid sampling in the Y-direction (1 for default)
0     IREF   : secondary or reference field option (see below)
0     IDOVER : overlay field identification (for IREF=1,2 only)
-5    LEVOVER: level of the overlay field (set to 0 if same as current FLDLEV)
0.0   RMIN   : overlay field minimum value to consider (0.0 for default)
0.0   RMAX   : overlay field maximum value to consider (0.0 for default)
$GRID LGRID  : Desired longitude/latitude grid spacing (degrees)
2     IPROJ  : map projection (see below).
0.0   PLON   : projection Pole longitude (west values are negative).
0.0   PLAT   : projection Pole latitude (south values are negative).
0.0   ROTA   : projection rotation angle (clockwise; degrees).
1     LMSK   : flag to color mask land: [0] no, [1] yes
-1    NPAGE  : number of plots per page (currently 1, 2, or 4)  
T     READGRD: logical switch to read in positions from grid NetCDF file.
F     PLTLOGO: logical switch draw Logo.
F     WRTHDR : logical switch to write out the plot header titles.
T     WRTBLAB: logical switch to write out the plot bottom title.
T     WRTRANG: logical switch to write out data range values and CI.
F     WRTFNAM: logical switch to write out input primary filename.
F     WRTDATE: logical switch to write out current date.
T     CST    : logical switch to read and plot coastlines and islands.
$BLAT $TLAT  : bottom and top map latitudes (south values are negative).
$LLON $RLON  : left and right map longitudes (west values are negative).
$var_file
$pa1_file
$par_file
$pri_file
$sec_file
$grd_file
$cst_file
EOF

mv -f $NCGM Temp30m_${DAY}.ncgm
if (${CONVERT} == 1) then
  $NCGM2GIF Temp30m_${DAY}.ncgm $RESOLUTION Temp30m_${DAY} >> & $LOG
endif

#=============================================================================

  $CCNT >> & $LOG << EOF
$YEAR $YDAY
$TITLE1
$TITLE2
$TITLE3
Salinity at 30m
1     NFIELDS: number of fields to plot. Line below, field(s) types:
24             field identification: FLDID(1:NFIELDS)
1     NLEVELS: number of depths/levels/isopycnals to plot (0 for all levels)
-30            depths (<0), levels (>0) or isopycnals (>1000) to plot
$SREC FRSTD  : first day to plot
$EREC LASTD  : last day to plot
$DT   DSKIP  : plot every other DSKIP days (see below)
0     FINDX  : forecast record to process, if any (atmospheric files only).
$VINT VINTRP : vertical interpolation scheme: 0=linear, 1:cubic splines
30    PMIN   : field minimum value for color palette (0.0 for default)
34.5  PMAX   : field maximum value for color palette (0.0 for default)
0     ICNT   : draw contours between color bands: 0=no, 1=yes
0.0   ISOVAL : iso-surface value to process (see below)
1.2   VLWD   : vector line width (1.0 for default)
4.0   VLSCL  : vector length scale (1.0 for default)
2     IVINC  : vector grid sampling in the X-direction (1 for default)
2     JVINC  : vector grid sampling in the Y-direction (1 for default)
0     IREF   : secondary or reference field option (see below)
3     IDOVER : overlay field identification (for IREF=1,2 only)
-5    LEVOVER: level of the overlay field (set to 0 if same as current FLDLEV)
0.0   RMIN   : overlay field minimum value to consider (0.0 for default)
0.0   RMAX   : overlay field maximum value to consider (0.0 for default)
$GRID LGRID  : Desired longitude/latitude grid spacing (degrees)
2     IPROJ  : map projection (see below).
0.0   PLON   : projection Pole longitude (west values are negative).
0.0   PLAT   : projection Pole latitude (south values are negative).
0.0   ROTA   : projection rotation angle (clockwise; degrees).
1     LMSK   : flag to color mask land: [0] no, [1] yes
-1    NPAGE  : number of plots per page (currently 1, 2, or 4)  
T     READGRD: logical switch to read in positions from grid NetCDF file.
F     PLTLOGO: logical switch draw Logo.
F     WRTHDR : logical switch to write out the plot header titles.
T     WRTBLAB: logical switch to write out the plot bottom title.
T     WRTRANG: logical switch to write out data range values and CI.
F     WRTFNAM: logical switch to write out input primary filename.
F     WRTDATE: logical switch to write out current date.
T     CST    : logical switch to read and plot coastlines and islands.
$BLAT $TLAT  : bottom and top map latitudes (south values are negative).
$LLON $RLON  : left and right map longitudes (west values are negative).
$var_file
$pa1_file
$par_file
$pri_file
$sec_file
$grd_file
$cst_file
EOF

mv -f $NCGM Salt30m_${DAY}.ncgm
if (${CONVERT} == 1) then
  $NCGM2GIF Salt30m_${DAY}.ncgm $RESOLUTION Salt30m_${DAY} >> & $LOG
endif

#=============================================================================

  $CSEC >> & $LOG << EOF
$YEAR $YDAY
$TITLE1
$TITLE2
$TITLE3
Potential Temperature (C) 
1     NFIELDS: number of fields to plot. Line below, field(s) types:
23             field identification: FLDID(1:NFIELDS).
$SREC FRSTD  : first day to plot
$EREC LASTD  : last day to plot
$DT   DSKIP  : plot every other DSKIP days (see below)
0     FINDX  : forecast record to process, if any (atmospheric files only).
$ISEC ISECPOS: section positions: [0] grid units, [1] Cartesian, [2] spherical
$XS1  XSEC1  : starting section X-position (grid units, or km, or degrees).
$YS1  YSEC1  : starting section Y-position (grid units, or km, or degrees).
$XE1  XSEC2  : ending section X-position (grid units, or km, or degrees).
$YE1  YSEC2  : ending section Y-position (grid units, or km, or degrees).
$DELS DELS   : section horizontal grid spacing (km or degrees).
0     SZTOP  : shallowest section depth to consider (meters).
100   SZBOT  : deepest section depth to consider (meters).
60    KMS    : number of refiment section levels between SZTOP and SZBOT.
$VINT VINTRP : vertical interpolation: [0] linear, [1] cubic splines.
11.0  PMIN   : field minimum value for color palette (0.0 for default).
26.0  PMAX   : field maximum value for color palette (0.0 for default).
0     ICNT   : draw contours between color bands: 0=no, 1=yes.
0     IREF   : secondary or reference field option (see below).
22    IDOVER : overlay field identification (for IREF=1,2 only).
0.0   RMIN   : overlay field minimum value to consider (0.0 for default).
0.0   RMAX   : overlay field maximum value to consider (0.0 for default).
1     NPAGE  : number of plots per page (currently 1, 2, or 4).
1     IFILL  : mask area below water column: [0] no, [1] yes.
F     READGRD: logical switch to read in positions from grid NetCDF file.
F     PLTLOGO: logical switch Logo.
F     WRTHDR : logical switch to write out the plot header titles.
T     WRTBLAB: logical switch to write out the plot bottom title.
T     WRTRANG: logical switch to write out data range values and CI.
F     WRTFNAM: logical switch to write out input primary filename.
F     WRTDATE: logical switch to write out current date.
$var_file
$pa1_file
$par_file
$pri_file
$sec_file
$grd_file
EOF

mv -f $NCGM Temp_${DAY}_S1.ncgm
if (${CONVERT} == 1) then
  $NCGM2GIF Temp_${DAY}_S1.ncgm $RESOLUTION Temp_${DAY}_S1 >> & $LOG
endif

#=============================================================================

  $CSEC >> & $LOG << EOF
$YEAR $YDAY
$TITLE1
$TITLE2
$TITLE3
Salinity 
1     NFIELDS: number of fields to plot. Line below, field(s) types:
24             field identification: FLDID(1:NFIELDS).
$SREC FRSTD  : first day to plot
$EREC LASTD  : last day to plot
$DT   DSKIP  : plot every other DSKIP days (see below)
0     FINDX  : forecast record to process, if any (atmospheric files only).
$ISEC ISECPOS: section positions: [0] grid units, [1] Cartesian, [2] spherical
$XS1  XSEC1  : starting section X-position (grid units, or km, or degrees).
$YS1  YSEC1  : starting section Y-position (grid units, or km, or degrees).
$XE1  XSEC2  : ending section X-position (grid units, or km, or degrees).
$YE1  YSEC2  : ending section Y-position (grid units, or km, or degrees).
$DELS DELS   : section horizontal grid spacing (km or degrees).
0     SZTOP  : shallowest section depth to consider (meters).
100   SZBOT  : deepest section depth to consider (meters).
60    KMS    : number of refiment section levels between SZTOP and SZBOT.
$VINT VINTRP : vertical interpolation: [0] linear, [1] cubic splines.
29.5  PMIN   : field minimum value for color palette (0.0 for default).
35.5  PMAX   : field maximum value for color palette (0.0 for default).
0     ICNT   : draw contours between color bands: 0=no, 1=yes.
0     IREF   : secondary or reference field option (see below).
22    IDOVER : overlay field identification (for IREF=1,2 only).
0.0   RMIN   : overlay field minimum value to consider (0.0 for default).
0.0   RMAX   : overlay field maximum value to consider (0.0 for default).
1     NPAGE  : number of plots per page (currently 1, 2, or 4).
1     IFILL  : mask area below water column: [0] no, [1] yes.
F     READGRD: logical switch to read in positions from grid NetCDF file.
F     PLTLOGO: logical switch Logo.
F     WRTHDR : logical switch to write out the plot header titles.
T     WRTBLAB: logical switch to write out the plot bottom title.
T     WRTRANG: logical switch to write out data range values and CI.
F     WRTFNAM: logical switch to write out input primary filename.
F     WRTDATE: logical switch to write out current date.
$var_file
$pa1_file
$par_file
$pri_file
$sec_file
$grd_file
EOF

mv -f $NCGM Salt_${DAY}_S1.ncgm
if (${CONVERT} == 1) then
  $NCGM2GIF Salt_${DAY}_S1.ncgm $RESOLUTION Salt_${DAY}_S1 >> & $LOG
endif

#=============================================================================

  $CSEC >> & $LOG << EOF
$YEAR $YDAY
$TITLE1
$TITLE2
$TITLE3
Sound Speed 
1     NFIELDS: number of fields to plot. Line below, field(s) types:
37             field identification: FLDID(1:NFIELDS).
$SREC FRSTD  : first day to plot
$EREC LASTD  : last day to plot
$DT   DSKIP  : plot every other DSKIP days (see below)
0     FINDX  : forecast record to process, if any (atmospheric files only).
$ISEC ISECPOS: section positions: [0] grid units, [1] Cartesian, [2] spherical
$XS1  XSEC1  : starting section X-position (grid units, or km, or degrees).
$YS1  YSEC1  : starting section Y-position (grid units, or km, or degrees).
$XE1  XSEC2  : ending section X-position (grid units, or km, or degrees).
$YE1  YSEC2  : ending section Y-position (grid units, or km, or degrees).
$DELS DELS   : section horizontal grid spacing (km or degrees).
0     SZTOP  : shallowest section depth to consider (meters).
100   SZBOT  : deepest section depth to consider (meters).
60    KMS    : number of refiment section levels between SZTOP and SZBOT.
$VINT VINTRP : vertical interpolation: [0] linear, [1] cubic splines.
1520  PMIN   : field minimum value for color palette (0.0 for default).
1550  PMAX   : field maximum value for color palette (0.0 for default).
0     ICNT   : draw contours between color bands: 0=no, 1=yes.
0     IREF   : secondary or reference field option (see below).
22    IDOVER : overlay field identification (for IREF=1,2 only).
0.0   RMIN   : overlay field minimum value to consider (0.0 for default).
0.0   RMAX   : overlay field maximum value to consider (0.0 for default).
1     NPAGE  : number of plots per page (currently 1, 2, or 4).
1     IFILL  : mask area below water column: [0] no, [1] yes.
F     READGRD: logical switch to read in positions from grid NetCDF file.
F     PLTLOGO: logical switch Logo.
F     WRTHDR : logical switch to write out the plot header titles.
T     WRTBLAB: logical switch to write out the plot bottom title.
T     WRTRANG: logical switch to write out data range values and CI.
F     WRTFNAM: logical switch to write out input primary filename.
F     WRTDATE: logical switch to write out current date.
$var_file
$pa1_file
$par_file
$pri_file
$sec_file
$grd_file
EOF

mv -f $NCGM sound_${DAY}_S1.ncgm
if (${CONVERT} == 1) then
  $NCGM2GIF sound_${DAY}_S1.ncgm $RESOLUTION sound_${DAY}_S1 >> & $LOG
endif

# End of assimlation cycle

  @ DAY += $DayStep

end
