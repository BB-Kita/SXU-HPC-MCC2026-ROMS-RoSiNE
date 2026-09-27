#!/bin/csh -f
#
# svn $Id: ncgm2gif.sh 833 2017-01-24 22:20:16Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2017 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                                                                       :::
# Description: Converts an NCGM file to a GIF file                      :::
#              (single or multi frame)                                  :::
#                                                                       :::
#   This script should be invoked with:                                 :::
#                                                                       :::
#       ncgm2gif.sh [options] ncgm_filename                             :::
#                                                                       :::
#   If the file "ncgm_filename" is of the form "xxx.ncgm", then the     :::
#   resultant GIF file will be called "xxx.gif".  Otherwise, the GIF    :::
#   file will be called "ncgm_filename.gif"                             :::
#                                                                       :::
#   The basic conversion steps taken are:                               :::
#                                                                       :::
#     NCGM -> individual NCGMs (if needed) -> raster file -> GIF        :::
#                                                                       :::
#   Several pieces of software are required to run this script:         :::
#                                                                       :::
#       1. NCAR Graphics (ctrans and ncgmstat)                          :::
#                                                                       :::
#       2. Any image processing tools like:                             :::
#          - imconv (part of Image Tools)                               :::
#              //ftp.sdsc.edu/pub/sdsc/graphics/imtools/                :::
#          - convert (part of ImageMagick Tools)                        :::
#              http://imagemagic.org                                    :::
#          - netpbm toolkit                                             :::
#              http://netpbm.sf.net                                     :::
#                                                                       :::
#       3. gifmerge - only needed if you are planning to create         :::
#                     multi-frame GIF files (written by Rene K. Muller  :::
#                     and based on "txtmerge" written by Mark Podlipec) :::
#                                                                       :::
#          http://www.iis.ee.ethz.ch/~kiwi/GIFMerge/                    :::
#                                                                       :::
#          Currently, the GIF file concatenation is commented out.      :::
#                                                                       :::
#   Options:                                                            :::
#                                                                       :::
#    -i             prompt user whenever a file might be overwritten.   :::
#                   The default is to overwrite files.                  :::
#                                                                       :::
#    -fmt [ras|vec] ctrans output format: raster or vector. For raster  :::
#                   you need 32-bit NCAR libraries. The vector format   :::
#                   is possible with either 32- 0r 64-bit libraries.    :::
#                                                                       :::
#    -res {w}x{h}   specifies output resolution in pixels where         :::
#                   {w} and {h} are integers specifying width and       :::
#                    height in pixels. Default is 500x500.              :::
#                                                                       :::
#    -loop N        For a multi-framed NCGM, indicates how many times   :::
#                   you want the GIF animation to be run beyond the     :::
#                   initial run. N=0 indicates infinite looping, N=1    :::
#                   indicates the animation will be run twice, etc.     :::
#                   The default is no looping.                          :::
#                                                                       :::
#    -delay N       If looping is turned on (see '-loop' above),        :::
#                   indicates the delay between frames in units of      :::
#                   1/100th of a second. The default is N=50            :::
#                   (0.5 seconds between frames).                       :::
#                                                                       :::
#   Other options to add later:                                         :::
#                                                                       :::
#      1. Changing raster output used                                   :::
#      2. "-clean" option                                               :::
#      3. Allow to select which frames you want                         :::
#         (like Tom's ncgmgrab script)                                  :::
#      4. Allow to change output file name                              :::
#                                                                       :::
#   Example usages:                                                     :::
#                                                                       :::
#     1. To convert a single-frame NCGM called "gmeta" to a GIF file:   :::
#                                                                       :::
#          ncgm2gif.sh gmeta                                            :::
#                                                                       :::
#     2. To convert a multi-frame NCGM called "example.ncgm" to a       :::
#        multi-framed GIF file that is 200x200 pixels:                  :::
#                                                                       :::
#          ncgm2gif.sh -res 200x200 example.ncgm                        :::
#                                                                       :::
#     3. To convert a multi-frame NCGM called "example.ncgm" to a       :::
#        multi-framed GIF file, and have the animation run 4 times      :::
#        with a delay of 20/100 seconds between each frame:             :::
#                                                                       :::
#          ncgm2gif.sh -loop 3 -delay 20 example.ncgm                   :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#  Initialize input options.

set clobberfiles
set gif_files

set loopstr = ""
set delay = 50
set wresltn = 500
set hresltn = 500
#set fmt = ras
set fmt = vec
set netpbm = 0

#  Check processing parameters.

while ($#argv > 0)
  switch ($1)
    case "-i":
      shift
      unset clobberfiles
    breaksw

    case "-loop":
      shift
      set loops = $1
      if ($loops < 0) then
        echo ""
        echo "  ncgm2gif.sh: The loop value must be an integer >= 0."
        echo ""
        exit 1
      endif
      set loopstr = ($loopstr "-l$loops")
      shift
    breaksw

    case "-delay":
      shift
      set delay = $1
      if ($delay <= 0) then
        echo ""
        echo "  ncgm2gif.sh: The delay must be an integer greater than 0."
        echo ""
        exit 1
      endif
      set loopstr = ($loopstr "-$delay")
      shift
    breaksw

    case "-fmt":
      shift
      set fmt = $1
    breaksw

    case "-res":
      shift
      set wresltn = `expr $1 : '\(.*\)x.*'`
      set hresltn = `expr $1 : '.*x\(.*\)'`
      shift
    breaksw

    case "-*":
      echo "$0 : Unknown option <$1>"
      echo ""
      echo "    usage: ncgm2gif.sh [options] ncgm_filename"
      echo ""
      echo "    Options:"
      echo ""
      echo "    -i              prompt user to overwrite files"
      echo ""
      echo "    -loop N         how many times to animate GIF files"
      echo "                      N=0 infinite looping"
      echo "    -delay N        animation frame delay in 1/100th of a second"
      echo ""
      echo "    -fmt [ras|vec]  ctrans output format: raster or vector"
      echo ""
      echo "    -res {w}x{h}    output resolution in pixels"
      echo ""
      exit 1
    breaksw

    default:
      set ncgmfile = $1
      shift
    breaksw
  endsw
end

#--------------------------------------------------------------------------
#  Initialize 
#--------------------------------------------------------------------------

#  Intermediate format raster or vector.  Vector format is better.

if ( $fmt == "ras" ) then
  set ctransfmt = sun
  set fmtext = sun
else if ( $fmt == "vec" ) then
  set ctransfmt = ps.color
  set fmtext = ps
endif

#  Set NCAR Graphics processing programs.  The user needs to define
#  NCARG_ROOT environmental variable elsewhere.

set MED=${NCARG_ROOT}/bin/med
set CTRANS=${NCARG_ROOT}/bin/ctrans

#  Set GIF processing program.

if ( $fmt == "ras" ) then
# set CONVERT="/home/arango/bin/imconv/imconv"
  set CONVERT="/opt/local/bin/convert"
# setenv NCARG_ROOT $NCARG32_ROOT
  set CTRANS=$NCARG_ROOT/bin/ctrans
else if ( $fmt == "vec" ) then
# set CONVERT="/opt/local/bin/convert +antialias -density 108"
  set CONVERT="/opt/local/bin/convert +antialias -density 108 -trim +repage"
  set CTRANS=$NCARG_ROOT/bin/ctrans
endif

if (! -f $ncgmfile) then
  echo ""
  echo "  ncgm2gif.sh: $ncgmfile doesn't exist"
  echo ""
  exit 1
endif

if ($ncgmfile:e == "ncgm") then
  set rootname = $ncgmfile:r
else
  set rootname = $ncgmfile
endif

#  Set input and output file names.

set ctransfile = $rootname.$fmtext
set giffile = $rootname.gif

#--------------------------------------------------------------------------
#  Make sure we have all the programs we need.
#--------------------------------------------------------------------------

set check_apps = no

if ($check_apps == "yes") then
  set apps = (ctrans ncgmstat convert gifmerge)

  echo ""
  echo "    Checking to be sure we have all the necessary applications..."

  foreach app($apps)
    echo -n "        $app..."
    which $app >& /dev/null
    if ($status != 0) then
      echo "not found"
      echo ""
      exit 1
    endif
    echo "okay"
  end
endif

#--------------------------------------------------------------------------
#  Count frames in NCGM file.
#--------------------------------------------------------------------------

set nframes = `ncgmstat -c $ncgmfile`

if ($nframes <= 0) then 
  echo ""
  echo "  ncgm2gif.sh:   Not measuring any frames in '$ncgmfile'."
  echo "                 Are you sure it's a valid NCGM?"
  echo ""
  exit 1
endif

#--------------------------------------------------------------------------
#  Check to be sure we don't overwrite existing files.
#--------------------------------------------------------------------------

if (! $?clobberfiles) then
  if (-f $ctransfile) then
    echo ""
    echo "    '$ctransfile' already exists."
    echo -n "    Overwrite? (y/n) (n) "
    set answer = $<
    if ("$answer" != "y" && "$answer" != "Y") then
      exit 1
    endif
  endif

  if (-f $giffile) then
    echo ""
    echo "    '$giffile' already exists."
    echo -n "    Overwrite? (y/n) (n) "
    set answer = $<
    if ("$answer" != "y" && "$answer" != "Y") then
      exit 1
    endif
  endif
endif

#--------------------------------------------------------------------------
#  Split NCGM file into individual ncgm files.
#  Files will be called 'med001.ncgm', 'med002.ncgm', etc.
#--------------------------------------------------------------------------

if ($nframes > 1) then
  echo ""
  echo "    Splitting your $nframes-frame NCGM into individual NCGMs..."
  $MED -e "1,$ split $nframes $rootname" $ncgmfile
else
  /bin/cp $ncgmfile {$rootname}001.ncgm
endif

#--------------------------------------------------------------------------
#  Convert NCGM(s) to raster or vector file(s).
#--------------------------------------------------------------------------

echo ""
echo "    Converting your NCGM(s) to GIF..."
echo ""

@ num = 1
while ( $num <= $nframes )
  set newnum = `echo $num | awk '{printf("%03d\n",$1)}'`
  set ctransfile = ${rootname}_${newnum}.${fmtext}
  if ( $fmt == "ras" ) then
    $CTRANS -d $ctransfmt -res ${wresltn}x${hresltn} $rootname{$newnum}.ncgm >! $ctransfile
  else if ( $fmt == "vec" ) then
    $CTRANS  -d $ctransfmt $rootname{$newnum}.ncgm >! $ctransfile
  else
    echo "Invalid format specified. The variable fmt must be set to 'ras' or 'vec'"
  endif
  if ($status != 0) then
    echo ""
    echo "  ncgm2gif.sh:  The 'ctrans' command failed. Please check your NCGM"
    echo "                to be sure it is valid."
    echo ""
    exit 1
  endif

#--------------------------------------------------------------------------
#  Convert raster or vector file to GIF.
#--------------------------------------------------------------------------

  echo "    Processing file: $ctransfile"
 
  if ($netpbm == 1) then
    if ($fmt == "ras") then
      rasttopnm $ctransfile | ppmtogif > ${rootname}_${newnum}.gif
    else
      pstopnm $ctransfile | ppmtogif > ${rootname}_${newnum}.gif
    endif
  else
    $CONVERT -flatten $ctransfile ${rootname}_${newnum}.gif
  endif

  if ($status != 0) then
    echo ""
    echo "  ncgm2gif.sh: The 'convert' command failed."
    echo ""
    exit 1
  endif
  /bin/rm $ctransfile $rootname{$newnum}.ncgm
  set gif_files = ($gif_files ${rootname}_${newnum}.gif)
  @ num+=1
end

if ($nframes == 1) then
  /bin/mv {$rootname}001.gif $giffile
  set gif_files = $giffile
else
# echo "gifmerge $loopstr $rootname\[001-$num\].gif >! $giffile"
# gifmerge $loopstr $gif_files >! $giffile
# /bin/rm $gif_files
endif

echo ""
echo "    'ncgm2gif.sh' completed successfully."
echo ""
echo "     GIF file(s): $gif_files "
echo ""

exit 0
