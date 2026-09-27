#!/bin/csh -f
#
# svn $Id$
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2017 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                                                                       :::
# Description: Converts an NCGM file to a PNG file                      :::
#              (single or multi frame)                                  :::
#                                                                       :::
#   This script should be invoked with:                                 :::
#                                                                       :::
#       ncgm2png.sh [options] ncgm_filename                             :::
#                                                                       :::
#   If the file "ncgm_filename" is of the form "xxx.ncgm", then the     :::
#   resultant PNG file will be called "xxx.png".  Otherwise, the PNG    :::
#   file will be called "ncgm_filename.png"                             :::
#                                                                       :::
#   The basic conversion steps taken are:                               :::
#                                                                       :::
#     NCGM -> individual NCGMs (if needed) -> raster file -> PNG        :::
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
#   Options:                                                            :::
#                                                                       :::
#    -i             prompt user whenever a file might be overwritten.   :::
#                   The default is to overwrite files.                  :::
#                                                                       :::
#    -fmt [ras|vec] ctrans output format: raster or vector. For raster  :::
#                   you need 32-bit NCAR libraries. The vector format   :::
#                   is possible with either 32- 0r 64-bit libraries.    :::
#                                                                       :::
#    -res           specifies output resolution:                        :::
#                                                                       :::
#                   raster interface: -res {w}x{h}                      :::
#                                                                       :::
#                     resolution in pixels where {w} and {h} are        :::
#                     integers specifying width and height in pixels.   :::
#                     Default is 500x500                                :::
#                                                                       :::
#                   vector interface: -res {d}                          :::
#                                                                       :::
#                     density {d} in dots per inch. Default is 300dpi   :::
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
#     To convert a NCGM file called "gmeta" to a PNG file:              :::
#                                                                       :::
#        ncgm2png.sh -fmt ras -res 1024x1024 gmeta  (raster interface)  :::
#                                                                       :::
#        ncgm2png.sh -fmt vec -res 300dpi gmeta     (vector interface)  :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#  Initialize input options.

set clobberfiles
set png_files

set loopstr = ""
set delay = 50
set dpi = 300
set wresltn = 500
set hresltn = 500
set fmt = ras
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
        echo "  ncgm2png.sh: The loop value must be an integer >= 0."
        echo ""
        exit 1
      endif
      set loopstr = ($loopstr "-l$loops")
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
      echo "    usage: ncgm2png.sh [options] ncgm_filename"
      echo ""
      echo "    Options:"
      echo ""
      echo "    -i              prompt user to overwrite files"
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

#  Set PNG processing program.

if ( $fmt == "ras" ) then
  set CONVERT="/opt/local/bin/convert"
  set CTRANS=$NCARG_ROOT/bin/ctrans
else if ( $fmt == "vec" ) then
  set CONVERT="/opt/local/bin/convert +antialias -density ${dpi} -trim +repage"
  set CTRANS=$NCARG_ROOT/bin/ctrans
endif

if (! -f $ncgmfile) then
  echo ""
  echo "  ncgm2png.sh: $ncgmfile doesn't exist"
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
set pngfile = $rootname.png

#--------------------------------------------------------------------------
#  Make sure we have all the programs we need.
#--------------------------------------------------------------------------

set check_apps = no

if ($check_apps == "yes") then
  set apps = (ctrans ncgmstat convert pngmerge)

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
  echo "  ncgm2png.sh:   Not measuring any frames in '$ncgmfile'."
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

  if (-f $pngfile) then
    echo ""
    echo "    '$pngfile' already exists."
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
  /bin/cp $ncgmfile {$rootname}_001.ncgm
endif

#--------------------------------------------------------------------------
#  Convert NCGM(s) to raster or vector file(s).
#--------------------------------------------------------------------------

echo ""
echo "    Converting your NCGM(s) to PNG..."
echo ""

@ num = 1
while ( $num <= $nframes )
  set newnum = `echo $num | awk '{printf("%03d\n",$1)}'`
  set ctransfile = ${rootname}_${newnum}.${fmtext}
  if ( $fmt == "ras" ) then
    $CTRANS -d $ctransfmt -res ${wresltn}x${hresltn} ${rootname}_${newnum}.ncgm >! $ctransfile
  else if ( $fmt == "vec" ) then
    $CTRANS -d $ctransfmt ${rootname}_${newnum}.ncgm >! $ctransfile
  else
    echo "Invalid format specified. The variable fmt must be set to 'ras' or 'vec'"
  endif
  if ($status != 0) then
    echo ""
    echo "  ncgm2png.sh:  The 'ctrans' command failed. Please check your NCGM"
    echo "                to be sure it is valid."
    echo ""
    exit 1
  endif

#--------------------------------------------------------------------------
#  Convert raster or vector file to PNG.
#--------------------------------------------------------------------------
 
  echo "    Processing file: $ctransfile"

  if ($netpbm == 1) then
    if ($fmt == "ras") then
      rasttopnm $ctransfile | ppmtopng > ${rootname}_{$newnum}.png
    else
      pstopnm $ctransfile | ppmtopng > ${rootname}_{$newnum}.png
    endif
  else
    $CONVERT -flatten $ctransfile ${rootname}_{$newnum}.png
  endif

  if ($status != 0) then
    echo ""
    echo "  ncgm2png.sh: The 'convert' command failed."
    echo ""
    exit 1
  endif
  /bin/rm $ctransfile ${rootname}_${newnum}.ncgm
  set png_files = ($png_files ${rootname}_{$newnum}.png)
  @ num+=1
end

if ($nframes == 1) then
  /bin/mv {$rootname}_001.png $pngfile
  set png_files = $pngfile
else
# what to do here? How to concatenate PNG files?
endif

echo ""
echo "    'ncgm2png.sh' completed successfully."
echo ""
echo "     PNG file(s): $png_files"
echo ""

exit 0
