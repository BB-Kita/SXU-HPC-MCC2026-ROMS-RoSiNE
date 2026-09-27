#!/bin/csh -f
#
# svn $Id$
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2017 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                                                                       :::
# Description: Converts an NCAR NCGM file to a vanila CGM file          :::
#              (single or multi frame)                                  :::
#                                                                       :::
#   This script should be invoked with:                                 :::
#                                                                       :::
#       ncgm2cgm.sh ncgmfile cgmfile                                    :::
#                                                                       :::
#   If the "ncgmfile" and "cgmfile" arguments are not provided, the     :::
#   default input file name is "gmeta" and the default root file name   :::
#   is "ocean".                                                         :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#  Initialize.

set cgm_files
set ncgmfile=gmeta
set cgm_root=ocean

set NCGM2CGM=${NCARG_ROOT}/bin/ncgm2cgm

if ( ($#argv) > 0 ) then
  set ncgmfile=$1
  echo " NCGM file: $ncgmfile"
endif
if ( ($#argv) > 1 ) then
  set cgm_root=$2
  echo " CGM  file: $cgm_root"
endif

if ( -e NCGM.dat ) then
    /bin/rm NCGM.dat
endif
ln -s $ncgmfile NCGM.dat

#--------------------------------------------------------------------------
#  Count frames in NCGM file.
#--------------------------------------------------------------------------

set nframes = `ncgmstat -c $ncgmfile`

if ($nframes <= 0) then 
  echo ""
  echo "  ncgm2cgm.sh:   Not measuring any frames in '$ncgmfile'."
  echo "                 Are you sure it's a valid NCGM?"
  echo ""
  goto EXIT
endif

echo ""
if ( -e $ncgmfile ) then
    echo " ncgm2cgm.sh: input NCAR NCGM file: $ncgmfile with $nframes frames"
else
    echo " ncgm2cgm.sh: input NCAR NCGM file, $ncgmfile, not found."
    goto EXIT
endif
echo ""

#--------------------------------------------------------------------------
# Read NCAR's NCGM file and split it every frame into a file
#--------------------------------------------------------------------------

if ( -e med001.ncgm ) then
    /bin/rm med*.cgm
    echo " ncgm2cgm.sh: files medNNN.ncgm existed so they were deleted."
    echo ""
endif
med -e 'r NCGM.dat' -e 's $'

#--------------------------------------------------------------------------
# Convert each NCGM files into CGM
#--------------------------------------------------------------------------

@ num = 1
while ( $num <= $nframes )
  set newnum = `echo $num | awk '{printf("%03d\n",$1)}'`
  $NCGM2CGM  < med{$newnum}.ncgm > ${cgm_root}_${newnum}.cgm
  @ num+=1
  set cgm_files = ($cgm_files ${cgm_root}_${newnum}.cgm)
end

/bin/rm med*.ncgm
/bin/rm NCGM.dat

echo "    'ncgm2cgm.sh' completed successfully."
echo ""
echo "     CGM file(s): $cgm_files "
echo ""

EXIT:
