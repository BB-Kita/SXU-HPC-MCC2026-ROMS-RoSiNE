#!/bin/csh -f
#
# svn $Id: copyright.sh 833 2017-01-24 22:20:16Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2017 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::: David Robertson :::
#                                                                       :::
# ROMS/TOMS Copyright Update Script                                     :::
#                                                                       :::
# Script to update the copyright information on 'plot' source files.    :::
# This script replaces the copyright string in the source files and     :::
# updates the copyright svn property. This script must be executed      :::
# from top level of the 'plot' source code.                             :::
#                                                                       :::
# Usage:                                                                :::
#                                                                       :::
#    ./src/Bin/copyright.sh [options]                                   :::
#                                                                       :::
# Options:                                                              :::
#                                                                       :::
#    -nosvn    skip updating of the svn properties. Meant for systems   :::
#                that don't have the comandline svn tools.              :::
#                                                                       :::
#    -verbose  list files that are modified.                            :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set search = "2002-2016 The ROMS/TOMS"
set replace = "2002-2017 The ROMS/TOMS"

# Directories to search for replacements.

set c_dirs = "Data Palettes src"

set setsvn = 1

# verbose is a csh command to print all lines of the script so I changed
# this variable to "verb".

set verb = 0

while ( ($#argv) > 0 )
  switch ($1)
    case "-nosvn":
      shift
      set setsvn = 0
    breaksw

    case "-verbose":
      shift
      set verb = 1
    breaksw

    case "-*":
      echo ""
      echo "$0 : Unknown option [ $1 ]"
      echo ""
      echo "Available Options:"
      echo ""
      echo "-nosvn    skip updating of the svn properties. Meant for systems"
      echo "            that don't have the comandline svn tools."
      echo ""
      echo "-verbose  list files that are modified"
      echo ""
      exit 1
    breaksw

  endsw
end

echo ""
echo "Replacing Copyright String in Files ..."
echo ""

# The "! -path '*/.svn/*'" is there to keep it from messing with
# files in the .svn directories. The "! -name 'copyright.*'" is to
# keep it from messing with the file that's making the reaplacements.
# There is no way to redirect only stderr with csh.

foreach FILE ( `find ${c_dirs} ! -path '*/.svn/*' ! -name 'copyright.*' -type f -print` )

# Double check that we're not changing a file in a .svn folder.

  if ( `echo $FILE | grep -vc '.svn/'` ) then
    if ( $verb == 1 ) then
      grep -l "${search}" $FILE && sed -i -e "s|${search}|${replace}|g" $FILE
    else
      grep -l "${search}" $FILE > /dev/null && sed -i -e "s|${search}|${replace}|g" $FILE
    endif
  else
    echo "There is a .svn in the path: $FILE skipped"
  endif

end

echo ""
echo "Done."
echo ""

if ( $setsvn == 1 ) then
  svn propset -R copyright '(c) 2002-2017 The ROMS/TOMS Group' Data
  svn propset -R copyright '(c) 2002-2017 The ROMS/TOMS Group' Palettes
  svn propset -R copyright '(c) 2002-2017 The ROMS/TOMS Group' src
  svn propset copyright '(c) 2002-2017 The ROMS/TOMS Group' .
else
  echo ""
  echo "Not updating svn properties."
  echo ""
endif

