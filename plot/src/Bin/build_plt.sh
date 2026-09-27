#!/bin/csh -f
#
# svn $Id: build_plt.sh 833 2017-01-24 22:20:16Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2017 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
#                                                                       :::
# ROMS/TOMS Plotting Package Compiling Script                           :::
#                                                                       :::
# Script to compile ROMS plotting package.                              :::
#                                                                       :::
# Q: How/why does this script work?                                     :::
#                                                                       :::
# A: This makefile configures user-defined options with a set of flags. :::
#    Browse the makefile to see these. If an option in the makefile     :::
#    uses the syntax ?= in setting the default, this means that make    :::
#    will check whether an environment variable by that name is set in  :::
#    the shell that calls make. If so the environment variable value    :::
#    overrides the default (and the user need not maintain separate     :::
#    makefiles, or frequently edit the makefile).                       :::
#                                                                       :::
# Usage:                                                                :::
#                                                                       :::
#    ./build.sh [options]                                               :::
#                                                                       :::
# Options:                                                              :::
#                                                                       :::
#    -j [N]      Compile in parallel using N CPUs                       :::
#                  omit argument for all available CPUs                 :::
#    -noclean    Do not clean already compiled objects                  :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set parallel = 0
set clean = 1

while ( ($#argv) > 0 )
  switch ($1)
    case "-noclean"
      shift
      set clean = 0
    breaksw

    case "-j"
      shift
      set parallel = 1
      if (`echo $1 | grep '^[0-9]\+$'` != "" ) then
        set NCPUS = "-j $1"
        shift
      else
        set NCPUS = "-j"
      endif
    breaksw

    case "-*":
      echo ""
      echo "$0 : Unknown option [ $1 ]"
      echo ""
      echo "Available Options:"
      echo ""
      echo "-j [N]      Compile in parallel using N CPUs"
      echo "              omit argument for all avaliable CPUs"
      echo "-noclean    Do not clean already compiled objects"
      echo ""
      exit 1
    breaksw

  endsw
end

# Set a local environmental variable to define the path to the directories
# where all this project's files are kept.

setenv MY_ROOT_DIR           ${HOME}/ocean/repository
setenv MY_PROJECT_DIR        ${PWD}

# The path to the user's local current ROMS plotting package source code.
#
# If using svn locally, this would be the user's Working Copy Path (WCPATH).
# Note that one advantage of maintaining your source code locally with svn
# is that when working simultaneously on multiple machines (e.g. a local
# workstation, a local cluster and a remote supercomputer) you can checkout
# the latest release and always get an up-to-date customized source on each
# machine. This script is designed to more easily allow for differing paths
# to the code and inputs on differing machines.

 setenv MY_PLT_SRC          ${MY_ROOT_DIR}/plot

# Set path of the directory containing makefile configuration (*.mk) files.
# The user has the option to specify a customized version of these files
# in a different directory than the one distributed with the source code,
# ${MY_PLT_SRC}/Compilers. If this is the case, the you need to keep
# these configurations files up-to-date.

 setenv COMPILERS            ${MY_PLT_SRC}/src/Compilers

#  Provide NCAR library version installed.  Several common blocks changes
#  in these versions.  These switch is used in routines Utility/cnt_frame.F
#  and Utility/hbackground.F to tune the appropriate parameters.

#setenv NCAR_VERSION NCARG410
#setenv NCAR_VERSION NCARG422
#setenv NCAR_VERSION NCARG430
#setenv NCAR_VERSION NCARG431
#setenv NCAR_VERSION NCARG441
 setenv NCAR_VERSION NCARG520

# Set tunable CPP options.
#
# Sometimes it is desirable to activate one or more CPP options to compile
# different algorithm options. If this is the case, specify each options
# here using the -D syntax. Notice also that you need to use shell's quoting
# syntax to enclose the definition.  Both single or double quotes work.

#setenv MY_CPP_FLAGS "-D"

# Other user defined environmental variables. See the ROMS makefile for
# details on other options the user might want to set here. Be sure to
# leave the switches meant to be off set to an empty string or commented
# out. Any string value (including off) will evaluate to TRUE in
# conditional if-statements.

#setenv USE_MPI             on          # distributed-memory parallelism
#setenv USE_MPIF90          on          # compile with mpif90 script
#setenv which_MPI           mpich       # compile with MPICH library
#setenv which_MPI           mpich2      # compile with MPICH2 library
#setenv which_MPI           openmpi     # compile with OpenMPI library

#setenv USE_OpenMP          on          # shared-memory parallelism

#setenv FORT                ifort
 setenv FORT                gfortran
#setenv FORT                pgi

#setenv PLT_DEBUG           on          # use Fortran debugging flags
 setenv PLT_LARGE           on          # activate 64-bit compilation
 setenv USE_NETCDF4         on          # compile with NetCDF-4 library
                                        # for OpenDAP support

#setenv USE_MY_LIBS         on          # use my library paths below

# There are several MPI libraries available. Here, we set the desired
# "mpif90" script to use during compilation. This only works if the make
# configuration file (say, Linux-pgi.mk) in the "Compilers" directory
# has the following definition for FC (Fortran Compiler) in the USE_MPI
# section:
#
#              FC := mpif90
#
# that is, "mpif90" defined without any path. Notice that the path
# where the MPI library is installed is computer dependent. Recall
# that you still need to use the appropriate "mpirun" to execute.
#
# However, the ROMS plotting package DOES NOT have currently parallel
# support. This is commented here for future use ...

if ($?USE_MPIF90) then
  switch ($FORT)

    case "ifort"
      if ($which_MPI == "mpich" ) then
        setenv PATH /opt/intelsoft/mpich/bin:$PATH
      else if ($which_MPI == "mpich2" ) then
        setenv PATH /opt/intelsoft/mpich2/bin:$PATH
      else if ($which_MPI == "openmpi" ) then
        setenv PATH /opt/intelsoft/openmpi/bin:$PATH
      endif
    breaksw

    case "pgi"
      if ($which_MPI == "mpich" ) then
        setenv PATH /opt/pgisoft/mpich/bin:$PATH
      else if ($which_MPI == "mpich2" ) then
        setenv PATH /opt/pgisoft/mpich2/bin:$PATH
      else if ($which_MPI == "openmpi" ) then
        setenv PATH /opt/pgisoft/openmpi/bin:$PATH
      endif
    breaksw

    case "gfortran"
      if ($which_MPI == "mpich2" ) then
        setenv PATH /opt/gfortransoft/mpich2/bin:$PATH
      else if ($which_MPI == "openmpi" ) then
        setenv PATH /opt/gfortransoft/openmpi/bin:$PATH
      endif
    breaksw

  endsw
endif

# If the USE_MY_LIBS is activated above, the path of the libraries
# required by ROMS can be set here using environmental variables
# which take precedence to the values specified in the make macro
# definitions file (Compilers/*.mk). For most applications, only
# the location of the NetCDF library is needed during compilation.
#
# Notice that when the USE_NETCDF4 macro is activated, we need the
# serial or parallel version of the NetCDF-4/HDF5 library. The
# configuration script NC_CONFIG (available since NetCDF 4.0.1)
# is used to set up all the required libraries according to the
# installed options (openDAP, netCDF4/HDF5 file format). The
# parallel library uses the MPI-I/O layer (usually available
# in MPICH2 and OpenMPI) requiring compiling with the selected
# MPI library.
#

if ($?USE_MY_LIBS) then

  switch ($FORT)

    case "ifort"

      setenv NCARG_ROOT           /opt/intelsoft/ncarg

      if ($?USE_NETCDF4) then
        if ($?USE_MPI) then
          if ($which_MPI == "mpich" ) then
            setenv NC_CONFIG      /opt/intelsoft/mpich/netcdf4/bin/nc-config
            setenv NETCDF_INCDIR  /opt/intelsoft/mpich/netcdf4/include
          else if ($which_MPI == "mpich2" ) then
            setenv NC_CONFIG      /opt/intelsoft/mpich2/netcdf4/bin/nc-config
            setenv NETCDF_INCDIR  /opt/intelsoft/mpich2/netcdf4/include
          else if ($which_MPI == "openmpi" ) then
            setenv NC_CONFIG      /opt/intelsoft/openmpi/netcdf4/bin/nc-config
            setenv NETCDF_INCDIR  /opt/intelsoft/openmpi/netcdf4/include
          endif
        else
          setenv NC_CONFIG        /opt/intelsoft/serial/netcdf4/bin/nc-config
          setenv NETCDF_INCDIR    /opt/intelsoft/serial/netcdf4/include
        endif
      else
        setenv NETCDF_INCDIR      /opt/intelsoft/serial/netcdf3/include
        setenv NETCDF_LIBDIR      /opt/intelsoft/serial/netcdf3/lib
      endif
    breaksw

    case "pgi"

      setenv NCARG_ROOT           /opt/pgisoft/ncarg

      if ($?USE_NETCDF4) then
        if ($?USE_MPI) then
          if ($which_MPI == "mpich" ) then
            setenv NC_CONFIG      /opt/pgisoft/mpich/netcdf4/bin/nc-config
            setenv NETCDF_INCDIR  /opt/pgisoft/mpich/netcdf4/include
          else if ($which_MPI == "mpich2" ) then
            setenv NC_CONFIG      /opt/pgisoft/mpich2/netcdf4/bin/nc-config
            setenv NETCDF_INCDIR  /opt/pgisoft/mpich2/netcdf4/include
          else if ($which_MPI == "openmpi" ) then
            setenv NC_CONFIG      /opt/pgisoft/openmpi/netcdf4/bin/nc-config
            setenv NETCDF_INCDIR  /opt/pgisoft/openmpi/netcdf4/include
          endif
        else
          setenv NC_CONFIG        /opt/pgisoft/serial/netcdf4/bin/nc-config
          setenv NETCDF_INCDIR    /opt/pgisoft/serial/netcdf4/include
        endif
      else
        setenv NETCDF_INCDIR      /opt/pgisoft/serial/netcdf3/include
        setenv NETCDF_LIBDIR      /opt/pgisoft/serial/netcdf3/lib
      endif
    breaksw

    case "gfortran"

      setenv NCARG_ROOT           /opt/gfortransoft/ncarg

      if ($?USE_NETCDF4) then
        if ($?USE_MPI) then
          if ($which_MPI == "mpich2" ) then
            setenv NC_CONFIG      /opt/gfortransoft/mpich2/netcdf4/bin/nc-config
            setenv NETCDF_INCDIR  /opt/gfortransoft/mpich2/netcdf4/include
          else if ($which_MPI == "openmpi" ) then
            setenv NC_CONFIG      /opt/gfortransoft/openmpi/netcdf4/bin/nc-config
            setenv NETCDF_INCDIR  /opt/gfortransoft/openmpi/netcdf4/include
          endif
        else
          setenv NC_CONFIG        /opt/gfortransoft/serial/netcdf4/bin/nc-config
          setenv NETCDF_INCDIR    /opt/gfortransoft/serial/netcdf4/include
        endif
      else
        setenv NETCDF_INCDIR      /opt/gfortransoft/serial/netcdf3/include
        setenv NETCDF_LIBDIR      /opt/gfortransoft/serial/netcdf3/lib
      endif
    breaksw

  endsw
endif

# Put the binary to execute in the following directory.

 setenv PLT_BINDIR           ${HOME}/bin
#setenv PLT_BINDIR           ${MY_PROJECT_DIR}

# Put the f90 files in a project specific Build directory to avoid conflict
# with other projects.

 setenv SCRATCH_DIR         ${MY_PROJECT_DIR}/Build_plt

# Go to the users source directory to compile. The options set above will
# pick up the application-specific code from the appropriate place.

 cd ${MY_PLT_SRC}/src

# Remove build directory.

if ( $clean == 1 ) then
  make clean
endif

# Compile (the binary will go to PLT_BINDIR set above).

if ( $parallel == 1 ) then
  make $NCPUS
else
  make
endif
