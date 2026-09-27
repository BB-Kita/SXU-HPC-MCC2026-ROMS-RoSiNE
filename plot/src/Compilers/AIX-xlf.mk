# svn $Id: AIX-xlf.mk 833 2017-01-24 22:20:16Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2017 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Include file for IBM xlf95_r Fortran Compiler
# -------------------------------------------------------------------------
#
# FC             Name of the fortran compiler to use
# FFLAGS         Flags to the fortran compiler
# CPP            Name of the C-preprocessor
# CPPFLAGS       Flags to the C-preprocessor
# LD             Program to load the objects into an executable
# LDFLAGS        Flags to the loader
# MDEPFLAGS      Flags for sfmakedepend  (-s if you keep .f files)
# NCARG_ROOT     NCAR's plotting libraries root directory
# NETCDF_INCDIR  NetCDF include directory
# NETCDF_LIBDIR  NetCDF libary directory
# RANLIB         Name of ranlib command

#
# First the defaults
#

               FC := xlf95_r
           FFLAGS := -qsuffix=f=f90 -qmaxmem=-1 -qarch=pwr4 -qtune=pwr4
              CPP := /usr/lib/cpp
         CPPFLAGS := -P -DAIX
       NCARG_ROOT ?= 
          LDFLAGS :=
               AR := ar
          ARFLAGS := -r
            MKDIR := mkdir -p
               RM := rm -f
           RANLIB := ranlib
	     PERL := perl
             TEST := test

        MDEPFLAGS := --cpp --fext=f90 --file=- --objdir=$(SCRATCH_DIR)

#
# Library locations, can be overridden by environment variables.
#

             LIBS := -L$(NCARG_ROOT)/lib -lncarg -lncarg_gks -lncarg_c
             LIBS += -L/usr/X11R6/lib64 -lX11

ifdef USE_NETCDF4
        NC_CONFIG ?= /usr/local/serial/netcdf4/bin/nc-config
    NETCDF_INCDIR ?= $(shell $(NC_CONFIG) --prefix)/include
             LIBS += $(shell $(NC_CONFIG) --flibs)
else
    NETCDF_INCDIR ?= /usr/local/serial/netcdf3/include
    NETCDF_LIBDIR ?= /usr/local/serial/netcdf3/lib
             LIBS += -L$(NETCDF_LIBDIR) -lnetcdf
endif

         CPPFLAGS += -I$(NETCDF_INCDIR)

ifdef USE_NETCDF4
 ifdef USE_MPIF90
               FC := mpxlf95_r
 endif
endif

ifdef PLT_LARGE
           FFLAGS += -q64
          ARFLAGS += -X 64
          LDFLAGS += -bmaxdata:0x200000000
else
          LDFLAGS += -bmaxdata:0x70000000
endif


ifdef PLT_DEBUG
           FFLAGS += -g -qfullpath
else
           FFLAGS += -O3 -qstrict
endif

#
# Use full path of compiler.
#

               FC := $(shell which ${FC})
               LD := $(FC)
