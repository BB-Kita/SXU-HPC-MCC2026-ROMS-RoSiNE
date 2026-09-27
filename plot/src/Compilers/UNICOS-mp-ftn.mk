# svn $Id: UNICOS-mp-ftn.mk 833 2017-01-24 22:20:16Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2017 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Include file for UNICOS FTN compiler on CRAY X1
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

               FC := ftn
           FFLAGS := -e I -e m
              CPP := $(HOME)/bin/cpp
         CPPFLAGS := -P -DCRAYX1 -DCRAY
       NCARG_ROOT ?=
          LDFLAGS :=
               AR := ar
          ARFLAGS := -r
            MKDIR := mkdir -p
               RM := rm -f
           RANLIB := touch
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
       MPIF90_DIR ?= /usr/local/openmpi/bin
               FC := $(MPIF90_DIR)/mpif90
 endif
endif

ifdef PLT_DEBUG
           FFLAGS += -G 0
else
           FFLAGS += -O 3,aggress
endif

#
# Use full path of compiler.
#

               FC := $(shell which ${FC})
               LD := $(FC)
