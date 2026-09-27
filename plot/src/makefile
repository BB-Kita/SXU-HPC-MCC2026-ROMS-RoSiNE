#  $Id: makefile 833 2017-01-24 22:20:16Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
# Copyright (c) 2002-2017 The ROMS/TOMS Group             Kate Hedstrom :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#                                                                       :::
#  ROMS/TOMS Plotting Code Master Makefile                              :::
#                                                                       :::
#  This makefile is designed to work only with GNU Make version 3.80 or :::
#  higher. It can be used in any architecture provided that there is a  :::
#  machine/compiler rules file in the  "Compilers"  subdirectory.  You  :::
#  may need to modify the rules file to specify the  correct path  for  :::
#  the NetCDF library.                                                  :::
#                                                                       :::
#  If appropriate,  the USER needs to modify the  macro definitions in  :::
#  in user-defined section below.  To activate an option set the macro  :::
#  to "on". For example, if you want to compile with debugging options  :::
#  set:                                                                 :::
#                                                                       :::
#    PLT_DEBUG := on                                                    :::
#                                                                       :::
#  Otherwise, leave macro definition blank.                             :::
#                                                                       :::
#  The USER needs to provide a value for the  macro FORT.  Choose  the  :::
#  appropriate value from the list below.                               :::
#                                                                       :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

NEED_VERSION := 3.80 3.81 3.82
$(if $(filter $(MAKE_VERSION),$(NEED_VERSION)),,        \
 $(error This makefile requires one of GNU make version $(NEED_VERSION).))

#--------------------------------------------------------------------------
#  Initialize some things.
#--------------------------------------------------------------------------

  sources    :=

#==========================================================================
#  Start of user-defined options. In some macro definitions below: "on" or
#  any other string means TRUE while blank (or spaces) is FALSE.
#==========================================================================
#
#  Activate debugging compiler options:

    PLT_DEBUG ?=

#  If applicable, activate 64-bit compilation:

    PLT_LARGE ?=

#  Sometimes it is desirable to activate one or more CPP options
#  using the -D syntax.

MY_CPP_FLAGS ?=

#  Use NetCDF-4/HDF5 libraries. Notice that the NetCDF-4/HDF5 libraries
#  are backwards compatible with NetCDF classic format file. Therefore,
#  when compiled with the NetCDF-4/HDF5 libraries this plotting package
#  can plot both NetCDF format files.
#
#  There are serial and parallel NetCDF-4 and HDF5 libraries.  The
#  parallel libraries also requires the MPI library. The plotting
#  package is serial but it can be compiled with the parallel
#  libraries. In order to allow compiling with both serial and
#  parallel libraries, the NCAR's library loader script "ncargf90"
#  is not longer used in the make configuration files located in the
#  Compilers directory.  The required plotting libraries are specified
#  explicitly as:
#
#            LIBS := -L$(NCARG_ROOT)/lib -lncarg -lncarg_gks -lncarg_c
#            LIBS += -L/usr/X11R6/lib64 -lX11

  USE_NETCDF4 ?=

ifdef USE_NETCDF4
   USE_MPIF90 ?=
endif

#--------------------------------------------------------------------------
#  We are going to include a file with all the settings that depend on
#  the system and the compiler. We are going to build up the name of the
#  include file using information on both. Set your compiler here from
#  the following list:
#
#  Operating System        Compiler(s)
#
#     AIX:                    xlf
#     ALPHA:                  f90
#     CYGWIN:                 g95, df, ifort
#     Darwin:                 f90, xlf
#     IRIX:                   f90
#     Linux:                  ftn, ifc, ifort, pgi, path, g95, gfortran
#     SunOS:                  f95
#     UNICOS-mp:              ftn
#     SunOS/Linux:            ftn (Cray cross-compiler)
#
#  Feel free to send us additional rule files to include! Also, be sure
#  to check the appropriate file to make sure it has the right paths to
#  NetCDF and so on.
#--------------------------------------------------------------------------

        FORT ?= gfortran

#--------------------------------------------------------------------------
#  Set directory for executable.
#--------------------------------------------------------------------------

  PLT_BINDIR ?= $(HOME)/bin

#--------------------------------------------------------------------------
#  Provide NCAR library version installed.  Several common blocks changes
#  in these versions.  These switch is used in routines Utility/cnt_frame.F
#  and Utility/hbackground.F to tune the appropriate parameters.
#--------------------------------------------------------------------------

#NCAR_VERSION ?= NCARG410
#NCAR_VERSION ?= NCARG422
#NCAR_VERSION ?= NCARG430
#NCAR_VERSION ?= NCARG431
#NCAR_VERSION ?= NCARG441
 NCAR_VERSION ?= NCARG520

#==========================================================================
#  End of user-defined options. See also the machine-dependent include
#  file being used above.
#==========================================================================

#--------------------------------------------------------------------------
#  Set directory for temporary objects.
#--------------------------------------------------------------------------

SCRATCH_DIR ?= Build_plt

 clean_list := core *.ipo $(SCRATCH_DIR)

ifeq "$(strip $(SCRATCH_DIR))" "."
  clean_list := core *.o *.oo *.mod *.f90 lib*.a *.bak
  clean_list += $(CURDIR)/*.ipo
endif
ifeq "$(strip $(SCRATCH_DIR))" "./"
  clean_list := core *.o *.oo *.ipo *.mod *.f90 lib*.a *.bak
  clean_list += $(CURDIR)/*.ipo
endif

#--------------------------------------------------------------------------
#  Set Pattern rules.
#--------------------------------------------------------------------------

%.o: %.F

%.o: %.f90
	cd $(SCRATCH_DIR); $(FC) -c $(FFLAGS) $(notdir $<)

%.f90: %.F
	$(CPP) $(CPPFLAGS) $(MY_CPP_FLAGS) $< > $*.f90
	$(CLEAN) $*.f90

CLEAN := Bin/cpp_clean

#--------------------------------------------------------------------------
#  Make functions for putting the temporary files in $(SCRATCH_DIR)
#  DO NOT modify this section; spaces and blank lines are needed.
#--------------------------------------------------------------------------

# $(call source-dir-to-binary-dir, directory-list)
source-dir-to-binary-dir = $(addprefix $(SCRATCH_DIR)/, $(notdir $1))

# $(call source-to-object, source-file-list)
source-to-object = $(call source-dir-to-binary-dir,   \
                   $(subst .F,.o,$1))

# $(call make-library, library-name, source-file-list)
define make-library
   libraries += $(SCRATCH_DIR)/$1
   sources   += $2

   $(SCRATCH_DIR)/$1: $(call source-dir-to-binary-dir,    \
                      $(subst .F,.o,$2))
	$(AR) $(ARFLAGS) $$@ $$^
	$(RANLIB) $$@
endef

# $(call f90-source, source-file-list)
f90-source = $(call source-dir-to-binary-dir,     \
                   $(subst .F,.f90,$1))

# $(compile-rules)
define compile-rules
  $(foreach f, $(local_src),       \
    $(call one-compile-rule,$(call source-to-object,$f), \
    $(call f90-source,$f),$f))
endef

# $(call one-compile-rule, binary-file, f90-file, source-files)
define one-compile-rule
  $1: $2 $3
	cd $$(SCRATCH_DIR); $$(FC) -c $$(FFLAGS) $(notdir $2)

  $2: $3
	$$(CPP) $$(CPPFLAGS) $$(MY_CPP_FLAGS) $$< > $$@
	$$(CLEAN) $$@

endef

#--------------------------------------------------------------------------
#  Set executables file names.
#--------------------------------------------------------------------------

ifdef PLT_DEBUG
   CNT := $(PLT_BINDIR)/cntG
  CCNT := $(PLT_BINDIR)/ccntG
   SEC := $(PLT_BINDIR)/secG
  CSEC := $(PLT_BINDIR)/csecG
else
   CNT := $(PLT_BINDIR)/cnt
  CCNT := $(PLT_BINDIR)/ccnt
   SEC := $(PLT_BINDIR)/sec
  CSEC := $(PLT_BINDIR)/csec
endif

#--------------------------------------------------------------------------
#  "uname -s" should return the OS or kernel name and "uname -m" should
#  return the CPU or hardware name. In practice the results can be pretty
#  flaky. Run the results through sed to convert "/" and " " to "-",
#  then apply platform-specific conversions.
#--------------------------------------------------------------------------

OS := $(shell uname -s | sed 's/[\/ ]/-/g')
OS := $(patsubst CYGWIN_%,CYGWIN,$(OS))
OS := $(patsubst MINGW%,MINGW,$(OS))
OS := $(patsubst sn%,UNICOS-sn,$(OS))

CPU := $(shell uname -m | sed 's/[\/ ]/-/g')

ifndef FORT
  $(error Variable FORT not set)
endif

COMPILERS ?= $(CURDIR)/Compilers

ifneq "$(MAKECMDGOALS)" "clean"
  include $(COMPILERS)/$(OS)-$(strip $(FORT)).mk
endif

#--------------------------------------------------------------------------
#  Pass the platform variables to the preprocessor as macros. Convert to
#  valid, upper-case identifiers.
#--------------------------------------------------------------------------

CPPFLAGS += -D$(shell echo ${OS} | tr "-" "_" | tr [a-z] [A-Z])
CPPFLAGS += -D$(shell echo ${CPU} | tr "-" "_" | tr [a-z] [A-Z])
CPPFLAGS += -D$(shell echo ${FORT} | tr "-" "_" | tr [a-z] [A-Z])

#--------------------------------------------------------------------------
#  Add NCAR library version.  The library common blocks used in routine
#  hbackground.F depend on the NCAR version installed in your system.
#  Execute "ncargversion" to find your installed version.
#--------------------------------------------------------------------------

CPPFLAGS += -D$(NCAR_VERSION) -Drmdocinc

#--------------------------------------------------------------------------
#  Build target directories.
#--------------------------------------------------------------------------

.PHONY: all

all: $(SCRATCH_DIR) $(SCRATCH_DIR)/MakeDepend $(CNT) $(CCNT) $(SEC) $(CSEC)

modules  := Utility Drivers

includes := Include Utility

vpath %.F $(modules)
vpath %.h $(includes)
vpath %.f90 $(SCRATCH_DIR)
vpath %.o $(SCRATCH_DIR)

include $(addsuffix /Module.mk,$(modules))

MDEPFLAGS += $(patsubst %,-I %,$(includes)) --silent --moddir $(SCRATCH_DIR)

CPPFLAGS += $(patsubst %,-I%,$(includes))

$(SCRATCH_DIR):
	$(shell $(TEST) -d $(SCRATCH_DIR) || $(MKDIR) $(SCRATCH_DIR) )

#--------------------------------------------------------------------------
#  Target to create libraries and dependecies.
#--------------------------------------------------------------------------

.PHONY: libraries

libraries: $(libraries)

#--------------------------------------------------------------------------
#  Target to create ROMS/TOMS dependecies.
#--------------------------------------------------------------------------

$(SCRATCH_DIR)/MakeDepend: makefile | $(SCRATCH_DIR)
	$(SFMAKEDEPEND) $(MDEPFLAGS) $(sources) > $(SCRATCH_DIR)/MakeDepend

.PHONY: depend

SFMAKEDEPEND := Bin/sfmakedepend

depend: $(SCRATCH_DIR)
	$(SFMAKEDEPEND) $(MDEPFLAGS) $(sources) > $(SCRATCH_DIR)/MakeDepend

ifneq "$(MAKECMDGOALS)" "clean"
  -include $(SCRATCH_DIR)/MakeDepend
endif

#--------------------------------------------------------------------------
#  Target to create tar file.
#--------------------------------------------------------------------------

.PHONY: tarfile

tarfile:
		tar --exclude=".svn"-cvf plot-3_0.tar *

.PHONY: zipfile

zipfile:
		zip -r plot-3_0.zip *

.PHONY: gzipfile

gzipfile:
		gzip -v plot-3_0.gzip *

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

.PHONY: clean

clean:
	$(RM) -r $(clean_list)

#--------------------------------------------------------------------------
#  A handy debugging target. This will allow to print the value of any
#  makefile defined macro (see http://tinyurl.com/8ax3j). For example,
#  to find the value of CPPFLAGS execute:
#
#        gmake print-CPPFLAGS
#  or
#        make print-CPPFLAGS
#--------------------------------------------------------------------------

.PHONY: print-%

print-%:
	@echo $* = $($*)
