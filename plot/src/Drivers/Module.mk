# svn $Id: Module.mk 833 2017-01-24 22:20:16Z arango $
#::::::::::::::::::::::::::::::::::::::::::::::::::::: Hernan G. Arango :::
# Copyright (c) 2002-2017 The ROMS/TOMS Group             Kate Hedstrom :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

local_sub  := Drivers

local_src  := $(wildcard $(local_sub)/*.F)

sources    += $(local_src)

cnt_obs    := $(SCRATCH_DIR)/cnt.o \
              $(SCRATCH_DIR)/cpmpxy2d.o \
              $(SCRATCH_DIR)/mapeod.o \
              $(SCRATCH_DIR)/stumsl.o \
              $(SCRATCH_DIR)/stumxy.o \
              $(SCRATCH_DIR)/vvumxy.o

ccnt_obs   := $(SCRATCH_DIR)/ccnt.o \
              $(SCRATCH_DIR)/cpmpxy2d.o \
              $(SCRATCH_DIR)/mapeod.o \
              $(SCRATCH_DIR)/stumsl.o \
              $(SCRATCH_DIR)/stumxy.o \
              $(SCRATCH_DIR)/vvumxy.o

sec_obs    := $(SCRATCH_DIR)/sec.o \
              $(SCRATCH_DIR)/cpmpxz2d.o 

csec_obs   := $(SCRATCH_DIR)/csec.o \
              $(SCRATCH_DIR)/cpmpxz2d.o 


ifdef LD_WINDOWS

$(CNT):	$(libraries) $(cnt_obs)
	$(LD) $(FFLAGS) $(cnt_obs) -o $@ $(libraries) $(LIBS_WIN32) $(LDFLAGS)
$(CCNT):$(libraries) $(ccnt_obs)
	$(LD) $(FFLAGS) $(ccnt_obs) -o $@ $(libraries) $(LIBS_WIN32) $(LDFLAGS)
$(SEC):	$(libraries) $(sec_obs)
	$(LD) $(FFLAGS) $(sec_obs) -o $@ $(libraries) $(LIBS_WIN32) $(LDFLAGS)
$(CSEC):$(libraries) $(csec_obs)
	$(LD) $(FFLAGS) $(csec_obs) -o $@ $(libraries) $(LIBS_WIN32) $(LDFLAGS)

else

$(CNT):	$(libraries) $(cnt_obs)
	$(LD) $(FFLAGS) $(LDFLAGS) $(cnt_obs) -o $@ $(libraries) $(LIBS) 
$(CCNT):$(libraries) $(ccnt_obs)
	$(LD) $(FFLAGS) $(LDFLAGS) $(ccnt_obs) -o $@ $(libraries) $(LIBS) 
$(SEC):	$(libraries) $(sec_obs)
	$(LD) $(FFLAGS) $(LDFLAGS) $(sec_obs) -o $@ $(libraries) $(LIBS) 
$(CSEC):$(libraries) $(csec_obs)
	$(LD) $(FFLAGS) $(LDFLAGS) $(csec_obs) -o $@ $(libraries) $(LIBS) 

endif

$(eval $(compile-rules))
