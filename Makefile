# 			WRITTEN BY ALESSANDRO ANAV, ENEA,          SEP. 2024                   	#
#                                                 						#
# 			   Makefile to compile the ENEA-REG ESM   				#
#                                                 						#
#                                                 						#
# 	Make sure to edit the Components.mk with the path of the components of ENEA-REG         #
#                                                 						#
#                                                 						#
#################################################################################################


ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)
include ./Components.mk

#FFLAGS=-w -assume bscc -Wl,--no-relax -mcmodel=large #-g -traceback -check all
FFLAGS  = -w -assume bscc  -g -traceback -check shape -check stack -check bounds -check pointers  #-check all
LDFLAGS=-Wl,--no-relax 

enea_reg.x: enea_reg.o esm_types.o esm_shared.o esm_config.o esm_utils.o esm_cpl.o esm.o atm.o ocn.o rtm.o libatm.a libocn.a librtm.a
	$(ESMF_F90LINKER) -qopenmp $(ESMF_F90LINKOPTS) $(LDFLAGS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS) libatm.a libocn.a librtm.a $(WRF_LIB) 

esm_types.o: esm_types.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  -c esm_types.F90 -o esm_types.o

esm_shared.o: esm_shared.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  -c esm_shared.F90 -o esm_shared.o

esm_config.o: esm_config.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  -c esm_config.F90 -o esm_config.o

esm_utils.o: esm_utils.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  -c esm_utils.F90 -o esm_utils.o

esm_cpl.o: esm_cpl.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  -c esm_cpl.F90 -o esm_cpl.o

atm.o: atm.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  $(WRF_INC)  -c atm.F90 -o atm.o

mit_gcm.o: mit_gcm.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  -c mit_gcm.F90 -o mit_gcm.o

ocn.o: ocn.F90 
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  $(MIT_INC)  -c ocn.F90 -o ocn.o

rtm.o: rtm.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  $(RTM_INC)  -c rtm.F90 -o rtm.o

esm.o: esm.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  -c esm.F90 -o esm.o

enea_reg.o: enea_reg.F90
	$(ESMF_F90COMPILER) $(FFLAGS) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP)  -c enea_reg.F90 -o enea_reg.o

mit_gcm.F90: $(MIT_DIR)/DUMMY.f
	tools/create.sh $(MIT_DIR)/DUMMY.f | \
        gawk -f tools/awk.prog | sed -e 's/\([0-9]\) d \([0-9]\)/\1d\2/g' > mit_gcm.F90	

libatm.a: $(WRF_OBJ)
	ar -crs libatm.a $(WRF_OBJ)

libocn.a: $(MIT_OBJ)
	ar -crs libocn.a $(MIT_OBJ)

librtm.a: $(RTM_OBJ)
	ar -crs librtm.a $(RTM_OBJ)

libs: libatm.a libocn.a librtm.a 

# module dependencies:
enea_reg.o: esm.o 
esm.o: esm_types.o esm_config.o esm_utils.o esm_cpl.o mit_gcm.o atm.o ocn.o rtm.o
ocn.o: esm_shared.o esm_config.o
atm.o: esm_shared.o esm_config.o
rtm.o: esm_shared.o esm_config.o
esm_cpl.o: esm_shared.o
esm_config.o: esm_shared.o
 
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f PET*.ESMF_LogFile *.nc *.stdout mit_gcm.F90
clean:
	rm -f regesm *.o *.mod *.a mit_gcm.F90
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

edit:
	nedit enea_reg.F90 esm.F90 atm.F90 ocn.F90 rtm.F90 &

run:
	$(ESMF_INTERNAL_MPIRUN) -np 4 ./enea_reg.x

