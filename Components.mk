#------------------------------------------------------------------------------------- WRF
export WRF_DIR=../WRF4.2.2

WRF_INC = \
	-I${WRF_DIR}/external/RSL_LITE/ \
	-I${WRF_DIR}/external/io_netcdf/\
	-I${WRF_DIR}/external/io_pnetcdf/\
	-I${WRF_DIR}/external/io_grib1/ \
	-I${WRF_DIR}/external/io_grib1/*/ \
	-I${WRF_DIR}/external/io_int/ \
	-I${WRF_DIR}/external/io_grib_share/ \
	-I${WRF_DIR}/external/fftpack/*/ \
	-I${WRF_DIR}/external/esmf_time_f90/ \
	-I${WRF_DIR}/phys/ \
	-I${WRF_DIR}/share/ \
	-I${WRF_DIR}/frame/ \
	-I${WRF_DIR}/dyn_em/ \
	-I${WRF_DIR}/main/ 

WRF_OBJ = \
	${WRF_DIR}/external/RSL_LITE/*.o \
	${WRF_DIR}/external/io_netcdf/*.o \
	${WRF_DIR}/external/io_pnetcdf/*.o \
	${WRF_DIR}/external/io_grib1/*.o \
	${WRF_DIR}/external/io_grib1/*/*.o \
	${WRF_DIR}/external/io_int/*.o \
	${WRF_DIR}/external/io_grib_share/*.o \
	${WRF_DIR}/external/fftpack/*/*.o \
	${WRF_DIR}/external/esmf_time_f90/*.o \
	${WRF_DIR}/phys/*.o \
	${WRF_DIR}/share/*.o \
	${WRF_DIR}/frame/*.o \
	${WRF_DIR}/dyn_em/*.o \
	${WRF_DIR}/main/module_wrf_top.o 

WRF_LIB = $(WRF_DIR)/main/libwrflib.a  \
	-L/afs/enea.it/software/pnetcdf/1_13_0/x86_64/cresco8/rhel9.4/install/impi-oneapi/lib -lpnetcdf 
	
#------------------------------------------------------------------------------------- MITgcm
export MIT_DIR=../MITgcm_z67/verification/MED12_COUPLED_JULY21/build

MIT_OBJ = \
	${MIT_DIR}/*.o
	
MIT_INC = \
	-I${MIT_DIR}/

MIT_LIB = \
	-L${MIT_DIR}/
	
#------------------------------------------------------------------------------------- RTM
export RTM_DIR=../CaMa-Flood_v4/src

RTM_OBJ = \
	${RTM_DIR}/*.o
	
RTM_INC = \
	-I${RTM_DIR}/

RTM_LIB = \
	-L${RTM_DIR}/ 	
		
