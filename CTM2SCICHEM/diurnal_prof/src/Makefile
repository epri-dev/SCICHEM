######################################################################
#
#   Makefile for diurnal_prof
#
#   NOTE: This Makefile assumes the NetCDF and IO/API libraries
#         have already been built and exist in the directories
#         specified below.
#
#   DIRECTORIES:
#
#     IOAPI_INC  - I/O-API include file directory
#     IOAPI_LIB  - I/O-API library file directory
#     NETCDF_LIB - NetCDF library file directory
#
######################################################################

IOAPI_INC = /usr/local/ioapi-3.1/ioapi/fixed_src
IOAPI_LIB = /usr/local/ioapi-3.1/Linux2_x86_64
NETCDF_LIB = /usr/local/netcdf-4.0/lib

######################################################################

EXE = diurnal_prof

FC = pgf90
LIB = -L$(IOAPI_LIB) -lioapi -L$(NETCDF_LIB) -lnetcdf
INC = -I$(IOAPI_INC)
OPT = -mcmodel=medium -O2 -pc 64 -Mdalign -Mnoframe -byteswapio -Mlfs -Bstatic_pgi

###FC  = ifort
###OPT = -mcmodel=medium -O2 -align dcommons -extend_source -convert big_endian
###LIB = -L$(IOAPI_LIB) -lioapi -L$(NETCDF_LIB) -lnetcdf
###INC = -I$(IOAPI_INC)

###FC  = gfortran
###OPT = -mcmodel=medium -O2 -fno-align-commons -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0
###LIB = -L$(IOAPI_LIB) -lioapi -L$(NETCDF_LIB) -lnetcdf
###INC = -I$(IOAPI_INC)

OBJ = $(EXE).o

$(EXE): $(OBJ)
	$(FC) $(INC) $(OPT) -o $@ $(OBJ) $(LIB)

.f.o :
	$(FC) -c -o $@ $(INC) $(OPT) $<

clean :
	rm -f *.o

