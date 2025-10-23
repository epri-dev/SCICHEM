######################################################################
#
#   Makefile for CAMx2IOAPI
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

IOAPI_INC  = /usr/local/ioapi-3.1/ioapi/fixed_src
IOAPI_LIB  = /usr/local/ioapi-3.1/Linux2_x86_64
NETCDF_LIB = /usr/local/netcdf-4.0/lib

######################################################################

FC  = pgf90
OPT = -mcmodel=medium -O2 -Mdalign -Mnoframe -byteswapio -Bstatic_pgi
LIB = -L$(IOAPI_LIB) -lioapi -L$(NETCDF_LIB) -lnetcdf
INC = -I$(IOAPI_INC)
EXE = camx2ioapi

###FC  = ifort
###OPT = -mcmodel=medium -O2 -align dcommons -extend_source -convert big_endian
###LIB = -L$(IOAPI_LIB) -lioapi -L$(NETCDF_LIB) -lnetcdf
###INC = -I$(IOAPI_INC)
###EXE = camx2ioapi

###FC  = gfortran
###OPT = -mcmodel=medium -O2 -fno-align-commons -fconvert=big-endian -frecord-marker=4 -ffixed-line-length-0
###LIB = -L$(IOAPI_LIB) -lioapi -L$(NETCDF_LIB) -lnetcdf
###INC = -I$(IOAPI_INC)
###EXE = camx2ioapi


.PHONY: all clean

all : $(EXE)


$(EXE) : $(EXE).o
	$(FC) $(OPT) -o $@ $^ $(LIB)

.f.o :
	$(FC) $(INC) $(OPT) -c $<

clean :
	rm -f *.o $(EXE)


