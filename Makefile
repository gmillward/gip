#
UNAMES = $(shell uname -s)
EXEC = gt_gip_model

FORTRAN=ifort
# optimized
# #FFLAGS= -O3 -r8 -I$(NETCDF)/include
# # full debug
# #FFLAGS= -g -O0 -traceback -warn all -check all -debug all -ftrapuv -fp-model precise -fpp -r8 -I$(NETCDF)/include
# # traceback debug
FFLAGS= -g -O0 -traceback -debug all -ftrapuv -fp-model precise -fpp -r8 -I$(NETCDF)/include
#FFLAGS= -g -O0 -traceback -r8 -I$(NETCDF)/include

# optimized
# #FFLAGS_FIXED= -O3 -r8 -i4
# # full debug
# #FFLAGS_FIXED= -g -O0 -traceback -warn all -check all -debug all -ftrapuv -fp-model precise -fpp -r8 -i4
# # traceback debug
FFLAGS_FIXED= -g -O0 -traceback -r8 -i4

LFLAGS=-L. -L$(NETCDF)/lib -lnetcdf
#
OBJS =  \
       params.o cons.o dynamo.o sunloc.o heelis.o \
       mudcom.o  mud.o  mudmod.o  muh2cr.o  util.o \
       GIP_ionosphere_plasmasphere.o  GT_thermosphere.o \
       tucan_time.o
# 
.SUFFIXES:  .f .f90 .F .f77
#
$(EXEC):	$(OBJS)
	$(FORTRAN) -o $@ $(OBJS) $(LFLAGS)  $(LIBS)
#
.f90.o:
	$(FORTRAN) -c $(FFLAGS) $<
#
.F.o:
	$(FORTRAN) -c $(FFLAGS_FIXED) $<
#
