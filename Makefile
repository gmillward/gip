#
UNAMES = $(shell uname -s)
EXEC = gt_gip_model

FORTRAN=ifort
FFLAGS= -O3 -r8 -I$(NETCDF)/include
#FFLAGS_F77=-O3 -r8
FFLAGS_FIXED= -O3 -r8 -i4
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
