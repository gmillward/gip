# Compiler and Flags
FC      = gfortran
#FFLAGS  = -O3 -Wall -fdefault-real-8 -fmax-stack-var-size=131072 -Wno-unused-variable
FFLAGS  = -O1 -Wall -fdefault-real-8 -Wno-unused-variable -Wno-maybe-uninitialized -Wunused-label -Wunused-function -Wunused-label -fmax-errors=1 -Werror=unused-dummy-argument

# NetCDF Paths (Adjust if using MacPorts or Intel)
NC_DIR  = /opt/homebrew
NF_DIR  = /opt/homebrew

# Include and Library flags
INCS    = -I$(NF_DIR)/include
LIBS    = -L$(NF_DIR)/lib -lnetcdff -L$(NC_DIR)/lib -lnetcdf

# Project Files
TARGET  = gt_gip_model
SRCS    = GIP_ionosphere_plasmasphere.f90  GT_thermosphere.f90  run_parameters.f90  tucan_time.f90
OBJS    = $(SRCS:.f90=.o)

# Rules
all: $(TARGET)

$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LIBS)

%.o: %.f90
	$(FC) $(FFLAGS) $(INCS) -c $<

clean:
	rm -f $(OBJS) $(TARGET) *.mod
