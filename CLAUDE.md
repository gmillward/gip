# GT-GIP 2026 Coupled Ionosphere-Thermosphere Model

## Overview

GT-GIP is a coupled upper-atmosphere model consisting of two components:

- **GT (Global Thermosphere)** — `GT_thermosphere.f90`: solves neutral atmosphere dynamics, temperature, winds, and composition (O, O2, N2) on a 15-level × 91-lat × 20-lon grid.
- **GIP (Global Ionosphere-Plasmasphere)** — `GIP_ionosphere_plasmasphere.f90`: solves ionospheric/plasmaspheric ion chemistry and transport using apex magnetic coordinates. GIP is called every `GIP_calling_frequency` GT timesteps.
- **tucan_time.f90**: main program; drives the coupled time loop, reads run parameters, and passes fields between GT and GIP.
- **run_parameters.f90**: module that reads the namelist-style input file from stdin.
- **tucan_time.f90**: reads tidal/electrodynamic auxiliary files from `static_files/`.

Output is NetCDF. The GT and GIP each write their own output files.

## Compiler requirements

- **Fortran**: `gfortran` (tested) or Intel `ifort`/`ifx`
- **NetCDF**: Fortran-enabled NetCDF library (e.g. via Homebrew: `brew install netcdf`)

## Compile

Set environment variables and build with:

```bash
FORTRAN_COMPILER=gfortran \
FORTRAN_LINKER=gfortran \
NETCDF=/opt/homebrew \
make -f Makefile.gt-gip \
  FFLAGS="-c -O3 -fdefault-real-8 -I/opt/homebrew/include" \
  LFLAGS="-L/opt/homebrew/lib -lnetcdf -lnetcdff"
```

> **Note**: The original Makefile uses `-r8` (Intel flag). With gfortran substitute `-fdefault-real-8`. The `-c` flag must be included in `FFLAGS` since the `.f90.o` rule does not include it.

For a debug build (bounds checking, backtraces):

```bash
FORTRAN_COMPILER=gfortran \
FORTRAN_LINKER=gfortran \
NETCDF=/opt/homebrew \
make -f Makefile.gt-gip \
  FFLAGS="-c -g -O0 -fdefault-real-8 -I/opt/homebrew/include -fcheck=all -fbacktrace" \
  LFLAGS="-L/opt/homebrew/lib -lnetcdf -lnetcdff"
```

## Run

The model reads its run parameters from stdin and expects input NetCDF files to already exist.

```bash
# Create output directory if it doesn't exist
mkdir -p data

# Symlink or copy input NetCDF files into data/
ln -sf /path/to/Day_80_180_new10.gt.nc  data/
ln -sf /path/to/Day_80_180_new10.gip.nc data/

# Run
./gt_gip_model < input_equinox_apex
```

The input file (e.g. `input_equinox_apex`) specifies:
- GT input/output NetCDF paths (lines 1–2)
- GIP input/output NetCDF paths (lines 3–4)
- `static_files/` directory (line 5)
- Physical switches and run parameters (day number, F10.7, timestep, duration, etc.)

All static auxiliary files must be present in `static_files/` — including `GIP_COORDS_FILE`, `GIP_apex_coords_etc.2000.0.format`, `hprof`, `angdif_for_GIP`, `btotal_dip_declination_old_dipole`, and others.

## gfortran compatibility fixes (vs original ifort code)

Three fixes were required to build and run cleanly with gfortran on ARM64:

1. **`GT_thermosphere.f90`** — `nf_def_var` and `nf_put_vara_real` calls passed scalars where NetCDF-Fortran expects rank-1 arrays; wrapped with `(/.../)`.
2. **`GIP_ionosphere_plasmasphere.f90`** — inverse interpolation arrays (`iht_above_inverse_3d` etc.) are dimensioned `interface_hts=31`, not `nhgt` (~86); the copy loops were incorrectly using `nhgt` as the slice bound.

## NaN monitoring

Each timestep the code checks key GT output fields (`Temperature_K_FROM_GT`, winds, `O_density_FROM_GT`) and key GIP output fields (`Ne_density_FROM_GIP_m3`, `Te_FROM_GIP_K`, `Ti_Oplus_FROM_GIP_K`) for NaNs. Any NaN triggers a warning line on stdout:

```
NaN WARNING: <variable_name>  nnloop= <timestep>
```
