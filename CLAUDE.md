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

## GIP grid coordinate system

- **mp** (1–80): magnetic longitude index
- **lp** (1–67): magnetic latitude index — **lp=1 is high latitude (polar), higher lp moves toward the equator**
- **interface_hts** (31 levels): fixed height grid from 90 km to 9000 km:
  `90, 95, 100, 105, 110, 115, 120, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400, 450, 500, 550, 600, 700, 800, 900, 1000, 2000, 4000, 6370, 9000 km`
- The E-F region transition is around indices 7–9 (120–150 km); the coarse spacing here (25 km jumps) is a known limitation.
- This grid was designed 30+ years ago under tight memory/compute constraints. A much finer, modern grid is a planned future development.

## O+ solver instabilities (Ofailed)

The O+ tridiagonal solver (`ML__DIFFUSION_EQUATION_O_PLUS`) occasionally fails to converge even after the 5x sub-grid refinement fallback. Diagnostic output added to the `Ofailed (5th attempt)` branch prints UT, local time, geographic longitude, and footpoint altitude.

From a full 1-day run (nnstop=1440, 60s timestep, F10.7=120, day 80):

- **Altitude:** Every failure occurs at `alt_low_km = 90.0` — the bottom of the grid. This is the E-region base where steep density gradients develop.
- **Local time:** Failures cluster tightly at **~06 LT (dawn terminator)**, with a secondary cluster at **~19–20 LT (dusk)**. The instability is triggered as a flux tube crosses from night into sunlight (or vice versa), when photoproduction switches on abruptly and creates density gradients the coarse 90 km grid cannot resolve.
- **Latitude:** Failures concentrate at **high magnetic latitudes** (lp=1–16) and a specific mid-latitude band (lp≈46–50). At high latitudes the terminator crosses field lines more obliquely, making the E-region gradient sharper.
- **Longitude:** The failing mp index sweeps through all values over the UT day, tracking the dawn terminator as it rotates westward — consistent with the LT diagnosis.
- **Hfailed:** Zero H+ solver failures in the same run; instability is purely in the O+ solver.

The root cause is the coarse grid spacing in the E-F transition region combined with the abrupt onset of photoionisation at the terminator. The long-term fix is a redesigned grid with finer resolution in the E region.

### Sub-grid refinement chain (branch: dawn-dusk-spatial-res)

`ML__O_PLUS_SUBGRID` was refactored to accept `n_sub` as a passed argument (previously hardcoded to 5). The fallback sequence on a coarse-grid failure is now:

1. **5x sub-grid** — handles most cases
2. **10x sub-grid** — triggered if 5x fails; solves ~83% of the original 223 failures
3. **20x sub-grid** — triggered if 10x fails; solves a further ~12%, leaving 11 survivors
4. **Fudge-factor retries** (attempts 2–5) — last resort for the 11 remaining cases

All 11 survivors are in the **lp≈46–50 band at ~06 LT**. These are near-equatorial E-region flux tubes (footpoints at ±3–7° geographic latitude, apex at ~143 km) where the O+ density collapses to near-zero at both footpoints during the night, then must jump many orders of magnitude into the F region over just a handful of grid points as the tube crosses the dawn terminator.

### Temporal resolution experiment

Reducing GIP calling frequency from 15 min to 5 min **increased** failures (223 → 728), with the dusk-side count exploding. This confirms the problem is primarily **spatial** (grid resolution along the tube in the E-F region), not temporal.

### Flux tube profile analysis (mp=7, lp=49)

This is a representative hard-to-solve tube. Key findings:
- IN=12445, IS=12537 — 93 grid points, apex at ~143 km altitude
- Geographic footpoints at ~+3° and ~-6.5° latitude (near-equatorial, spanning the magnetic equator)
- At local times away from the terminator the O+ profile is a smooth arch (classic dayside)
- As LT approaches ~06, the profile collapses: footpoint densities drop to near-zero while the apex retains significant density, creating an extreme gradient (~10⁸ m⁻³ difference) bridged by only a few coarse grid points in the E-F transition
- The collapse is rapid — it occurs over just 2–3 GIP calls (30–45 simulated minutes), consistent with both spatial and temporal resolution being marginal at the terminator

Plots: `profile_mp7_lp49.png` (single snapshot), `profile_evolution_mp7_lp49.png` (full-day evolution coloured by LT).

### Next steps to investigate
- Solving in **log-density space** rather than linear space for the tridiagonal — physically appropriate given the many-orders-of-magnitude variation, and would make the problem better conditioned near the terminator
- Longer term: redesign the flux-tube grid with finer spacing in the E-F transition region (~120–200 km)
