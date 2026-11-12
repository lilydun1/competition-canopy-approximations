# MAESPA executables

- `maespa.out` — the original x86_64 build (2021) used for the first round of simulations. Compiled with MAESPA's default `MAXT = 1500` (maximum number of trees). Needs `/usr/local/gfortran/lib/libgfortran.5.dylib` to run.
- `maespa-maxt10000` — arm64 build used for the 2026 rerun (`Sims_2026.R`). Built on 2026.11.13 from the MAESPA source at `bitbucket.org/remkoduursma/maespa` commit `0cb01e8` (local changes: the program name in the Makefile and whitespace in namelist declarations), with one edit: `MAXT = 1500` → `MAXT = 10000` in `maestcom.f90`. Compiler: gfortran (Homebrew GCC). With the default `MAXT`, this build reproduces the original `Dayflx.dat` outputs exactly for stands of up to 1,500 trees.

## Why `MAXT` matters

When `notrees` in `Trees.dat` exceeds `MAXT`, MAESPA warns `NO OF TREES IN TREES FILE EXCEEDED MAXIMUM` (in `Maeserr.dat`), keeps only the first `MAXT` trees, fails to read the `xycoords` namelist, and regenerates tree coordinates on its own grid (`READXYZ` in `inout.f90`). Tree attributes are then applied by index, so the edge-wall trees and the focal tree end up in the wrong places. In the first round this affected the two densest competition stands (2,025 and 2,401 trees). The 2026 stands have up to 7,921 trees; `Sims_2026.R` stops if any run reports the warning.
