# Canopy approximations and light competition

A research project testing how well the **canopy approximations** used in large-scale
vegetation models capture light competition between trees. A detailed 3D
radiative-transfer model — **MAESPA** — is treated as ground truth, and three simpler
"big-leaf"-style approximations are compared against it:

- **PPA** — Perfect Plasticity Approximation
- **FLAT TOP**
- **DEEP CROWN**

The comparison metric is **absorbed PAR** (`absPAR`, MJ m⁻² d⁻¹). Three computational
experiments are run:

| Experiment | Question | Suffix in code |
|------------|----------|----------------|
| **Competition** | Focal tree vs. stand, as the focal tree's relative height varies | `_fla_0.1`, `_fla_27` |
| **Stand interception** | Whole-stand light interception across LAI | `_stand_fla_0.1`, `_stand_fla_27` |
| **Self-shading** | Effect of varying focal-tree leaf area | `_c_fla` |

(`fla` = focal-tree leaf area: 0.1 m² for a "single-leaf" focal tree, 27 m² for a
full-canopy one.)

## Repository structure

The repo is **two independent RStudio projects**. Scripts use relative paths and
`source()`, so **set the working directory to the relevant project folder before
running** — not the repo root.

### `Maespa simulations/` — generate the ground-truth data

Builds virtual tree stands and runs the MAESPA Fortran model on them.

| Path | Purpose |
|------|---------|
| `R/Function.R` | Functions to construct stands (`create_simulation`, `create_simulation_f_stand`), run MAESPA (`run_simulation`), and load output (`load_output`). |
| `Sims.R` | Top-level driver: loops over parameter grids, builds stand folders from `template_A`, runs MAESPA, aggregates `Dayflx.dat` outputs, writes `maespa_*.csv`. |
| `R/Results.R` | Quick plots of the raw MAESPA output. |
| `template_A/`, `template_WD*/` | MAESPA input templates (`confile.dat`, `Trees.dat`, `phy.dat`, `str.dat`, `met.dat`, `watpars.dat`); copied and edited per simulation. |
| `src/maespa.out` | The compiled MAESPA executable (see *Known issues*). |
| `simulations_*/` | Generated stand folders, one per parameter combination. |

### `models/` — fit the approximations and make the figures

Loads the MAESPA outputs, computes the canopy approximations, and produces the paper
figures.

| Path | Purpose |
|------|---------|
| `Function_M.R` | All the approximation logic: `load_maespa_results`, `process_experiment`, `calculate_averages`, `model_lai` (FLAT TOP + PPA), `deep_crown_set_up` (DEEP CROWN), `PAR_calculator` (1- and 2-stream Beer's-law light absorption). |
| `running.R` | Top-level driver: loads each experiment's MAESPA csv + simulation folders, runs the approximations, builds the `final_results_*` data frames. **Run before `Results.R`** — it creates the objects `Results.R` plots. One block per experiment. |
| `Results.R` | **Where the figures and statistics (RMSE / R²) are generated**, one block per experiment. Each plot is drawn to the active device and saved as both PNG and PDF into `figures/` via the `save_fig` helper. |
| `Different maespa csv/` | Pre-computed MAESPA outputs (`maespa_fla_0.1.csv`, etc.). These let `models/` run without re-running MAESPA. |
| `met.dat` | Hourly PAR forcing used by `PAR_calculator`. |
| `figures/` | Output figures (created on first run of `Results.R`). |

## Stand naming convention

Simulation folders are named `H{H}_V{V}_L{L}_F{F}_fla{fla}_S{S}`:

- `H` — mean stand height
- `V` — coefficient of variation of trunk heights
- `L` — stand LAI (sets the number of trees)
- `F` — focal-tree height ÷ stand height (the competition ratio)
- `fla` — focal-tree leaf area (m²)
- `S` — random seed / replicate (1–3, averaged over)

## Running the workflow

**To just reproduce the figures**, you only need step 3 below — the MAESPA outputs are
already checked in under `models/Different maespa csv/`.

1. In `Maespa simulations/`: source `R/Function.R`, then run `Sims.R` to build stands and
   run MAESPA → writes `maespa_*.csv`.
2. Copy/refresh those csvs into `models/Different maespa csv/`.
3. In `models/`: run `running.R` (it sources `Function_M.R`), then `Results.R` for the
   figures + statistics.

## Dependencies

R packages: `tidyverse`, `ggplot2`, `purrr`, `pracma`, `Metrics`, `rgl`, and **`Maeswrap`**
— not on CRAN, install from GitHub:

```r
remotes::install_github("RemkoDuursma/Maeswrap")
```

`Maeswrap` provides `readmet`, `parseFile`, `replacePAR`, and `readdayflux`.

## Known issues

- **The MAESPA binary likely won't run as-is on Apple Silicon.** `src/maespa.out` is an
  x86_64 Mach-O built against `/usr/local/gfortran/lib/libgfortran.5.dylib`; on an arm64
  Mac it needs Rosetta 2 **and** the gfortran runtime at that exact path. The `models/`
  figure pipeline does **not** need the binary — it uses the checked-in csvs.
- `load_maespa_results` deliberately copies `absPAR` into both `absPAR_one_s` and
  `absPAR_two_s` — MAESPA has no one-stream model, so its two-stream value is used for
  both comparisons.
