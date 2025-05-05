
## Analysis Summary

The script performs:

- Cleaning and transformation of survey data
- Summarisation of bed capacity by hospital and bed type (e.g. ICU, Neonatal, IMC, etc.)
- Calculation of beds per capita by canton and region
- Generation of bar plots and a geographic visualisation of bed distribution

All plots are generated using `ggplot2`, and data summaries are exported to `.csv`.

##  Dependencies

The analysis was run in **R (version ≥ 4.0)** using the following packages (all open source):

- `dplyr`, `tidyr`, `ggplot2`
- `sf`, `rnaturalearth`, `rnaturalearthdata`
- `patchwork`, `writexl`

Install all packages using:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "patchwork", "writexl"))
