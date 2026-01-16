# Advanced Visualization in R - Project

Visualization project for trading strategy performance analysis.

## Files

- `fig1_Strategy_Performance.R` - Regime-aware performance and risk context
- `fig2_Drawdown.R` - Drawdown analysis visualization
- `fig3_Annual_Comparison.R` - Annual performance treemap
- `Analysis_Description.pdf` - Short description of my analysis

## Data

Data files are in the `Data/` folder:
- `df_EC_LS_MAIN.csv` - Long/Short strategy data
- `df_EC_LO_MAIN.csv` - Long-Only strategy data

## Usage

Each script is self-contained. Set your working directory to the Project folder and run:

```r
source("fig1_Strategy_Performance.R")
```

Scripts will export PNG files automatically.

## Requirements

Main packages:
- ggplot2
- dplyr
- scales
- treemapify
- patchwork
- ggrepel
