# CrossSectionDemos

Example code of simple things one can do with [Open Source Asset Pricing data](https://www.openassetpricing.com), created with our [open source code](https://github.com/OpenSourceAP/CrossSection).

Each file is an **independent R script** that downloads what it needs. OpenAP data come from the [OpenAP Hugging Face dataset](https://huggingface.co/datasets/OpenSourceAP/CrossSection) through the [OpenSourceAP.DownloadR](https://github.com/tomz23/OpenSourceAP.DownloadR) package. No account or token is needed, except for the two scripts that use CRSP data from WRDS.

## Setup

```r
install.packages(c("tidyverse", "gridExtra", "knitr", "OpenSourceAP.DownloadR"))
```

The scripts need OpenSourceAP.DownloadR 0.2.0 or later, which reads from Hugging Face. Check with `packageVersion("OpenSourceAP.DownloadR")`.

Scripts use the latest data release by default. To use another, change `OpenAP$new()` to e.g. `OpenAP$new('2024_10')`. Releases from 2020_05 through 2025_10 are available.

The WRDS scripts read credentials from the environment variables `WRDS_USER` and `WRDS_PASS` or from `~/.pgpass`, and prompt otherwise.

## List of Scripts

Scripts are listed from lightest / fastest (top) to heaviest / slowest (bottom).

| File                            | Description                                                                                                                         |
| ------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| `plot_anomaly.R`                | Plots selected long-short returns along with sample end and publication dates                                                       |
| `openap_vs_frenchweb.R`         | Compares `BM` and `BMdec` long-short returns to Ken French's value portfolios                                                       |
| `mclean_pontiff_main.R`         | Replicates the fact that returns are mostly there out-of-sample (McLean and Pontiff 2016)                                           |
| `correlation_demo.R`            | Correlations, principal components, and a dendrogram of long-short returns                                                          |
| `make_pi_from_anomalies.R`      | Estimates $\pi$ from anomaly returns, using the CLT and the mean absolute deviation of the standard normal                          |
| `old_vs_new_returns_check.R`    | Compares portfolio returns from two releases                                                                                        |
| `daily_covid_demo.R`            | Plots daily long-short returns (all signals) during the March 2020 crash                                                            |
| `ff1993_style_implementation.R` | Builds a Fama-French 1993 style factor from a firm characteristic and CRSP (**WRDS**), compares to Ken French's HML and the release |
| `dl_signals_add_crsp.R`         | Creates the data for machine learning: all signed characteristics plus CRSP Price, Size, STreversal (**WRDS**, about 20 GB of RAM)  |

## Details

- Run each script from this folder, e.g. `Rscript plot_anomaly.R`.
- Output and intermediate files go in `temp/`, which each script creates.
