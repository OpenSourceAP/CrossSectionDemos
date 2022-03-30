# CrossSectionDemos

Example code of simple things one can do with [Open Source Asset Pricing data](www.openassetpricing.com), created with our [open source code](https://github.com/OpenSourceAP/CrossSection).

Each file is an **independent script** that **automatically downloads** data from the internet. You just need the googledrive R package and a Google Drive account.

## List of Scripts

Scripts are listed from lightest / fastest (top) to heaviest / slowest (bottom).

| File | Description |
| --- | --- |
| `plot_anomaly.R` | Downloads selected long-short returns and plots along with sample and publication dates  |
| `openap_vs_frenchweb.R` | Downloads `BM` and `BMdec` portfolios and compares to Ken French's data |
| `daily_covid_demo.R` | Downloads daily returns (all signals) and plots performance in March 2020 |  
| `FF1993_style_implementation.R` | Downloads selected characteristic, constructs Fama-French 1993 style factor (building on 2x3 sort with Size), compares with Ken French's HML. (*2x3 implementations are part of our dataset as of March 2022*)|
| `dl_signal_add_crsp.R` | Creates the data you need for machine learning stuff.  Downloads all downloadable predictor characteristics (1.5 gigs zipped), downloads CRSP signals from WRDS, merges, and saves to disk (5.6 gig csv). |
|`mclean_pontiff_main.R`| Replicates the fact that returns are mostly there out-of-sample (McLean and Pontiff 2016) |
|`old_vs_new_returns_check.R`| Compares returns from two different releases |

## Details

- Assumes working directory is the same folder the file is in.
- Creates folder `temp/` in working directory and puts output and intermediate files there
- **Links to Google Drive URLs may need to be updated**
  - For the March 2022 Release use https://drive.google.com/drive/folders/1O18scg9iBTiBaDiQFhoGxdn4FdsbMqGo
  - For the April 2021 Release use https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R
