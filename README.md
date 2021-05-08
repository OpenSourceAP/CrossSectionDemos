# CrossSectionDemos

Example code of simple things one can do with [Open Source Asset Pricing data](https://sites.google.com/site/chenandrewy/open-source-ap).

Each file is an **independent script** that **automatically downloads** data from the internet. You just need the googledrive R package and a Google Drive account.

## List of Scripts

Scripts are listed from lightest / fastest (top) to heaviest / slowest (bottom).

| File | Description |
| --- | --- |
| `plot_anomaly.R` | Downloads selected long-short returns and plots along with sample and publication dates  |
| `openap_vs_frenchweb.R` | Downloads `BM` and `BMdec` portfolios and compares to Ken French's data |
| `daily_covid_demo.R` | Downloads daily returns (all signals) and plots performance in March 2020 |  
| `FF1993_style_implementation.R` | Downloads selected characteristic, constructs Fama-French 1993 style factor (building on 2x3 sort with Size), compares with Ken French's HML |
| `dl_signal_add_crsp.R` | Creates the data you need for machine learning stuff.  Downloads all downloadable predictor characteristics (1.5 gigs zipped), downloads CRSP signals from WRDS, merges, and saves to disk (5.6 gig csv). |

## Details

- Assumes working directory is the same folder the file is in.
- Creates folder `temp/` in working directory and puts output and intermediate files there
