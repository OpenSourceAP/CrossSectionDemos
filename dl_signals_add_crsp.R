# Andrew 2020 04
#
# creates what you need to do machine learning style stuff:
# all signed firm characteristics in the release, plus the CRSP signals
# Price, Size, and STreversal (signed), merged into one wide panel.
# Needs a lot of RAM (about 20 GB) and a WRDS account.
#
# Run:     Rscript dl_signals_add_crsp.R  (from this folder)
#          WRDS credentials come from WRDS_USER / WRDS_PASS, ~/.pgpass, or a prompt
# Inputs:  OpenAP signed firm characteristics (via OpenSourceAP.DownloadR),
#          CRSP CIZ monthly stock file (crsp.msf_v2, WRDS, via the same package)
# Outputs: temp/signed_predictors_all_wide.parquet, temp/SignalDoc.csv,
#          coverage summaries to console


# ==== ENVIRONMENT ====
rm(list = ls())
tic = Sys.time()

library(tidyverse)
library(nanoparquet)
library(OpenSourceAP.DownloadR)

# latest release by default, or e.g. OpenAP$new('2024_10')
openap = OpenAP$new()

# output path
outpath = 'temp/'
dir.create(outpath, showWarnings = FALSE)

# ==== DOWNLOAD ====

# signed so that high values predict high returns, including the CRSP signals
signalwide = openap$dl_all_signals(signed = TRUE)

write_parquet(signalwide, paste0(outpath, 'signed_predictors_all_wide.parquet'))
write.csv(openap$dl_signal_doc(), paste0(outpath, 'SignalDoc.csv'), row.names = FALSE)

print(paste0('Download and save took ', round(difftime(Sys.time(), tic, units = 'mins'), 1), ' minutes'))

# ==== SUMMARIZE WHAT YOU GOT ====

## broad overview ====
obs = signalwide %>%
  select(-permno) %>%
  group_by(yyyymm) %>%
  summarize(across(everything(), ~ sum(!is.na(.x))))

widesum = obs %>% pivot_longer(
  -yyyymm
  , names_to = 'signalname'
  , values_to = 'obs'
) %>%
  filter(obs >= 1) %>%
  group_by(signalname) %>%
  summarize(
    date_begin = min(yyyymm)
    , date_end = max(yyyymm)
    , mean_firmobs_per_month = floor(mean(obs))
  )

print(paste0(
  'In ', outpath, 'signed_predictors_all_wide.parquet you have the following signals'
))
print(widesum, n = 10)

## overview of recent data ====

# a few annual signals extend past CRSP, so end at the last month with CRSP data
recent_nobs = obs %>%
  filter(yyyymm <= max(yyyymm[STreversal > 0])) %>%
  slice_max(yyyymm, n = 12) %>%
  arrange(yyyymm) %>%
  column_to_rownames('yyyymm') %>%
  t()

print('Number of firms with data in recent months')
print(head(recent_nobs, 10))
print('...')
print(tail(recent_nobs, 10))
