# 2022 03 14 Andrew: the best way I can think of to calculate pi
# (use the CLT and MAD of the standard normal)
#
# Run:     Rscript make_pi_from_anomalies.R  (from this folder)
# Inputs:  OpenAP 12-month holding period portfolio returns (via OpenSourceAP.DownloadR)
# Outputs: temp/pi_from_anomalies.png, pi estimate to console

# ENVIRONMENT ====
rm(list = ls())
library(tidyverse)
library(OpenSourceAP.DownloadR)

# latest release by default, or e.g. OpenAP$new('2024_10')
openap = OpenAP$new()

dir.create('temp', showWarnings = FALSE)

# DOWNLOAD ANOMALY RETURNS =====

# (standardized holding period is better behaved)
port = openap$dl_port('holdper12') %>%
  filter(!is.na(ret))

ret = port$ret
nsignal = n_distinct(port$signalname)

# DEMEAN AND BOOTSTRAP STANDARD NORMAL APPROXIMATION ====

set.seed(314)

# bootstrap
nboot = 1e5
T = 12*30 # 30 years

tstat = numeric(length = nboot)
for (i in 1:nboot){
  tempret = sample(ret, size = T, replace = T)
  tstat[i] = mean(tempret)/sd(tempret)*sqrt(T)
}
tstat = tstat - mean(tstat)

# CALCULATE PI ===

# find pi with mean absolute deviation
mad = cumsum(abs(tstat))/(1:nboot)
pi_anom = 2/mad^2

# plot
png('temp/pi_from_anomalies.png', width = 6, height = 4, units = 'in', res = 200)
iselect = seq(10,nboot,100)
plot(
  iselect, pi_anom[iselect]
  , xlab = '# bootstraps used'
  , ylab = paste0('pi from ', nsignal, ' stock market anomalies')
  , ylim = c(2,4)
  )
text(nboot*0.7, pi_anom[nboot]+0.3
     , labels = paste0('pi is roughly ', sprintf('%.4f', pi_anom[nboot])))
dev.off()

# output to console
print(pi_anom[nboot])
