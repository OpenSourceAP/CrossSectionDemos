# 2022 03 14 Andrew: the best way I can think of to calculate pi
# (use the CLT and MAD of the standard normal)

# ENVIRONMENT ====
rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)

# root of April 2021 release on Gdrive
pathRelease = 'https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R'

# DOWNLOAD ANOMALY RETURNS AND DE-MEAN =====

# get gdrive dribble 
# (standardized holding period is better behaved)
target_dribble = pathRelease %>% drive_ls() %>%
  filter(name=='Portfolios') %>% drive_ls() %>%
  filter(name=='Full Sets Alt') %>% drive_ls() %>%
  filter(name=='PredictorAltPorts_HoldPer_12.zip')

# download and import
drive_download(target_dribble[1,], path = 'temp/deleteme.zip', overwrite = T)
unzip('temp/deleteme.zip', exdir = 'temp')
ret = fread('temp/PredictorAltPorts_HoldPer_12.csv') %>% 
  filter(!is.na(ret)) %>% 
  pull(ret)

# DEMEAN AND BOOTSTRAP STANDARD NORMAL APPROXIMATION ====

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
iselect = seq(10,nboot,100)
plot(
  iselect, pi_anom[iselect]
  , xlab = '# bootstraps used'
  , ylab = 'pi from 205 stock market anomalies'
  , ylim = c(2,4)
  )
text(nboot*0.8, pi_anom[nboot]+0.1
     , labels = paste0('pi is roughly ', sprintf('%.4f', pi_anom[nboot])))

# output to console
pi_anom[nboot]


