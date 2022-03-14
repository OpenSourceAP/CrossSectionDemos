# 2022 03 14 Andrew: the best way I can think of to calculate pi
# (use the CLT and Geary 1935's MAD of the standard normal)

# ENVIRONMENT ====

rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)
library(readxl)
library(RColorBrewer)
library(lubridate)

### USER ENTRY

# root of April 2021 release on Gdrive
pathRelease = 'https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R'
url_prefix = 'https://drive.google.com/uc?export=download&id='

# login to gdrive
# this prompts a login
pathRelease %>% drive_ls()

# create temporary directory 
dir.create('temp/')

# DOWNLOAD DATA =====

# all long-short returns (OP) 
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='Portfolios') %>% drive_ls() %>% 
  filter(name=='Full Sets OP') %>% drive_ls() %>% 
  filter(name=='PredictorLSretWide.csv')

drive_download(target_dribble[1,], path = 'temp/deleteme.csv', overwrite = T)

ret0 = fread('temp/deleteme.csv') %>% 
  pivot_longer(-c(date),names_to = 'signalname', values_to = 'ret') %>% 
  filter(!is.na(ret)) 


# DEMEAN AND BOOTSTRAP STANDARD NORMAL APPROXIMATION ====
set.seed(3.14)

# demean
temp = ret0 %>% 
  group_by(signalname) %>% 
  summarize(
    rbar = mean(ret)
  )

ret1 = ret0 %>% 
  left_join(temp) %>% 
  mutate(
    ret1 = ret - rbar
  )

# bootstrap 
nboot = 1e6
T = 360

almost_normal = numeric(length = nboot)
for (i in 1:nboot){
  tempret = sample(ret1$ret1, size = T, replace = T)
  almost_normal[i] = mean(tempret)/sd(tempret)*sqrt(T)
}

# PLOT ====
# almost_normal = rnorm(nboot)

mad = cumsum(abs(almost_normal))/(1:nboot) 

best_pi = 2/mad^2

plot(
  best_pi[100:nboot]
  , xlab = '# bootstraps used'
  , ylab = 'best pi'
  )
text(nboot*0.9, 3.4, labels = paste0('best pi is ', sprintf('%.4f', best_pi[nboot])))

best_pi[nboot]