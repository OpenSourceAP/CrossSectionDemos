
# Compare old and new returns from PredictorPortsFull.csv, where
# old returns correspond to the last version of OpenAssetPricing,
# and new returns correspond to the most recent release.

# ENVIRONMENT ====
rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)

dir.create('temp/')

# Global constants
URL_PREFIX <-  "https://drive.google.com/uc?export=download&id="
OLD_PATH_RELEASES <- "https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R"
NEW_PATH_RELEASES <- "https://drive.google.com/drive/folders/1O18scg9iBTiBaDiQFhoGxdn4FdsbMqGo"

#=====================================================================#
# Download files                                                  ====
#=====================================================================#

# Get id of old PredictorPortsFull. Will prompt a google drive login.
id <-  OLD_PATH_RELEASES %>% drive_ls() %>%
  filter(name == "Portfolios") %>% drive_ls() %>% 
  filter(name == "Full Sets OP") %>% drive_ls() %>% 
  filter(name == "PredictorPortsFull.csv") %>% 
  drive_download(path = "temp/PredictorPortsFullOld.csv", overwrite = TRUE)

# Download data
old_PredictorPortsFull <- fread("temp/PredictorPortsFullOld.csv")

# Get id of new PredictorPortsFull
id <-  NEW_PATH_RELEASES %>% drive_ls() %>% 
  filter(name == "Portfolios") %>% drive_ls() %>% 
  filter(name == "Full Sets OP") %>% drive_ls() %>% 
  filter(name == "PredictorPortsFull.csv") %>% 
  drive_download(path = "temp/PredictorPortsFullNew.csv", overwrite = TRUE)

# Download data
new_PredictorPortsFull <- fread("temp/PredictorPortsFullNew.csv")

# Get id of SignalDoc
id <-  NEW_PATH_RELEASES %>% drive_ls() %>% 
  filter(name == "SignalDoc.csv") %>% 
  drive_download(path = "temp/SignalDoc.csv", overwrite = TRUE)

SignalDoc <- fread("temp/SignalDoc.csv")



#=====================================================================#
# Mutate dataframes and join                                      ====
#=====================================================================#

# Drop unwanted variables
old_PredictorPortsFull <- old_PredictorPortsFull %>% 
  select(-signallag, -Nlong, -Nshort) %>% 
  rename(old_ret = ret)

# Drop unwanted variables
new_PredictorPortsFull <- new_PredictorPortsFull %>% 
  select(-signallag, -Nlong, -Nshort) %>% 
  rename(new_ret = ret)

# Join and keep observations that match
PredictorPortsFull <- inner_join(
  old_PredictorPortsFull, new_PredictorPortsFull,
  by = c("signalname", "port", "date"))

# Keep only relevant variables
SignalDoc <- SignalDoc %>% 
  select(Acronym, Year, SampleStartYear, SampleEndYear) %>% 
  rename(YearPub = Year)

# Bring sample years
PredictorPortsFull <- inner_join(
  PredictorPortsFull, SignalDoc,
  by = c("signalname" = "Acronym")) %>% 
  # Classify observations as in-sample or post-publication
  mutate(
    yr = year(date),
    samptype = case_when(
      SampleStartYear <= yr & yr <= SampleEndYear ~ "in-samp",
      yr > YearPub ~ "post-pub"
    )
  )

#=====================================================================#
# Run regressions and export results                              ====
#=====================================================================#

# Regression by group. New returns on old returns
check <- PredictorPortsFull[, list(
  intercept = coef(lm(new_ret ~ old_ret))[1],
  slope = coef(lm(new_ret ~ old_ret))[2],
  rsq = summary(lm(new_ret ~ old_ret))$r.squared*100
  ),
  by = c("signalname", "port", "samptype")]

# Export results
write.csv(check, "temp/PredictorPortsCheck.csv", row.names = FALSE)



#=====================================================================#
# Summary stats ====
#=====================================================================#

check_ls = check %>% 
  filter(port == 'LS', !is.na(samptype), !is.na(slope)) %>% 
  mutate(rsq = rsq) 

sumstat = check_ls %>% 
  group_by(samptype) %>% 
  summarize(
    min = quantile(rsq, 0)
    , p05 = quantile(rsq, 0.05)
    , p25 = quantile(rsq, 0.25)
    , p50 = quantile(rsq, 0.50)
    , p75 = quantile(rsq, 0.75)
    , p95 = quantile(rsq, 0.95)
    , max = quantile(rsq, 1)    
  ) %>% 
  as.data.frame()

print('Rsq from regressing new long-short OP returns on old')
print(sumstat)

check_ls %>% 
  filter(samptype == 'in-samp', rsq < 95) %>% 
  arrange(rsq)
  
check_ls %>% 
  filter(samptype == 'post-pub', rsq < 95) %>% 
  arrange(rsq)

# COMPARE ONE ====
signalselect = 'Price'

plotme = PredictorPortsFull %>% 
  filter(signalname == signalselect , port == 'LS') %>% 
  select(date, old_ret, new_ret) %>% 
  pivot_longer(
    c(old_ret, new_ret), names_to = 'vintage', values_to = 'ret'
  ) 

yearbegin = 1990
yearend   = yearbegin + 5
p1 = plotme %>%  
  filter(year(date) >= yearbegin, year(date) <= yearend) %>%   
  ggplot(aes(x=date, y = ret, group = vintage, color = vintage)) +
  geom_line() + 
  theme_minimal() 

yearbegin = 2015
yearend   = yearbegin + 5
p2 = plotme %>%  
  filter(year(date) >= yearbegin, year(date) <= yearend) %>%   
  ggplot(aes(x=date, y = ret, group = vintage, color = vintage)) +
  geom_line() + 
  theme_minimal()

library(gridExtra)

grid.arrange(p1,p2,nrow = 1)

check_ls %>% 
  filter(signalname == signalselect)



# yearbegin = 1990
# yearend   = 2020  
# plotme %>%  
#   filter(year(date) >= yearbegin, year(date) <= yearend) %>%   
#   ggplot(aes(x=date, y = ret, group = vintage, color = vintage)) +
#   geom_line() + 
#   theme_minimal()
