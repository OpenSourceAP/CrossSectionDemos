# 2021 12 Andrew Chen: replicate the main result of McLean-Pontiff
#
# Run:     Rscript mclean_pontiff_main.R  (from this folder)
# Inputs:  OpenAP long-short returns and SignalDoc (via OpenSourceAP.DownloadR)
# Outputs: decay table to console, temp/mclean_pontiff_boot.png

# ENVIRONMENT ====

rm(list = ls())
library(tidyverse)
library(OpenSourceAP.DownloadR)

# latest release by default, or e.g. OpenAP$new('2024_10')
openap = OpenAP$new()

dir.create('temp', showWarnings = FALSE)

# DOWNLOAD DATA =====

## signal documentation
signaldoc = openap$dl_signal_doc() %>%
  as_tibble() %>%
  mutate(
    signalname = Acronym
    , pubdate = as.Date(paste0(Year, '-12-31'))
    , sampend = as.Date(paste0(SampleEndYear, '-12-31'))
    , sampstart = as.Date(paste0(SampleStartYear, '-01-01'))
  ) %>%
  arrange(signalname) %>%
  select(signalname, pubdate, sampend, sampstart)

## all long-short returns (OP) ====
ret0 = openap$dl_port('op') %>%
  filter(port == 'LS', !is.na(ret)) %>%
  select(date, signalname, ret)


# MERGE AND FIND OUT OF SAMPLE RETURNS ====
ret1 = ret0 %>%
  left_join(signaldoc, by = 'signalname') %>%
  mutate(
    samptype = case_when(
      (date >= sampstart) & (date <= sampend) ~ 'in-samp'
      , (date > sampend) & (date <= pubdate) ~ 'out-of-samp'
      , (date > pubdate) ~ 'post-pub'
      , TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(samptype))

sumsignal = ret1 %>%
  group_by(signalname, samptype) %>%
  summarize(
    rbar = mean(ret)
    , tstat = rbar/sd(ret)*sqrt(n())
    , .groups = 'drop'
  )

# remove bad reproductions / bad predictors (if desired)
signalok = sumsignal %>%
  filter(samptype=='in-samp') %>%
  filter(tstat > -Inf) %>%
  transmute(signalname)
sumsignal = sumsignal %>% inner_join(signalok, by = 'signalname')

# check out of sample decay simple way ====
sumsamp = sumsignal %>%
  group_by(samptype) %>%
  summarize(rbar = mean(rbar), nsignal = n())


baseline = sumsamp %>% filter(samptype == 'in-samp') %>% pull(rbar)

sumsamp = sumsamp %>%
  mutate(
    decay = (baseline - rbar)/baseline
  )

print(sumsamp)


# BOOTSTRAP MEAN DISTRIBUTIONS ====
# clustered by month

set.seed(6)

nboot = 200

bootfun = function(sampname){

  # make wide dataset, use NA if not correct sample
  wide_is = ret1 %>%
    filter(samptype == sampname) %>%
    select(signalname, date, ret) %>%
    pivot_wider(
      names_from = signalname, values_from = ret
    ) %>%
    select(-date) %>%
    as.matrix()

  # make array that only has enough signals in each month (10)
  tgood = rowSums(!is.na(wide_is), na.rm=T) > 10
  mat = wide_is[tgood, ]
  nmonth = dim(mat)[1]

  # bootstrap pooled mean
  rboot = rep(NA_real_, nboot)
  for (i in 1:nboot){
    tempt = sample(1:nmonth, replace = T)
    rboot[i] = mat[tempt,]  %>% as.vector %>% mean(na.rm=T)
  }

  return(rboot)

} # end bootfun


# bootstrap for each sample type
rboot1 = bootfun('in-samp')
rboot2 = bootfun('out-of-samp')

# compile and plot
bootdat = data.frame(
  pooled_mean_ret = rboot1, samptype = 'in-samp'
) %>%
  rbind(
    data.frame(
      pooled_mean_ret = rboot2, samptype = 'out-of-samp'
    )
  )


bootdat  %>%
  ggplot(aes(x=pooled_mean_ret, fill=samptype)) +
  geom_histogram(
    alpha = 0.6, position = 'identity', breaks = seq(0,1,0.025), aes(y=after_stat(density))
  ) +
  ggtitle('bootstrapped distribution') +
  labs(x='pooled mean return (% monthly)') +
  geom_vline(xintercept = 0) +
  theme_minimal()

ggsave('temp/mclean_pontiff_boot.png', width = 6, height = 4, bg = 'white')
