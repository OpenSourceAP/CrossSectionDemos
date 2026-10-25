# Compare portfolio returns from two OpenAP releases, by regressing new
# returns on old returns for each signal, portfolio, and sample period.
#
# Run:     Rscript old_vs_new_returns_check.R  (from this folder)
# Inputs:  portfolio returns from two releases and SignalDoc (via OpenSourceAP.DownloadR)
# Outputs: temp/PredictorPortsCheck.csv, temp/old_vs_new_rsq_hist.png,
#          temp/old_vs_new_selected.png, summary tables to console

# ENVIRONMENT ====
rm(list = ls())
library(tidyverse)
library(gridExtra)
library(knitr)
library(OpenSourceAP.DownloadR)

### USER ENTRY
OLD_RELEASE = '2024_10'
NEW_RELEASE = '2025_10'

# 'op' for original papers, or another dataset from list_port(),
# e.g. 'quintiles_vw' or 'vw_force'
DATASET = 'op'

signalselect = 'ProbInformedTrading' # signal to plot at the end

dir.create('temp', showWarnings = FALSE)

#=====================================================================#
# Download data                                                   ====
#=====================================================================#

openap_old = OpenAP$new(OLD_RELEASE)
openap_new = OpenAP$new(NEW_RELEASE)

old_ports = openap_old$dl_port(DATASET)
new_ports = openap_new$dl_port(DATASET)

SignalDoc = openap_new$dl_signal_doc() %>%
  as_tibble() %>%
  select(Acronym, Year, SampleStartYear, SampleEndYear) %>%
  rename(YearPub = Year)

#=====================================================================#
# Join                                                            ====
#=====================================================================#

# Join and keep observations that match
PredictorPortsFull = inner_join(
  old_ports %>% select(signalname, port, date, old_ret = ret)
  , new_ports %>% select(signalname, port, date, new_ret = ret)
  , by = c("signalname", "port", "date")
  ) %>%
  filter(!is.na(old_ret), !is.na(new_ret))

# Bring sample years
PredictorPortsFull = inner_join(
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

# Regression of new returns on old returns
regstats = function(new_ret, old_ret){
  fit = lm(new_ret ~ old_ret)
  tibble(
    intercept = coef(fit)[1],
    slope = coef(fit)[2],
    rsq = summary(fit)$r.squared*100,
    new_rbar = mean(new_ret),
    old_rbar = mean(old_ret)
  )
}

temp1 = PredictorPortsFull %>%
  filter(!is.na(samptype)) %>%
  group_by(signalname, port, samptype) %>%
  summarize(regstats(new_ret, old_ret), .groups = 'drop')

temp2 = PredictorPortsFull %>%
  filter(yr >= SampleStartYear) %>%
  group_by(signalname, port) %>%
  summarize(regstats(new_ret, old_ret), .groups = 'drop') %>%
  mutate(samptype = 'full-samp')

check = bind_rows(temp1,temp2) %>% arrange(signalname,port,samptype)

write.csv(check, "temp/PredictorPortsCheck.csv", row.names = FALSE)


#=====================================================================#
# Summary stats output to console ====
#=====================================================================#

check_ls = check %>%
  filter(port == 'LS', !is.na(slope))

sumstat = check_ls %>%
  group_by(samptype) %>%
  summarize(
      p05 = quantile(rsq, 0.05)
    , p10 = quantile(rsq, 0.10)
    , p25 = quantile(rsq, 0.25)
    , p50 = quantile(rsq, 0.50)
    , p75 = quantile(rsq, 0.75)
    , p90 = quantile(rsq, 0.90)
    , p95 = quantile(rsq, 0.95)
  )

print(kable(sumstat, digits = 2
  , caption = paste0('Cross-predictor distribution of R-sq from regressing ', NEW_RELEASE
                     , ' LS returns on ', OLD_RELEASE)))

# mean return differences among poorly matching signals
check_ls %>%
  filter(rsq < 98.4) %>%
  mutate(dret = new_rbar - old_rbar) %>%
  group_by(samptype) %>%
  summarize(nsignal = n(), mean_abs_dret = mean(abs(dret)), mean_dret = mean(dret)) %>%
  print()

# lowest rsq signals
check_ls %>%
  filter(samptype == 'full-samp') %>%
  select(signalname, rsq, new_rbar, old_rbar) %>%
  arrange(rsq) %>%
  print(n = 20)


#=====================================================================#
# Plot summaries ====
#=====================================================================#

# Histogram of Rsq
ggplot(check_ls, aes(x = rsq, fill = samptype)) +
  geom_histogram(bins = 50, alpha = 0.5, position = "identity") +
  labs(title = "R-squared of New on Old Returns", x = "R-squared (%)", y = "Number of Signals") +
  theme_minimal(base_size = 18)

ggsave('temp/old_vs_new_rsq_hist.png', width = 10, height = 6, bg = 'white')


#=====================================================================#
# Check selected signal ====
#=====================================================================#

plotme = PredictorPortsFull %>%
  filter(signalname == signalselect , port == 'LS') %>%
  select(date, old_ret, new_ret) %>%
  pivot_longer(
    c(old_ret, new_ret), names_to = 'vintage', values_to = 'ret'
  )

plot_window = function(yearbegin){
  plotme %>%
    filter(year(date) >= yearbegin, year(date) <= yearbegin + 5) %>%
    ggplot(aes(x=date, y = ret, group = vintage
               , color = vintage, linetype = vintage)) +
    geom_line(linewidth = 1) +
    ggtitle(signalselect) +
    theme_minimal()
}

g = arrangeGrob(plot_window(2000), plot_window(2015), nrow = 1)
ggsave('temp/old_vs_new_selected.png', g, width = 12, height = 4, bg = 'white')

check %>%
  filter(signalname == signalselect) %>%
  arrange(samptype, port) %>%
  print(n = 50)
