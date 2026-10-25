# 2021 03 Andrew Chen: plot selected anomal(ies)
#
# Run:     Rscript plot_anomaly.R  (from this folder)
# Inputs:  OpenAP long-short returns and SignalDoc (via OpenSourceAP.DownloadR),
#          Ken French's market factor (downloaded)
# Outputs: temp/plot_anomaly.png, summary stats to console

# ==== ENVIRONMENT ====

rm(list = ls())
library(tidyverse)
library(OpenSourceAP.DownloadR)

### USER ENTRY
# first signal will be the focus
# use 'mkt_rf' to compare with market excess return (CRSPVW)
signallist = c('TrendFactor','mkt_rf')
# signallist = c('IndIPO','CompEquIss','ShareIss1Y')
years_presamp = 15 # for x axis limits

# latest release by default, or e.g. OpenAP$new('2024_10')
openap = OpenAP$new()

dir.create('temp', showWarnings = FALSE)

# reads one block of monthly or daily data from a Ken French zip
read_french = function(zipname, block = 1){
  url = paste0('https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/', zipname, '_CSV.zip')
  zipfile = tempfile(fileext = '.zip')
  download.file(url, zipfile, quiet = TRUE)
  lines = readLines(unz(zipfile, unzip(zipfile, list = TRUE)$Name[1]))
  is_data = grepl('^\\s*[0-9]{6,8}\\s*,', lines)
  start = which(is_data & !c(FALSE, head(is_data, -1)))[block]
  end = start + match(FALSE, is_data[start:length(lines)], nomatch = length(lines) - start + 2) - 2
  read.csv(text = c(lines[start - 1], lines[start:end]), check.names = FALSE) %>%
    rename(yyyymm = 1) %>%
    as_tibble()
}

# ==== DOWNLOAD DATA =====

## signal documentation
signaldoc = openap$dl_signal_doc() %>%
  as_tibble() %>%
  mutate(
    signalname = Acronym
    , pubdate = as.Date(paste0(Year, '-12-31'))
    , sampend = as.Date(paste0(SampleEndYear, '-12-31'))
  ) %>%
  arrange(signalname)

print('Here are some signals you can choose from')
signaldoc %>% select(signalname,Authors,LongDescription) %>% print(n=20)

## long-short returns
signalopenap = signallist[signallist != 'mkt_rf'] # mkt_rf is dl from ff, not here

doctarget = signaldoc %>% filter(signalname == signalopenap[1])

port = openap$dl_port('op', signalopenap) %>%
  filter(port == 'LS') %>%
  transmute(date, ret, signalname)

# ==== ADD MARKET BENCHMARK ====

ff = read_french('F-F_Research_Data_Factors') %>%
  transmute(yyyymm, ret = `Mkt-RF`, signalname = 'mkt_rf')

ff = ff %>%
  inner_join(
    port %>% distinct(date) %>% mutate(yyyymm = year(date)*100+month(date))
    , by = 'yyyymm'
  ) %>%
  select(date, ret, signalname)

port = rbind(port,ff)

# ==== PLOT ====

## standardize and accumulate
plotme = port %>%
  filter(
    date >= doctarget$sampend - years(years_presamp)
    , !is.na(ret)
  )
plotme = plotme %>%
  arrange(signalname,date) %>%
  group_by(signalname) %>%
  mutate(cret = cumprod(1+ret/100)) %>%
  ungroup %>%
  mutate(signalname = factor(signalname, levels = signallist))


yloc = (max(plotme$cret)+min(plotme$cret))/2
papername = paste0(doctarget$Authors, ' ', doctarget$Year, ' (', signallist[1], ') ')

ggplot(plotme, aes(x=date,y=cret)) +
  geom_line(aes(linetype = signalname, color = signalname), linewidth=1.2) +
  xlab("") + ylab('Cumulative Long-Short Return') +
  geom_vline(xintercept = doctarget$pubdate, color = 'red') +
  annotate('text', x = doctarget$pubdate, y = yloc
           , label = paste0("\n ", papername, ' Published'), colour="red", angle=90, size=3) +
  geom_vline(xintercept = doctarget$sampend, color = 'blue') +
  annotate('text', x = doctarget$sampend, y = yloc
           , label = paste0("\n ", papername, ' Sample Ends'), colour="blue", angle=90, size=3) +
  theme_minimal(base_size = 10) +
  theme(legend.title = element_blank())  +
  scale_color_brewer(palette = 'Dark2')


ggsave('temp/plot_anomaly.png', width = 6, height = 4, bg = 'white')

# ==== SUMMARY STATS ====

port %>%
  group_by(signalname) %>%
  summarize( rbar = mean(ret, na.rm=T), vol = sd(ret, na.rm=T), nmonth = n() ) %>%
  print(n=100)
