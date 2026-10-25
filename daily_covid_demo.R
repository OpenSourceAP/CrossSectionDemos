# plot anomaly returns during the Covid crash.  Andrew Chen 2021 04
#
# Run:     Rscript daily_covid_demo.R  (from this folder)
# Inputs:  OpenAP daily VW portfolio returns for all signals (via OpenSourceAP.DownloadR),
#          Ken French's daily factors (downloaded)
# Outputs: temp/covid_anom_select.png, temp/covid_anom_all.png

# ==== ENVIRONMENT ====
rm(list = ls())
library(tidyverse)
library(OpenSourceAP.DownloadR)

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

# ==== IMPORT DATA ====

## OPEN AP DATA
# all daily VW portfolios, a big file that can take a minute
all = openap$dl_port('daily_op_vw') %>%
  filter(port == 'LS', date >= '2019-01-01', !is.na(ret)) %>%
  transmute(date, ret, port = signalname)

agg = all %>%
  group_by(date) %>%
  summarize(ret = mean(ret)) %>%
  mutate(port = 'mean_anomaly')

nsignal = n_distinct(all$port)

all = rbind(agg,all)

## KEN FRENCH DATA DAILY
ffd = read_french('F-F_Research_Data_Factors_daily') %>%
  pivot_longer(-yyyymm, names_to = 'port', values_to = 'ret') %>%
  transmute(date = as.Date(as.character(yyyymm), "%Y%m%d"), ret, port) %>%
  filter(!is.na(ret), date >= '2019-01-01')

all = rbind(all,ffd)


# ==== PLOT SELECTED ====

signallist =c(
  'Mkt-RF'
  , 'BMdec'
  , 'EarningsSurprise'
  , 'OPLeverage'
)
labellist = c(
  'CRSP Index'
  ,'Book / Market'
  ,'Earnings Surprise'
  , 'Operating Leverage'
)
colorlist = c(
  'grey'
  , 'red'
  , 'blue'
  , 'green'
)
sizelist = c(
  1.5, 1, 1, 1
)

small = all %>%
  filter(port %in% signallist) %>%
  filter(date >= '2020-02-01') %>%
  filter(date <= '2020-05-30') %>%
  group_by(port) %>%
  arrange(date) %>%
  mutate(cret = cumprod(1+ret/100)-1, cret = cret*100) %>%
  mutate(
    port = factor(port, levels = signallist, labels = labellist)
  )

# grouped plot
ggplot(small, aes(x=date,y=cret)) +
  geom_line(aes(color = port, linewidth=port)) +
  xlab("2020") + ylab('VW Long-Short Cum Return (%)')  +
  theme_minimal(base_size = 18) +
  scale_color_manual(breaks = labellist, values= colorlist)  +
  scale_linewidth_manual(breaks = labellist, values= sizelist) +
  theme(legend.title = element_blank())

ggsave(filename = "temp/covid_anom_select.png", width = 10, height = 6, bg = 'white')

# ==== PLOT ALL VS VW ====

plotme = all %>%
  filter(date >= '2020-01-01') %>%
  filter(date <= '2020-05-30') %>%
  group_by(port) %>%
  arrange(date) %>%
  mutate(cret = cumprod(1+ret/100)-1, cret = cret*100)

plotme2 = plotme %>% filter(port == 'mean_anomaly')
plotme3 = plotme %>% filter(port == 'Mkt-RF')

ggplot(plotme, aes(x=date,y=cret)) +
  geom_line(aes(group = port, color='a')) +
  geom_line(data=plotme2, aes(color = 'b'), linewidth = 1.5) +
  geom_line(data=plotme3, aes(color = 'c'), linewidth = 1.5) +
  scale_color_manual(name = 'data source',
                   values =c(
                     'a'='grey'
                     ,'b'='blue'
                     ,'c'='black'
                     ),
                   labels = c(
                     paste0(nsignal, ' Anomalies')
                     ,'Average Anomaly'
                     ,'CRSP VW'
                     )) +
  xlab("2020") + ylab('VW Long-Short Cum Return (%)')+
  theme_minimal(base_size = 18) +
  theme(legend.title = element_blank())  +
  coord_cartesian(ylim = c(-50,50))

ggsave(filename = "temp/covid_anom_all.png", width = 10, height = 6, bg = 'white')
