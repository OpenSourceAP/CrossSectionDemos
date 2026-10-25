# compare to Ken French's data
# Andrew Chen 2021 04
#
# Run:     Rscript openap_vs_frenchweb.R  (from this folder)
# Inputs:  OpenAP BM and BMdec long-short returns (via OpenSourceAP.DownloadR),
#          Ken French's BE-ME portfolios and HML (downloaded)
# Outputs: temp/openap_vs_french_close.png

# ENVIRONMENT ====
rm(list = ls())
library(tidyverse)
library(OpenSourceAP.DownloadR)

### USER ENTRY
datebegin = as.Date('2019-01-01') # plot runs from here to the end of the data

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

# DOWNLOAD AND PROCESS French data (ff) ====

# BE-ME quintiles: block 1 is value weighted, block 2 is equal weighted
ff1 = read_french('Portfolios_Formed_on_BE-ME', block = 2) %>%
  transmute(yyyymm, ret = `Hi 20` - `Lo 20`, port = 'ff_ew5')

ff2 = read_french('Portfolios_Formed_on_BE-ME', block = 1) %>%
  transmute(yyyymm, ret = `Hi 20` - `Lo 20`, port = 'ff_vw5')

ff3 = read_french('F-F_Research_Data_Factors') %>%
  transmute(yyyymm, ret = HML, port = 'ff_hml')

ff = rbind(ff1,ff2,ff3)

# DOWNLOAD AND PROCESS OPENAP DATA (cz)====

cz = openap$dl_port('op', c('BM','BMdec')) %>%
  filter(port == 'LS') %>%
  transmute(
    yyyymm = year(date)*100 + month(date)
    , ret
    , port = signalname
  )

# MERGE AND CHECK ====
both = rbind(ff,cz)


info = data.frame(
  levels      = c('ff_hml', 'ff_vw5', 'ff_ew5', 'BMdec', 'BM')
  , labels    = c('Ken French HML (FF93)', 'Ken French VW', 'Ken French EW','Open AP BMdec (a la FF92)','Open AP BM (a la RRL85)')
  , colors    = c('gray', 'black','blue','red','magenta')
  , linetypes = c('solid', 'solid','solid','twodash','dotted')
  , rank      = c(3,4,5,1,2)
) %>% arrange(rank)

plotme = both %>%
  mutate(
    date = as.Date(as.character(yyyymm*100+28), '%Y%m%d')
  ) %>%
  mutate(
    port = factor(port, levels = info$levels, labels = info$labels)
  )

# plot through the last month that all series have
dateend = plotme %>% group_by(port) %>% summarize(d = max(date)) %>% pull(d) %>% min()

ggplot(
  plotme %>%
    filter(date >= datebegin, date <= dateend) %>%
    group_by(port) %>% arrange(date) %>%
    mutate(
      ret = if_else(abs(date - datebegin) <= 31 , 0, ret)
      , cret = 100*(cumprod(1+ret/100) - 1)
    )
  , aes(x=date, y=cret, group=port)) +
  geom_line(aes(linetype = port, color = port), linewidth = 0.75) +
  ylab('Cumulative Return on Value (%)')+
  xlab('') +
  ggtitle('Open Asset Pricing vs Ken French') +
  theme_minimal(base_size = 18) +
  theme(
    legend.title = element_blank()
    , legend.position = 'bottom'
  )  +
  guides(color = guide_legend(ncol = 2), linetype = guide_legend(ncol = 2)) +
  scale_color_manual(
    breaks = info$labels
    , values= info$colors
  ) +
  scale_linetype_manual(
    breaks = info$labels
    , values = info$linetypes
  )

ggsave(filename = "temp/openap_vs_french_close.png", width = 10, height = 6, bg = 'white')
