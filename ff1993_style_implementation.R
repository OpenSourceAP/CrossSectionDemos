# for making FF1993 style factors from OpenAP firm characteristics
# andrew 2021 05
#
# Run:     Rscript ff1993_style_implementation.R  (from this folder)
#          WRDS credentials come from WRDS_USER / WRDS_PASS, ~/.pgpass, or a prompt
# Inputs:  OpenAP firm characteristic and FF93-style portfolios (via OpenSourceAP.DownloadR),
#          CRSP CIZ monthly stock file (crsp.msf_v2, WRDS), Ken French's HML (downloaded)
# Outputs: temp/openap_vs_french_hml.png, correlations to console

# FF1993 style is based on WRDS:
# https://wrds-www.wharton.upenn.edu/pages/support/applications/risk-factors-and-industry-benchmarks/fama-french-factors/

# ==== ENVIRONMENT ====

rm(list = ls())
library(tidyverse)
library(getPass)
library(RPostgres)
library(OpenSourceAP.DownloadR)

### USER ENTRY
# name signal to make factor from
signalname = 'BMdec'
datebegin = as.Date('2010-01-01') # for the plot

# latest release by default, or e.g. OpenAP$new('2024_10')
openap = OpenAP$new()

dir.create('temp', showWarnings = FALSE)

# login to wrds (empty user and password fall back to PGUSER and ~/.pgpass)
user = Sys.getenv('WRDS_USER')
pass = Sys.getenv('WRDS_PASS')
if ((user == '' | pass == '') & !file.exists('~/.pgpass')){
  user = getPass('wrds username: ')
  pass = getPass('wrds password: ')
}

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  user=user,
                  password=pass,
                  sslmode='require')

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


# ==== DOWNLOAD DATA ====

# OpenAP signal (unsigned, as constructed)
signal = openap$dl_signal(signalname) %>%
  rename(signal = !!signalname)


# CRSP monthly (CIZ format)
# mthret includes delisting returns, so no delisting adjustment is needed.
# The share code and exchange screens follow WRDS's mapping of the legacy
# shrcd in (10,11) and exchcd in (1,2,3):
# https://wrds-www.wharton.upenn.edu/pages/support/manuals-and-overviews/crsp/stocks-and-indices/crsp-stock-and-indexes-version-2/
crsp = dbGetQuery(wrds,
  "select permno, mthcaldt as date, mthret as ret, mthprc as prc, shrout,
     primaryexch,
     sharetype = 'NS' and securitytype = 'EQTY' and securitysubtype = 'COM'
       and usincflg = 'Y' and issuertype in ('ACOR', 'CORP') as common
   from crsp.msf_v2
   where conditionaltype = 'RW' and tradingstatusflg = 'A'
  "
) %>%
  as_tibble() %>%
  # convert ret to pct, other formatting
  mutate(
    ret = 100*ret
    , date = as.Date(date)
    , me = abs(prc) * shrout
    , yyyymm = year(date) * 100 + month(date)
  )

dbDisconnect(wrds)

# ==== ASSIGN TO 2X3 PORTFOLIOS ====

# full join to keep as many me obs as possible, ff1993, page 8
signaljune = signal %>%
  filter(yyyymm %% 100 == 6) %>%
  full_join(
    crsp %>%
      filter(yyyymm %% 100 == 6) %>%
      select(permno,yyyymm,me,primaryexch,common)
    , by = c('permno','yyyymm')
  )


# FF93 is unclear about the share code screen here, but
# WRDS does it
nysebreaks = signaljune %>%
  filter(primaryexch == 'N', common) %>%
  group_by(yyyymm) %>%
  summarise(
    qsignal_l = quantile(signal,0.3, na.rm=T)
    , qsignal_h = quantile(signal,0.7, na.rm=T)
    , qme_mid = quantile(me,0.5, na.rm=T)
  )


# only NYSE, AMEX, Nasdaq common stocks, ff93 p8-9
# we already exclude negative BE
port6 = signaljune %>%
  filter(
    primaryexch %in% c('N','A','Q'), common
  ) %>%
  left_join(nysebreaks, by=c('yyyymm')) %>%
  mutate(
    q_signal = case_when(
      signal <= qsignal_l ~ 'L'
      , signal <= qsignal_h ~ 'M'
      , signal > qsignal_h ~ 'H'
    )
    , q_me = case_when(
      me <= qme_mid ~ 'S'
      , me > qme_mid ~ 'B'
    )
    , port6 = paste0(q_me,q_signal)
  ) %>%
  select(
    permno, yyyymm, port6
  )

# ==== FIND MONTHLY FACTOR RETURNS ====


# find VW returns for port6
port6ret = crsp %>%
  # merge annual port6 onto returns
  filter(floor(yyyymm/100)>=1963) %>%
  select(permno,yyyymm,ret,me) %>%
  left_join(port6, by=c('permno','yyyymm')) %>%
  # fill and lag
  group_by(permno) %>%
  arrange(permno,yyyymm) %>%
  fill(port6) %>%
  mutate(
    port6lag = lag(port6)
    , melag = lag(me)
  ) %>%
  filter(!is.na(melag)) %>%
  # find value-weighted returns by port6lag month
  group_by(port6lag, yyyymm) %>%
  summarize(
    ret_vw = weighted.mean(ret,melag,na.rm=T)
    , .groups = 'drop'
  )


# equal weight extreme portfolios to make FF1993-style factor
ff93style = port6ret %>%
  pivot_wider(
    names_from = port6lag, values_from = ret_vw
  ) %>%
  transmute(
    yyyymm
    , HML_here = 0.5*(SH+BH) - 0.5*(SL+BL)
  )


# ==== COMPARE TO KEN FRENCH AND THE OPENAP RELEASE ====

french = read_french('F-F_Research_Data_Factors') %>%
  transmute(yyyymm, HML_french = HML)

# the release's own FF93-style long-short portfolio for the same signal
release = openap$dl_port('ff93', signalname) %>%
  filter(port == 'LS') %>%
  transmute(yyyymm = year(date)*100 + month(date), HML_openap_release = ret)

compare = ff93style %>%
  inner_join(french, by = 'yyyymm') %>%
  inner_join(release, by = 'yyyymm')

print(paste0('Correlations of monthly returns, ', min(compare$yyyymm), '-', max(compare$yyyymm)))
print(cor(compare %>% select(-yyyymm), use = 'pairwise.complete.obs'))

plotme = compare %>%
  pivot_longer(
    cols = -yyyymm
    , names_to = 'dataset'
    , values_to = 'ret'
  ) %>%
  mutate(
    date = as.Date(as.character(yyyymm*100+28), '%Y%m%d')
  )


# plot close up cumulative returns
ggplot(
  plotme %>%
    filter(date >= datebegin) %>%
    group_by(dataset) %>% arrange(date) %>%
    mutate(
      ret = if_else(abs(date - datebegin) <= 31 , 0, ret)
      , cret = 100*(cumprod(1+ret/100) - 1)
    )
  , aes(x=date, y=cret, group=dataset)) +
  geom_line(aes(linetype = dataset, color = dataset), linewidth = 0.75) +
  ylab('Cumulative Return (%)')+
  xlab('') +
  theme_minimal(base_size = 18) +
  theme(
    legend.title = element_blank()
    , legend.position = 'bottom'
  )


ggsave(filename = "temp/openap_vs_french_hml.png", width = 10, height = 6, bg = 'white')
