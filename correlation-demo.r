# 2023 08 andrew: self-contained demo for studying long-short correlations

# Environment -------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)
library(RColorBrewer)
library(gt)
library(gridExtra)
library(ggdendro)

### USER ENTRY

# root of August 2023 release
pathRelease = 'https://drive.google.com/drive/u/0/folders/1EP6oEabyZRamveGNyzYU0u6qJ-N43Qfq'

# login to gdrive
# this prompts a login
pathRelease %>% drive_ls()

# create temporary directory 
dir.create('temp/')

MATBLUE = rgb(0,0.4470,0.7410)
MATRED = rgb(0.8500, 0.3250, 0.0980)
MATYELLOW = rgb(0.9290, 0.6940, 0.1250)

NICEBLUE = "#619CFF"
NICEGREEN = "#00BA38"
NICERED = "#F8766D"


# Download Data -----------------------------------------------------------

## download signal documentation and show user
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='SignalDoc.csv')

drive_download(target_dribble, path = 'temp/deleteme.csv', overwrite = T)

signaldoc = fread('temp/deleteme.csv') %>% 
  filter(Cat.Signal == 'Predictor') %>% 
  mutate(
    signalname = Acronym
    , pubdate = as.Date(paste0(Year, '-12-31'))
    , sampend = as.Date(paste0(SampleEndYear, '-12-31'))
    , sampstart = as.Date(paste0(SampleStartYear, '-01-01'))
    , longname = LongDescription
  ) %>% 
  arrange(signalname) %>% 
  select(signalname, pubdate, sampend, sampstart, longname)  %>% 
  mutate(
    longname = str_to_title(longname)
  )

# fix duplicate long names
signaldoc = signaldoc %>% 
  group_by(longname) %>% 
  arrange(longname, signalname) %>% 
  mutate(dup = n() >1) %>% 
  mutate(
    longname = if_else(dup, signalname, longname )
  ) %>% 
  ungroup()


# download all long-short returns (OP) 
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='Portfolios') %>% drive_ls() %>% 
  filter(name=='Full Sets OP') %>% drive_ls() %>% 
  filter(name=='PredictorLSretWide.csv')

drive_download(target_dribble[1,], path = 'temp/deleteme.csv', overwrite = T)

ret0 = fread('temp/deleteme.csv') %>% 
  pivot_longer(-c(date),names_to = 'signalname', values_to = 'ret') %>% 
  filter(!is.na(ret)) 


# Find correlations --------------------------------------------------------

retwide = ret0 %>% 
  left_join(
    signaldoc %>% select(signalname,longname) 
  ) %>% 
  filter(year(date) >= 1963 & year(date)) %>% 
  select(-longname) %>% 
  pivot_wider(names_from = 'signalname', values_from = 'ret')

cmat = cor(retwide %>% select(-date), use = 'pairwise.complete.obs')

cmatdf =  cmat %>% as.data.frame() %>% 
  mutate(
    name1 = rownames(cmat)
  ) %>% 
  pivot_longer(
    cols = -c('name1'), names_to = 'name2', values_to = 'cor'
  )


# Plot cor and pca --------------------------------------------------------

chen_theme = theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1)    
    # Font sizes
    , axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    
    # Tweaking legend
    legend.position = c(0.7, 0.8),
    legend.text.align = 0,
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(t = 5, r = 20, b = 5, l = 5), 
    legend.key.size = unit(1.5, "cm")
    , legend.title = element_blank()    
    
    # grid lines
    ,panel.grid.major = element_line(color = "grey90",
                                    size = 0.5,
                                    linetype = 1)
  ) 


cmatlong = cmat[lower.tri(cmat)] %>% as_tibble() %>% 
  rename(cor = value)

p_cor = ggplot(cmatlong, aes(x=cor)) +
  geom_histogram(color = 'black', fill = 'grey') +
  chen_theme +
  xlab('Signed Long-short Return Corr') +
  ylab('Number of Anomaly Pairs')


eigval = eigen(cmat)$value

plotme = tibble(
  npc = 1:length(eigval)
  , pctexpl = cumsum(eigval) / sum(eigval) *100  
)

p_pca = ggplot(plotme, aes(x=npc,y=pctexpl)) +
  geom_line(size = 0.8) +
  chen_theme +
  coord_cartesian(xlim = c(0,80), ylim = c(0,100))+
  scale_y_continuous(breaks = seq(0,100,20)) +
  xlab('Number of Principal Components') +
  ylab('% Variance Explained')

g = arrangeGrob(p_cor, p_pca, nrow = 1)

ggsave('temp/cor_and_pca.png', g)


# Show Tables for New Predictors  ---------------------------------------------------------------------

# most correlated
plotme = cmatdf %>% 
  filter(
    name1 %in% c('CPVolSpread','RIVolSpread','dVolCall','dVolPut','dCPVolSpread')
  ) %>% 
  filter(cor < 1) %>% 
  group_by(name1) %>% 
  arrange(name1, desc(cor)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  left_join(
    signaldoc %>% transmute(name1 = signalname, longname1 = longname)
  ) %>% 
  left_join(
    signaldoc %>% transmute(name2 = signalname, longname2 = longname)
  ) %>% 
  transmute(
    `New Predictor` = longname1
    , `Most Correlated Predictor` = longname2
    , `Correlation` = cor
  ) %>% 
  arrange(-Correlation)


tab = plotme %>% 
  gt() %>% 
  fmt_number(Correlation)


gtsave(tab, 'temp/new_predictor_cor_1.png')


# second most correlated
plotme = cmatdf %>% 
  filter(
    name1 %in% c('CPVolSpread','RIVolSpread','dVolCall','dVolPut','dCPVolSpread')
  ) %>% 
  filter(cor < 1) %>% 
  group_by(name1) %>% 
  arrange(name1, desc(cor)) %>% 
  filter(row_number() == 2) %>% 
  ungroup() %>% 
  left_join(
    signaldoc %>% transmute(name1 = signalname, longname1 = longname)
  ) %>% 
  left_join(
    signaldoc %>% transmute(name2 = signalname, longname2 = longname)
  ) %>% 
  transmute(
    `New Predictor` = longname1
    , `2nd Most Correlated Predictor` = longname2
    , `Correlation` = cor
  ) %>% 
  arrange(-Correlation)


tab = plotme %>% 
  gt() %>% 
  fmt_number(Correlation)


gtsave(tab, 'temp/new_predictor_cor_2.png')

# Dendrogram --------------------------------------------------------------

# make dendogram based on correlation matrix
dendro = dist(1-cmat) %>% 
    hclust()  %>% 
    as.dendrogram()

# ggdendrogram turns out to make the nicest plot of all the different R methods
ggdendrogram(dendro, rotate = T) +
  labs(title = '          Correlation Clusters\n in the Chen-Zimmermann Data')

scale = 2
ggsave('temp/ggdendro.pdf', height = 15*scale, width = 2.5*scale)
ggsave('temp/ggdendro.png', height = 15*scale, width = 2.5*scale)


