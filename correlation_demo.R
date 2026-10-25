# 2023 08 andrew: self-contained demo for studying long-short correlations
#
# Run:     Rscript correlation_demo.R  (from this folder)
# Inputs:  OpenAP long-short returns and SignalDoc (via OpenSourceAP.DownloadR)
# Outputs: temp/cor_and_pca.png, temp/dendrogram.pdf,
#          most-correlated tables for selected predictors to console

# Environment -------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(gridExtra)
library(knitr)
library(OpenSourceAP.DownloadR)

### USER ENTRY
# predictors to show most correlated neighbors for
signalselect = c('CPVolSpread','RIVolSpread','dVolCall','dVolPut','dCPVolSpread')

# latest release by default, or e.g. OpenAP$new('2024_10')
openap = OpenAP$new()

dir.create('temp', showWarnings = FALSE)


# Download Data -----------------------------------------------------------

## signal documentation
signaldoc = openap$dl_signal_doc() %>%
  as_tibble() %>%
  filter(Cat.Signal == 'Predictor') %>%
  mutate(
    signalname = Acronym
    , longname = str_to_title(LongDescription)
  ) %>%
  arrange(signalname) %>%
  select(signalname, longname)

# fix duplicate long names
signaldoc = signaldoc %>%
  group_by(longname) %>%
  mutate(dup = n() > 1) %>%
  ungroup() %>%
  mutate(longname = if_else(dup, signalname, longname)) %>%
  select(-dup)


# all long-short returns (OP)
ret0 = openap$dl_port('op') %>%
  filter(port == 'LS', !is.na(ret)) %>%
  select(date, signalname, ret)


# Find correlations --------------------------------------------------------

retwide = ret0 %>%
  filter(year(date) >= 1963) %>%
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
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
    # Font sizes
    , axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.text = element_text(size = 18),

    # Tweaking legend
    legend.position = 'inside',
    legend.position.inside = c(0.7, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.margin = margin(t = 5, r = 20, b = 5, l = 5),
    legend.key.size = unit(1.5, "cm")
    , legend.title = element_blank()

    # grid lines
    ,panel.grid.major = element_line(color = "grey90",
                                    linewidth = 0.5,
                                    linetype = 1)
  )


cmatlong = tibble(cor = cmat[lower.tri(cmat)])

p_cor = ggplot(cmatlong, aes(x=cor)) +
  geom_histogram(color = 'black', fill = 'grey', bins = 30) +
  chen_theme +
  xlab('Signed Long-short Return Corr') +
  ylab('Number of Anomaly Pairs')


# pairwise correlations need not be positive semidefinite, so drop tiny negative eigenvalues
eigval = pmax(eigen(cmat, symmetric = TRUE, only.values = TRUE)$values, 0)

plotme = tibble(
  npc = 1:length(eigval)
  , pctexpl = cumsum(eigval) / sum(eigval) *100
)

p_pca = ggplot(plotme, aes(x=npc,y=pctexpl)) +
  geom_line(linewidth = 0.8) +
  chen_theme +
  coord_cartesian(xlim = c(0,80), ylim = c(0,100))+
  scale_y_continuous(breaks = seq(0,100,20)) +
  xlab('Number of Principal Components') +
  ylab('% Variance Explained')

g = arrangeGrob(p_cor, p_pca, nrow = 1)

ggsave('temp/cor_and_pca.png', g, width = 14, height = 6, bg = 'white')


# Show Tables for Selected Predictors ---------------------------------------

# k-th most correlated predictor for each selected predictor
neighbor_table = function(k){
  cmatdf %>%
    filter(name1 %in% signalselect, name1 != name2) %>%
    group_by(name1) %>%
    arrange(name1, desc(cor)) %>%
    filter(row_number() == k) %>%
    ungroup() %>%
    left_join(signaldoc %>% transmute(name1 = signalname, longname1 = longname), by = 'name1') %>%
    left_join(signaldoc %>% transmute(name2 = signalname, longname2 = longname), by = 'name2') %>%
    transmute(
      `Selected Predictor` = longname1
      , `Neighbor` = longname2
      , `Correlation` = cor
    ) %>%
    arrange(-Correlation)
}

print(kable(neighbor_table(1), digits = 2, caption = 'Most Correlated Predictor'))
print(kable(neighbor_table(2), digits = 2, caption = '2nd Most Correlated Predictor'))


# Dendrogram --------------------------------------------------------------

# make dendrogram based on correlation matrix
dendro = as.dist(1-cmat) %>%
  hclust()

scale = 2
pdf('temp/dendrogram.pdf', height = 15*scale, width = 2.5*scale)
par(mar = c(2, 0.5, 3, 8), cex = 0.5)
plot(as.dendrogram(dendro), horiz = TRUE
     , main = 'Correlation Clusters\nin the Chen-Zimmermann Data')
dev.off()
