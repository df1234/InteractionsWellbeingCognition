################################################################################
##################### Couplings Mental Health & Cognition ######################
################################ Data Summary ##################################           
################################################################################

rm(list=ls()) # clear all

### Load packages
library(ggplot2)
library(reshape)
library(dplyr)
library(viridis)

### Load data - Can be requested from https://www.nichd.nih.gov/research/supported/seccyd
data_seccyd = readRDS("data_seccyd_transf.rda")

################################################################################
### Spahetti Plot

### Get data into shape
# Maths
quant_sub    = data_seccyd[,grep("*_quant", names(data_seccyd), value=T)]
quant_sub$ID = data_seccyd$ID
names(quant_sub) = c("wave 1\nage 6-7","wave 2\nage 8-9","wave 3\nage 10-11","wave 4\nage 15","ID")
quant_plot = melt(quant_sub, "ID")
quant_plot$measure = "Maths"

# Vocab
vvocab_sub    = data_seccyd[,grep("*_vocab", names(data_seccyd), value=T)]
vocab_sub$ID = data_seccyd$ID
names(vocab_sub) = c("wave 1\nage 6-7","wave 2\nage 8-9","wave 3\nage 10-11","wave 4\nage 15","ID")
vocab_plot = melt(vocab_sub, "ID")
vocab_plot$measure = "Vocabulary"

# Planning
toh_sub    = data_seccyd[,grep("*_toh", names(data_seccyd), value=T)]
toh_sub$ID = data_seccyd$ID
names(toh_sub) = c("wave 1\nage 6-7","wave 2\nage 8-9","wave 3\nage 10-11","wave 4\nage 15","ID")
toh_plot = melt(toh_sub, "ID")
toh_plot$measure = "Planning"

# Internalizing
int_sub    = data_seccyd[,grep("*_int", names(data_seccyd), value=T)]
int_sub$ID = data_seccyd$ID
names(int_sub) = c("wave 1\nage 6-7","wave 2\nage 8-9","wave 3\nage 10-11","wave 4\nage 15","ID")
int_plot = melt(int_sub, "ID")
int_plot$measure = "Internalizing"

# Externalizing
ext_sub    = data_seccyd[,grep("*_ext", names(data_seccyd), value=T)]
ext_sub$ID = data_seccyd$ID
names(ext_sub) = c("wave 1\nage 6-7","wave 2\nage 8-9","wave 3\nage 10-11","wave 4\nage 15","ID")
levels(ext_sub)  = gsub(":","\n",levels(ext_sub))
ext_plot = melt(ext_sub, "ID")
ext_plot$measure = "Externalizing"

# Loneliness
lonly_sub    = data_seccyd[,grep("*_lonly", names(data_seccyd), value=T)]
lonly_sub$ID = data_seccyd$ID
names(lonly_sub) = c("wave 1\nage 6-7","wave 2\nage 8-9","wave 3\nage 10-11","wave 4\nage 15","ID")
lonly_plot = melt(lonly_sub, "ID")
lonly_plot$measure = "Loneliness"

### Put it all together
plot_data = rbind(quant_plot,vocab_plot,toh_plot,int_plot,ext_plot,lonly_plot)
plot_data$measure = as.factor(plot_data$measure)

### Plot
ggplot(data = plot_data, aes(x=variable, y=value)) + 
  geom_point(position= "jitter", aes(group = ID), size=1, alpha=.1) + 
  geom_line (position= "jitter", aes(group = ID), alpha=.1) +
  stat_smooth(aes(group = 1, colour=measure), method="loess", se=F, size=2) +
  facet_wrap(~ measure, scales = "free_y", ncol=2) +
  theme(legend.position="none",text = element_text(size=16)) +
  labs(x = "Wave", y = "Score (percentage of maximum possible score)")+
  scale_colour_viridis(discrete=T, option="C")
ggsave('seccyd_spaghetti.tiff', height=12, width=9)
