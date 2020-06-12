################################################################################
##################### Couplings Mental Health & Cognition ######################
##################################### ULGM #####################################         
################################################################################

rm(list=ls()) # clear all

### Load packages
library(lavaan)

### Load data - Can be requested from https://www.nichd.nih.gov/research/supported/seccyd
data_seccyd = readRDS("data_seccyd_transf.rda")

################################################################################
### LGM Cognition - Maths
model_LGM_cog_quant <-
  '
i_cog =~ 1*g1_wjr_quant + 1*g3_wjr_quant + 1*g5_wjr_quant + 1*g9_wjr_quant
s_cog =~ 0*g1_wjr_quant +   g3_wjr_quant +   g5_wjr_quant + 1*g9_wjr_quant
'
fit_LGM_cog_quant <- growth(model_LGM_cog_quant, data=data_seccyd, missing='fiml', estimator='mlr')
fitMeasures(fit_LGM_cog_quant, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_LGM_cog_quant, standardized=T, rsquare=T)

################################################################################
### LGM Cognition - Vocabulary
model_LGM_cog_vocab <-
  '
i_cog    =~ 1*g1_wjr_vocab + 1*g3_wjr_vocab + 1*g5_wjr_vocab + 1*g9_wjr_vocab
s_cog    =~ 0*g1_wjr_vocab +   g3_wjr_vocab +   g5_wjr_vocab + 1*g9_wjr_vocab
'
fit_LGM_cog_vocab <- growth(model_LGM_cog_vocab, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_LGM_cog_vocab, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_LGM_cog_vocab, standardized=T, rsquare=T)

################################################################################
### LGM Cognition - Planning
model_LGM_cog_toh <-
  '
i_cog =~ 1*g1_toh_plan + 1*g3_toh_plan + 1*g5_toh_plan
s_cog =~ 0*g1_toh_plan +   g3_toh_plan + 1*g5_toh_plan

g1_toh_plan ~~ e_toh*g1_toh_plan
g3_toh_plan ~~ e_toh*g3_toh_plan
g5_toh_plan ~~ e_toh*g5_toh_plan
'
fit_LGM_cog_toh <- growth(model_LGM_cog_toh, data=data_seccyd, missing='fiml', estimator='mlr')
fitMeasures(fit_LGM_cog_toh , fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_LGM_cog_toh, standardized=T, rsquare=T)

################################################################################
### LGM Wellbeing - Externalizing
model_LGM_men_ext <-
  '
i_men    =~ 1*g1_cbcl_ext + 1*g3_cbcl_ext + 1*g5_cbcl_ext + 1*g9_cbcl_ext
s_men    =~ 0*g1_cbcl_ext +   g3_cbcl_ext +   g5_cbcl_ext + 1*g9_cbcl_ext
'
fit_LGM_men_ext <- growth(model_LGM_men_ext, data=data_seccyd, missing='fiml', estimator='mlr')
fitMeasures(fit_LGM_men_ext, fit.measures=c("cfi","rmsea","srmr"))
summary(fit_LGM_men_ext, standardized=T, rsquare=T)

################################################################################
### LGM Wellbeing - Internalizing
model_LGM_men_int <-
  '
i_men    =~ 1*g1_cbcl_int + 1*g3_cbcl_int + 1*g5_cbcl_int + 1*g9_cbcl_int
s_men    =~ 0*g1_cbcl_int + 2*g3_cbcl_int + 4*g5_cbcl_int + 8*g9_cbcl_int

g1_cbcl_int ~~ e_men*g1_cbcl_int
g3_cbcl_int ~~ e_men*g3_cbcl_int
g5_cbcl_int ~~ e_men*g5_cbcl_int
g9_cbcl_int ~~ e_men*g9_cbcl_int
'
fit_LGM_men_int <- growth(model_LGM_men_int, data=data_seccyd, missing='fiml', estimator='mlr')
fitMeasures(fit_LGM_men_int, fit.measures=c("cfi","rmsea","srmr"))
summary(fit_LGM_men_int, standardized=T, rsquare=T)

################################################################################
### LGM Wellbeing - Loneliness

model_LGM_men_lonly <-
  '
i_men    =~ 1*g1_lsdq_lonly + 1*g3_lsdq_lonly + 1*g5_lsdq_lonly + 1*g9_lsdq_lonly
s_men    =~ 0*g1_lsdq_lonly + 2*g3_lsdq_lonly + 4*g5_lsdq_lonly + 8*g9_lsdq_lonly

g3_lsdq_lonly ~~ g5_lsdq_lonly
'
fit_LGM_men_lonly <- growth(model_LGM_men_lonly, data=data_seccyd, missing='fiml', estimator='mlr')
fitMeasures(fit_LGM_men_lonly, fit.measures=c("cfi","rmsea","srmr"))
summary(fit_LGM_men_lonly, standardized=T, rsquare=T)

###############################################################################
