################################################################################
##################### Couplings Mental Health & Cognition ######################
##################################### BLGM #####################################         
################################################################################

rm(list=ls()) # clear all

### Load packages
library(lavaan)

### Load data - Can be requested from https://www.nichd.nih.gov/research/supported/seccyd
data_seccyd = readRDS("data_seccyd_transf.rda")

################################################################################
### BLGM Vocabulary & Externalizing
model_BLGM_vocab_ext <-
  '
i_cog    =~ 1*g1_wjr_vocab + 1*g3_wjr_vocab + 1*g5_wjr_vocab + 1*g9_wjr_vocab
s_cog    =~ 0*g1_wjr_vocab +   g3_wjr_vocab +   g5_wjr_vocab + 1*g9_wjr_vocab

i_men    =~ 1*g1_cbcl_ext + 1*g3_cbcl_ext + 1*g5_cbcl_ext + 1*g9_cbcl_ext
s_men    =~ 0*g1_cbcl_ext +   g3_cbcl_ext +   g5_cbcl_ext + 1*g9_cbcl_ext

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_cbcl_ext ~~ res1*g1_wjr_vocab
g3_cbcl_ext ~~ res3*g3_wjr_vocab
g5_cbcl_ext ~~ res5*g5_wjr_vocab
g9_cbcl_ext ~~ res9*g9_wjr_vocab
'
fit_BLGM_vocab_ext <- growth(model_BLGM_vocab_ext, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_vocab_ext, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_BLGM_vocab_ext, standardized=T, rsquare=T)

################################################################################
### BLGM Vocabulary & Internalizing
model_BLGM_vocab_int <-
  '
i_cog    =~ 1*g1_wjr_vocab + 1*g3_wjr_vocab + 1*g5_wjr_vocab + 1*g9_wjr_vocab
s_cog    =~ 0*g1_wjr_vocab +   g3_wjr_vocab +   g5_wjr_vocab + 1*g9_wjr_vocab

i_men    =~ 1*g1_cbcl_int + 1*g3_cbcl_int + 1*g5_cbcl_int + 1*g9_cbcl_int
s_men    =~ 0*g1_cbcl_int + 2*g3_cbcl_int + 4*g5_cbcl_int + 8*g9_cbcl_int

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_cbcl_int ~~ res*g1_wjr_vocab
g3_cbcl_int ~~ res*g3_wjr_vocab
g5_cbcl_int ~~ res*g5_wjr_vocab
g9_cbcl_int ~~ res*g9_wjr_vocab
'
fit_BLGM_vocab_int <- growth(model_BLGM_vocab_int, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_vocab_int, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_BLGM_vocab_int, standardized=T, rsquare=T)

###############################################################################
### BLGM Maths & Externalizing
model_BLGM_quant_ext <-
  '
i_cog    =~ 1*g1_wjr_quant + 1*g3_wjr_quant + 1*g5_wjr_quant + 1*g9_wjr_quant
s_cog    =~ 0*g1_wjr_quant +   g3_wjr_quant +   g5_wjr_quant + 1*g9_wjr_quant

i_men    =~ 1*g1_cbcl_ext + 1*g3_cbcl_ext + 1*g5_cbcl_ext + 1*g9_cbcl_ext
s_men    =~ 0*g1_cbcl_ext +   g3_cbcl_ext +   g5_cbcl_ext + 1*g9_cbcl_ext

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_cbcl_ext ~~ res*g1_wjr_quant
g3_cbcl_ext ~~ res*g3_wjr_quant
g5_cbcl_ext ~~ res*g5_wjr_quant
g9_cbcl_ext ~~ res*g9_wjr_quant

g1_wjr_quant ~~ e_cog*g1_wjr_quant
g3_wjr_quant ~~ e_cog*g3_wjr_quant
g5_wjr_quant ~~ e_cog*g5_wjr_quant
g9_wjr_quant ~~ e_cog*g9_wjr_quant
'
fit_BLGM_quant_ext <- growth(model_BLGM_quant_ext, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_quant_ext, fit.measures=c("cfi","rmsea","srmr"))
summary(fit_BLGM_quant_ext, standardized=T, rsquare=T)

################################################################################
### BLGM Maths & Internalizing
model_BLGM_quant_int <-
  '
i_cog    =~ 1*g1_wjr_quant + 1*g3_wjr_quant + 1*g5_wjr_quant + 1*g9_wjr_quant
s_cog    =~ 0*g1_wjr_quant +   g3_wjr_quant +   g5_wjr_quant + 1*g9_wjr_quant

i_men    =~ 1*g1_cbcl_int + 1*g3_cbcl_int + 1*g5_cbcl_int + 1*g9_cbcl_int
s_men    =~ 0*g1_cbcl_int + 2*g3_cbcl_int + 4*g5_cbcl_int + 8*g9_cbcl_int

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_cbcl_int ~~ res*g1_wjr_quant
g3_cbcl_int ~~ res*g3_wjr_quant
g5_cbcl_int ~~ res*g5_wjr_quant
g9_cbcl_int ~~ res*g9_wjr_quant

g1_wjr_quant ~~ e_cog*g1_wjr_quant
g3_wjr_quant ~~ e_cog*g3_wjr_quant
g5_wjr_quant ~~ e_cog*g5_wjr_quant
g9_wjr_quant ~~ e_cog*g9_wjr_quant
'
fit_BLGM_quant_int <- growth(model_BLGM_quant_int, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_quant_int, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_BLGM_quant_int, standardized=T, rsquare=T)

###############################################################################
### BLGM Planning & Externalizing
model_BLGM_toh_ext <-
  '
i_cog    =~ 1*g1_toh_plan + 1*g3_toh_plan + 1*g5_toh_plan
s_cog    =~ 0*g1_toh_plan +   g3_toh_plan + 1*g5_toh_plan

i_men    =~ 1*g1_cbcl_ext + 1*g3_cbcl_ext + 1*g5_cbcl_ext + 1*g9_cbcl_ext
s_men    =~ 0*g1_cbcl_ext +   g3_cbcl_ext +   g5_cbcl_ext + 1*g9_cbcl_ext

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_cbcl_ext ~~ res*g1_toh_plan
g3_cbcl_ext ~~ res*g3_toh_plan
g5_cbcl_ext ~~ res*g5_toh_plan
'
fit_BLGM_toh_ext <- growth(model_BLGM_toh_ext, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_toh_ext, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_BLGM_toh_ext, standardized=T, rsquare=T, fit.measures=T)

###############################################################################
### BLGM Planning & Internalizing
model_BLGM_toh_int <-
  '
i_cog    =~ 1*g1_toh_plan + 1*g3_toh_plan + 1*g5_toh_plan
s_cog    =~ 0*g1_toh_plan +   g3_toh_plan + 1*g5_toh_plan

i_men    =~ 1*g1_cbcl_int + 1*g3_cbcl_int + 1*g5_cbcl_int + 1*g9_cbcl_int
s_men    =~ 0*g1_cbcl_int + 2*g3_cbcl_int + 4*g5_cbcl_int + 8*g9_cbcl_int

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_cbcl_int ~~ res*g1_toh_plan
g3_cbcl_int ~~ res*g3_toh_plan
g5_cbcl_int ~~ res*g5_toh_plan
'
fit_BLGM_toh_int <- growth(model_BLGM_toh_int, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_toh_int, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_BLGM_toh_int, standardized=T, rsquare=T)

###############################################################################
### BLGM Vocabulary & Loneliness
model_BLGM_vocab_lonly <-
  '
i_cog    =~ 1*g1_wjr_vocab + 1*g3_wjr_vocab + 1*g5_wjr_vocab + 1*g9_wjr_vocab
s_cog    =~ 0*g1_wjr_vocab +   g3_wjr_vocab +   g5_wjr_vocab + 1*g9_wjr_vocab

i_men    =~ 1*g1_lsdq_lonly + 1*g3_lsdq_lonly + 1*g5_lsdq_lonly + 1*g9_lsdq_lonly
s_men    =~ 0*g1_lsdq_lonly +   g3_lsdq_lonly +   g5_lsdq_lonly + 1*g9_lsdq_lonly

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_lsdq_lonly ~~ res*g1_wjr_vocab
g3_lsdq_lonly ~~ res*g3_wjr_vocab
g5_lsdq_lonly ~~ res*g5_wjr_vocab
g9_lsdq_lonly ~~ res*g9_wjr_vocab
'
fit_BLGM_vocab_lonly <- growth(model_BLGM_vocab_lonly, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_vocab_lonly, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_BLGM_vocab_lonly, standardized=T, rsquare=T, fit.measures=T)

###############################################################################
### BLGM Maths & Loneliness
model_BLGM_quant_lonly <-
  '
i_cog    =~ 1*g1_wjr_quant + 1*g3_wjr_quant + 1*g5_wjr_quant + 1*g9_wjr_quant
s_cog    =~ 0*g1_wjr_quant +   g3_wjr_quant +   g5_wjr_quant + 1*g9_wjr_quant

i_men    =~ 1*g1_lsdq_lonly + 1*g3_lsdq_lonly + 1*g5_lsdq_lonly + 1*g9_lsdq_lonly
s_men    =~ 0*g1_lsdq_lonly +   g3_lsdq_lonly +   g5_lsdq_lonly + 1*g9_lsdq_lonly

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_lsdq_lonly ~~ res1*g1_wjr_quant
g3_lsdq_lonly ~~ res3*g3_wjr_quant
g5_lsdq_lonly ~~ res5*g5_wjr_quant
g9_lsdq_lonly ~~ res9*g9_wjr_quant

g1_wjr_quant ~~ e_cog*g1_wjr_quant
g3_wjr_quant ~~ e_cog*g3_wjr_quant
g5_wjr_quant ~~ e_cog*g5_wjr_quant
g9_wjr_quant ~~ e_cog*g9_wjr_quant
'
fit_BLGM_quant_lonly <- growth(model_BLGM_quant_lonly, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_quant_lonly, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_BLGM_quant_lonly, standardized=T, rsquare=T, fit.measures=T)

###############################################################################
### BLGM Cognition - toh & lonly
model_BLGM_toh_lonly <-
  '
i_cog    =~ 1*g1_toh_plan + 1*g3_toh_plan + 1*g5_toh_plan
s_cog    =~ 0*g1_toh_plan +   g3_toh_plan + 1*g5_toh_plan

i_men    =~ 1*g1_lsdq_lonly + 1*g3_lsdq_lonly + 1*g5_lsdq_lonly + 1*g9_lsdq_lonly
s_men    =~ 0*g1_lsdq_lonly +   g3_lsdq_lonly +   g5_lsdq_lonly + 1*g9_lsdq_lonly

s_men ~ i_cog
s_cog ~ i_men

i_men ~~ i_cog
s_men ~~ s_cog

g1_lsdq_lonly ~~ res*g1_toh_plan
g3_lsdq_lonly ~~ res*g3_toh_plan
g5_lsdq_lonly ~~ res*g5_toh_plan

g3_lsdq_lonly ~~ g5_lsdq_lonly # see resid(fit_LGM_men_lonly , type="cor")
'
fit_BLGM_toh_lonly <- growth(model_BLGM_toh_lonly, data=data_seccyd, missing='fiml', estimator='mlr') 
fitMeasures(fit_BLGM_toh_lonly, fit.measures=c("cfi","rmsea","srmr")) 
summary(fit_BLGM_toh_lonly, standardized=T, rsquare=T)

###############################################################################
