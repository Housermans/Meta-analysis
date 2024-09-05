#Meta-analysis and forest plots
#1 
library(dplyr)
library(readxl)
library(meta)
library(ggplot2)
library(tidyverse)
library(gridtext)

setwd("C:/Meta-analysis R")

#Uploaden dataset from excel:
dataset <- read_excel("dataset_MA.xlsx")
View(dataset)

#Make Year, Phase and Drug class a factor:
dataset$Publication <- factor(dataset$Publication) 
dataset$Stage <- factor(dataset$Stage)
dataset$Drug <- factor(dataset$Drug, levels = c("IMiD", "PI", "mAb", "Cell therapy",
                                                "ADC", "ICI", "Kinase inhibitor", "Hsp90i",
                                                "Other"))
dataset$Years <- fct_collapse(dataset$Publication, "2010-2012" =
                                c("2010", "2011", "2012"),
                              "2013-2015" = c("2013", "2014", "2015"),                                                        "2016-2018" = c("2016", "2017", "2018"),
                              "2019-2020" = c("2019", "2020"))

#Make different sets for Phase I and Phase II:
PhaseI <- dataset %>% filter(Stage %in% c("Phase I"))
PhaseII <- dataset %>% filter(Stage %in% c("Phase II"))

View(PhaseI)

#Analysis for Phase I:
MA_I <- metaprop(event = PR, n = N,
                       data = PhaseI,
                       method = "Inverse",
                       sm = gs("smprop"),
                       incr = gs("incr"),
                       method.ci = gs("method.ci"),
                       level = gs("level"),
                       level.comb = gs("level.comb"),
                       comb.random = gs("comb.random"),
                       hakn = gs("hakn"),
                       adhoc.hakn = gs("adhoc.hakn"),
                       method.tau = "DL",
                       method.tau.ci = "J",
                       tau.preset = NULL,
                       TE.tau = NULL,
                       tau.common = gs("tau.common"),
                       prediction = gs("prediction"),
                       level.predict = gs("level.predict"),
                       null.effect = NA,
                       method.bias = gs("method.bias"),
                       backtransf = gs("backtransf"),
                       pscale = 100,
                       title = gs("Meta-analysis"),
                       complab = gs("complab"),
                       outclab = "",
                       keepdata = gs("keepdata"),
                       warn = gs("warn"),
                       control = NULL)
MA_I


#Make forestplot for Phase:

tiff("MA_ORR1.tiff",width=800,height=1200)
forest(MA_I, xlim = c(-19,110),
       comb.fixed = FALSE,
       comb.random = TRUE, 
       study.results = TRUE, 
       subgroup = FALSE,
       prediction = TRUE,
       type.random = "diamond",
       type.study = "square",
       weight.study = "random",
       #squaresize = 0.5,
       resid.hetstat = FALSE,
       print.tau2 = FALSE,
       studlab = PhaseI$Authors,
       #text.random = " ",
       col.square = "black", 
       col.diamond = "red", 
       sortvar = Publication,
       col.diamond.lines = "black", 
       zero.pval = FALSE,
       leftlabs = c('Study', 'Events', 'Total'),
       rightlabs = c('Proportion', '95% CI', 'Weight'),
       just = "left",
       pscale = 100,
       pooled.events = TRUE,
       smlab = "Effect size",
       overall = TRUE,
       test.subgroup.random = FALSE,
       col.by = 'black',
       hetstat = FALSE,
       overall.hetstat = TRUE,
       print.Q.subgroup = FALSE,
       
)
dev.off()




#Analysis for Phase II:

MA_II <- metaprop(event = PR, n = N,
                 data = PhaseII,
                 method = "Inverse",
                 sm = gs("smprop"),
                 incr = gs("incr"),
                 method.ci = gs("method.ci"),
                 level = gs("level"),
                 level.comb = gs("level.comb"),
                 comb.random = gs("comb.random"),
                 hakn = gs("hakn"),
                 adhoc.hakn = gs("adhoc.hakn"),
                 method.tau = "DL",
                 method.tau.ci = "J",
                 tau.preset = NULL,
                 TE.tau = NULL,
                 tau.common = gs("tau.common"),
                 prediction = gs("prediction"),
                 level.predict = gs("level.predict"),
                 null.effect = NA,
                 method.bias = gs("method.bias"),
                 backtransf = gs("backtransf"),
                 pscale = 100,
                 title = gs("Meta-analysis"),
                 complab = gs("complab"),
                 outclab = "",
                 keepdata = gs("keepdata"),
                 warn = gs("warn"),
                 control = NULL)
MA_II

#Make forestplot for Phase II:

tiff("MA_ORR2.tiff",width=800,height=1200)
forest(MA_II, xlim = c(-19,110),
       comb.fixed = FALSE,
       comb.random = TRUE, 
       study.results = TRUE, 
       subgroup = FALSE,
       prediction = TRUE,
       type.random = "diamond",
       type.study = "square",
       weight.study = "random",
       #squaresize = 0.5,
       resid.hetstat = FALSE,
       print.tau2 = FALSE,
       studlab = PhaseII$Authors,
       #text.random = " ",
       col.square = "black", 
       col.diamond = "red", 
       sortvar = Publication,
       col.diamond.lines = "black", 
       zero.pval = FALSE,
       leftlabs = c('Study', 'Events', 'Total'),
       rightlabs = c('Proportion', '95% CI', 'Weight'),
       just = "left",
       pscale = 100,
       pooled.events = TRUE,
       smlab = "Effect size",
       overall = TRUE,
       test.subgroup.random = FALSE,
       col.by = 'black',
       hetstat = FALSE,
       overall.hetstat = TRUE,
       print.Q.subgroup = FALSE,
       
)
dev.off()






-------------------------------------------------------------------------------------------------------------------
  
#Analysis and plots for MR (Minimal Response):
  
  
MR_I <- metaprop(event = MR, n = N,
                       data = PhaseI,
                       method = "Inverse",
                       sm = gs("smprop"),
                       incr = gs("incr"),
                       allincr = gs("allincr"),
                       addincr = gs("addincr"),
                       method.ci = gs("method.ci"),
                       level = gs("level"),
                       level.comb = gs("level.comb"),
                       comb.random = gs("comb.random"),
                       hakn = gs("hakn"),
                       adhoc.hakn = gs("adhoc.hakn"),
                       method.tau = "DL",
                       method.tau.ci = "J",
                       tau.preset = NULL,
                       TE.tau = NULL,
                       tau.common = gs("tau.common"),
                       prediction = gs("prediction"),
                       level.predict = gs("level.predict"),
                       null.effect = NA,
                       method.bias = gs("method.bias"),
                       backtransf = gs("backtransf"),
                       pscale = 100,
                       title = gs("Meta-analysis"),
                       complab = gs("complab"),
                       outclab = "",
                       keepdata = gs("keepdata"),
                       warn = gs("warn"),
                       control = NULL)
MR_I

tiff("MA_MR1.tiff",width=800,height=1200)
forest(MR_I, xlim = c(-19,110),
       comb.fixed = FALSE,
       comb.random = TRUE, 
       study.results = TRUE, 
       subgroup = TRUE,
       prediction = TRUE,
       type.random = "diamond",
       resid.hetstat = FALSE,
       print.tau2 = FALSE,
       studlab = PhaseI$Authors,
       text.random = "Random effects model",
       col.square = "black", 
       col.diamond = "red", 
       sortvar = Publication,
       col.diamond.lines = "black", 
       zero.pval = FALSE,
       leftlabs = c('Study', 'Events', 'Total'),
       rightlabs = c('Proportion', '95% CI', 'Weight'),
       just = "center",
       pscale = 100,
       pooled.events = TRUE,
       smlab = "Effect size",
)
dev.off()




MR_II <- metaprop(event = MR, n = N,
                 data = PhaseII,
                 method = "Inverse",
                 sm = gs("smprop"),
                 incr = gs("incr"),
                 allincr = gs("allincr"),
                 addincr = gs("addincr"),
                 method.ci = gs("method.ci"),
                 level = gs("level"),
                 level.comb = gs("level.comb"),
                 comb.random = gs("comb.random"),
                 hakn = gs("hakn"),
                 adhoc.hakn = gs("adhoc.hakn"),
                 method.tau = "DL",
                 method.tau.ci = "J",
                 tau.preset = NULL,
                 TE.tau = NULL,
                 tau.common = gs("tau.common"),
                 prediction = gs("prediction"),
                 level.predict = gs("level.predict"),
                 null.effect = NA,
                 method.bias = gs("method.bias"),
                 backtransf = gs("backtransf"),
                 pscale = 100,
                 title = gs("Meta-analysis"),
                 complab = gs("complab"),
                 outclab = "",
                 keepdata = gs("keepdata"),
                 warn = gs("warn"),
                 control = NULL)
MR_II

tiff("MA_MR2.tiff",width=800,height=1200)
forest(MR_II, xlim = c(-19,110),
       comb.fixed = FALSE,
       comb.random = TRUE, 
       study.results = TRUE, 
       subgroup = TRUE,
       prediction = TRUE,
       type.random = "diamond",
       resid.hetstat = FALSE,
       print.tau2 = FALSE,
       studlab = PhaseII$Authors,
       text.random = "Random effects model",
       col.square = "black", 
       col.diamond = "red", 
       sortvar = Publication,
       col.diamond.lines = "black", 
       zero.pval = FALSE,
       leftlabs = c('Study', 'Events', 'Total'),
       rightlabs = c('Proportion', '95% CI', 'Weight'),
       just = "center",
       pscale = 100,
       pooled.events = TRUE,
       smlab = "Effect size",
)
dev.off()




---------------------------------------------------------------------------------------------
  #Subgroup analysis
  
MA_I_years <- metaprop(event = PR, n = N,
                          data = PhaseI,
                          method = "Inverse",
                          sm = gs("smprop"),
                          incr = gs("incr"),
                          method.ci = gs("method.ci"),
                          level = gs("level"),
                          level.comb = gs("level.comb"),
                          comb.random = gs("comb.random"),
                          hakn = gs("hakn"),
                          adhoc.hakn = gs("adhoc.hakn"),
                          method.tau = "DL",
                          method.tau.ci = "J",
                          tau.preset = NULL,
                          TE.tau = NULL,
                          tau.common = gs("tau.common"),
                          prediction = gs("prediction"),
                          level.predict = gs("level.predict"),
                          null.effect = NA,
                          method.bias = gs("method.bias"),
                          backtransf = gs("backtransf"),
                          pscale = 100,
                          title = gs("Meta-analysis"),
                          complab = gs("complab"),
                          outclab = "",
                          keepdata = gs("keepdata"),
                          warn = gs("warn"),
                          byvar = Years,
                          control = NULL)
MA_I_years

tiff("MA_ORR1_years.tiff",width=700,height=750)
forest(MA_I_years, xlim = c(-19,110),
       comb.fixed = FALSE,
       comb.random = TRUE, 
       study.results = FALSE, 
       subgroup = TRUE,
       prediction = FALSE,
       type.random = "diamond",
       resid.hetstat = FALSE,
       print.tau2 = FALSE,
       #studlab = PhaseI$Authors,
       text.random = " ",
       col.square = "black", 
       col.diamond = "red", 
       sortvar = Publication,
       col.diamond.lines = "black", 
       zero.pval = FALSE,
       leftlabs = c('Subgroup', 'Proportion', '95%-CI'),
       leftcols = c("studlab", "effect", "ci"),
       rightcols = FALSE, 
       just = "left",
       pscale = 100,
       pooled.events = TRUE,
       smlab = "Effect size",
       overall = FALSE,
       test.subgroup.random = TRUE,
       col.by = 'black',
       layout = 'JAMA',
       weight.subgroup = "weight",
       hetstat = FALSE,
       overall.hetstat = FALSE,
       print.Q.subgroup = FALSE,
       colgap.left = unit(10, 'mm'),
       print.subgroup.labels = TRUE,
       print.byvar = FALSE
       
)
dev.off()


MA_I_drugs <- metaprop(event = PR, n = N,
                       data = PhaseI,
                       method = "Inverse",
                       sm = gs("smprop"),
                       incr = gs("incr"),
                       method.ci = gs("method.ci"),
                       level = gs("level"),
                       level.comb = gs("level.comb"),
                       comb.random = gs("comb.random"),
                       hakn = gs("hakn"),
                       adhoc.hakn = gs("adhoc.hakn"),
                       method.tau = "DL",
                       method.tau.ci = "J",
                       tau.preset = NULL,
                       TE.tau = NULL,
                       tau.common = gs("tau.common"),
                       prediction = gs("prediction"),
                       level.predict = gs("level.predict"),
                       null.effect = NA,
                       method.bias = gs("method.bias"),
                       backtransf = gs("backtransf"),
                       pscale = 100,
                       title = gs("Meta-analysis"),
                       complab = gs("complab"),
                       outclab = "",
                       keepdata = gs("keepdata"),
                       warn = gs("warn"),
                       byvar = Drug,
                       control = NULL)
MA_I_drugs  

tiff("MA_ORR1_drugs.tiff",width=700,height=750)
forest(MA_I_drugs, xlim = c(-19,110),
       comb.fixed = FALSE,
       comb.random = TRUE, 
       study.results = FALSE, 
       subgroup = TRUE,
       prediction = FALSE,
       type.random = "diamond",
       resid.hetstat = FALSE,
       print.tau2 = FALSE,
       #studlab = PhaseI$Authors,
       text.random = " ",
       col.square = "black", 
       col.diamond = "lightblue", 
       sortvar = Publication,
       col.diamond.lines = "black", 
       zero.pval = FALSE,
       leftlabs = c('Subgroup', 'Proportion', '95%-CI'),
       leftcols = c("studlab", "effect", "ci"),
       rightcols = FALSE, 
       just = "left",
       pscale = 100,
       pooled.events = TRUE,
       smlab = "Effect size",
       overall = FALSE,
       test.subgroup.random = TRUE,
       col.by = 'black',
       layout = 'JAMA',
       weight.subgroup = "weight",
       hetstat = FALSE,
       overall.hetstat = FALSE,
       print.Q.subgroup = FALSE,
       colgap.left = unit(10, 'mm'),
       print.subgroup.labels = TRUE,
       print.byvar = FALSE
)
dev.off()





--------------------------------------------------------------------------------
  #Subgroup analysis phase II
  
  
MA_II_years <- metaprop(event = PR, n = N,
                         data = PhaseII,
                         method = "Inverse",
                         sm = gs("smprop"),
                         incr = gs("incr"),
                         method.ci = gs("method.ci"),
                         level = gs("level"),
                         level.comb = gs("level.comb"),
                         comb.random = gs("comb.random"),
                         hakn = gs("hakn"),
                         adhoc.hakn = gs("adhoc.hakn"),
                         method.tau = "DL",
                         method.tau.ci = "J",
                         tau.preset = NULL,
                         TE.tau = NULL,
                         tau.common = gs("tau.common"),
                         prediction = gs("prediction"),
                         level.predict = gs("level.predict"),
                         null.effect = NA,
                         method.bias = gs("method.bias"),
                         backtransf = gs("backtransf"),
                         pscale = 100,
                         title = gs("Meta-analysis"),
                         complab = gs("complab"),
                         outclab = "",
                         keepdata = gs("keepdata"),
                         warn = gs("warn"),
                         byvar = Years,
                         control = NULL)
MA_II_years  

tiff("MA_ORR2_years.tiff",width=700,height=750)
forest(MA_II_years, xlim = c(-19,110),
       comb.fixed = FALSE,
       comb.random = TRUE, 
       study.results = FALSE, 
       subgroup = TRUE,
       prediction = FALSE,
       type.random = "diamond",
       resid.hetstat = FALSE,
       print.tau2 = FALSE,
       #studlab = PhaseII$Authors,
       text.random = " ",
       col.square = "black", 
       col.diamond = "red", 
       sortvar = Publication,
       col.diamond.lines = "black", 
       zero.pval = FALSE,
       leftlabs = c('Subgroup', 'Proportion', '95%-CI'),
       leftcols = c("studlab", "effect", "ci"),
       rightcols = FALSE, 
       just = "left",
       pscale = 100,
       pooled.events = TRUE,
       smlab = "Effect size",
       overall = FALSE,
       test.subgroup.random = TRUE,
       col.by = 'black',
       layout = 'JAMA',
       weight.subgroup = "weight",
       hetstat = FALSE,
       overall.hetstat = FALSE,
       print.Q.subgroup = FALSE,
       colgap.left = unit(10, 'mm'),
       print.subgroup.labels = TRUE,
       print.byvar = FALSE
)
dev.off()


MA_II_drugs <- metaprop(event = PR, n = N,
                       data = PhaseII,
                       method = "Inverse",
                       sm = gs("smprop"),
                       incr = gs("incr"),
                       method.ci = gs("method.ci"),
                       level = gs("level"),
                       level.comb = gs("level.comb"),
                       comb.random = gs("comb.random"),
                       hakn = gs("hakn"),
                       adhoc.hakn = gs("adhoc.hakn"),
                       method.tau = "DL",
                       method.tau.ci = "J",
                       tau.preset = NULL,
                       TE.tau = NULL,
                       tau.common = gs("tau.common"),
                       prediction = gs("prediction"),
                       level.predict = gs("level.predict"),
                       null.effect = NA,
                       method.bias = gs("method.bias"),
                       backtransf = gs("backtransf"),
                       pscale = 100,
                       title = gs("Meta-analysis"),
                       complab = gs("complab"),
                       outclab = "",
                       keepdata = gs("keepdata"),
                       warn = gs("warn"),
                       byvar = Drug,
                       control = NULL)
MA_II_drugs  

tiff("MA_ORR2_drugs.tiff",width=700,height=750)
forest(MA_II_drugs, xlim = c(-19,110),
       comb.fixed = FALSE,
       comb.random = TRUE, 
       study.results = FALSE, 
       subgroup = TRUE,
       prediction = FALSE,
       type.random = "diamond",
       resid.hetstat = FALSE,
       print.tau2 = FALSE,
       #studlab = PhaseII$Authors,
       text.random = " ",
       col.square = "black", 
       col.diamond = "red", 
       sortvar = Publication,
       col.diamond.lines = "black", 
       zero.pval = FALSE,
       leftlabs = c('Subgroup', 'Proportion', '95%-CI'),
       leftcols = c("studlab", "effect", "ci"),
       rightcols = FALSE, 
       just = "left",
       pscale = 100,
       pooled.events = TRUE,
       smlab = "Effect size",
       overall = FALSE,
       test.subgroup.random = TRUE,
       col.by = 'black',
       layout = 'JAMA',
       weight.subgroup = "weight",
       hetstat = FALSE,
       overall.hetstat = FALSE,
       print.Q.subgroup = FALSE,
       colgap.left = unit(10, 'mm'),
       print.subgroup.labels = TRUE,
       print.byvar = FALSE
)
dev.off()


  

