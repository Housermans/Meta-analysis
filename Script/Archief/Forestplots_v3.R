#load packages
library("metadat")
library("meta")
library("readxl")
library(dplyr)

rm(list=ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# script.dir <- dirname(sys.frame(1)$ofile) 
home_dir <- dirname(script_dir)
input <- file.path(home_dir, "Input")
output <- file.path(home_dir, "Output")
plot <- file.path(home_dir, "Plots")

db <- read_excel(file.path(output, "filtered_db_230601.xlsx"))

#variable for number evaluable patients (CRC or all cancer types)
db$N_eval_eff_total <- ifelse(db$CRC_specific == "1", db$N_eva_eff_crc, 
                          ifelse(db$CRC_specific == "0", db$N_eva_eff, NA))

db$N_eval_eff_total
db <- db %>% filter(!is.na(db$N_eval_eff_total))

db_p1 <- db %>% filter(Phase == "1")
db_p1 <- db_p1 %>% filter(!is.na(db_p1$N_pat_safety))
db_p2 <- db %>% filter(Phase == "2")
db_p2 <- db_p2 %>% filter(!is.na(db_p2$N_pat_safety))

# Eigen dataset --------------------------
# db$N_SD_shortlong <- ifelse(is.na(db$N_SD_short), db$N_SD_long, db$N_SD_short)
db$events_CBR <- db$N_SD_14 + db$N_PR + db$N_CR
db$events_ORR <- db$N_PR + db$N_CR

events_CBR_all <- db$events_CBR
db_events_CBR <- db %>% filter(!is.na(db$events_CBR))
events_CBR <- db_events_CBR$events_CBR
patients_events_CBR <- db_events_CBR$N_eval_eff_total
studlab_events_CBR <- paste(db_events_CBR$Aut_1, db_events_CBR$Year_published_online, sep = ", ")
drugtype_CBR <- db_events_CBR$DrugType

events_ORR <- db$events_ORR
drugtype <- db$DrugType

db_year <- db %>% filter(!is.na(db$year_group))
year <- db_year$year_group
patients_year <- db_year$N_eval_eff_total
events_CBR_year <- db_year$events_CBR
studlab_year <- paste(db_year$Aut_1, db_year$Year_published_online, sep = ", ")
patients <- db$N_eval_eff_total
patients_safety <- db$N_pat_safety

patients_safety_p1 <- db_p1$N_pat_safety
patients_safety_p2 <- db_p2$N_pat_safety
studlab <- paste(db$Aut_1, db$Year_published_online, sep = ", ")
DLT <- db_p1$N_DLT
disc <- db_p2$N_Disc
studlab_p1 <- paste(db_p1$Aut_1, db_p1$Year_published_online, sep = ", ")
studlab_p2 <- paste(db_p2$Aut_1, db_p2$Year_published_online, sep = ", ")
drugtype_p1 <- db_p1$DrugType
drugtype_p2 <- db_p2$DrugType

#plots----------------------
#plot CBR
CBR <- metaprop(events_CBR, patients_events_CBR, studlab_events_CBR, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5))
forest_CBR <- forest(CBR)
rstudioapi::savePlotAsImage(file.path(plot, "CBR.png"), width = 800, height = 2200)

#plot ORR
ORR <- metaprop(events_ORR, patients, studlab, sm="PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method="Inverse", control=list(stepadj=0.5))
forest_ORR <- forest(ORR)
rstudioapi::savePlotAsImage(file.path(plot, "ORR.png"), width = 800, height = 3200)

#plot DLT toxicity (phas 1)
DLT_plot <- metaprop(DLT, patients_safety_p1, studlab_p1, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, 
                method = "Inverse", control=list(stepadj=0.5))
forest_DLT <- forest(DLT_plot)
rstudioapi::savePlotAsImage(file.path(plot, "DLT.png"), width = 800, height = 2000)

#plot discontinuation toxicity (phase 2)
disc_plot <- metaprop(disc, patients_safety_p2, studlab_p2, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5))
forest_disc <- forest(disc_plot)
rstudioapi::savePlotAsImage(file.path(plot, "disc.png"), width = 800, height = 1400)

#plot CBR subgroup year
CBR_year <- metaprop(events_CBR_year, patients_year, byvar = year, studlab_year, sm = "PLOGIT", 
                         comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                         prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_year <- forest(CBR_year)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_year.png"), width = 800, height = 4000)

library(metafor)
Estimate<-transf.ilogit(CBR_year$TE.random.w) #back transformation logit
Upper<-transf.ilogit(CBR_year$upper.random.w)
Lower<-transf.ilogit(CBR_year$lower.random.w)

df_estimate <- as.data.frame(Estimate)
df_upper <- as.data.frame(Upper)
df_lower <- as.data.frame(Lower)

df_estimate$drug <- row.names(df_estimate)
df_upper$drug <- row.names(df_upper)
df_lower$drug <- row.names(df_lower)

merge1 <- merge(df_estimate, df_upper)
merge2 <- merge(merge1, df_lower)
merge2$index <- 1:5
merge2

library(ggplot2)
library(ggpubr)

plot_year <- ggplot(data=merge2, aes(y=index, x=Estimate, xmin=Lower, xmax=Upper)) +
  labs(x = "Effect size (95%-CI)")+
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(merge2), labels=merge2$drug) +
  geom_vline(xintercept=0.33, color='black', linetype='dashed', alpha=.5) +
  theme_classic()+
  theme(
    axis.line.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.title.y  = element_blank(),
    plot.title = element_text(hjust =0.5)
  )
plot_year

t1 <- ggplot(data=merge2) +
  geom_text(aes(y=drug, x=1, label= paste0(round(Estimate, digits=2))), vjust=0) +
  #Add a line above graph
  geom_hline(yintercept=11.6, size=2) + 
  labs(title='', x='Effect Size', y = 'Drug type') +
  ggtitle("Proportion") +
  xlab("  ") +
  theme_classic(base_size=9) +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    #axis.text.y  = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.length=unit(0.3,"cm"),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(color="white"),
    plot.title = element_text(hjust =0.5)
  )
t1

t2 <- ggplot(data=merge2) +
  geom_text(aes(y=drug, x=1, label= paste0("(",round(Lower, digits=2),"; ", round(Upper, digits=2),")")), vjust=0) +
  #Add a line above graph
  geom_hline(yintercept=11.6, size=2) + 
  ggtitle("95%-CI") +
  theme_classic(base_size=9) +
  xlab("  ") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "white"),
        axis.text.y  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x = element_line(color = "white"),
        axis.ticks.length=unit(0.3,"cm"),
        axis.title.y  = element_blank(),
        axis.text.x = element_text(color="white"),
        plot.title = element_text(hjust =0.5)
  )
t2

library(cowplot)
library(grid)
#Put the individual components of the forest plot together
grid_year <- plot_grid(
  t1, t2, plot_year,
  rel_widths = c(3.5, 3, 4),
  ncol = 3,nrow=1)

grid_year

tiff(file.path(plot, "year_subanalysis.tiff"),
     units="in",
     width = (4),
     height = (2),
     res=300) # The height of the plot in inches
grid_year
dev.off()

#plot CBR subgroup
CBR_subgroup <- metaprop(events_CBR_all, patients, byvar = drugtype, studlab, sm = "PLOGIT", 
                comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_subrgoup <- forest(CBR_subgroup)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_subgroup.png"), width = 800, height = 4000)

CBR_subgroup_noNA <- metaprop(events_CBR, patients_events_CBR, byvar = drugtype_CBR, studlab_events_CBR, sm = "PLOGIT", 
                         comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                         prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_subrgoup <- forest(CBR_subgroup_noNA)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_subgroup_noNA.png"), width = 800, height = 3000)

Estimate<-transf.ilogit(CBR_subgroup$TE.random.w) #back transformation logit
Upper<-transf.ilogit(CBR_subgroup$upper.random.w)
Lower<-transf.ilogit(CBR_subgroup$lower.random.w)

df_estimate <- as.data.frame(Estimate)
df_upper <- as.data.frame(Upper)
df_lower <- as.data.frame(Lower)
 
df_estimate$drug <- row.names(df_estimate)
df_upper$drug <- row.names(df_upper)
df_lower$drug <- row.names(df_lower)

merge1 <- merge(df_estimate, df_upper)
merge2 <- merge(merge1, df_lower)
merge2$index <- 1:11
merge2

#https://rgraphs.com/high-quality-forest-plots-in-r-ggplot2/

plot_drugs <- ggplot(data=merge2, aes(y=index, x=Estimate, xmin=Lower, xmax=Upper)) +
  labs(x = "Effect size (95%-CI)")+
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(merge2), labels=merge2$drug) +
  geom_vline(xintercept=0.33, color='black', linetype='dashed', alpha=.5) +
  theme_classic()+
  theme(
    axis.line.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.title.y  = element_blank(),
    plot.title = element_text(hjust =0.5)
  )
plot_drugs

t1 <- ggplot(data=merge2) +
  geom_text(aes(y=drug, x=1, label= paste0(round(Estimate, digits=2))), vjust=0) +
  #Add a line above graph
  geom_hline(yintercept=11.6, size=2) + 
  labs(title='', x='Effect Size', y = 'Drug type') +
  ggtitle("Proportion") +
  xlab("  ") +
  theme_classic(base_size=9) +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    #axis.text.y  = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.length=unit(0.3,"cm"),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(color="white"),
    plot.title = element_text(hjust =0.5)
  )
t1

t2 <- ggplot(data=merge2) +
  geom_text(aes(y=drug, x=1, label= paste0("(",round(Lower, digits=2),"; ", round(Upper, digits=2),")")), vjust=0) +
  #Add a line above graph
  geom_hline(yintercept=11.6, size=2) + 
  ggtitle("95%-CI") +
  theme_classic(base_size=9) +
  xlab("  ") +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "white"),
        axis.text.y  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.ticks.x = element_line(color = "white"),
        axis.ticks.length=unit(0.3,"cm"),
        axis.title.y  = element_blank(),
        axis.text.x = element_text(color="white"),
        plot.title = element_text(hjust =0.5)
  )
t2

#Put the individual components of the forest plot together
grid_drugs <- plot_grid(
  t1, t2, plot_drugs,
  rel_widths = c(3, 2, 4),
  ncol = 3,nrow=1)

grid_drugs

tiff(file.path(plot, "drugs_subanalysis.tiff"),
     units="in",
     width = (6),
     height = (4),
     res=300) # The height of the plot in inches
grid_drugs
dev.off()

#plot ORR subgroup
ORR_subgroup <- metaprop(events_ORR, patients, byvar = drugtype, studlab, sm = "PLOGIT", 
                              comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                              prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_ORR_subrgoup <- forest(ORR_subgroup)
rstudioapi::savePlotAsImage(file.path(plot, "ORR_subgroup.png"), width = 800, height = 5000)

#plot DLT toxicity (phase 1) subgroup
DLT_subgroup <- metaprop(DLT, patients_safety_p1, byvar = drugtype_p1, studlab_p1, sm = "PLOGIT", 
                         comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                         prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_DLT_subgroup <- forest(DLT_subgroup)
rstudioapi::savePlotAsImage(file.path(plot, "DLT_subgroup.png"), width = 800, height = 3000)

#plot discontinuation toxicity (phase 2) subgroup
disc_subgroup <- metaprop(disc, patients_safety_p2, byvar = drugtype_p2, studlab_p2, sm = "PLOGIT", 
                         comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                         prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_disc_subgroup <- forest(disc_subgroup)
rstudioapi::savePlotAsImage(file.path(plot, "disc_subgroup.png"), width = 800, height = 3000)

