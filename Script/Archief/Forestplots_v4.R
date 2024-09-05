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

db <- read_excel(file.path(output, "filtered_db_230706.xlsx"))

#variable for number evaluable patients (CRC or all cancer types)
db$N_eval_eff_total <- ifelse(db$CRC_specific == "1", db$N_eva_eff_crc, 
                          ifelse(db$CRC_specific == "0", db$N_eva_eff, NA))

db <- db %>% filter(!is.na(db$N_eval_eff_total))

db <- db %>%
  mutate(PS_0_1 = PS_0 + PS_1,
         PS_ratio = PS_2/ (PS_2+PS_0_1))

db$Med_age_cat <- cut(
  as.numeric(db$Med_age),
  breaks = c(0, 55, 60, 65, Inf),
  labels = c("<55", "56-60","61-65", ">65"),
  include.lowest = TRUE
) # make age categories based on verdeling

# Eigen dataset --------------------------
db$N_total_check <- db$N_PD + db$N_SD_short + db$N_PR+ db$N_CR
db_N_check <- db$N_total_check == db$N_eval_eff_total
db_N_check <- cbind(db$Aut_1, db$Year_published_online, db_N_check) # check N totaal = PD + SD + PR + CR
db_N_check_false <- db_N_check %>% filter(db_N_check == FALSE)

# db$N_SD_shortlong <- ifelse(is.na(db$N_SD_short), db$N_SD_long, db$N_SD_short)
db$events_CBR <- db$N_SD_14 + db$N_PR + db$N_CR
db$events_ORR <- db$N_PR + db$N_CR

#make additional databases for sub-analysis
db$safetytimes5 <- db$N_pat_safety*5
db$studlab <- paste(db$Aut_1, db$Year_published_online, sep = ", ")
db_top5 <- db %>% filter(!is.na(db$safetytimes5))
db_top5 <- db_top5 %>% filter(!is.na(db_top5$`N_events_top5_Gr3+`))
db_gr3 <- db %>% filter(!is.na(db$N_pat_safety))
db_gr3 <- db_gr3 %>% filter(!is.na(db_gr3$`N_pat_GR3+`))
db_events_CBR <- db %>% filter(!is.na(db$events_CBR))
db_year <- db %>% filter(!is.na(db$year_group))
db_year_events_CBR <- db_year %>% filter(!is.na(db_year$events_CBR))
db_year_events_ORR <- db_year %>% filter(!is.na(db_year$events_ORR))
db_p1 <- db %>% filter(Phase == "1")
db_p1 <- db_p1 %>% filter(!is.na(db_p1$N_pat_safety))
db_p1_DLT <- db_p1 %>% filter(!is.na(db_p1$N_DLT))
db_p2 <- db %>% filter(Phase == "2")
db_p2 <- db_p2 %>% filter(!is.na(db_p2$N_pat_safety))
db_p2_disc <- db_p2 %>% filter(!is.na(db_p2$N_Disc))
db_p1_top5 <- db_p1 %>% filter(!is.na(db_p1$`N_events_top5_Gr3+`))
db_p2_top5 <- db_p2 %>% filter(!is.na(db_p2$`N_events_top5_Gr3+`))

#categories median prior lines
db_events_CBR$Med_lines <- as.numeric(db_events_CBR$Med_lines)  
#mean(db_events_CBR$Med_lines, na.rm = TRUE)  #3.5 mean   
db_events_CBR$Med_lines_35 <- ifelse(db_events_CBR$Med_lines <3.5, "< 3.5", ">= 3.5")
db_events_CBR_med_lines_35 <- db_events_CBR %>% filter(!is.na(db_events_CBR$Med_lines_35))
db_events_CBR_lineslow <- db_events_CBR %>% filter(Med_lines < 3.5)
db_events_CBR_lineshigh <- db_events_CBR %>% filter(Med_lines >= 3.5)

#categories PS 
db_events_CBR$PS_ratio10 <- ifelse(db_events_CBR$PS_ratio <0.1, "< 10% PS 2", ">= 10% PS 2")
db_events_CBR_PS_ratio10 <- db_events_CBR %>% filter(!is.na(db_events_CBR$PS_ratio10))

#categories median age
db_events_CBR_med_age_cat <- db_events_CBR %>% filter(!is.na(db_events_CBR$Med_age_cat))

#plots----------------------
#plot CBR
CBR <- metaprop(db_events_CBR$events_CBR, db_events_CBR$N_eval_eff_total, db_events_CBR$studlab, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5))
forest_CBR <- forest(CBR)
rstudioapi::savePlotAsImage(file.path(plot, "CBR.png"), width = 800, height = 3100)

#plot ORR
ORR <- metaprop(db$events_ORR, db$N_eval_eff_total, db$studlab, sm="PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method="Inverse", control=list(stepadj=0.5))
forest_ORR <- forest(ORR)
rstudioapi::savePlotAsImage(file.path(plot, "ORR.png"), width = 800, height = 4500)

#plot DLT toxicity (phase 1)
DLT_plot <- metaprop(db_p1$N_DLT, db_p1$N_pat_safety, db_p1$studlab, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, 
                method = "Inverse", control=list(stepadj=0.5))
forest_DLT <- forest(DLT_plot)
rstudioapi::savePlotAsImage(file.path(plot, "DLT.png"), width = 800, height = 3000)

#plot discontinuation toxicity (phase 2)
disc_plot <- metaprop(db_p2_disc$N_Disc, db_p2_disc$N_pat_safety, db_p2_disc$studlab, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5))
forest_disc <- forest(disc_plot)
rstudioapi::savePlotAsImage(file.path(plot, "disc.png"), width = 800, height = 1200)

#plot top 5 toxicity 
top5_plot <- metaprop(db_top5$`N_events_top5_Gr3+`, db_top5$safetytimes5, db_top5$studlab, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5))
forest_top5 <- forest(top5_plot)
rstudioapi::savePlotAsImage(file.path(plot, "top5.png"), width = 800, height = 4000)

#plot top 5 toxicity (phase 1)
top5_phase1_plot <- metaprop(db_p1_top5$`N_events_top5_Gr3+`, db_p1_top5$safetytimes5, db_p1_top5$studlab, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5))
forest_top5_phase1 <- forest(top5_phase1_plot)
rstudioapi::savePlotAsImage(file.path(plot, "top5_phase1.png"), width = 800, height = 4000)

#plot top 5 toxicity (phase 2)
top5_phase2_plot <- metaprop(db_p2_top5$`N_events_top5_Gr3+`, db_p2_top5$safetytimes5, db_p2_top5$studlab, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5))
forest_top5_phase2 <- forest(top5_phase2_plot)
rstudioapi::savePlotAsImage(file.path(plot, "top5_phase2.png"), width = 800, height = 4000)

#plot patients grade 3+ 
grade3_plot <- metaprop(db_gr3$`N_pat_GR3+`, db_gr3$N_pat_safety, byvar = db_gr3$Phase, db_gr3$studlab, sm = "PLOGIT", comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5))
forest_grade3 <- forest(grade3_plot)
rstudioapi::savePlotAsImage(file.path(plot, "grade3.png"), width = 800, height = 2400)

#plot ORR subgroup year
ORR_year <- metaprop(db_year_events_ORR$events_ORR, db_year_events_ORR$N_eval_eff_total, byvar = db_year_events_ORR$year_group, db_year_events_ORR$studlab, sm = "PLOGIT", 
                     comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                     prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_ORR_year <- forest(ORR_year)
rstudioapi::savePlotAsImage(file.path(plot, "ORR_year.png"), width = 800, height = 3500)

library(metafor)
Estimate<-transf.ilogit(ORR_year$TE.random.w) #back transformation logit
Upper<-transf.ilogit(ORR_year$upper.random.w)
Lower<-transf.ilogit(ORR_year$lower.random.w)
#RUN HIERNA VANAF PUNT Y OM EEN SUMMARY PLOT TE MAKEN EN DENK AAN AANPASSEN NAAM OM OP TE SLAAN

#plot CBR subgroup year
CBR_year <- metaprop(db_year_events_CBR$events_CBR, db_year_events_CBR$N_eval_eff_total, byvar = db_year_events_CBR$year_group, db_year_events_CBR$studlab, sm = "PLOGIT", 
                         comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                         prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_year <- forest(CBR_year)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_year.png"), width = 800, height = 3500)

library(metafor)
Estimate<-transf.ilogit(CBR_year$TE.random.w) #back transformation logit
Upper<-transf.ilogit(CBR_year$upper.random.w)
Lower<-transf.ilogit(CBR_year$lower.random.w)

#PUNT Y
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

library(dplyr)
merge <- merge2 %>% arrange(index)

plot_year <- ggplot(data=merge2, aes(y=-index, x=Estimate, xmin=Lower, xmax=Upper)) +
  labs(x = "Effect size (95%-CI)")+
  ggtitle("") +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(merge2), labels=merge2$drug) +
  geom_vline(xintercept=0.06, color='black', linetype='dashed', alpha=.5) +
  theme_classic(base_size = 9)+
  theme(
    axis.line.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.title.y  = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(7, 1, 19, 1, unit = "pt")   
  ) +
  scale_x_continuous(breaks = seq(0.1,  0.6, by = 0.1),
                     labels = scales::number_format(accuracy = 0.01))  

plot_year

t1 <- ggplot(data = merge2) +
  geom_text(aes(y = reorder(drug, -index), x = 1, label = sprintf("%0.2f", Estimate)), vjust = 0) +
  geom_hline(yintercept = 11.6, size = 2) +
  labs(title = '', x = 'Effect Size', y = 'Drug type') +
  ggtitle("Proportion") +
  xlab("  ") +
  theme_classic(base_size = 9) +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "white"),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(vjust = -.2)
  )

t1


t2 <- ggplot(data = merge2) +
  geom_text(aes(y = reorder(drug, -index), x = 1, label = sprintf("(%0.2f; %0.2f)", Lower, Upper)), vjust = 0) +
  geom_hline(yintercept = 11.6, size = 2) +
  ggtitle("95%-CI") +
  theme_classic(base_size = 9) +
  xlab("  ") +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "white"),
    plot.title = element_text(hjust = 0.5)
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

tiff(file.path(plot, "year_subanalysis.tiff"), #CHANGE NAME OF PLOT
     units="in",
     width = (4),
     height = (2),
     res=300) # The height of the plot in inches
grid_year
dev.off()

#plot CBR subgroup
CBR_subgroup <- metaprop(db$events_CBR, db$N_eval_eff_total, byvar = db$DrugType2, db$studlab, sm = "PLOGIT", 
                comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_subrgoup <- forest(CBR_subgroup)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_subgroup.png"), width = 800, height = 5500)

CBR_subgroup_noNA <- metaprop(db_events_CBR$events_CBR, db_events_CBR$N_eval_eff_total, byvar = db_events_CBR$DrugType2, db_events_CBR$studlab, sm = "PLOGIT", 
                         comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                         prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_subrgoup <- forest(CBR_subgroup_noNA)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_subgroup_noNA.png"), width = 800, height = 4000)

Estimate<-transf.ilogit(CBR_subgroup_noNA$TE.random.w) #back transformation logit
Upper<-transf.ilogit(CBR_subgroup_noNA$upper.random.w)
Lower<-transf.ilogit(CBR_subgroup_noNA$lower.random.w)

df_estimate <- as.data.frame(Estimate)
df_upper <- as.data.frame(Upper)
df_lower <- as.data.frame(Lower)
 
df_estimate$drug <- row.names(df_estimate)
df_upper$drug <- row.names(df_upper)
df_lower$drug <- row.names(df_lower)

merge1 <- merge(df_estimate, df_upper)
merge2 <- merge(merge1, df_lower)
merge2$index <- rank(merge2$Estimate) #chemo immunotherapy only has NAs in events! THerefore 8 not 9 

#https://rgraphs.com/high-quality-forest-plots-in-r-ggplot2/

View(merge2)

plot_drugs <- ggplot(data=merge2, aes(y=index, x=Estimate, xmin=Lower, xmax=Upper)) +
  labs(x = "Effect size (95%-CI)")+
  ggtitle("") +
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
    plot.title = element_text(hjust =0.5),
    plot.margin = margin(2, 1, 12, 1, unit = "pt")) +
  scale_x_continuous(breaks = seq(0.1,  0.6, by = 0.1),
                     labels = scales::number_format(accuracy = 0.01))   

  
plot_drugs

t1 <- ggplot(data=merge2) +
  geom_text(aes(y=reorder(drug, index), x=1, label= paste0(round(Estimate, digits=2))), vjust=0) +
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
  geom_text(aes(y=reorder(drug, index), x=1, label= paste0("(",round(Lower, digits=2),"; ", round(Upper, digits=2),")")), vjust=0) +
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
     height = (2.7),
     res=300) # The height of the plot in inches
grid_drugs
dev.off()

#plot ORR subgroup
ORR_subgroup <- metaprop(db$events_ORR, db_year$studlab, byvar = db$DrugType2, db$studlab, sm = "PLOGIT", 
                              comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                              prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_ORR_subrgoup <- forest(ORR_subgroup)
rstudioapi::savePlotAsImage(file.path(plot, "ORR_subgroup.png"), width = 800, height = 5000)

#plot DLT toxicity (phase 1) subgroup
DLT_subgroup <- metaprop(db_p1$N_DLT, db_p1$N_pat_safety, byvar = db_p1$DrugType2, db_p1$studlab, sm = "PLOGIT", 
                         comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                         prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_DLT_subgroup <- forest(DLT_subgroup)
rstudioapi::savePlotAsImage(file.path(plot, "DLT_subgroup.png"), width = 800, height = 6000)

#plot discontinuation toxicity (phase 2) subgroup
disc_subgroup <- metaprop(db_p2$N_Disc, db_p2$N_pat_safety, byvar = db_p2$DrugType2, db_p2$studlab, sm = "PLOGIT", 
                         comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                         prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_disc_subgroup <- forest(disc_subgroup)
rstudioapi::savePlotAsImage(file.path(plot, "disc_subgroup.png"), width = 800, height = 6000)

#plot CBR subgroup PS
CBR_PS <- metaprop(db_events_CBR_PS_ratio10$events_CBR, db_events_CBR_PS_ratio10$N_eval_eff_total, byvar = db_events_CBR_PS_ratio10$PS_ratio10, db_events_CBR_PS_ratio10$studlab, sm = "PLOGIT", 
                      comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                      prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_PS <- forest(CBR_PS)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_PS.png"), width = 800, height = 3500)

Estimate<-transf.ilogit(CBR_PS$TE.random.w) #back transformation logit
Upper<-transf.ilogit(CBR_PS$upper.random.w)
Lower<-transf.ilogit(CBR_PS$lower.random.w)
#RUN HIERNA VANAF PUNT X OM EEN SUMMARY PLOT TE MAKEN EN DENK AAN AANPASSEN NAAM OM OP TE SLAAN

#plot CBR subgroup age median 60
CBR_age <- metaprop(db_events_CBR_med_age_cat$events_CBR, db_events_CBR_med_age_cat$N_eval_eff_total, byvar = db_events_CBR_med_age_cat$Med_age_cat, db_events_CBR_med_age_cat$studlab, sm = "PLOGIT", 
                   comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                   prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_age <- forest(CBR_age)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_age.png"), width = 800, height = 3500)

Estimate<-transf.ilogit(CBR_age$TE.random.w) #back transformation logit
Upper<-transf.ilogit(CBR_age$upper.random.w)
Lower<-transf.ilogit(CBR_age$lower.random.w)
#RUN HIERNA VANAF PUNT X OM EEN SUMMARY PLOT TE MAKEN EN DENK AAN AANPASSEN NAAM OM OP TE SLAAN

#plot CBR subgroup lines
CBR_lines <- metaprop(db_events_CBR_med_lines_35$events_CBR, db_events_CBR_med_lines_35$N_eval_eff_total, byvar = db_events_CBR_med_lines_35$Med_lines_35, db_events_CBR_med_lines_35$studlab, sm = "PLOGIT", 
                     comb.fixed=FALSE, comb.random=TRUE, method = "Inverse", control=list(stepadj=0.5),
                     prediction.subgroup=TRUE, test.subgroup=TRUE, overall = FALSE, overall.hetstat = FALSE)
forest_CBR_lines <- forest(CBR_lines)
rstudioapi::savePlotAsImage(file.path(plot, "CBR_lines.png"), width = 800, height = 3500)

Estimate<-transf.ilogit(CBR_lines$TE.random.w) #back transformation logit
Upper<-transf.ilogit(CBR_lines$upper.random.w)
Lower<-transf.ilogit(CBR_lines$lower.random.w)

#DIT IS PUNT X
df_estimate <- as.data.frame(Estimate)
df_upper <- as.data.frame(Upper)
df_lower <- as.data.frame(Lower)

df_estimate$drug <- row.names(df_estimate)
df_upper$drug <- row.names(df_upper)
df_lower$drug <- row.names(df_lower)

merge1 <- merge(df_estimate, df_upper)
merge2 <- merge(merge1, df_lower)
merge2$index <- c(1, 4, 2, 3)
merge2
merge <- merge2 %>% arrange(index)

plot_estimates <- ggplot(data=merge2, aes(y=-index, x=Estimate, xmin=Lower, xmax=Upper)) +
  labs(x = "Effect size (95%-CI)")+
  ggtitle("") +
  geom_point() + 
  geom_errorbarh(height=.1) +
  scale_y_continuous(breaks=1:nrow(merge2), labels=merge2$drug) +
  geom_vline(xintercept=0.33, color='black', linetype='dashed', alpha=.5) +
  theme_classic(base_size = 9)+
  theme(
    axis.line.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.title.y  = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(7, 1, 19, 1, unit = "pt")   
  ) +
  scale_x_continuous(breaks = seq(0.1,  0.6, by = 0.1),
                     labels = scales::number_format(accuracy = 0.01))  

plot_estimates

t1 <- ggplot(data = merge2) +
  geom_text(aes(y = reorder(drug, -index), x = 1, label = sprintf("%0.2f", Estimate)), vjust = 0) +
  geom_hline(yintercept = 11.6, size = 2) +
  labs(title = '', x = 'Effect Size', y = 'Drug type') +
  ggtitle("Proportion") +
  xlab("  ") +
  theme_classic(base_size = 9) +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "white"),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(vjust = -.2)
  )

t1


t2 <- ggplot(data = merge2) +
  geom_text(aes(y = reorder(drug, -index), x = 1, label = sprintf("(%0.2f; %0.2f)", Lower, Upper)), vjust = 0) +
  geom_hline(yintercept = 11.6, size = 2) +
  ggtitle("95%-CI") +
  theme_classic(base_size = 9) +
  xlab("  ") +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "white"),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "white"),
    plot.title = element_text(hjust = 0.5)
  )

t2

#Put the individual components of the forest plot together
grid_lines <- plot_grid(
  t1, t2, plot_estimates,
  rel_widths = c(3.5, 3, 4),
  ncol = 3,nrow=1)
 
grid_lines 

tiff(file.path(plot, "age_subanalysis.tiff"), #NAAM AANPASSEN AAN SUBANALYSE
     units="in",
     width = (4),
     height = (1.5),
     res=300) # The height of the plot in inches
grid_lines
dev.off()

