library(readxl)
library(ggplot2)
library(dplyr)
library(writexl)
library(meta)
library(tidyr)
library(patchwork)

rm(list=ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# script.dir <- dirname(sys.frame(1)$ofile) 
home_dir <- dirname(script_dir)
input <- file.path(home_dir, "Input")
output <- file.path(home_dir, "Output")
plot <- file.path(home_dir, "Plots")
plot_d <- file.path(plot, "Descriptive")

d <- read_excel(file.path(input, "databaseV230706draft.xlsx"))

histogramm <- function(df, descriptive, bins=30, title=paste("Histogram of",descriptive)) {
  ggplot(df, aes(x=get(descriptive))) +
    geom_histogram(color = "black", fill = "blue", bins=bins) +
      labs(title = title,
                x = descriptive,
                y = "Count") +
      theme_classic() + 
      xlab("")
}

barplot <- function(df, descriptive, title=descriptive, numeric=F) {
  df <- df %>% arrange(get(descriptive))
  if (numeric == TRUE) {
    ggplot(df, aes(x=get(descriptive))) +
      geom_bar(color = "black", aes(fill=factor(get(descriptive)))) +
      geom_text(aes(label = after_stat(count)), stat = "count",  vjust = -0.5) + 
      theme_classic() +
      theme(axis.text.x= element_text(angle=45, vjust=1, hjust=1), legend.position = "none") +
      ggtitle(title) +
      xlab("") +
      labs(fill = descriptive)
  } else {
    ggplot(df, aes(x=get(descriptive))) +
      geom_bar(color = "black", aes(fill="#e377c2")) +
      geom_text(aes(label = after_stat(count)), stat = "count",  vjust = -0.5) + 
      theme_classic() +
      theme(axis.text.x= element_text(angle=45, vjust=1, hjust=1), legend.position = "none") +
      ggtitle(title) +
      xlab("") +
      labs(fill = descriptive)
  }
}

pieplot <- function(df, descriptive) { 
  ggplot(df, aes(x=factor(1), fill = get(descriptive))) + 
    geom_bar(width = 1) + coord_polar("y") + theme_classic() + 
    geom_text(aes(x=1.7, label = after_stat(count)), stat = "count", vjust = -0.5) + 
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position = "none") +
    xlab("")
  }

summary(d)
var_list <-  lapply(d, table) # geeft het aantal keren dat elke waarde voorkomt in elke kolom van d

d_included <- d[is.na(d$Flag_problems), ]
d_string_complete <- d_included

d_na_nr <- filter(d, if_any(everything(), ~ .x %in% c("NA", "NR"))) # select only the rows that have NA or NR in any column
n_na_nr <- sapply(d_na_nr, function(x) subset(table(x), names(table(x)) %in% c("NA", "NR"))) # gives the number of times that NA or NR occur in each column of d_na_nr
names_vec <- names(n_na_nr)[which(sapply(n_na_nr, length) > 0)] # gives a vector of the names of the elements that have a length greater than 0

d <- mutate_at(d, vars(names_vec), na_if, "NA") # replace "NA" strings with NA
d <- mutate_at(d, vars(names_vec), na_if, "NR") # replace "NR" strings with NA

d_included <- d[is.na(d$Flag_problems), ]
d_complete <- d_included[d_included$Gevuld > 0.8,] # alleen artikelen die afgemaakt zijn 
d_removed <- d_included[d_included$Gevuld < 0.79,]

change_to_numeric <- c("PFS_med")
d_complete <- mutate_at(d_complete, vars(all_of(change_to_numeric)), as.numeric)
d_complete <- mutate_at(d_complete, vars(starts_with("N_")), as.numeric) #remove text fields
d_complete <- mutate_at(d_complete, vars(starts_with("T_")), as.numeric) #remove text fields 
d_complete <- mutate_at(d_complete, vars(starts_with("PS_")), as.numeric) #remove text fields 

d_complete <- d_complete %>% mutate(
                              CRC_total_ratio = N_inc_crc/N_inc,
                              CBR_short = (N_SD_short + N_PR + N_CR) / N_eva_eff,
                              CBR = (N_SD_14 + N_PR + N_CR) / N_eva_eff,
                              CBR_CRC_short = (N_SD_short + N_PR + N_CR) / N_eva_eff_crc,
                              CBR_CRC = (N_SD_14 + N_PR + N_CR) / N_eva_eff_crc,
                              ORR = (N_PR + N_CR) / N_eva_eff,
                              ORR_CRC = (N_PR + N_CR) / N_eva_eff_crc,
                              CBR_all = (pmax(N_SD_short, N_SD_14, N_SD_long, na.rm=TRUE) + N_PR + N_CR) / N_eva_eff,
                              CBR_CRC_all = (pmax(N_SD_short, N_SD_14, N_SD_long, na.rm=TRUE) + N_PR + N_CR) / N_eva_eff_crc, 
                              events_CBR = pmax(N_SD_short, N_SD_14, N_SD_long, na.rm=TRUE) + N_PR + N_CR, 
                              events_ORR = N_PR + N_CR,
                              year_group = ifelse(Year_published_online < 2013, "2010-2012", 
                                            ifelse( Year_published_online < 2015, "2013-2014", 
                                            ifelse(Year_published_online < 2017, "2015-2016", 
                                            ifelse(Year_published_online < 2019, "2017-2018", 
                                            ifelse(Year_published_online > 2019, "2019-2021", NA))))))

d <- d %>% mutate(include = ifelse(Gevuld > 0.86, "Included", "Excluded"))


d_complete <- d_complete %>% 
  mutate(mono_combi = ifelse(substr(ID, 1, 1) == "D", "Monotherapy", "Combination"))

unique(d_complete$DrugType2)
d_complete$DrugType2 <- ifelse(d_complete$DrugType == "Kinase Inhibitor", "Receptor or signal transduction",
                               ifelse(d_complete$DrugType == "Antibody-based", "Receptor or signal transduction",
                                      ifelse(d_complete$DrugType == "Monoclonal antibody", "Receptor or signal transduction",
                                             ifelse(d_complete$DrugType == "Multi Targeted", "Receptor or signal transduction",
                                                    ifelse(d_complete$DrugType == "Chemo immunotherapy", "Chemo Immunotherapy",
                                                           ifelse(d_complete$DrugType == "Antibody", "Receptor or signal transduction",
                                                                  ifelse(d_complete$DrugType == "Small Molecule", "Receptor or signal transduction",
                                                                         d_complete$DrugType)))))))


# top 5 event calc
d_top5 <- d_complete %>%
  mutate(
    safety_pop5 = N_pat_safety*5,
    Ratio_event_pat = `N_events_top5_Gr3+` / safety_pop5
  )

histogramm(d_top5, "Ratio_event_pat") 

write_xlsx(d_top5, file.path(output, "filtered_db_230706.xlsx"))
# write.xlsx(d_complete, file.path(output, "filtered_db.xlsx"))


missing <-  d_complete %>% summarise_all(~sum(is.na(.)))
not_missing <- d_complete %>% summarise_all(~sum(!is.na(.)))
nrow(d_complete)

d_complete_1 <- d_complete %>% filter(Phase == 1)
d_complete_2 <- d_complete %>% filter(Phase == 2)
d_complete$CRC_specific <- factor(d_complete$CRC_specific,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("No", "Yes", "Yes but not complete (waterfall plot)", "Yes but not complete (no SD_short)"))

d_string_complete <- d_complete

d_string_complete$N_SD_long_fac <-  as.factor(d_string_complete$N_SD_long)
d_string_complete$N_SD_long_fac <-  ifelse(is.na(d_string_complete$N_SD_long_fac), "Not Reported", "Reported") # relabel NA
d_string_complete$N_SD_long_fac <-  as.factor(d_string_complete$N_SD_long_fac)

convert <- c("N_PD", "N_SD_short", "N_SD_long", "N_SD_14", "N_PR","N_CR", "PFS_med", 
             "N_DLT", "N_Disc", "N_pat_GR3+","N_events_top5_Gr3+","N_Gr3+_cat", "N_events_Gr3+") #add N_eval_DLT?

add_level <- function(x, y) { factor(x, levels = c(levels(x), y)) }

d_string_complete2 <- d_string_complete %>% mutate(across(all_of(convert), ~ifelse(is.na(.), "Not Reported", "Reported")))
d_string_complete2 <- d_string_complete2 %>% mutate(across(all_of(convert), ~as.factor(.)))

efficacy_cols <- c("N_PD", "N_SD_short","N_SD_14", "N_SD_long", "N_PR", "N_PR", "N_CR", "PFS_med")
safety_cols1 <- c("N_DLT", "N_Disc")
safety_cols2 <- c("N_pat_GR3+","N_events_top5_Gr3+","N_Gr3+_cat", "N_events_Gr3+")

d_string_complete2 <- d_string_complete2 %>% mutate(across(all_of(efficacy_cols), ~factor(., levels=c("Not Reported", "Reported"))))

d_string_complete_efficacy <- d_string_complete2 %>% select(all_of(efficacy_cols)) 

d_long_efficacy <- pivot_longer(d_string_complete_efficacy, cols=all_of(efficacy_cols), names_to = "column", values_to = "level")
efficacy <- ggplot(d_long_efficacy, aes(x = column, fill = level)) + 
  geom_bar(position = "stack", color = "black") + theme_classic() + 
  theme(axis.text.x= element_text(angle=45, vjust=1, hjust=1, size=12)) + 
  ggtitle("Availability of efficacy metrics") +
  xlab("") +
  scale_x_discrete(labels = c("# CR", "# PD", "# PR", "# SD 14 wks","# SD long", "# SD best response", "# Median PFS")) + 
  scale_fill_manual(values = c("white", "#00A88E")) 
efficacy
#remove(list = c("d_string_complete_efficacy", "d_long_efficacy"))

d_string_complete_safety1 <- d_string_complete2 %>% select(all_of(safety_cols1), Phase) 
d_long_safety1 <- pivot_longer(d_string_complete_safety1, cols=all_of(safety_cols1), names_to = "column", values_to = "level")
d_long_safety1_phase1 <- d_long_safety1 %>% filter(Phase == 1)
d_long_safety1_phase2 <- d_long_safety1 %>% filter(Phase == 2)

safe1_phase1 <- ggplot(d_long_safety1_phase1, aes(x = column, fill = level)) + 
  geom_bar(position = "stack", color = "black") + theme_classic() + 
  theme(axis.text.x= element_text(angle=40, vjust=1, hjust=1, size=12), legend.position = "none") + 
  scale_x_discrete(labels = c("# patients drug discontinuation", "# patients DLT")) + 
  xlab("") + 
  labs(title = "Phase 1")+
  scale_fill_manual(values = c("white", "#00A88E")) 
#remove(list = c("d_string_complete_safety1", "d_long_safety1"))

safe1_phase2 <- ggplot(d_long_safety1_phase2, aes(x = column, fill = level)) + 
  geom_bar(position = "stack", color = "black") + theme_classic() + 
  theme(axis.text.x= element_text(angle=40, vjust=1, hjust=1, size=12), legend.position = "none") + 
  scale_x_discrete(labels = c("# patients drug discontinuation", "# patients DLT")) + 
  xlab("") + 
  labs(title = "Phase 2") +
  scale_fill_manual(values = c("white", "#00A88E")) 
#remove(list = c("d_string_complete_safety1", "d_long_safety1"))
safe1_phase2

d_string_complete_safety2 <- d_string_complete2 %>% select(all_of(safety_cols2)) 
d_long_safety2 <- pivot_longer(d_string_complete_safety2, cols=all_of(safety_cols2), names_to = "column", values_to = "level")
safe2 <- ggplot(d_long_safety2, aes(x = column, fill = level)) + 
  geom_bar(position = "stack", color = "black") + theme_classic() + 
  theme(axis.text.x= element_text(angle=40, vjust=1, hjust=1, size=12)) + 
  scale_x_discrete(labels = c("# events GR3+","# events GR3+ top 5 categories","# GR3+ categories", "# patients GR3+ AE")) + 
  xlab("") +
  labs(title = "Phase 1 & 2")+
  scale_fill_manual(values = c("white", "#00A88E")) 
remove(list = c("d_string_complete_safety2", "d_long_safety2"))

d_complete_filtered <- d_complete %>% 
  group_by(Journal) %>% 
  mutate(count = n(), Journal_new = if_else(count == 1, 'other', Journal)) %>% 
  select(-count)


p <- barplot(d_complete, "Phase", title="Number of studies per Phase")
ggsave(file.path(plot_d, "Phase.png"), plot=p, width=2000, height=2000, units="px")

#raw data plots
medage <- barplot(d_complete, "Med_age", title="Median age") #check verdeling age median
maxage <- barplot(d_complete, "Max_age", title="Maximum age") #check verdeling age maximum
mps <- barplot(d_complete, "Max_PS", title="Maximum Performance score")
medl <- barplot(d_complete, "Med_lines", title="Median of lines")
minl <- barplot(d_complete,"Min_lines", title="Minimum of lines")
maxl <- barplot(d_complete, "Max_lines", title="Maximum of lines")

#devide variables into nice categories
d_complete$Max_age_cat <- cut(
  as.numeric(d_complete$Max_age),
  breaks = c(0, 65, 70, 75, 80, 85, Inf),
  labels = c("<66","66-70", "71-75", "76-80", "81-85", ">85"),
  include.lowest = TRUE
) # make age categories based on verdeling

d_complete$Med_age_cat <- cut(
  as.numeric(d_complete$Med_age),
  breaks = c(0, 55, 60, 65, Inf),
  labels = c("<55", "56-60","61-65", ">65"),
  include.lowest = TRUE
) # make age categories based on verdeling

d_complete$Med_lines_cat <- cut(
  as.numeric(d_complete$Med_lines),
  breaks = c(0, 2.5, 3.5, 4.5, Inf),
  labels = c("<3", "3-3.5","4-4.5", "5+"),
  include.lowest = TRUE
) # make age categories based on verdeling

d_complete$Min_lines_cat <- cut(
  as.numeric(d_complete$Min_lines),
  breaks = c(0, 0.1, 1, 2, Inf),
  labels = c("0", "1","2", ">2"),
  include.lowest = TRUE
) # make age categories based on verdeling

d_complete$Max_lines_cat <- cut(
  as.numeric(d_complete$Max_lines),
  breaks = c(0, 5, 7, 9, 11, Inf),
  labels = c("<6", "6-7","8-9","10-11", ">11"),
  include.lowest = TRUE
) # make age categories based on verdeling

PS_right <- d_complete$N_inc == d_complete$PS_0 + d_complete$PS_1 + d_complete$PS_2 
d_checkPS <- cbind (PS_right, d_complete$Aut_1, d_complete$Journal)
d_checkPS <-as.data.frame(d_checkPS)
d_checkPS_false <- d_checkPS %>% filter(PS_right == FALSE)
View(d_checkPS_false) #moeten we nog checken

d_complete <- d_complete %>%
  mutate(PS_0_1 = PS_0 + PS_1,
         PS_ratio = PS_2/ (PS_2+PS_0_1))

d_complete$PS_ratio_cat <- cut(d_complete$PS_ratio,
                               breaks = c(0, 0.1, 0.2, Inf),
                               labels = c("<0.1","0.1-0.2",">0.2"),
                               include.lowest = TRUE)

psratio <- barplot(d_complete, "PS_ratio_cat", title="Performance score \nratio 2/0 or 1")
medagecat <- barplot(d_complete, "Med_age_cat", title="Median\n age") 
maxagecat <- barplot(d_complete, "Max_age_cat", title="Maximum\n age") 
medlcat <- barplot(d_complete, "Med_lines_cat", title="Median prior \ntreatment lines") 
minlcat <- barplot(d_complete, "Min_lines_cat", title="Minimum prior \ntreatment lines") 
maxlcat <- barplot(d_complete, "Max_lines_cat", title="Maximum prior \ntreatment lines") 

ps_age_lines  <- psratio + medagecat + maxagecat + minlcat + medlcat + maxlcat
ggsave(file.path(plot_d, "ps_age_lines.png"), plot=ps_age_lines, width=2500, height=3000, units="px")

d_complete_ns <- d_complete %>% filter(CRC_specific == "No")
d_complete_ns <- d_complete_ns %>%
  mutate(over_fifty = ifelse(CRC_total_ratio >= 0.5, "Yes", "No"),
         percentile = cut(CRC_total_ratio, breaks = pretty(CRC_total_ratio), labels = c("20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90%-100")))
crcs <- barplot(d_complete, "CRC_specific", title="CRC\nspecific") + xlab("")
crctr2 <- histogramm(d_complete_ns, "CRC_total_ratio", bins=20, title="Histogram of \nCRC:All ratio\nNon-specific")
crctr3 <- barplot(d_complete_ns, "percentile", title = "Percentiles CRC/All ratio \nfor non-specific cases")
efficacy <- efficacy + ggtitle("Efficacy Parameters")
crc_spec_eff <- crcs + crctr3 + efficacy + plot_layout(design="122223333")
crc_spec_eff

ggsave(file.path(plot_d, "SpecCRC_eff.png"), plot=crc_spec_eff, width=2800, height=1650, units="px")

p <- barplot(d_complete, "Phase", title="N of studies \nper phase")
ds1 <- barplot(d_complete_1, "Cohort_desc", title="Studies in phase 1") + xlab("")
ds2 <- barplot(d_complete_2, "Cohort_desc", title="Studies in phase 2") + xlab("")
ds <- p + ds1 + ds2 + plot_layout(design="12223")
ggsave(file.path(plot_d, "Descriptor of studies.png"), plot=ds,width=3500, height=2000, units="px")


ypo <- barplot(d_complete, "Year_published_online", numeric=T) + xlab("")
d_complete_filtered1 <- d_complete_filtered %>%
  group_by(Journal_new) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

ggplot(d_complete_filtered, aes(x=get("Journal_new"))) +
  geom_bar(color = "black", aes(fill="#e377c2")) +
  geom_text(aes(label = after_stat(count)), stat = "count",  vjust = -0.5) + 
  theme_classic() +
  theme(axis.text.x= element_text(angle=45, vjust=1, hjust=1), legend.position = "none") +
  ggtitle("Journal") +
  xlab("") +
  labs(fill = "Journal_new")+
  coord_flip()

d_complete_filtered1 <- d_complete_filtered %>%
  group_by(Journal_new) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

j <- ggplot(d_complete_filtered1, aes(x = reorder(Journal_new, Frequency), y = Frequency)) +
  geom_bar(color = "black", fill = "grey", stat = "identity") +
  geom_text(aes(label = Frequency), hjust = -0.3) +
  theme_classic() +
  theme(axis.text.y = element_text(angle = 0, hjust = 0.5), legend.position = "none") +
  ggtitle("Frequency of Scientific Journals") +
  ylab("Frequency") +
  xlab("Journal") +
  coord_flip()

jypo <- ypo + j + plot_layout(design="11222")
ggsave(file.path(plot_d, "Journal+year.png"), plot=jypo,width=3500, height=1600, units="px")

dt <- barplot(d_complete, "DrugType2", title="Number of studies per \ntreatment category") + xlab("") #ERROR: drug type not in database
yg <- barplot(d_complete, "year_group", title="Number of studies per \nyear group") + xlab("")
dtyg <- dt + yg + plot_layout(design="112")
ggsave(file.path(plot_d, "Treatment+YearGroup.png"), plot=dtyg,width=2000, height=1600, units="px")

library("cowplot") 
library(gridExtra)
#plot_grid
empty_plot <- ggplot() +
  theme_void() +
  theme(plot.margin = unit(c(0, 0.3, 0, 0), "in"))

tiff(file = file.path(plot_d, "SafetyMetrics.png"),
     units="in",
     width = (7),
     height = (5),
     res=300) 
plot_grid(
  empty_plot,
  safe1_phase1,
  safe1_phase2,
  safe2,
  nrow = 1,
  rel_widths = c(0.36, 0.5,0.5,1.25))
dev.off()

included <- barplot(d, "include") 
#reason <- barplot(d_removed, "ProblemClass", title="Reason of exclusion") #ERROR: problem class not in database
#exclusion <- included + reason + plot_layout(design="1222")
#ggsave(file.path(plot_d, "Exclusion_reason.png"), plot=exclusion,width=3500, height=2000, units="px")

d_complete$N_DLT_ratio <- d_complete$N_DLT/ d_complete$N_eval_DLT
d_complete$N_Disc_ratio <- d_complete$N_Disc/d_complete$N_pat_safety
d_complete$N_pat_GR3_ratio <- d_complete$`N_pat_GR3+`/d_complete$N_pat_safety
d_complete <- d_complete %>%
  mutate(
    safety_top5 = N_pat_safety*5,
    Ratio_event_pat = `N_events_top5_Gr3+` / safety_top5
  )

#correlatieplots tox
corplot1 <- ggplot(d_complete, aes(x=N_Disc_ratio, y=Ratio_event_pat)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie GR3+ top 5 events \n& discontinuations",x="Discontinuations", y = "GR3+ events top 5") + 
  theme_classic()  +
  ylim(0, .4) 
corplot1 #matige correlatie

corplot2 <- ggplot(d_complete, aes(x=N_Disc_ratio, y=N_DLT_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie DLT \n& discontinuations",x="Discontinuations", y = "DLT") + 
  theme_classic() +
  ylim(0, .4) 
corplot2 #matige correlatie

corplot3 <- ggplot(d_complete, aes(x=N_Disc_ratio, y=N_pat_GR3_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie GR3+ events \n& discontinuations",x="Discontinuations", y = "GR3+ events") + 
  theme_classic() +
  xlim(0,.4) 
corplot3 #matige correlatie

corplot4 <- ggplot(d_complete, aes(x=N_pat_GR3_ratio, y=N_DLT_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie DLT \n& GR 3+ events",x="GR3+ events", y = "DLT") + 
  theme_classic() +
  xlim(0, .4)  + ylim(0, .4)
corplot4 #matige correlatie

corplot5 <- ggplot(d_complete, aes(x=Ratio_event_pat, y=N_DLT_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie DLT \n& GR3+ events top 5",x=" GR3+ events top 5", y = "DLT") + 
  theme_classic() +
  xlim(0, .4)  + ylim(0, .4)
corplot5 #matige correlatie

corplot6 <- ggplot(d_complete, aes(x=Ratio_event_pat, y=N_pat_GR3_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie GR3+ events \n&  GR3+ events top 5",x= "GR3+ events top 5", y = "Grade 3+ events") + 
  theme_classic() +
  xlim(0, .4)  
corplot6 #matige correlatie

corplots <- corplot1 + corplot2  + corplot3 + corplot4 + corplot5  + corplot6 + plot_layout(design="123\n456")
ggsave(file.path(plot_d, "Corplots.png"), plot=corplots,width=2700, height=2000, units="px")

