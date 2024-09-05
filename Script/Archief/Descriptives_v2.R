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

d <- read_excel(file.path(input, "databaseV230504_2.xlsx"))

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
      geom_bar(color = "black", aes(fill=get(descriptive))) +
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

d_na_nr <- filter(d, if_any(everything(), ~ .x %in% c("NA", "NR"))) # select only the rows that have NA or NR in any column
n_na_nr <- sapply(d_na_nr, function(x) subset(table(x), names(table(x)) %in% c("NA", "NR"))) # gives the number of times that NA or NR occur in each column of d_na_nr
names_vec <- names(n_na_nr)[which(sapply(n_na_nr, length) > 0)] # gives a vector of the names of the elements that have a length greater than 0

d <- mutate_at(d, vars(names_vec), na_if, "NA") # replace "NA" strings with NA
d <- mutate_at(d, vars(names_vec), na_if, "NR") # replace "NR" strings with NA

d_included <- d[is.na(d$Flag_problems), ]
d_complete <- d_included[d_included$Gevuld > 0.8,] # alleen artikelen die afgemaakt zijn 
d_string_complete <- d_complete
d_removed <- d_included[d_included$Gevuld < 0.70,]

change_to_numeric <- c("PFS_med")
d_complete <- mutate_at(d_complete, vars(all_of(change_to_numeric)), as.numeric)
d_complete <- mutate_at(d_complete, vars(starts_with("N_")), as.numeric) #remove text fields
d_complete <- mutate_at(d_complete, vars(starts_with("T_")), as.numeric) #remove text fields 

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

write_xlsx(d_complete, file.path(output, "filtered_db4.xlsx"))
# write.xlsx(d_complete, file.path(output, "filtered_db.xlsx"))


missing <-  d_complete %>% summarise_all(~sum(is.na(.)))
not_missing <- d_complete %>% summarise_all(~sum(!is.na(.)))
nrow(d_complete)

d_complete_1 <- d_complete %>% filter(Phase == 1)
d_complete_2 <- d_complete %>% filter(Phase == 2)
d_complete$CRC_specific <- factor(d_complete$CRC_specific,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("No", "Yes", "Yes but not complete (waterfall plot)", "Yes but not complete (no SD_short)"))


d_string_complete$N_SD_long_fac <-  as.factor(d_string_complete$N_SD_long)
d_string_complete$N_SD_long_fac <-  factor(d_string_complete$N_SD_long_fac, 
                                       levels = c("NA", "NR"),
                                       labels = c("Not Applicable", "Not Reported")) # relabel NA
levels(d_string_complete$N_SD_long_fac) <-  c(levels(d_string_complete$N_SD_long_fac), "Reported") # add "reported as a new level
d_string_complete$N_SD_long_fac <- replace(d_string_complete$N_SD_long_fac, is.na(d_string_complete$N_SD_long_fac), "Reported")

colnames(d_string_complete)



convert <- c("N_PD", "N_SD_short", "N_SD_long", "N_PR", "N_PR", "PFS_med", 
             "N_DLT", "N_Disc", "N_pat_GR3+","N_events_top5_Gr3+","N_Gr3+_cat", "N_events_Gr3+") #add N_eval_DLT?

add_level <- function(x, y) { factor(x, levels = c(levels(x), y)) }

#ERROR: onderstaande werkt niet (NB d_string_complete variabelen zijn ook geen factor)
d_string_complete2 <- d_string_complete %>% mutate(across(all_of(convert), ~factor(., levels = c("NA"), 
                                                                                   labels = c("Not Applicable"))))
d_string_complete2 <- d_string_complete %>% 
  mutate(across(all_of(convert), ~factor(., levels = c("NA"), labels = c("Not Applicable")))) %>% 
  mutate(across(all_of(convert), ~add_level(., "Reported")))
d_string_complete2 <- d_string_complete2 %>% mutate(across(all_of(convert), ~replace(., is.na(.), "Reported")))

efficacy_cols <- c("N_PD", "N_SD_short", "N_SD_long", "N_PR", "N_PR", "N_CR", "PFS_med")
safety_cols1 <- c("N_DLT", "N_Disc")
safety_cols2 <- c("N_pat_GR3+","N_events_top5_Gr3+","N_Gr3+_cat", "N_events_Gr3+")

d_string_complete_efficacy <- d_string_complete2 %>% select(all_of(efficacy_cols)) 
d_long_efficacy <- pivot_longer(d_string_complete_efficacy, cols=all_of(efficacy_cols), names_to = "column", values_to = "level")
efficacy <- ggplot(d_long_efficacy, aes(x = column, fill = level)) + 
  geom_bar(position = "stack") + theme_classic() + 
  theme(axis.text.x= element_text(angle=45, vjust=1, hjust=1, size=12)) + 
  ggtitle("Availability of each efficacy metric") +
  xlab("")
remove(list = c("d_string_complete_efficacy", "d_long_efficacy"))

d_string_complete_safety1 <- d_string_complete2 %>% select(all_of(safety_cols1)) 
d_long_safety1 <- pivot_longer(d_string_complete_safety1, cols=all_of(safety_cols1), names_to = "column", values_to = "level")
safe1 <- ggplot(d_long_safety1, aes(x = column, fill = level)) + 
  geom_bar(position = "stack") + theme_classic() + 
  theme(axis.text.x= element_text(angle=40, vjust=1, hjust=1, size=12)) + 
  scale_x_discrete(labels = c("# patients drug discontinuation", "# patients DLT")) + 
  xlab("")
remove(list = c("d_string_complete_safety1", "d_long_safety1"))

d_string_complete_safety2 <- d_string_complete2 %>% select(all_of(safety_cols2)) 
d_long_safety2 <- pivot_longer(d_string_complete_safety2, cols=all_of(safety_cols2), names_to = "column", values_to = "level")
safe2 <- ggplot(d_long_safety2, aes(x = column, fill = level)) + 
  geom_bar(position = "stack") + theme_classic() + 
  theme(axis.text.x= element_text(angle=40, vjust=1, hjust=1, size=12)) + 
  scale_x_discrete(labels = c("# events GR3 and higher","# events GR3 and higher /nin top 5 categories","# GR3 and higher categories", "# patients with Grade 3 or higher AE")) + 
  xlab("")
remove(list = c("d_string_complete_safety2", "d_long_safety2"))

d_complete_filtered <- d_complete %>% 
  group_by(Journal) %>% 
  mutate(count = n(), Journal_new = if_else(count == 1, 'other', Journal)) %>% 
  select(-count)




p <- barplot(d_complete, "Phase", title="Number of studies per Phase")
ggsave(file.path(plot_d, "Phase.png"), plot=p, width=2000, height=2000, units="px")

mps <- barplot(d_complete, "Max_PS", title="Maximum Performance score")
minl <- barplot(d_complete,"Min_lines", title="Minimum of lines")
maxl <- barplot(d_complete, "Max_lines", title="Maximum of lines")
inex <- mps + minl + maxl
ggsave(file.path(plot_d, "MAXPS.png"), plot=mps, width=2000, height=2000, units="px")
ggsave(file.path(plot_d, "inex.png"), plot=inex, width=4000, height=2000, units="px")

d_complete_ns <- d_complete %>% filter(CRC_specific == "No")
d_complete_ns <- d_complete_ns %>%
  mutate(over_fifty = ifelse(CRC_total_ratio >= 0.5, "Yes", "No"),
         percentile = cut(CRC_total_ratio, breaks = pretty(CRC_total_ratio), labels = c("30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", ">90%")))
crcs <- barplot(d_complete, "CRC_specific", title="Specificity of efficacy evaluation") + xlab("")
crctr <- histogramm(d_complete, "CRC_total_ratio", bins=20, title="Histogram of\nCRC:All ratio")
crctr2 <- histogramm(d_complete_ns, "CRC_total_ratio", bins=20, title="Histogram of \nCRC:All ratio\nNon-specific")
crctr3 <- barplot(d_complete_ns, "percentile", title = "Percentiles CRC/All ratio for\nnon-specific cases")
efficacy <- efficacy + ggtitle("Efficacy Parameters")
crc_spec_eff <- crctr + crcs + efficacy + plot_layout(design="11222333")
crc_spec <- crcs + crctr2 + plot_layout(design="112")
crc_spec2 <- crcs + crctr3 + plot_layout(design="11122")

ggsave(file.path(plot_d, "SpecCRC_eff.png"), plot=crc_spec_eff, width=3500, height=2000, units="px")
ggsave(file.path(plot_d, "SpecCRC_only.png"), plot=crc_spec, width=2500, height=2000, units="px")
ggsave(file.path(plot_d, "SpecCRC_only_bar.png"), plot=crc_spec2, width=2500, height=2000, units="px")

p <- barplot(d_complete, "Phase", title="N of studies \nper Phase")
ds1 <- barplot(d_complete_1, "Cohort_desc", title="Studies in phase 1") + xlab("")
ds2 <- barplot(d_complete_2, "Cohort_desc", title="Studies in phase 2") + xlab("")
ds <- p + ds1 + ds2 + plot_layout(design="12233")
ggsave(file.path(plot_d, "Descriptor of studies.png"), plot=ds,width=3500, height=2000, units="px")

ypo <- barplot(d_complete, "Year_published_online", numeric=T) + xlab("")
j <- barplot(d_complete_filtered, "Journal_new") + xlab("")
jypo <- ypo + j
ggsave(file.path(plot_d, "Journal+year.png"), plot=jypo,width=3500, height=2000, units="px")

#dt <- barplot(d_complete, "DrugType", title="Number of studies per \ntreatment category") + xlab("") #ERROR: drug type not in database
#yg <- barplot(d_complete, "year_group", title="Number of studies per \nyear group") + xlab("")
#dtyg <- dt + yg + plot_layout(design="112")
#ggsave(file.path(plot_d, "Treatment+YearGroup.png"), plot=dtyg,width=4000, height=2000, units="px")

safe1 <- safe1 + theme(plot.margin=unit(c(0.5, 0.5, 0.5, 3), 'cm')) 
safe2 <- safe2 + xlab("") + ylab("")
saver <- safe1 + safe2 + plot_annotation(title="Availability of safety metrics")
ggsave(file.path(plot_d, "SafetyMetrics.png"), plot=saver,width=4500, height=2000, units="px")

efficacy <- efficacy
ggsave(file.path(plot_d, "EfficacyMetrics.png"), plot=efficacy,width=3000, height=2000, units="px")

#included <- barplot(d, "include") 
#reason <- barplot(d_removed, "ProblemClass", title="Reason of exclusion") #ERROR: problem class not in database
#exclusion <- included + reason + plot_layout(design="1222")
#ggsave(file.path(plot_d, "Exclusion_reason.png"), plot=exclusion,width=3500, height=2000, units="px")
