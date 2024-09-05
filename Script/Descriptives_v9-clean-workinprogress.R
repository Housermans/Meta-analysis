# Data loading ----
library(readxl)
library(ggplot2)
library(dplyr)
library(writexl)
library(meta)
library(tidyr)
library(patchwork)
library(cowplot) 
library(gridExtra)
library(stringr)

rm(list=ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# script.dir <- dirname(sys.frame(1)$ofile) 
home_dir <- dirname(script_dir)
input <- file.path(home_dir, "Input")
output <- file.path(home_dir, "Output")
plot <- file.path(home_dir, "Plots")
plot_d <- file.path(plot, "Descriptive")

d <- read_excel(file.path(input, "databaseV231106.xlsx"))

# Data Cleaning and Preparation ----

# summary(d)
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

# summary(d_complete) #277 unique reports/articles
#View(d_complete) #check numbers

# print(sum_per_phase)

change_to_numeric <- c("PFS_med")
d_complete <- mutate_at(d_complete, vars(all_of(change_to_numeric)), as.numeric)
d_complete <- mutate_at(d_complete, vars(starts_with("N_")), as.numeric) #remove text fields
d_complete <- mutate_at(d_complete, vars(starts_with("T_")), as.numeric) #remove text fields 
d_complete <- mutate_at(d_complete, vars(starts_with("Min_")), as.numeric) #remove text fields 
d_complete <- mutate_at(d_complete, vars(starts_with("Med_")), as.numeric) #remove text fields 
d_complete <- mutate_at(d_complete, vars(starts_with("Max_")), as.numeric) #remove text fields 
d_complete <- mutate_at(d_complete, vars(starts_with("PS_")), as.numeric) #remove text fields 

d_complete <- d_complete %>% mutate(
                              N_eva_eff_both = ifelse(CRC_specific == 1, N_eva_eff_crc, N_eva_eff),
                              CRC_total_ratio = N_inc_crc/N_inc,
                              CBR_short = (N_SD_short + N_PR + N_CR) / N_eva_eff,
                              CBR = (N_SD_14 + N_PR + N_CR) / N_eva_eff,
                              CBR_CRC_short = (N_SD_short + N_PR + N_CR) / N_eva_eff_crc,
                              CBR_CRC = (N_SD_14 + N_PR + N_CR) / N_eva_eff_crc,
                              CBR_MSI = (SD14_MSI + PR_MSI + CR_MSI) / n_eva_MSI,
                              CBR_MSS = (SD14_MSS + PR_MSS + CR_MSS) / n_eva_MSS,
                              ORR = (N_PR + N_CR) / N_eva_eff,
                              ORR_CRC = (N_PR + N_CR) / N_eva_eff_crc,
                              ORR_MSI = (SD14_MSI + PR_MSI + CR_MSI) / n_eva_MSI,
                              ORR_MSS = (SD14_MSS + PR_MSS + CR_MSS) / n_eva_MSS,
                              CBR_all = (pmax(N_SD_short, N_SD_14, N_SD_long, na.rm=TRUE) + N_PR + N_CR) / N_eva_eff,
                              CBR_CRC_all = (pmax(N_SD_short, N_SD_14, N_SD_long, na.rm=TRUE) + N_PR + N_CR) / N_eva_eff_crc, 
                              CBR_both = ifelse(CRC_specific == 1, CBR_CRC, CBR),
                              CBR_short_both = ifelse(CRC_specific == 1, CBR_CRC_short, CBR_short),
                              ORR_both = ifelse(CRC_specific == 1, ORR_CRC, ORR),
                              events_CBR = pmax(N_SD_short, N_SD_14, N_SD_long, na.rm=TRUE) + N_PR + N_CR, 
                              events_ORR = N_PR + N_CR,
                              year_group = ifelse(Year_published_online < 2013, "2010-2012", 
                                            ifelse( Year_published_online < 2016, "2013-2015", 
                                            ifelse(Year_published_online < 2019, "2016-2019", 
                                            ifelse(Year_published_online >= 2019, "2019-2021", NA)))),
                              targeted = ifelse(Cohort_target == "Not targeted", "Not targeted", "Targeted")
                              )


sum(str_detect(d_complete$Aut_1, "[0-9]")) #39 numbers (double articles)
#str_detect(d_complete$Aut_1, "[0-9]") == TRUE #check authors with numbers
studies_with_digit <- d_complete[str_detect(d_complete$Aut_1, "[0-9]"), "Aut_1"]
# View(studies_with_digit)

d_complete$Aut_1[d_complete$Aut_1 == "Daud, A. I. (2)"] <- "Daud, A. I."
d_complete$Aut_1[d_complete$Aut_1 == "Said, R. (1)"] <- "Said, R."
d_complete$Aut_1[d_complete$Aut_1 == "Samalin, E. (3)"] <- "Samalin, E. (2)"
d_complete$Aut_1[d_complete$Aut_1 == "Zhang, B. (1)"] <- "Zhang, B."

sum(str_detect(d_complete$Aut_1, "[0-9]")) #36 numbers (double articles) after correction, 2 with 3 cohorts --> 277-((36-2)/2) = 260
sum_per_phase <- d_complete %>%
  group_by(Phase) %>%
  summarise(Total_N_inc = sum(N_inc))

d <- d %>% mutate(include = ifelse(Gevuld > 0.86, "Included", "Excluded"))

d_complete <- d_complete %>% 
  mutate(mono_combi = ifelse(substr(ID, 1, 1) == "D", "Monotherapy", "Combination"))

# top 5 event calc
d_top5 <- d_complete %>%
  mutate(
    safety_pop5 = N_pat_safety*5,
    Ratio_event_top5 = `N_events_top5_Gr3+` / safety_pop5
  )

missing <-  d_complete %>% summarise_all(~sum(is.na(.)))
not_missing <- d_complete %>% summarise_all(~sum(!is.na(.)))
nrow(d_complete) #267

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

efficacy_cols <- c("N_PD", "N_SD_short","N_SD_14", "N_SD_long", "N_PR", "N_PR", "N_CR", "PFS_med")
safety_cols1 <- c("N_DLT", "N_Disc")
safety_cols2 <- c("N_pat_GR3+","N_events_top5_Gr3+","N_Gr3+_cat", "N_events_Gr3+")

# Function Definitions ----

histogramm <- function(df, descriptive, bins=30, title=paste("Histogram of",descriptive)) {
  ggplot(df, aes(x=get(descriptive))) +
    geom_histogram(color = "black", fill = "blue", bins=bins) +
    labs(title = title,
         x = descriptive,
         y = "Count") +
    theme_classic() + 
    xlab("")
}

barplot_nocategory <- function(df, descriptive, title=descriptive, numeric=F) {
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

barplot <- function(df, descriptive, title=descriptive, category, numeric=F) {
  df <- df %>% arrange(get(descriptive))
  if (numeric == TRUE) {
    ggplot(df, aes(x=get(descriptive))) +
      geom_bar(color = "black", aes(fill=factor(get(descriptive))), position="dodge") +
      geom_text(aes(label = after_stat(count), group=factor(get(category))), stat = "count",  vjust = -0.5, position=position_dodge(width=0.9)) + 
      theme_classic() +
      theme(axis.text.x= element_text(angle=45, vjust=1, hjust=1), legend.position = "none") +
      ggtitle(title) +
      xlab("") +
      labs(fill = descriptive)
  } else {
    ggplot(df, aes(x=get(descriptive))) +
      geom_bar(color = "black", aes(fill=get(category)), position="dodge") +
      scale_fill_manual(values=c("#F8766D", "#A020F0", "green", "black")) +
      geom_text(aes(label = after_stat(count), group=factor(get(category))), stat = "count",  vjust = -0.5, position=position_dodge(width=0.9)) + 
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

add_level <- function(x, y) {
  factor(x, levels = c(levels(x), y)) 
}

# Data Analysis ----

d_string_complete2 <- d_string_complete %>% mutate(across(all_of(convert), ~ifelse(is.na(.), "Not Reported", "Reported")))
d_string_complete2 <- d_string_complete2 %>% mutate(across(all_of(convert), ~as.factor(.)))

d_string_complete2 <- d_string_complete2 %>% mutate(across(all_of(efficacy_cols), ~factor(., levels=c("Not Reported", "Reported"))))

d_string_complete_efficacy <- d_string_complete2 %>% select(all_of(efficacy_cols), Phase) 

d_long_efficacy <- pivot_longer(d_string_complete_efficacy, cols=all_of(efficacy_cols), names_to = "column", values_to = "level")


# Assuming d_long_efficacy is your long format data
# Group by level, Phase, and column, then count
count_data <- d_long_efficacy %>%
  group_by(level, Phase, column) %>%
  summarize(count = n(), .groups = 'drop')

# Calculate total counts for each phase and metric
total_counts <- count_data %>%
  group_by(Phase, column) %>%
  summarize(total = sum(count), .groups = 'drop')

# Join total counts with count data and calculate percentages
percentage_data_efficacy <- count_data %>%
  left_join(total_counts, by = c("Phase", "column")) %>%
  mutate(percentage = (count / total) * 100)

# View the percentage data
# percentage_data_efficacy


d_string_complete_safety1 <- d_string_complete2 %>% select(all_of(safety_cols1), Phase) 
d_long_safety1 <- pivot_longer(d_string_complete_safety1, cols=all_of(safety_cols1), names_to = "column", values_to = "level")
d_long_safety1_phase1 <- d_long_safety1 %>% filter(Phase == 1)
d_long_safety1_phase2 <- d_long_safety1 %>% filter(Phase == 2)

d_string_complete_safety12 <- d_string_complete2 %>% select(all_of(safety_cols1), all_of(safety_cols2), Phase) 
d_long_safety12 <- pivot_longer(d_string_complete_safety12, cols=c(all_of(safety_cols1), all_of(safety_cols2)), names_to = "column", values_to = "level")

# Assuming d_long_safety1 is your long format data
# Group by level, Phase, and column, then count
count_data_s <- d_long_safety12 %>%
  group_by(level, Phase, column) %>%
  summarize(count = n(), .groups = 'drop')

# Calculate total counts for each phase and metric
total_counts_s <- count_data_s %>%
  group_by(Phase, column) %>%
  summarize(total = sum(count), .groups = 'drop')

# Join total counts with count data and calculate percentages
percentage_data_safety <- count_data_s %>%
  left_join(total_counts_s, by = c("Phase", "column")) %>%
  mutate(percentage = (count / total) * 100) %>% filter(level == "Reported")

# View the percentage data
# percentage_data_safety

d_string_complete_safety2 <- d_string_complete2 %>% select(all_of(safety_cols2)) 
d_long_safety2 <- pivot_longer(d_string_complete_safety2, cols=all_of(safety_cols2), names_to = "column", values_to = "level")

d_complete_filtered <- d_complete %>% 
  group_by(Journal) %>% 
  mutate(count = n(), Journal_new = if_else(count == 1, 'other', Journal)) %>% 
  select(-count)

d_complete <- d_complete %>%
  mutate(PS_0_1 = PS_0 + PS_1,
         PS_ratio = PS_2/ (PS_2+PS_0_1))

PS_right <- d_complete$N_inc == d_complete$PS_0 + d_complete$PS_1 + d_complete$PS_2 
d_checkPS <- cbind (PS_right, d_complete$Aut_1, d_complete$Journal)
d_checkPS <-as.data.frame(d_checkPS)
d_checkPS_false <- d_checkPS %>% filter(PS_right == FALSE)
#View(d_checkPS_false) #Heeft Lidwien gecheckt, soms is PS3

d_complete$N_DLT_ratio <- d_complete$N_DLT/ d_complete$N_eval_DLT
d_complete$N_Disc_ratio <- d_complete$N_Disc/d_complete$N_pat_safety
d_complete$N_pat_GR3_ratio <- d_complete$`N_pat_GR3+`/d_complete$N_pat_safety
d_complete <- d_complete %>%
  mutate(
    safety_top5 = N_pat_safety*5,
    Ratio_event_top5 = `N_events_top5_Gr3+` / safety_top5
  )

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


# Plots ----

histogramm(d_top5, "Ratio_event_top5") 


efficacy <- ggplot(d_long_efficacy, aes(x = column, fill = level)) + 
  geom_bar(position = "stack", color = "black") + theme_classic() + 
  theme(axis.text.x= element_text(angle=45, vjust=1, hjust=1, size=12)) + 
  ggtitle("Availability of efficacy metrics") +
  xlab("") +
  scale_x_discrete(labels = c("# CR", "# PD", "# PR", "# SD 14 wks","# SD long", "# SD best response", "# Median PFS")) + 
  scale_fill_manual(values = c("white", "#00A88E")) 
efficacy
#remove(list = c("d_string_complete_efficacy", "d_long_efficacy"))

efficacy_by_phase <- ggplot(d_long_efficacy, aes(x = column, fill = level)) + 
  geom_bar(position = "stack", color = "black") + 
  facet_wrap(~ Phase) +  # Adding facet_wrap to separate by phase
  theme_classic() + 
  theme(axis.text.x= element_text(angle=45, vjust=1, hjust=1, size=12)) + 
  ggtitle("Availability of efficacy metrics by Phase") +
  xlab("") +
  scale_x_discrete(labels = c("# CR", "# PD", "# PR", "# SD 14 wks","# SD long", "# SD best response", "# Median PFS")) + 
  scale_fill_manual(values = c("white", "#00A88E"))
efficacy_by_phase

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


safe2 <- ggplot(d_long_safety2, aes(x = column, fill = level)) + 
  geom_bar(position = "stack", color = "black") + theme_classic() + 
  theme(axis.text.x= element_text(angle=40, vjust=1, hjust=1, size=12)) + 
  scale_x_discrete(labels = c("# events GR3+","# events GR3+ top 5 categories","# GR3+ categories", "# patients GR3+ AE")) + 
  xlab("") +
  labs(title = "Phase 1 & 2")+
  scale_fill_manual(values = c("white", "#00A88E")) 
remove(list = c("d_string_complete_safety2", "d_long_safety2"))

p <- barplot_nocategory(d_complete, "Phase", title="Number of studies per Phase")
# ggsave(file.path(plot_d, "Phase.png"), plot=p, width=800, height=1500, units="px")

#raw data plots
medage <- barplot_nocategory(d_complete, "Med_age", title="Median age") #check verdeling age median
maxage <- barplot_nocategory(d_complete, "Max_age", title="Maximum age") #check verdeling age maximum
mps <- barplot_nocategory(d_complete, "Max_PS", title="Maximum Performance score")
medl <- barplot_nocategory(d_complete, "Med_lines", title="Median of lines")
minl <- barplot_nocategory(d_complete,"Min_lines", title="Minimum of lines")
maxl <- barplot_nocategory(d_complete, "Max_lines", title="Maximum of lines")


d_complete$PS_ratio_cat <- cut(d_complete$PS_ratio,
                               breaks = c(0, 0.00001, 0.1, Inf),
                               labels = c("<0","0-0.1",">0.1"),
                               include.lowest = TRUE)

psratio <- barplot(d_complete, "PS_ratio_cat", title="Performance score \nratio 2/0 or 1", category="Phase")
medagecat <- barplot(d_complete, "Med_age_cat", title="Median\n age", category="Phase") 
maxagecat <- barplot(d_complete, "Max_age_cat", title="Maximum\n age", category="Phase") 
medlcat <- barplot(d_complete, "Med_lines_cat", title="Median prior \ntreatment lines", category="Phase") 
minlcat <- barplot(d_complete, "Min_lines_cat", title="Minimum prior \ntreatment lines", category="Phase") 
maxlcat <- barplot(d_complete, "Max_lines_cat", title="Maximum prior \ntreatment lines", category="Phase") 

ps_age_lines  <- psratio + medagecat + maxagecat + minlcat + medlcat + maxlcat
# ggsave(file.path(plot_d, "ps_age_lines.png"), plot=ps_age_lines, width=2500, height=3000, units="px")

#violinplots
violin_med_age <- ggplot(d_complete, aes(x = Phase, y = Med_age, fill = Phase)) +
  geom_violin() +
  labs(title = "Distribution median age",x = "Phase",  y = "Median age"  ) +
  scale_fill_manual(values = c("#007965", "#C9A8CE")) +
  theme_classic()
  
violin_med_age

#save plot
# ggsave(file.path(plot_d, "violin_med_age.png"), plot=violin_med_age ,width=1100, height=900, units="px")

#violinplots
violin_max_age <- ggplot(d_complete, aes(x = Phase, y = Max_age, fill = Phase)) +
  geom_violin() +
  labs(title = "Distribution maximum age",x = "Phase",  y = "Maximum age"  ) +
  scale_fill_manual(values = c("#007965", "#C9A8CE")) +
  theme_classic()

violin_max_age

#save plot
# ggsave(file.path(plot_d, "violin_max_age.png"), plot=violin_max_age ,width=1100, height=900, units="px")

#violinplots
violin_max_lines <- ggplot(d_complete, aes(x = Phase, y = Max_lines, fill = Phase)) +
  geom_violin() +
  labs(title = "Distribution maximum \nprior treatment lines",x = "Phase",  y = "Maximum prior treatment lines"  ) +
  scale_fill_manual(values = c("#007965", "#C9A8CE")) +
  theme_classic()

violin_max_lines

#save plot
# ggsave(file.path(plot_d, "violin_max_lines.png"), plot=violin_max_lines ,width=1100, height=900, units="px")

#violinplots
violin_min_lines <- ggplot(d_complete, aes(x = Phase, y = Min_lines, fill = Phase)) +
  geom_violin() +
  labs(title = "Distribution minimum \nprior treatment lines",x = "Phase",  y = "Minimum prior treatment lines"  ) +
  scale_fill_manual(values = c("#007965", "#C9A8CE")) +
  theme_classic()

violin_min_lines

#save plot
# ggsave(file.path(plot_d, "violin_min_lines.png"), plot=violin_min_lines ,width=1100, height=900, units="px")

#violinplots
violin_med_lines <- ggplot(d_complete, aes(x = Phase, y = Med_lines, fill = Phase)) +
  geom_violin() +
  labs(title = "Distribution median \nprior treatment lines",x = "Phase",  y = "Median prior treatment lines"  ) +
  scale_fill_manual(values = c("#007965", "#C9A8CE")) +
  theme_classic()

violin_med_lines

#save plot
# ggsave(file.path(plot_d, "violin_med_lines.png"), plot=violin_med_lines ,width=1100, height=900, units="px")

#violinplots
violin_year <- ggplot(d_complete, aes(x = Phase, y = Year_published_online, fill = Phase)) +
  geom_violin() +
  labs(title = "Distribution \nyear published",x = "Phase",  y = "Year published online"  ) +
  scale_fill_manual(values = c("#007965", "#C9A8CE")) +
  theme_classic()

violin_year

#save plot
# ggsave(file.path(plot_d, "violin_year.png"), plot=violin_year ,width=1100, height=900, units="px")

d_complete_ns <- d_complete %>% filter(CRC_specific == "No")
d_complete_ns <- d_complete_ns %>%
  mutate(over_fifty = ifelse(CRC_total_ratio >= 0.5, "Yes", "No"),
         percentile = cut(CRC_total_ratio, breaks = pretty(CRC_total_ratio), labels = c("20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90%-100")))
crcs <- barplot(d_complete, "CRC_specific", title="CRC\nspecific", category="Phase") + xlab("")
crctr2 <- histogramm(d_complete_ns, "CRC_total_ratio", bins=20, title="Histogram of \nCRC:All ratio\nNon-specific")
crctr3 <- barplot(d_complete_ns, "percentile", title = "Percentiles CRC/All ratio \nfor non-specific cases", category="Phase")
efficacy <- efficacy + ggtitle("Efficacy Parameters")
crc_spec_eff <- crcs + crctr3 + efficacy + plot_layout(design="122223333")
crc_spec_eff

# ggsave(file.path(plot_d, "SpecCRC_eff.png"), plot=crc_spec_eff, width=2800, height=1650, units="px")

ypo <- barplot_nocategory(d_complete, "Year_published_online", numeric=T) + xlab("")
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
# ggsave(file.path(plot_d, "Journal+year.png"), plot=jypo,width=3500, height=1600, units="px")

is <- barplot(d_complete, "Investigational_status", title="Number of studies per \ninvestigational status", category="Phase") + xlab("") 
dt <- barplot(d_complete, "DrugType", title="Number of studies per \ntreatment category", category="Phase") + xlab("") 
yg <- barplot(d_complete, "year_group", title="Number of studies per \nyear group", category="Phase") + xlab("")
dtyg <- is + dt + yg + plot_layout(design="123")
# ggsave(file.path(plot_d, "Treatment+YearGroup.png"), plot=dtyg,width=3000, height=2000, units="px")

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



#correlatieplots tox
corplot1 <- ggplot(d_complete, aes(x=N_Disc_ratio, y=Ratio_event_top5)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie GR3+ top 5 events \n& discontinuations",x="Discontinuations", y = "GR3+ events top 5") + 
  theme_classic()  +
  ylim(0, .4) 
cor.test(d_complete$N_Disc_ratio, d_complete$Ratio_event_top5)
corplot1 #goede correlatie

corplot2 <- ggplot(d_complete, aes(x=N_Disc_ratio, y=N_DLT_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie DLT \n& discontinuations",x="Discontinuations", y = "DLT") + 
  theme_classic() +
  ylim(0, .4) 
cor.test(d_complete$N_Disc_ratio, d_complete$N_DLT_ratio)
corplot2 #geen correlatie

corplot3 <- ggplot(d_complete, aes(x=N_Disc_ratio, y=N_pat_GR3_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie GR3+ events \n& discontinuations",x="Discontinuations", y = "Ratio of patients with GR3+ events/All patients") + 
  theme_classic() +
  xlim(0,.4) 
cor.test(d_complete$N_Disc_ratio, d_complete$N_pat_GR3_ratio)
corplot3 #Zeer goede correlatie

corplot4 <- ggplot(d_complete, aes(x=N_pat_GR3_ratio, y=N_DLT_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie DLT \n& GR 3+ events",x="Ratio of patients with GR3+ events/All patients", y = "DLT") + 
  theme_classic() +
  xlim(0, .4)  + ylim(0, .4)
cor.test(d_complete$N_DLT_ratio, d_complete$N_pat_GR3_ratio)
corplot4 #Geen correlatie

corplot5 <- ggplot(d_complete, aes(x=Ratio_event_top5, y=N_DLT_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie DLT \n& GR3+ events top 5",x=" GR3+ events top 5", y = "DLT") + 
  theme_classic() +
  xlim(0, .4)  + ylim(0, .4)
cor.test(d_complete$N_DLT_ratio, d_complete$Ratio_event_top5)
corplot5 #Geen correlatie

corplot6 <- ggplot(d_complete, aes(x=Ratio_event_top5, y=N_pat_GR3_ratio)) + 
  #annotate("text", x = -60, y = 0.95, label = substitute(paste(italic('R'), ' = 0.81, ', italic('p'), ' = 0.05')),  hjust = 0, size =3)+
  geom_point(size =1) +
  labs(title = "Correlatie GR3+ events \n&  GR3+ events top 5",x= "GR3+ events top 5", y = "Ratio of patients with GR3+ events/All patients") + 
  theme_classic() +
  xlim(0, .4)  
cor.test(d_complete$N_pat_GR3_ratio, d_complete$Ratio_event_top5)
corplot6 #Uitstekende correlatie

corplots <- corplot1 + corplot2  + corplot3 + corplot4 + corplot5  + corplot6 + plot_layout(design="123\n456")
# ggsave(file.path(plot_d, "Corplots.png"), plot=corplots,width=2700, height=2000, units="px")
# Conclusie corplots: aantal patienten met gr3+ events en top 5 gr3+ correleert erg goed (R=0.7, p<0.00000000001), top 5 correleert ook 
# met aantal discontinuations (R=0.5, p<0.00000001. Dus bruikbare maat die goed beschikbaar is.  DLT correleert totaal niet met Gr3 events (top 5 of patienten)

d_complete$Phase
library(ggpubr)
corplot_by_phase <- ggplot(d_complete, aes(x = Ratio_event_top5, y = CBR_both)) + 
  geom_point(aes(size = N_eva_eff_both, color = Phase, alpha = 0.5)) +
  # geom_smooth(method = "lm", se = FALSE, aes(group = Phase)) +
  # stat_cor(aes(group = Phase)) +
  guides (alpha = "none", colour = guide_legend(override.aes = list(alpha = 0.7))) + 
  labs(title = "Efficacy & toxicity", x = "GR3+ events top 5", y = "CBR", size = "Number evaluated \npatients in study") + 
  theme_classic() +
  xlim(0, 0.4) +
  scale_color_manual(values = c("#007965", "#C9A8CE"))
corplot_by_phase

ggsave(file.path(plot_d, "corplot_by_phase.png"), plot=corplot_by_phase, width=2000, height=2000, units="px")

library(viridis)
color_palette <- viridis_pal(option = "H")(6)

corplot_dt <- ggplot(d_complete, aes(x = Ratio_event_top5, y = CBR_both)) + 
  geom_point(aes(size = N_eva_eff_both, color = DrugType, alpha = 0.7)) + 
  # geom_smooth(method = "lm", se = FALSE, aes(color = DrugType, group = DrugType)) +
  # stat_cor(aes(color = DrugType, group = DrugType), size =2, label.x = 0.3) +
  guides(alpha = "none") + 
  labs(title = "Clinical benefit rate vs rate of most common \ngrade 3+ adverse events per included trial \nby drug type and number of patients", x = "Rate of top 5 GR3+ adverse events", y = "CBR", size = "Number evaluated \npatients in study", color="Class of drug tested") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlim(0, 0.4) +
  scale_color_manual(values = color_palette)
corplot_dt
ggsave(file.path(plot_d, "Corplot_cbrvstop5_drugtype.png"), plot=corplot_dt, width=1800, height=1300, units="px")

corplot_ef_tox_yg <- ggplot(d_complete, aes(x = Ratio_event_top5, y = CBR_both)) + 
  geom_point(aes(size = N_eva_eff_both, color = year_group, alpha = 0.7)) +
  guides (alpha = "none", colour = guide_legend(override.aes = list(alpha = 0.7))) + 
  labs(title = "Efficacy & toxicity", x = "GR3+ events top 5", y = "CBR", size = "Number evaluated \npatients in study") + 
  theme_classic() +
  xlim(0, 0.4) +
  scale_color_manual(values = c("#CD2626", "dodgerblue3", "#CDAD00", "#548B54"))
corplot_ef_tox_yg

corplot_ef_tox_yg_phase <- ggplot(d_complete, aes(x = Ratio_event_top5, y = CBR_both)) + 
  geom_point(aes(color = year_group, shape=Phase), alpha=0.7, size=3.5) +
  guides (alpha = "none", colour = guide_legend(override.aes = list(alpha = 0.7))) + 
  labs(title = "Clinical benefit rate vs rate of most common \ngrade 3+ adverse events per included trial \nby year group and phase", x = "Rate of top 5 GR3+ adverse events", y = "CBR", size = "Number evaluated \npatients in study", color="Year group") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlim(0, 0.4) +
  scale_color_manual(values = c("#CD2626", "dodgerblue3", "#CDAD00", "#548B54")) + 
  scale_shape_manual(values = c(17, 16))
corplot_ef_tox_yg_phase


ggsave(file.path(plot_d, "corplot_by_yg.png"), plot=corplot_ef_tox_yg, width=2000, height=2000, units="px")
ggsave(file.path(plot_d, "corplot_by_yg_phase.png"), plot=corplot_ef_tox_yg_phase, width=1800, height=1300, units="px")

corplot_ef_tox_dt <- ggplot(d_complete, aes(x = Ratio_event_top5, y = CBR_both)) + 
  geom_point(aes(size = N_eva_eff_both, color = DrugType, alpha = 0.5, shape=Phase)) +
  guides (alpha = "none", colour = guide_legend(override.aes = list(alpha = 0.5))) + 
  labs(title = "Efficacy & toxicity", x = "GR3+ events top 5", y = "CBR", size = "Number evaluated \npatients in study") + 
  theme_classic() +
  xlim(0, 0.4) +
  scale_color_manual(values = c("#CD2626", "dodgerblue3", "#CDAD00", "#548B54", "#8B5A00", "darkslategray"))
corplot_ef_tox_dt




corplot7 <- ggplot(d_complete, aes(x = Ratio_event_top5, y = CBR_both)) + 
  geom_point(aes(size = N_eva_eff_both, color = Phase, alpha = 0.5, shape=drug_type)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Phase)) +
  guides (alpha = "none") + 
  labs(title = "Efficacy & toxicity", x = "GR3+ events top 5", y = "CBR", size = "Number evaluated \npatients in study") + 
  theme_classic() +
  xlim(0, 0.4) +
  scale_color_manual(values = c("#007965", "#C9A8CE"))

# # For Phase 1
# phase1_data <- subset(d_complete, Phase == "1")
# cor_test1 <- cor.test(phase1_data$Ratio_event_top5, phase1_data$CBR_both)
# 
# # For Phase 2
# phase2_data <- subset(d_complete, Phase == "2")
# cor_test2 <- cor.test(phase2_data$Ratio_event_top5, phase2_data$CBR_both)
# 
# # Print the results
# print(paste("Correlation for Phase 1: ", cor_test1$estimate))
# print(paste("p-value for Phase 1: ", cor_test1$p.value))
# 
# print(paste("Correlation for Phase 2: ", cor_test2$estimate))
# print(paste("p-value for Phase 2: ", cor_test2$p.value))
# 
# plot_N_Disc_ratio <- ggplot(d_complete, aes(x = N_Disc_ratio, y = CBR_both)) + 
#   geom_point(aes(size = N_eva_eff_both, color = Phase, alpha = 0.5)) +
#   geom_smooth(method = "lm", se = FALSE, aes(group = Phase)) +
#   guides (alpha = "none") +
#   labs(title = "Efficacy & toxicity with N_Disc_ratio", x = "N_Disc_ratio", y = "CBR") + 
#   theme_classic() +
#   scale_color_manual(values = c("#007965", "#C9A8CE"))
# 
# plot_N_Disc_ratio
# 
# # Phase 1
# cor_test_N_Disc_ratio_phase1 <- cor.test(subset(d_complete, Phase == "1")$N_Disc_ratio, subset(d_complete, Phase == "1")$CBR_both)
# 
# # Phase 2
# cor_test_N_Disc_ratio_phase2 <- cor.test(subset(d_complete, Phase == "2")$N_Disc_ratio, subset(d_complete, Phase == "2")$CBR_both)
# 
# 
# plot_N_pat_GR3_ratio <- ggplot(d_complete, aes(x = N_pat_GR3_ratio, y = CBR_both)) + 
#   geom_point(aes(size = N_eva_eff_both, color = Phase, alpha = 0.5)) +
#   geom_smooth(method = "lm", se = FALSE, aes(group = Phase)) +
#   guides (alpha = "none") +
#   labs(title = "Efficacy & toxicity with N_pat_GR3_ratio", x = "N_pat_GR3_ratio", y = "CBR") + 
#   theme_classic() +
#   scale_color_manual(values = c("#007965", "#C9A8CE"))
# 
# plot_N_pat_GR3_ratio
# 
# # Phase 1
# cor_test_N_pat_GR3_ratio_phase1 <- cor.test(subset(d_complete, Phase == "1")$N_pat_GR3_ratio, subset(d_complete, Phase == "1")$CBR_both)
# p=0.19, enige met een richting van een trend, rest is allemaal lager
# 
# # Phase 2
# cor_test_N_pat_GR3_ratio_phase2 <- cor.test(subset(d_complete, Phase == "2")$N_pat_GR3_ratio, subset(d_complete, Phase == "2")$CBR_both)




drug_types <- unique(d_complete$DrugType)

for (drug_type in drug_types) {
  subset_data <- subset(d_complete, DrugType == drug_type)
  cor_test <- cor.test(subset_data$Ratio_event_top5, subset_data$CBR_both)
  
  print(paste("Correlation for DrugType", drug_type, ":", cor_test$estimate))
  print(paste("p-value for DrugType", drug_type, ":", cor_test$p.value))
}

targeted_cols <-  c("year_group", "targeted")
d_long_string_complete_target <- d_string_complete %>% select(all_of(targeted_cols)) %>% pivot_longer(cols=all_of(targeted_cols), names_to = "column", values_to = "level")


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

d_complete_ratio <- d_complete %>%
  count(year_group, targeted) %>%
  group_by(year_group) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

# Plot the stacked bar graph
targeted_yeargroup <- ggplot(d_complete_ratio, aes(x = year_group, y = ratio, fill = targeted)) +
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() + 
  labs(
    title = "Ratio of targeted and not targeted \nby year group",
    x = "Year group",
    y = "Ratio",
    fill = "Target"
  ) +theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("Targeted" = "#00A88E", "Not targeted" = "white")) +
  scale_y_continuous(labels = scales::percent)
targeted_yeargroup

# Replace 'year_group' with 'year' in your data processing and plotting code
d_complete_ratio_year <- d_complete %>%
  count(Year_published_online, targeted) %>%  # Change this line
  group_by(Year_published_online) %>%  # Change this line
  mutate(ratio = n / sum(n)) %>%
  ungroup()
# 
# Plot the stacked bar graph
targeted_year <- ggplot(d_complete_ratio_year, aes(x = Year_published_online, y = ratio, fill = targeted)) +  # Change this line
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() +
  labs(
    title = "Ratio of targeted and not targeted \nby year",  # Change this line
    x = "Year",  # Change this line
    y = "Ratio",
    fill = "Target"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Targeted" = "#00A88E", "Not targeted" = "white")) +
  scale_y_continuous(labels = scales::percent)
targeted_year

# Compute the ratio of targeted to not targeted for each Year_published_online and Phase
d_complete_ratio_yearphase <- d_complete %>%
  count(Year_published_online, Phase, targeted) %>%
  spread(targeted, n, fill = 0) %>%
  group_by(Year_published_online, Phase) %>%
  mutate(targeted_ratio = `Targeted` / (`Targeted` + `Not targeted`)) %>%
  ungroup()

# Create line plot
line_plot_by_phase <- ggplot(d_complete_ratio_yearphase, aes(x = Year_published_online, y = targeted_ratio)) +
  geom_line(aes(linetype = Phase), size = 1) +
  geom_point(aes(shape = Phase), size = 3) +
  theme_classic() +
  labs(
    title = "Ratio of targeted to not targeted studies by Phase and Year",
    x = "Year",
    y = "Targeted to Not Targeted Ratio",
    linetype = "Phase",
    shape = "Phase"
  ) +
  scale_y_continuous(labels = scales::percent)

line_plot_by_phase

percentages_df_targeted <- d_complete_ratio %>%
  filter(year_group %in% c("2010-2012", "2019-2021"),  # Replace with the year groups you're interested in
         targeted %in% c("Targeted")) %>%  # Replace with the drug types you're interested in
  group_by(year_group, targeted) %>%
  summarise(total = sum(ratio)) %>%
  mutate(percentage = (total / sum(total)) * 100)

print(percentages_df_targeted)

#save the plot
# ggsave(file.path(plot_d, "Targeted_YearGroup.png"), plot=targeted_yeargroup, width=1100, height=1200, units="px")

# Calculate the ratio within each category of "Investigational_status" and "year_group"
d_complete_ratio_investigation <- d_complete %>%
  count(year_group, Investigational_status) %>%
  group_by(year_group) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

# Plot the stacked bar graph
investigational_status_plot <- ggplot(d_complete_ratio_investigation, aes(x = year_group, y = ratio, fill = Investigational_status)) +
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() +
  labs(
    title = "Ratio of investigational status \nby year group",
    x = "Year group",
    y = "Ratio",
    fill = "Investigational Status"
  ) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("lightblue", "#00A88E", "#C9A8CE")) +
  scale_y_continuous(labels = scales::percent) # You can add your custom colors using scale_fill_manual if needed

investigational_status_plot

# Save the plot
# ggsave(file.path(plot_d, "Investigational_Status_YearGroup.png"), plot = investigational_status_plot, width=1700, height=1200, units="px")

# Calculate the ratio within each category of "PS_ratio_cat" and "year_group"
d_complete_ratio_ps <- d_complete %>%
  count(year_group, PS_ratio_cat) %>%
  group_by(year_group) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

d_complete_ratio_ps_no_na <- d_complete %>%
  filter(!is.na(PS_ratio_cat)) %>%  # Step 1: Filter out NA values in PS_ratio_cat
  count(year_group, PS_ratio_cat) %>%  # Step 2: Count non-NA values
  group_by(year_group) %>%
  mutate(ratio = n / sum(n)) %>%  # Step 3: Calculate ratios
  ungroup()


# Plot the stacked bar graph
ps_ratio_plot <- ggplot(d_complete_ratio_ps, aes(x = year_group, y = ratio, fill = PS_ratio_cat)) +
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() +
  labs(
    title = "Ratio of PS ratio category \nby year group",
    x = "Year group",
    y = "Ratio",
    fill = "PS ratio category"
  ) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("lightblue", "#00A88E", "#C9A8CE", "white")) +
  scale_y_continuous(labels = scales::percent) # You can add your custom colors using scale_fill_manual if needed

ps_ratio_plot

d_complete_ratio_ps

percentages_df_ps_ratio <- d_complete_ratio_ps %>%
  filter(year_group %in% c("2010-2012", "2019-2021"),  # Replace with the year groups you're interested in
         PS_ratio_cat %in% c("<0", "0-0.1", ">0.1")) %>%  # Replace with the drug types you're interested in
  group_by(year_group, PS_ratio_cat) %>%
  summarise(total = sum(ratio)) %>%
  mutate(inverse = 1-total)

percentages_df_ps_ratio

percentages_df_ps_ratio_no_na <- d_complete_ratio_ps_no_na %>%
  filter(year_group %in% c("2010-2012", "2019-2021"),  # Replace with the year groups you're interested in
         PS_ratio_cat %in% c("<0", "0-0.1", ">0.1")) %>%  # Replace with the drug types you're interested in
  group_by(year_group, PS_ratio_cat) %>%
  summarise(total = sum(ratio)) %>%
  mutate(inverse = 1-total)

percentages_df_ps_ratio_no_na

# Save the plot
# ggsave(file.path(plot_d, "PS_Ratio_Category_YearGroup.png"), plot = ps_ratio_plot, width=1200, height=1200, units="px")

# Calculate the ratio within each category of "Med_age_cat" and "year_group"
d_complete_ratio_age <- d_complete %>%
  count(year_group, Med_age_cat) %>%
  group_by(year_group) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

  # Plot the stacked bar graph
med_age_plot <- ggplot(d_complete_ratio_age, aes(x = year_group, y = ratio, fill = Med_age_cat)) +
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() +
  labs(
    title = "Ratio of median age category \nby year group",
    x = "Year group",
    y = "Ratio",
    fill = "Median age category"
  ) +  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("lightblue", "#00A88E", "#C9A8CE", "grey"), na.value = "white") +
  scale_y_continuous(labels = scales::percent) # You can add your custom colors using scale_fill_manual if needed

med_age_plot

histogramm(d_complete, "Med_age") # approaches normal distribution, so it's okay to use the mean of medians!

# Group by Year_published_online and Phase, then calculate the mean of median ages
grouped_data_mean <- d_complete %>%
  group_by(Year_published_online, Phase) %>%
  summarise(mean_of_medians = mean(Med_age, na.rm = TRUE))

# Create line plot
ggplot(grouped_data_mean, aes(x = Year_published_online, y = mean_of_medians, group = Phase)) +
  geom_line(aes(linetype = Phase)) +
  geom_point(aes(shape = Phase)) +
  labs(
    title = "Mean of Median Ages by Phase and Year",
    x = "Year",
    y = "Mean of Median Ages",
    linetype = "Phase",
    shape = "Phase"
  ) +
  theme_classic()

interaction_model <- lm(mean_of_medians ~ Year_published_online * Phase, data = grouped_data_mean)

# summary(interaction_model)

# Call:
#   lm(formula = mean_of_medians ~ Year_published_online * Phase, 
#      data = grouped_data_mean)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.80882 -0.35724 -0.07277  0.64679  1.25346 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                  280.01650  144.53442   1.937   0.0663 .
# Year_published_online         -0.10966    0.07171  -1.529   0.1411  
# Phase2                       472.40634  193.16335   2.446   0.0234 *
#   Year_published_online:Phase2  -0.23349    0.09583  -2.437   0.0238 *
#
## Coefficients:
#   This section contains the heart of the output:
#   (Intercept): The intercept is the expected value of mean_of_medians when all other 
#   variables are zero. Here, it's estimated to be 280.01650. The p-value (0.0663) suggests 
#   that it is marginally non-significant at a 0.05 alpha level.
# 
# Year_published_online: 
#   This is the slope for Year_published_online for Phase 1 studies. It shows a negative value 
#   (-0.10966), implying a decreasing trend in the mean_of_medians over the years for Phase 1 
#   studies. However, with a p-value of 0.1411, this trend is not statistically significant at 
#   the 0.05 level.
# 
# Phase2: 
#   This shows the difference in the intercepts between Phase 1 and Phase 2. The intercept for 
#   Phase 2 studies would be 280.01650 + 472.40634. This is significant (p-value: 0.0234), indicating 
#   that there is a meaningful difference in the intercepts between the two Phases.
# 
# Year_published_online:Phase2: 
#   This is the interaction term, and it shows the difference in slopes between Phase 1 and Phase 2. 
#   A negative value (-0.23349) suggests a stronger decreasing trend in Phase 2 compared to Phase 1. 
#   With a p-value of 0.0238, this is statistically significant, confirming that the rate of decrease 
#   in mean_of_medians is different for the two Phases.
# 
# In summary, your model suggests that there is a significant difference in the trends of mean_of_medians 
#   over the years between Phase 1 and Phase 2 studies. Specifically, the decreasing trend is significantly 
#   stronger in Phase 2 studies.

percentages_df_age <- d_complete_ratio_age %>%
  filter(year_group %in% c("2010-2012", "2019-2021"),  # Replace with the year groups you're interested in
         Med_age_cat %in% c("<55", "56-60")) %>%  # Replace with the drug types you're interested in
  group_by(year_group, Med_age_cat) %>%
  summarise(total = sum(ratio)) %>%
  mutate(percentage = (total / sum(total)) * 100)

print(percentages_df_age)

# Save the plot
# ggsave(file.path(plot_d, "Med_Age_Category_YearGroup.png"), plot = med_age_plot, width=1200, height=1200, units="px")

# Calculate the ratio within each category of "PS_ratio_cat" and "year_group"
d_complete_ratio_drug <- d_complete %>%
  count(year_group, DrugType) %>%
  group_by(year_group) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

# Plot the stacked bar graph
drugtype_year_plot <- ggplot(d_complete_ratio_drug, aes(x = year_group, y = ratio, fill = DrugType)) +
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() +
  labs(
    title = "Ratio of drugtype \nby year group",
    x = "Year Group",
    y = "Ratio",
    fill = "Drugtype Category"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("lightblue", "#00A88E", "#C9A8CE", "grey","white", "salmon")) +
  scale_y_continuous(labels = scales::percent) # You can add your custom colors using scale_fill_manual if needed

drugtype_year_plot

percentages_df_drugtype <- d_complete_ratio_drug %>%
       filter(year_group %in% c("2010-2012", "2019-2021"),  # Replace with the year groups you're interested in
                           DrugType %in% c("Chemotherapy")) %>%  # Replace with the drug types you're interested in
       group_by(year_group, DrugType) %>%
       summarise(total = sum(ratio)) %>%
       mutate(percentage = (total / sum(total)) * 100)

print(percentages_df_drugtype)

# ggsave(file.path(plot_d, "DrugType_Category_YearGroup.png"), plot = drugtype_year_plot, width=1300, height=1100, units="px")

# Calculate the ratio within each category of "PS_ratio_cat" and "year_group"
d_complete_ratio_drug_investigational <- d_complete %>%
  count(DrugType, Investigational_status) %>%
  group_by(DrugType) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

d_complete_ratio_drug_investigational_yg <- d_complete %>%
  count(year_group, Investigational_status) %>%
  group_by(year_group) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

# Plot the stacked bar graph
drugtype_investigational_plot <- ggplot(d_complete_ratio_drug_investigational, aes(x = DrugType, y = ratio, fill = Investigational_status)) +
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() +
  labs(
    title = "Ratio of drugtype \nby investigational status",
    x = "Drugtype",
    y = "Ratio",
    fill = "Investigational status"
  ) +   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("lightblue", "#00A88E", "#C9A8CE")) +
  scale_y_continuous(labels = scales::percent) # You can add your custom colors using scale_fill_manual if needed

drugtype_investigational_plot

percentages_df_drugtype_investigational_yg <- d_complete_ratio_drug_investigational_yg %>%
  filter(year_group %in% c("2010-2012", "2019-2021"),  # Replace with the year groups you're interested in
         Investigational_status %in% c("Investigational Monotherapy")) %>%  # Replace with the drug types you're interested in
  group_by(year_group, Investigational_status) %>%
  summarise(total = sum(ratio)) %>%
  mutate(inverse = 1-total)


print(percentages_df_drugtype_investigational_yg)

# ggsave(file.path(plot_d, "DrugType_Category_investigationalstatus.png"), plot = drugtype_investigational_plot, width=1700, height=1200, units="px")

# Calculate the ratio within each category 
d_complete_ratio_phase_investigational <- d_complete %>%
  count(Phase, Investigational_status) %>%
  group_by(Phase) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

# Plot the stacked bar graph
phase_investigational_plot <- ggplot(d_complete_ratio_phase_investigational, aes(x = Phase, y = ratio, fill = Investigational_status)) +
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() +
  labs(
    title = "Ratio of phase \nby investigational status",
    x = "Phase",
    y = "Ratio",
    fill = "Investigational status"
  ) +   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("lightblue", "#00A88E", "#C9A8CE")) +
  scale_y_continuous(labels = scales::percent) # You can add your custom colors using scale_fill_manual if needed

phase_investigational_plot

# ggsave(file.path(plot_d, "Phase_Category_investigationalstatus.png"), plot = phase_investigational_plot, width=1500, height=1200, units="px")

# Calculate the ratio within each category 
d_complete_ratio_phase_drugtype <- d_complete %>%
  count(Phase, DrugType) %>%
  group_by(Phase) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

# Plot the stacked bar graph
phase_drugtype_plot <- ggplot(d_complete_ratio_phase_drugtype, aes(x = Phase, y = ratio, fill = DrugType)) +
  geom_bar(stat="identity", position = "stack", color = "black") +
  theme_classic() +
  labs(
    title = "Ratio of phase \nby drug type",
    x = "Phase",
    y = "Ratio",
    fill = "Drug type"
  ) +   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("lightblue", "#00A88E", "#C9A8CE", "grey","white", "salmon"))+
  scale_y_continuous(labels = scales::percent) # You can add your custom colors using scale_fill_manual if needed

phase_drugtype_plot

# ggsave(file.path(plot_d, "Phase_Category_DrugType.png"), plot = phase_drugtype_plot, width=1200, height=1200, units="px")

# Output ----
# write_xlsx(d_top5, file.path(output, "filtered_db_231106.xlsx"))
# write.xlsx(d_complete, file.path(output, "filtered_db.xlsx"))

