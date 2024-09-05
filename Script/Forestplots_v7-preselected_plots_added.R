#load packages
library(metadat)
library(meta)
library(readxl)
library(dplyr)
library(metafor)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(grid)
library(patchwork)


rm(list=ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# script.dir <- dirname(sys.frame(1)$ofile) 
home_dir <- dirname(script_dir)
input <- file.path(home_dir, "Input")
output <- file.path(home_dir, "Output")
plot_dir <- file.path(home_dir, "Plots")
plot_dir_sum <- file.path(plot_dir, "summary")
plot_dir_com <- file.path(plot_dir, "complete")

db <- read_excel(file.path(output, "filtered_db_231023.xlsx"))

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

db$N_pat_GR3 <- db$`N_pat_GR3+` # om te zorgen dat je deze variabele makkelijker kan aanroepen
db$N_events_GR3 <- db$`N_events_Gr3+` # om te zorgen dat je deze variabele makkelijker kan aanroepen
db$N_events_top5_GR3 <- db$`N_events_top5_Gr3+` # om te zorgen dat je deze variabele makkelijker kan aanroepen


# Eigen dataset --------------------------
# db$N_total_check <- db$N_PD + db$N_SD_short + db$N_PR+ db$N_CR
# db_N_check <- db$N_total_check == db$N_eval_eff_total
# db_N_check <- cbind(db$Aut_1, db$Year_published_online, db_N_check) # check N totaal = PD + SD + PR + CR
# #db_N_check_false <- db_N_check %>% filter(db_N_check == FALSE)

# db$N_SD_shortlong <- ifelse(is.na(db$N_SD_short), db$N_SD_long, db$N_SD_short)
db$events_CBR <- db$N_SD_14 + db$N_PR + db$N_CR
db$events_ORR <- db$N_PR + db$N_CR
db$safetytimes5 <- db$N_pat_safety*5
db$studlab <- paste(db$Aut_1, db$Year_published_online, sep = ", ")

#categories voorbehandeling
db$Med_lines <- as.numeric(db$Med_lines)
#mean(db$Med_lines, na.rm = TRUE)  #3.5 mean   
db$Med_lines_35 <- ifelse(db$Med_lines <3.5, "< 3.5", ">= 3.5")

#categories PS 
db$PS_ratio10 <- ifelse(db$PS_ratio <0.1, "< 10% PS 2", ">= 10% PS 2")
db$PS_ratio010 <- ifelse(db$PS_ratio == 0, "No PS 2",
                        ifelse(db$PS_ratio > 0 & db$PS_ratio < 0.1, "0-10% PS 2", ">10% PS 2"))

#categories median age
# db_med_age_cat <- db_events_CBR %>% filter(!is.na(db_events_CBR$Med_age_cat))

db$Targeted <- ifelse(db$Cohort_target == "Not targeted", "Not targeted", "Targeted")

# Categories for fun:
journal_counts <- table(db$Journal)
top_10_journals <- names(sort(journal_counts, decreasing=TRUE)[1:10])
top_10_journals_db <- db %>% filter(Journal %in% top_10_journals)

#plots----------------------
create_forestplot <- function(data, events_col, n_col,  plot_name,  phase = NULL) {
  # Filter by phase if specified
  if (!is.null(phase)) {
    data <- data %>% filter(Phase == phase)
  }
  
  # Filter out missing values for specified columns and sort in alphabetic order
  data <- data %>% 
    filter(!is.na(data[[events_col]]) & !is.na(data[[n_col]]))
  data <- arrange(data, Aut_1) # zet de db op alfabetische volgorde
  
  
  plot_result <- metaprop(data[[events_col]], data[[n_col]], data[["studlab"]], 
                          sm = "PLOGIT", 
                          comb.fixed = FALSE, 
                          comb.random = TRUE, 
                          method = "Inverse", 
                          control = list(stepadj = 0.5))
  forest_result <- forest(plot_result)
  plot_height = 100 + length(forest_result$studlab) * 20 # past hoogte van plot aan aan het aantal studies in de categorie
  rstudioapi::savePlotAsImage(file.path(plot_dir, paste0(plot_name, ".png")), width = 850, height = plot_height)
}

# Simpele forestplots
create_forestplot(db, "events_CBR", "N_eval_eff_total", "CBR")
create_forestplot(db, "events_ORR", "N_eval_eff_total", "ORR")
create_forestplot(db, "N_DLT", "N_pat_safety", "DLT_phase1", phase = 1)
create_forestplot(db, "N_Disc", "N_pat_safety", "DrugDisc_phase2", phase = 2)
create_forestplot(db, "N_events_top5_Gr3+", "safetytimes5", "top5")
create_forestplot(db, "N_events_top5_Gr3+", "safetytimes5", "top5_phase1", phase = 1)
create_forestplot(db, "N_events_top5_Gr3+", "safetytimes5", "top5_phase2", phase = 2)
create_forestplot(db, "N_pat_GR3", "N_pat_safety", "pat_gr3")
create_forestplot(db, "N_pat_GR3", "N_pat_safety", "pat_gr3_phase1", phase = 1)
create_forestplot(db, "N_pat_GR3", "N_pat_safety", "pat_gr3_phase2", phase = 2)

create_summary_forestplot <- function(data, events_col, n_col,  plot_name, plot_title, category, phase = NULL, Width = 1700, plot_complete = FALSE) {
  # Filter by phase if specified
  if (!is.null(phase)) {
    data <- data %>% filter(Phase == phase)
    plot_title <- paste(plot_title, "for phase", phase, "trials") 
  }
  
  # Filter out missing values for specified columns and sort in alphabetic order
  data <- data %>% 
    filter(!is.na(data[[events_col]]) & !is.na(data[[n_col]]) & !is.na(data[[category]]))
  
  # Calculate the number of studies in each category
  n_category <- length(unique(data[[category]]))
  n_studies <- data %>% group_by(data[[category]]) %>% summarise(n_studies = n()) 
  n_studies <- n_studies %>% rename(category = "data[[category]]")
  
  plot_result <- metaprop(data[[events_col]], data[[n_col]], data[["studlab"]], 
                          byvar = data[[category]],
                          sm = "PLOGIT", 
                          comb.fixed = FALSE, 
                          comb.random = TRUE, 
                          method = "Inverse", 
                          control = list(maxiter = 1000, tol = 1e-06, stepadj = 0.2, verbose = FALSE), #eerder hier alleen stepadj = 0.5, maar dat gaf @23-10-2023 problemen die er eerder niet waren. Daarom nu aangepast.
                          prediction.subgroup=TRUE, 
                          test.subgroup=TRUE, 
                          overall = FALSE, 
                          overall.hetstat = FALSE)
  forest_result <- forest(plot_result)
  plot_height = 100 + length(forest_result$studlab) * 20 + n_category * 80 # past hoogte van plot aan aan het aantal studies in de categorie
  
  if (plot_complete == TRUE) {
      rstudioapi::savePlotAsImage(file.path(plot_dir_com, paste0(plot_name, "_all.png")), width = 850, height = plot_height)
  }
  
  Estimate<-transf.ilogit(plot_result$TE.random.w) #back transformation logit
  Upper<-transf.ilogit(plot_result$upper.random.w)
  Lower<-transf.ilogit(plot_result$lower.random.w)
  
  df_estimate <- as.data.frame(Estimate)
  df_upper <- as.data.frame(Upper)
  df_lower <- as.data.frame(Lower)
  
  df_estimate$category <- row.names(df_estimate)
  df_upper$category <- row.names(df_upper)
  df_lower$category <- row.names(df_lower)
  
  merge1 <- merge(df_estimate, df_upper)
  merge2 <- merge(merge1, df_lower)
  merge2$index <- 1:n_category
  merge2
  merge3 <- merge(merge2, n_studies)
  
  merge <- merge3 %>% arrange(index)
  
  cat_n <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = n_studies), vjust = 0) +
    geom_hline(yintercept = 11.6, linewidth = 2) +
    labs(title = '', x = 'N', y = 'Category') +
    ggtitle("N") +
    theme_classic(base_size = 9) +
    xlab("  ") +
    theme(
      axis.line.y = element_blank(),
      axis.line.x = element_line(color = "white"),
      # axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = "white"),
      axis.ticks.length = unit(0.3, "cm"),
      axis.title.y = element_blank(),
      axis.text.x = element_text(color = "white"),
      # plot.title = element_text(hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_text(vjust = -.2, size = 11)
    )
  
  t1 <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = sprintf("%0.2f", Estimate)), vjust = 0) +
    geom_hline(yintercept = 11.6, linewidth = 2) +
    ggtitle("Proportion") +
    xlab("  ") +
    theme_classic(base_size = 9) +
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
  
  
  t2 <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = sprintf("(%0.2f; %0.2f)", Lower, Upper)), vjust = 0) +
    geom_hline(yintercept = 11.6, linewidth = 2) +
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
  
  plot_category <- ggplot(data=merge, aes(y=-index, x=Estimate, xmin=Lower, xmax=Upper)) +
    labs(x = "Effect size (95%-CI)")+
    ggtitle("Effect size") +
    geom_point() + 
    geom_errorbarh(height=.1) +
    scale_y_continuous(breaks=1:nrow(merge), labels=merge$category) +
    #geom_vline(xintercept=0.06, color='black', linetype='dashed', alpha=.5) +
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
  
  
  #Put the individual components of the forest plot together
  plot <- cat_n + t1 + t2 + plot_category + 
    plot_annotation(title=plot_title, theme=theme(plot.title=element_text(hjust=0.5, size = 10))) +
    plot_layout(design="12344")
  plot
  Height = 400 + n_category * 100
  ggsave(file.path(plot_dir_sum, paste0(plot_name, "_summary.png")), plot=plot, width=Width, height=Height, units="px")
}  
  
#ORR
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year", plot_title = "ORR by year", category="year_group")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year_p1", plot_title = "ORR by year", phase=1, category="year_group")  
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year_p2", plot_title ="ORR by year", phase=2, category="year_group") 
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_phase", plot_title = "ORR by clinical trial phase", category="Phase", plot_complete = TRUE)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_drugtype", plot_title = "ORR by tested compound category", category="DrugType", Width=2300,  plot_complete = TRUE)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_drugtype_p1", plot_title = "ORR by tested compound category", phase=1, category="DrugType", Width=2300,  plot_complete = TRUE)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_drugtype_p2", plot_title = "ORR by tested compound category", phase=2, category="DrugType", Width=2300,  plot_complete = TRUE)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Investigational", plot_title = "ORR by investigational status", category="Investigational_status", Width=2700)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Investigational_p1", plot_title = "ORR by investigational status", phase=1, category="Investigational_status", Width=2700)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Investigational_p2", plot_title = "ORR by investigational status", phase=2, category="Investigational_status", Width=2700)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_PS", plot_title = "ORR by performance status categories", category="PS_ratio010")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Targeted", plot_title = "ORR by if targeted or not", category="Targeted")
create_summary_forestplot(top_10_journals_db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Journal", plot_title = "ORR by Journal", category="Journal", Width=2800)

#CBR
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year", plot_title = "CBR by year", category="year_group")  
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year_p1", plot_title = "CBR by year", phase=1, category="year_group")  
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year_p2", plot_title = "CBR by year", phase=2, category="year_group")  
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_phase", plot_title = "CBR by clinical trial phase", category="Phase", plot_complete = TRUE)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_drugtype", plot_title = "CBR by tested compound category", category="DrugType", Width=2300, plot_complete = TRUE)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_drugtype_p1", plot_title = "CBR by tested compound category", phase=1, category="DrugType", Width=2300, plot_complete = TRUE)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_drugtype_p2", plot_title = "CBR by tested compound category", phase=2, category="DrugType", Width=2300, plot_complete = TRUE)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Investigational", plot_title = "CBR by investigational status", category="Investigational_status", Width=2700)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Investigational_p1", plot_title = "CBR by investigational status", phase=1, category="Investigational_status", Width=2700)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Investigational_p2", plot_title = "CBR by investigational status", phase=2, category="Investigational_status", Width=2700)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_PS", plot_title = "CBR by performance status categories", category="PS_ratio010")
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Targeted", plot_title = "CBR by if targeted or not", category="Targeted")
create_summary_forestplot(top_10_journals_db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Journal", plot_title = "CBR by Journal", category="Journal", Width=2800)

# create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_extractor", plot_title = "CBR by extractor", category="Extraction", Width = 1500)

#plot patients grade 3+ 
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year", plot_title = "Patients with any grade 3 or higher toxicity by year", category="year_group", Width=1500)  
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year_p1", plot_title = "Patients with any grade 3 or higher toxicity by year", category="year_group", phase = 1, Width=1500)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year_p2", plot_title = "Patients with any grade 3 or higher toxicity by year", category="year_group", phase = 2, Width=1500)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_phase", plot_title = "Patients with any grade 3 or higher toxicity by clinical trial phase", category="Phase", Width=1500)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_drugtype", plot_title = "Patients with any grade 3 or higher toxicity by tested compound category",category="DrugType", Width=2200)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_targeted", plot_title = "Patients with any grade 3 or higher toxicity by if population was targeted",category="Targeted", Width=1700)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_investigationalstat", plot_title = "Patients with any grade 3 or higher toxicity by investigational status",category="Investigational_status", Width=2700)

#plot top 5 toxicity
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year", plot_title = "Top 5 category gr3+ events ratio by year", category="year_group")  
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year_p1", plot_title = "Top 5 category gr3+ events ratio by year", category="year_group", phase = 1)  
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year_p2", plot_title = "Top 5 category gr3+ events ratio by year", category="year_group", phase = 2)  
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_phase", plot_title = "Top 5 category gr3+ events ratio by clinical trial phase", category="Phase", Width=1600)
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_drugtype", plot_title = "Top 5 category gr3+ events ratio by tested compound category", category="DrugType", Width=2100)
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_targeted", plot_title = "Top 5 category gr3+ events ratio by if population was targeted", category="Targeted", Width=1700)
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_investigationalstat", plot_title = "Top 5 category gr3+ events ratio by investigational status", category="Investigational_status", Width=2700)

#plot DLT toxicity (phase 1) subgroup
create_summary_forestplot(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_year_p1", plot_title = "Patients with a DLT by year category", category="year_group", phase = 1)
# create_summary_forestplot(db, "N_DLT", "N_eval_DLT", plot_name = "DLT_eval_DLT_p1", plot_title = "Patients with a DLT by year category", category="year_group", phase = 1)
create_summary_forestplot(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_drugtype_p1", plot_title = "Patients with a DLT by tested compound category", category="DrugType", phase = 1, Width=2000)

#plot discontinuation toxicity (phase 2) subgroup
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_phase", plot_title = "Patients that discontinued due to adverse events by clinical trial phase", category="Phase", Width=1800, plot_complete =  TRUE)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_year_p2", plot_title = "Patients that discontinued due to adverse events by year category", category="year_group", phase = 2, Width=1800)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_year", plot_title = "Patients that discontinued due to adverse events by year category", category="year_group", Width=1800)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_drugtype_p2", plot_title = "Patients that discontinued due to adverse events by tested compound category", category="DrugType", phase = 2, Width=2000)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_targeted_p2", plot_title = "Patients that discontinued due to adverse events by if targeted or not", category="Targeted", phase = 2, Width=2000)

#plot CBR subgroup age median 60


#plot CBR subgroup lines


create_summary_forestplot_investigational <- function(data, events_col, n_col,  plot_name, plot_title, category, phase = NULL, Width = 3000, plot_complete = FALSE) {
  # Filter by phase if specified
  if (!is.null(phase)) {
    data <- data %>% filter(Phase == phase)
    plot_title <- paste(plot_title, "for phase", phase, "trials") 
  }
  
  # Filter out missing values for specified columns and sort in alphabetic order
  data <- data %>% 
    filter(!is.na(data[[events_col]]) & !is.na(data[[n_col]]) & !is.na(data[[category]]))
  
  
  
  # Calculate the number of studies in each category
  data <- data %>% mutate(investigational_category = paste(data[[category]], Investigational_status))
  n_category <- length(unique(data$investigational_category))
  
  n_studies <- data %>% group_by(data[[category]], Investigational_status, investigational_category) %>% summarise(n_studies = n()) 
  n_studies <- n_studies %>% rename(main_category = "data[[category]]", category = investigational_category)
  
  plot_result <- metaprop(data[[events_col]], data[[n_col]], data[["studlab"]], 
                          subgroup = data[["investigational_category"]],
                          sm = "PLOGIT", 
                          comb.fixed = FALSE, 
                          comb.random = TRUE, 
                          method = "Inverse", 
                          control = list(stepadj = 0.5),
                          prediction.subgroup=TRUE, 
                          test.subgroup=TRUE, 
                          overall = FALSE, 
                          overall.hetstat = FALSE)
  forest_result <- forest(plot_result)
  plot_height = 100 + length(forest_result$studlab) * 20 + n_category * 80 # past hoogte van plot aan aan het aantal studies in de categorie
  
  if (plot_complete == TRUE) {
    rstudioapi::savePlotAsImage(file.path(plot_dir_com, paste0(plot_name, "_investigational_all.png")), width = 950, height = plot_height)
  }
  
  Estimate<-transf.ilogit(plot_result$TE.random.w) #back transformation logit
  Upper<-transf.ilogit(plot_result$upper.random.w)
  Lower<-transf.ilogit(plot_result$lower.random.w)
  
  df_estimate <- as.data.frame(Estimate)
  df_upper <- as.data.frame(Upper)
  df_lower <- as.data.frame(Lower)
  
  df_estimate$category <- row.names(df_estimate)
  df_upper$category <- row.names(df_upper)
  df_lower$category <- row.names(df_lower)
  
  merge1 <- merge(df_estimate, df_upper)
  merge2 <- merge(merge1, df_lower)
  merge2$index <- 1:n_category
  merge2
  merge3 <- merge(merge2, n_studies)
  
  merge <- merge3 %>% arrange(index)
  
  main_cat <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = main_category), vjust = 0, size = 3) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
    labs(title = '', x = 'N', y = 'Category') +
    ggtitle("Category") +
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
  
  invest <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = Investigational_status), vjust = 0, size = 3) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
    labs(title = '', x = 'N', y = 'Category') +
    ggtitle("Investigational_status") +
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
  
  n <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = n_studies), vjust = 0, size = 4) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
    labs(title = '', x = 'N', y = 'Category') +
    ggtitle("N") +
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
  
  t1 <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = sprintf("%0.2f", Estimate)), vjust = 0) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
    ggtitle("Proportion") +
    xlab("  ") +
    theme_classic(base_size = 9) +
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
  
  
  t2 <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = sprintf("(%0.2f; %0.2f)", Lower, Upper)), vjust = 0) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
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
  
  plot_category <- ggplot(data=merge, aes(y=-index, x=Estimate, xmin=Lower, xmax=Upper)) +
    labs(x = "Effect size (95%-CI)")+
    ggtitle("Effect size") +
    geom_point() + 
    geom_errorbarh(height=.1) +
    scale_y_continuous(breaks=1:nrow(merge), labels=merge$category) +
    #geom_vline(xintercept=0.06, color='black', linetype='dashed', alpha=.5) +
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
  
  
  #Put the individual components of the forest plot together
  plot <- main_cat + invest + n + t1 + t2 + plot_category + 
    plot_annotation(title=plot_title, theme=theme(plot.title=element_text(hjust=0.5, size = 10))) +
    plot_layout(design="1111222222344556666")
  plot
  Height = 400 + n_category * 100
  ggsave(file.path(plot_dir_sum, paste0(plot_name, "_summary_investigational.png")), plot=plot, width=Width, height=Height, units="px")
}  

# create_summary_forestplot_investigational(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year", plot_title = "ORR by year and investigational status", category="year_group")
# create_summary_forestplot_investigational(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year_p1", plot_title = "ORR by year and investigational status", phase=1, category="year_group")  
# create_summary_forestplot_investigational(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year_p2", plot_title ="ORR by year and investigational status", phase=2, category="year_group") 
create_summary_forestplot_investigational(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_phase", plot_title = "ORR by clinical trial phase and investigational status", category="Phase")
create_summary_forestplot_investigational(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_drugtype", plot_title = "ORR by tested compound category and investigational status", category="DrugType")
create_summary_forestplot_investigational(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Targeted", plot_title = "ORR by if targeted or not and investigational status", category="Targeted")



#CBR
# create_summary_forestplot_investigational(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year", plot_title = "CBR by year and investigational status", category="year_group")
# create_summary_forestplot_investigational(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year_p1", plot_title = "CBR by year and investigational status", phase=1, category="year_group")
# create_summary_forestplot_investigational(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year_p2", plot_title = "CBR by year and investigational status", phase=2, category="year_group")
create_summary_forestplot_investigational(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_phase", plot_title = "CBR by clinical trial phase and investigational status", category="Phase")
create_summary_forestplot_investigational(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_drugtype", plot_title = "CBR by tested compound category and investigational status", category="DrugType")
create_summary_forestplot_investigational(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Targeted", plot_title = "CBR by if targeted or not and investigational status", category="Targeted")

# create_summary_forestplot_investigational(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_extractor", plot_title = "CBR by extractor and investigational status", category="Extraction", Width = 1500)

#plot patients grade 3+
# create_summary_forestplot_investigational(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year", plot_title = "Patients with any grade 3 or higher toxicity by year and investigational status", category="year_group", Width=1500)
# create_summary_forestplot_investigational(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year_p1", plot_title = "Patients with any grade 3 or higher toxicity by year and investigational status", category="year_group and investigational status", phase = 1, Width=1500)
# create_summary_forestplot_investigational(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year_p2", plot_title = "Patients with any grade 3 or higher toxicity by year and investigational status", category="year_group and investigational status", phase = 2, Width=1500)
create_summary_forestplot_investigational(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_phase", plot_title = "Patients with any grade 3 or higher toxicity by clinical trial phase and investigational status", category="Phase")
create_summary_forestplot_investigational(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_drugtype", plot_title = "Patients with any grade 3 or higher toxicity by tested compound category",category="DrugType")
create_summary_forestplot_investigational(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_targeted", plot_title = "Patients with any grade 3 or higher toxicity by if population was targeted",category="Targeted")

#plot top 5 toxicity
# create_summary_forestplot_investigational(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year", plot_title = "Top 5 category gr3+ events ratio by year and investigational status", category="year_group")
# create_summary_forestplot_investigational(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year_p1", plot_title = "Top 5 category gr3+ events ratio by year and investigational status", category="year_group and investigational status", phase = 1)
# create_summary_forestplot_investigational(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year_p2", plot_title = "Top 5 category gr3+ events ratio by year and investigational status", category="year_group and investigational status", phase = 2)
create_summary_forestplot_investigational(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_phase", plot_title = "Top 5 category gr3+ events ratio by clinical trial phase and investigational status", category="Phase")
create_summary_forestplot_investigational(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_drugtype", plot_title = "Top 5 category gr3+ events ratio by tested compound category and investigational status", category="DrugType")

#plot DLT toxicity (phase 1) subgroup
# create_summary_forestplot_investigational(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_year", plot_title = "Patients with a DLT by year category and investigational status", category="year_group and investigational status", phase = 1)
# create_summary_forestplot_investigational(db, "N_DLT", "N_eval_DLT", plot_name = "DLT_eval_DLT_p1", plot_title = "Patients with a DLT by year category and investigational status", category="year_group and investigational status", phase = 1)
# create_summary_forestplot_investigational(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_drugtype", plot_title = "Patients with a DLT by tested compound category and investigational status", category="DrugType and investigational status", phase = 1, Width=2000)

#plot discontinuation toxicity (phase 2) subgroup
# create_summary_forestplot_investigational(db, "N_Disc", "N_pat_safety", plot_name = "Disc_all_safety_year", plot_title = "Patients that discontinued due to adverse events by year category and investigational status", category="year_group and investigational status", phase = 2, Width=1800)
# create_summary_forestplot_investigational(db, "N_Disc", "N_pat_safety", plot_name = "Disc_all_safety_drugtype", plot_title = "Patients that discontinued due to adverse events by tested compound category and investigational status", category="DrugType and investigational status", phase = 2, Width=2000)
# create_summary_forestplot_investigational(db, "N_Disc", "N_pat_safety", plot_name = "Disc_all_safety_targeted", plot_title = "Patients that discontinued due to adverse events by if targeted or not and investigational status", category="Targeted and investigational status", phase = 2, Width=2000)

#plot CBR subgroup age median 60

create_summary_forestplot_preselected <- function(data, events_col, n_col,  plot_name, plot_title, category, phase = NULL, Width = 3000, plot_complete = FALSE) {
  # Filter by phase if specified
  if (!is.null(phase)) {
    data <- data %>% filter(Phase == phase)
    plot_title <- paste(plot_title, "for phase", phase, "trials") 
  }
  
  # Filter out missing values for specified columns and sort in alphabetic order
  data <- data %>% 
    filter(!is.na(data[[events_col]]) & !is.na(data[[n_col]]) & !is.na(data[[category]]))
  
  # Calculate the number of studies in each category
  data <- data %>% mutate(targeted_category = paste(data[[category]], Targeted))  # Assuming 'Targeted' is the column that specifies if it's targeted or not
  n_category <- length(unique(data$targeted_category))
  
  n_studies <- data %>% group_by(data[[category]], Targeted, targeted_category) %>% summarise(n_studies = n()) 
  n_studies <- n_studies %>% rename(main_category = "data[[category]]", category = targeted_category)
  
  plot_result <- metaprop(data[[events_col]], data[[n_col]], data[["studlab"]], 
                          subgroup = data[["targeted_category"]],
                          sm = "PLOGIT", 
                          comb.fixed = FALSE, 
                          comb.random = TRUE, 
                          method = "Inverse", 
                          control = list(stepadj = 0.5),
                          prediction.subgroup=TRUE, 
                          test.subgroup=TRUE, 
                          overall = FALSE, 
                          overall.hetstat = FALSE)
  forest_result <- forest(plot_result)
  plot_height = 100 + length(forest_result$studlab) * 20 + n_category * 80 # past hoogte van plot aan aan het aantal studies in de categorie
  
  if (plot_complete == TRUE) {
    rstudioapi::savePlotAsImage(file.path(plot_dir_com, paste0(plot_name, "_targeted_all.png")), width = 950, height = plot_height)
  }
  
  Estimate<-transf.ilogit(plot_result$TE.random.w) #back transformation logit
  Upper<-transf.ilogit(plot_result$upper.random.w)
  Lower<-transf.ilogit(plot_result$lower.random.w)
  
  df_estimate <- as.data.frame(Estimate)
  df_upper <- as.data.frame(Upper)
  df_lower <- as.data.frame(Lower)
  
  df_estimate$category <- row.names(df_estimate)
  df_upper$category <- row.names(df_upper)
  df_lower$category <- row.names(df_lower)
  
  merge1 <- merge(df_estimate, df_upper)
  merge2 <- merge(merge1, df_lower)
  merge2$index <- 1:n_category
  merge2
  merge3 <- merge(merge2, n_studies)
  
  merge <- merge3 %>% arrange(index)
  
  main_cat <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = main_category), vjust = 0, size = 3) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
    labs(title = '', x = 'N', y = 'Category') +
    ggtitle("Category") +
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
  
  invest <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = Targeted), vjust = 0, size = 3) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
    labs(title = '', x = 'N', y = 'Category') +
    ggtitle("Investigational_status") +
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
  
  n <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = n_studies), vjust = 0, size = 4) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
    labs(title = '', x = 'N', y = 'Category') +
    ggtitle("N") +
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
  
  t1 <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = sprintf("%0.2f", Estimate)), vjust = 0) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
    ggtitle("Proportion") +
    xlab("  ") +
    theme_classic(base_size = 9) +
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
  
  
  t2 <- ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = sprintf("(%0.2f; %0.2f)", Lower, Upper)), vjust = 0) +
    # geom_hline(yintercept = 11.6, linewidth = 2) +
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
  
  plot_category <- ggplot(data=merge, aes(y=-index, x=Estimate, xmin=Lower, xmax=Upper)) +
    labs(x = "Effect size (95%-CI)")+
    ggtitle("Effect size") +
    geom_point() + 
    geom_errorbarh(height=.1) +
    scale_y_continuous(breaks=1:nrow(merge), labels=merge$category) +
    #geom_vline(xintercept=0.06, color='black', linetype='dashed', alpha=.5) +
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
  
  
  #Put the individual components of the forest plot together
  plot <- main_cat + invest + n + t1 + t2 + plot_category + 
    plot_annotation(title=plot_title, theme=theme(plot.title=element_text(hjust=0.5, size = 10))) +
    plot_layout(design="1111222222344556666")
  plot
  Height = 400 + n_category * 100
  ggsave(file.path(plot_dir_sum, paste0(plot_name, "_summary_targeted.png")), plot=plot, width=Width, height=Height, units="px")
}  

create_summary_forestplot_preselected(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_drugtype", plot_title = "ORR by tested compound category and if trial population was preselected", category="DrugType")
create_summary_forestplot_preselected(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_drugtype", plot_title = "CBR by tested compound category and if trial population was preselected", category="DrugType")
