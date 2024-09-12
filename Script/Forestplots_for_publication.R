# Forestplots_for_publication.R
#
# This script generates forest plots for a meta-analysis of clinical trials
# in metastatic colorectal cancer. It analyzes efficacy and safety outcomes
# across different trial phases, drug types, and other subgroups.
#
# Last updated: 12-09-2024
# Authors: M.A. Huismans and L.P. Smabers
#
# Package versions used in this script:
# metadat: 1.2.0
# meta: 7.0.0
# readxl: 1.4.3
# dplyr: 1.1.4
# metafor: 4.6.0
# ggplot2: 3.5.1
# ggpubr: 0.6.0
# cowplot: 1.1.3
# grid: 4.4.0
# patchwork: 1.2.0
# stringr: 1.5.1
# tidyr: 1.3.1

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
library(stringr)
library(tidyr)

# Clear workspace
rm(list=ls())

# Set up directories
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
home_dir <- dirname(script_dir)
input <- file.path(home_dir, "Input")
plot_dir <- file.path(home_dir, "Plots")
plot_dir_sum <- file.path(plot_dir, "summary")
plot_dir_com <- file.path(plot_dir, "complete")

# Load data
db <- read_excel(file.path(input, "filtered_database.xlsx"))

# Data Preparation ----
# Create variable for number of evaluable patients (CRC or all cancer types)
db <- db %>%
  mutate(N_eval_eff_total = case_when(
    CRC_specific == "1" ~ N_eva_eff_crc,
    CRC_specific == "0" ~ N_eva_eff,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(N_eval_eff_total))

# Calculate performance status ratios
db <- db %>%
  mutate(
    PS_0_1 = PS_0 + PS_1,
    PS_ratio = PS_2 / (PS_2 + PS_0_1)
  )

# Create age categories
db <- db %>%
  mutate(Med_age_cat = cut(
    as.numeric(Med_age),
    breaks = c(0, 55, 60, 65, Inf),
    labels = c("<55", "56-60", "61-65", ">65"),
    include.lowest = TRUE
  ))

# Rename safety variables for easier access
db <- db %>%
  rename(
    N_pat_GR3 = `N_pat_GR3+`,
    N_events_GR3 = `N_events_Gr3+`,
    N_events_top5_GR3 = `N_events_top5_Gr3+`
  )

# Calculate efficacy outcomes
db <- db %>%
  mutate(
    events_CBR = N_SD_14 + N_PR + N_CR,
    events_ORR = N_PR + N_CR,
    events_CBR_MSI = SD14_MSI + PR_MSI + CR_MSI,
    events_CBR_MSS = SD14_MSS + PR_MSS + CR_MSS,
    events_ORR_MSI = PR_MSI + CR_MSI,
    events_ORR_MSS = PR_MSS + CR_MSS,
    events_n_eva_MSI = n_eva_MSI,
    events_n_eva_MSS = n_eva_MSS
  )

# Prepare MSI/MSS data
db_MS <- db %>%
  dplyr::select(Aut_1, Journal, starts_with("events_")) %>%
  filter(!is.na(events_CBR_MSS))

long_MS <- db_MS %>%
  pivot_longer(
    cols = starts_with("events_"),
    names_to = c(".value", "MSS_MSI"),
    names_pattern = "events_(CBR|ORR|n_eva)_(MSS|MSI)"
  ) %>%
  filter(n_eva != 0)

# Create additional categories
db <- db %>%
  mutate(
    Med_lines_35 = if_else(Med_lines < 3.5, "< 3.5", ">= 3.5"),
    PS_ratio10 = if_else(PS_ratio < 0.1, "< 20% PS 2", ">= 10% PS 2"),
    PS_ratio010 = case_when(
      PS_ratio == 0 ~ "No PS 2",
      PS_ratio > 0 & PS_ratio < 0.1 ~ "0-10% PS 2",
      PS_ratio >= 0.1 ~ ">10% PS 2"
    ),
    Targeted = if_else(Cohort_target == "Not targeted", "Not targeted", "Targeted"),
    ratio_MSI = n_eva_MSI / (n_eva_MSI + n_eva_MSS),
    MSI_20 = if_else(ratio_MSI < 0.2, "< 20% MSI", ">= 20% MSI"),
    studlab = paste0(Aut_1, ", ", Year_published_online)
  )

#plots----------------------
# Function to create basic forest plots
create_forestplot <- function(data, events_col, n_col, plot_name, phase = NULL) {
  # Filter by phase if specified
  if (!is.null(phase)) {
    data <- data %>% filter(Phase == phase)
  }
  
  # Prepare data for meta-analysis
  data <- data %>% 
    filter(!is.na(!!sym(events_col)) & !is.na(!!sym(n_col))) %>%
    arrange(Aut_1)
  
  # Perform meta-analysis
  plot_result <- metaprop(
    event = data[[events_col]], 
    n = data[[n_col]], 
    studlab = data[["studlab"]], 
    sm = "PLOGIT", 
    comb.fixed = FALSE, 
    comb.random = TRUE, 
    method = "Inverse", 
    control = list(stepadj = 0.5)
  )
  
  # Generate forest plot
  forest_result <- forest(plot_result)
  
  # Calculate plot dimensions
  plot_height <- 100 + length(forest_result$studlab) * 20
  
  # Save plot
  rstudioapi::savePlotAsImage(
    file.path(plot_dir, paste0(plot_name, ".png")), 
    width = 850, 
    height = plot_height
  )
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

# Function to generate category plot
generate_category_plot <- function(merge) {
  ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, label = n_studies), vjust = 0) +
    geom_hline(yintercept = 11.6, linewidth = 2) +
    labs(title = '', x = 'N', y = 'Category') +
    ggtitle("N") +
    theme_classic(base_size = 9) +
    xlab("  ") +
    theme(
      axis.line.y = element_blank(),
      axis.line.x = element_line(color = "white"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = "white"),
      axis.ticks.length = unit(0.3, "cm"),
      axis.title.y = element_blank(),
      axis.text.x = element_text(color = "white"),
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_text(vjust = -.2, size = 11)
    )
}

# Function to generate estimate plot
generate_estimate_plot <- function(merge) {
  ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, 
                  label = ifelse(is.na(Estimate), "", sprintf("%0.1f", Estimate*100))), 
              vjust = 0) +
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
}

# Function to generate CI plot
generate_ci_plot <- function(merge) {
  ggplot(data = merge) +
    geom_text(aes(y = reorder(category, -index), x = 1, 
                  label = sprintf("(%0.1f; %0.1f)", Lower*100, Upper*100)), 
              vjust = 0) +
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
}

# Function to generate forest plot
generate_forest_plot <- function(merge, events_col) {
  if (events_col == "events_ORR") {
    limits <- c(0, 40)
    breaks <- c(0, 40)
  } else if (events_col == "events_CBR") {
    limits <- c(0, 100)
    breaks <- c(0, 100)
  } else if (events_col == "N_pat_GR3") {
    limits <- c(0, 100)
    breaks <- c(0, 100)
  } else if (events_col == "N_Disc") {
    limits <- c(0, 40)
    breaks <- c(0, 40)
  } else if (events_col == "N_DLT") {
    limits <- c(0, 50)
    breaks <- c(0, 50)
  } else {
    limits <- c(0, 100)  # Default limits
    breaks <- c(0, 100)
  }  
  
  ggplot(data=merge, aes(y=-index, x=Estimate*100, xmin=Lower*100, xmax=Upper*100)) +
    labs(x = "Effect size (95%-CI)") +
    ggtitle("Effect size") +
    geom_point(shape = 22, fill = "grey", size = 3) + 
    geom_linerange(aes(xmin=Lower*100, xmax=Upper*100), size = 0.5)+
    scale_y_continuous(breaks=1:nrow(merge), labels=merge$category) +
    geom_rect(data=merge[nrow(merge), ], 
              aes(xmin = Lower*100, xmax = Upper*100, 
                  ymin = -nrow(merge)-0.05, ymax = -nrow(merge)+0.05),
              fill = "darkred", linetype = 0) +
    theme_classic(base_size = 9) +
    theme(
      axis.line.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y  = element_blank(),
      axis.title.y  = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(7, 1, 19, 1, unit = "pt")   
    ) +
    scale_x_continuous(breaks = breaks, labels = breaks,  limits = limits)
}

# Function to generate subgroup annotation
generate_subgroup_annotation <- function(plot_result) {
  if (plot_result$pval.Q.b.random < 0.001) {
    paste("Test for subgroup differences: p <", "0.001")
  } else {
    paste("Test for subgroup differences: p =", sprintf("%.3f", plot_result$pval.Q.b.random))
  }
}

create_summary_forestplot <- function(data, events_col, n_col, plot_name, plot_title, category, 
                                      phase = NULL, Width = 1700, plot_complete = TRUE, 
                                      use_prediction_intervals = FALSE) {
  # Prepare data
  prep_data <- prepare_summary_data(data, events_col, n_col, category, phase)
  
  # Perform meta-analysis
  plot_result <- perform_summary_meta_analysis(prep_data$data, events_col, n_col, category)
  
  # Prepare plot data
  merge <- prepare_plot_data(plot_result, prep_data$n_studies, use_prediction_intervals)
  
  # Generate individual plot components
  cat_n <- generate_category_plot(merge)
  t1 <- generate_estimate_plot(merge)
  t2 <- generate_ci_plot(merge)
  plot_category <- generate_forest_plot(merge, events_col)
  
  # Combine plots
  combined_plot <- plot_grid(
    cat_n, t1, t2, plot_category,
    nrow = 1,
    rel_heights = c(1),
    rel_widths = c(((((max(stringr::str_width(unique(merge$category))))/20)+0.50)*4), 2.5, 3, 6)
  ) +
    plot_annotation(
      title = plot_title, 
      theme = theme(plot.title = element_text(hjust = 0.5, size = 11))
    )
  
  # Add annotation for subgroup differences
  annotation_text <- generate_subgroup_annotation(plot_result)
  final_plot <- combined_plot + 
    annotation_custom(grob = textGrob(annotation_text, gp = gpar(fontsize = 9)), 
                      ymin = 0, ymax = 0, xmin = -Inf, xmax = Inf)
  
  # Save plot
  Height <- 300 + (prep_data$n_category + 2) * 100
  Width <- 1150 + max(((stringr::str_width(unique(merge$category)))/20)+0.55)*410
  ggsave(file.path(plot_dir_sum, paste0(plot_name, "_summary.pdf")), 
         plot = final_plot, width = Width, height = Height, units = "px")
  
  # Save complete plot if requested
  if (plot_complete) {
    forest_result <- forest(plot_result)
    plot_height <- 100 + length(forest_result$studlab) * 20 + prep_data$n_category * 80
    rstudioapi::savePlotAsImage(file.path(plot_dir_com, paste0(plot_name, "_all.png")), 
                                width = 850, height = plot_height)
  }
}

#ORR
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year", plot_title = "ORR by year", category="year_group")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year_p1", plot_title = "ORR by year", phase=1, category="year_group")  
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_year_p2", plot_title ="ORR by year", phase=2, category="year_group") 
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_phase", plot_title = "ORR by clinical trial phase", category="Phase", plot_complete = TRUE)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_drugtype", plot_title = "ORR by tested compound category", category="DrugType",plot_complete = TRUE)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_drugtype_p1", plot_title = "ORR by tested compound category", phase=1, category="DrugType", plot_complete = TRUE)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_drugtype_p2", plot_title = "ORR by tested compound category", phase=2, category="DrugType", plot_complete = TRUE)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Investigational", plot_title = "ORR by investigational status", category="Investigational_status")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Investigational_p1", plot_title = "ORR by investigational status", phase=1, category="Investigational_status")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Investigational_p2", plot_title = "ORR by investigational status", phase=2, category="Investigational_status")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_PS", plot_title = "ORR by performance status categories", category="PS_ratio010")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Targeted", plot_title = "ORR by if population was preselected or not", category="Targeted")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Targeted_p1", plot_title = "ORR by if population was preselected or not", category="Targeted", phase=1)
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Targeted_p2", plot_title = "ORR by if population was preselected or not", category="Targeted", phase=2)
create_summary_forestplot(top_10_journals_db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_Journal", plot_title = "ORR by Journal", category="Journal")
create_summary_forestplot(db, "events_ORR", "N_eval_eff_total", plot_name = "ORR_MSI", plot_title = "ORR by MSI ratio", category="MSI_20", plot_complete = TRUE)
create_summary_forestplot(long_MS, "ORR", "n_eva", plot_name = "ORR_MS", plot_title = "ORR by MS", category="MSS_MSI", plot_complete = TRUE)

#CBR
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year", plot_title = "CBR by year", category="year_group")  
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year_p1", plot_title = "CBR by year", phase=1, category="year_group")  
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_year_p2", plot_title = "CBR by year", phase=2, category="year_group")  
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_phase", plot_title = "CBR by clinical trial phase", category="Phase", plot_complete = TRUE)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_drugtype", plot_title = "CBR by tested compound category", category="DrugType",plot_complete = TRUE)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_drugtype_p1", plot_title = "CBR by tested compound category", phase=1, category="DrugType", plot_complete = TRUE)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_drugtype_p2", plot_title = "CBR by tested compound category", phase=2, category="DrugType", plot_complete = TRUE)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Investigational", plot_title = "CBR by investigational status", category="Investigational_status", Width=2700)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Investigational_p1", plot_title = "CBR by investigational status", phase=1, category="Investigational_status")
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Investigational_p2", plot_title = "CBR by investigational status", phase=2, category="Investigational_status")
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_PS", plot_title = "CBR by performance status categories", category="PS_ratio010")
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Targeted", plot_title = "CBR by if population was preselected or not", category="Targeted")
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Targeted_p1", plot_title = "CBR by if population was preselected or not", category="Targeted", phase=1)
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Targeted_p2", plot_title = "CBR by if population was preselected or not", category="Targeted", phase=2)
create_summary_forestplot(top_10_journals_db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_Journal", plot_title = "CBR by Journal", category="Journal")
create_summary_forestplot(db, "events_CBR", "N_eval_eff_total", plot_name = "CBR_MSI", plot_title = "CBR by MSI raio", category="MSI_20", plot_complete = TRUE)  
create_summary_forestplot(long_MS, "CBR", "n_eva", plot_name = "CBR_MS", plot_title = "CBR by MS", category="MSS_MSI", plot_complete = TRUE)

#plot patients grade 3+ 
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year", plot_title = "Patients with any grade 3 or higher toxicity by year", category="year_group")  
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year_p1", plot_title = "Patients with any grade 3 or higher toxicity by year", category="year_group", phase = 1)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_year_p2", plot_title = "Patients with any grade 3 or higher toxicity by year", category="year_group", phase = 2)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_phase", plot_title = "Patients with any grade 3 or higher toxicity by clinical trial phase", category="Phase")
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_drugtype", plot_title = "Patients with any grade 3 or higher toxicity by tested compound category",category="DrugType")
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_drugtype_p1", plot_title = "Patients with any grade 3 or higher toxicity by tested compound category",phase=1, category="DrugType")
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_drugtype_p2", plot_title = "Patients with any grade 3 or higher toxicity by tested compound category",phase=2, category="DrugType")
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_targeted", plot_title = "Patients with any grade 3 or higher toxicity by if population was preselected",category="Targeted")
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_targeted_p1", plot_title = "Patients with any grade 3 or higher toxicity by if population was preselected",category="Targeted",phase=1)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_targeted_p2", plot_title = "Patients with any grade 3 or higher toxicity by if population was preselected",category="Targeted",phase=2)
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_investigationalstat", plot_title = "Patients with any grade 3 or higher toxicity by investigational status",category="Investigational_status")
create_summary_forestplot(db, "N_pat_GR3", "N_pat_safety", plot_name = "pat_gr3_PS", plot_title = "Patients with any grade 3 or higher toxicity by performance status categories",category="PS_ratio010")

#plot top 5 toxicity
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year", plot_title = "Top 5 category gr3+ events ratio by year", category="year_group")  
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year_p1", plot_title = "Top 5 category gr3+ events ratio by year", category="year_group", phase = 1)  
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_year_p2", plot_title = "Top 5 category gr3+ events ratio by year", category="year_group", phase = 2)  
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_phase", plot_title = "Top 5 category gr3+ events ratio by clinical trial phase", category="Phase")
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_drugtype", plot_title = "Top 5 category gr3+ events ratio by tested compound category", category="DrugType")
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_targeted", plot_title = "Top 5 category gr3+ events ratio by if population was preselected", category="Targeted")
create_summary_forestplot(db, "N_events_top5_GR3", "safetytimes5", plot_name = "events_gr3_top5_investigationalstat", plot_title = "Top 5 category gr3+ events ratio by investigational status", category="Investigational_status")

#plot DLT toxicity (phase 1) subgroup
create_summary_forestplot(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_year_p1", plot_title = "Patients with a DLT by year category", category="year_group", phase = 1)
# create_summary_forestplot(db, "N_DLT", "N_eval_DLT", plot_name = "DLT_eval_DLT_p1", plot_title = "Patients with a DLT by year category", category="year_group", phase = 1)
create_summary_forestplot(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_targeted_p1", plot_title = "Patients with a DLT by if population was preselected or not", category="Targeted", phase = 1)
create_summary_forestplot(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_drugtype_p1", plot_title = "Patients with a DLT by tested compound category", category="DrugType", phase = 1, plot_complete = TRUE)
create_summary_forestplot(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_investigationalstat_p1", plot_title = "Patients with a DLT by investigational status", category="Investigational_status", phase = 1)

#plot discontinuation toxicity (phase 2) subgroup
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_phase", plot_title = "Patients that discontinued due to adverse events by clinical trial phase", category="Phase", plot_complete =  TRUE)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_year_p1", plot_title = "Patients that discontinued due to adverse events by year category", category="year_group", phase = 1)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_year_p2", plot_title = "Patients that discontinued due to adverse events by year category", category="year_group", phase = 2)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_year", plot_title = "Patients that discontinued due to adverse events by year category", category="year_group")
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_drugtype", plot_title = "Patients that discontinued due to adverse events by tested compound category", category="DrugType")
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_drugtype_p1", plot_title = "Patients that discontinued due to adverse events by tested compound category", category="DrugType", phase = 1)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_drugtype_p2", plot_title = "Patients that discontinued due to adverse events by tested compound category", category="DrugType", phase = 2)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_targeted", plot_title = "Patients that discontinued due to adverse events by if population was preselected or not", category="Targeted")
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_targeted_p1", plot_title = "Patients that discontinued due to adverse events by if population was preselected or not", category="Targeted", phase = 1)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_targeted_p2", plot_title = "Patients that discontinued due to adverse events by if population was preselected or not", category="Targeted", phase = 2)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_investigationalstat", plot_title = "Patients that discontinued due to adverse events by investigational status", category="Investigational_status")
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_investigationalstat_p1", plot_title = "Patients that discontinued due to adverse events by investigational status", category="Investigational_status", phase = 1)
create_summary_forestplot(db, "N_Disc", "N_pat_safety", plot_name = "Disc_safety_investigationalstat_p2", plot_title = "Patients that discontinued due to adverse events by investigational status", category="Investigational_status", phase = 2)
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
  sprintf("%0.2f", Estimate)

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

create_summary_forestplot(db, "N_DLT", "N_pat_safety", plot_name = "DLT_all_safety_drugtype_p1", plot_title = "Patients with a DLT by tested compound category", category="DrugType", phase = 1, plot_complete = TRUE)
db_p1 <- db %>% filter(Phase == 1)
MA_II_years <- metaprop(event = N_DLT, n = N_pat_safety,
                        data = db_p1,
                        method = "Inverse",
                        sm = gs("smprop"),
                        incr = gs("incr"),
                        #method.ci = gs("method.ci"), 
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
                        byvar = DrugType,
                        control = NULL)
MA_II_years

setwd(plot_dir_sum)
tiff("DLT_all_safety_drugtype_p1.tiff",width=700,height=750)
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
       weight.study = "random",
       hetstat = FALSE,
       overall.hetstat = FALSE,
       print.Q.subgroup = FALSE,
       colgap.left = unit(10, 'mm'),
       print.subgroup.labels = TRUE,
       print.byvar = FALSE
)
dev.off()
