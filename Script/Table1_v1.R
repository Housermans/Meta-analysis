#load packages
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(gt)

rm(list=ls())
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# script.dir <- dirname(sys.frame(1)$ofile) 
home_dir <- dirname(script_dir)
input <- file.path(home_dir, "Input")
output <- file.path(home_dir, "Output")
plot_dir <- file.path(home_dir, "Plots")
plot_dir_sum <- file.path(plot_dir, "summary")
plot_dir_com <- file.path(plot_dir, "complete")

db <- read_excel(file.path(output, "filtered_db_231106.xlsx"))

glimpse(db)

#variable for number evaluable patients (CRC or all cancer types)
db$N_eval_eff_total <- ifelse(db$CRC_specific == "1", db$N_eva_eff_crc, 
                          ifelse(db$CRC_specific == "0", db$N_eva_eff, NA))


db_phase1 <- filter(db, Phase==1)
db_phase2 <- filter(db, Phase==2)
library(dplyr)

library(dplyr)

total_patients <- db %>% 
  group_by(Phase) %>% 
  summarise(
    n = sum(N_inc),
    n_crc = sum(N_inc_crc, na.rm = TRUE), 
    mean_n = mean(N_inc, na.rm = TRUE), 
    n_sd = sd(N_inc, na.rm = TRUE), 
    n_se = n_sd / sqrt(n()), 
    n_95ci_lower = mean_n - 1.96 * n_se,
    n_95ci_upper = mean_n + 1.96 * n_se,
    n_with_95ci = paste0(round(mean_n, 0), " (", round(n_95ci_lower, 0),"-", round(n_95ci_upper, 0), ")"),
    PS01 = sum(PS_0, PS_1, na.rm = TRUE),
    PS2 = sum(PS_2, na.rm = TRUE), 
    mean_median_age = mean(Med_age, na.rm = TRUE), 
    mean_median_line = mean(Med_lines, na.rm = TRUE), 
    median_age = median(Med_age, na.rm = TRUE), 
    median_line = median(Med_lines, na.rm = TRUE),
    IQR_age = paste0(" (", round(quantile(Med_age, probs = 0.25, na.rm = TRUE), 0), 
                    "-", round(quantile(Med_age, probs = 0.75, na.rm = TRUE), 0), ")", sep = ""),
    IQR_line = paste0(" (", round(quantile(Med_lines, probs = 0.25, na.rm = TRUE), 0), 
                     "-", round(quantile(Med_lines, probs = 0.75, na.rm = TRUE), 0), ")", sep = ""),
    median_and_IQR_age = paste0(round(median_age, 0), IQR_age),
    median_and_IQR_line = paste0(round(median_line, 0), IQR_line)
  ) %>%
  ungroup()


articles_by_phase <- db %>%
  mutate(
    PS2_category = case_when(
      is.na(PS_2) | is.na(PS_0) | is.na(PS_1) ~ "missing",
      PS_2 == 0 ~ "No PS2",
      PS_2 / N_inc < 0.1 ~ "<10% PS2",
      PS_2 / N_inc >= 0.1 ~ ">10% PS2"
    )
  ) %>%
  group_by(Phase, PS2_category) %>%
  summarise(count = n()) %>%
  ungroup()

# View the results
print(articles_by_phase)


patients_by_yeargroup <- db %>% 
  group_by(Phase, year_group) %>% 
  summarise(n= sum(N_inc), 
            n_crc = sum(N_inc_crc, na.rm=T), 
            percentage = (n_crc/n)*100) %>% #TODO: best grote verschillen per year group, maakt dat uit?
  ungroup()

patients_by_drugtype <- db %>%
  group_by(Phase, DrugType) %>%
  summarise(n= sum(N_inc), 
            n_crc = sum(N_inc_crc, na.rm=T), 
            percentage = (n_crc/n)*100) %>% 
  ungroup()

patients_by_investigational <- db %>%
  group_by(Phase, Investigational_status) %>%
  summarise(n= sum(N_inc), 
            n_crc = sum(N_inc_crc, na.rm=T), 
            percentage = (n_crc/n)*100) %>% 
  ungroup()

patients_by_preselected <- db %>%
  group_by(Phase, targeted) %>%
  summarise(n= sum(N_inc), 
            n_crc = sum(N_inc_crc, na.rm=T), 
            percentage = (n_crc/n)*100) %>% 
  ungroup()


trials_by_yeargroup <- db %>% 
  group_by(Phase, year_group) %>% 
  count() %>%
  ungroup()

trials_by_drugtype <- db %>%
  group_by(Phase, DrugType) %>%
  count() %>%
  ungroup()

trials_by_investigational <- db %>%
  group_by(Phase, Investigational_status) %>%
  count() %>%
  ungroup()

trials_by_preselected <- db %>%
  group_by(Phase, targeted) %>%
  count() %>%
  ungroup()



# Formatting the table (using `gt` for example)
formatted_table <- gt(combined_table)

# Exporting to Word
gt::gtsave(formatted_table, filename = "table1.docx")

# Exporting to Excel
write_xlsx(combined_table, path = "table1.xlsx")
