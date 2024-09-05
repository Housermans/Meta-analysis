library(readxl)
library(dplyr)
library(writexl)

rm(list=ls())

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
home_dir <- dirname(script_dir)
input <- file.path(home_dir, "Input")
output <- file.path(home_dir, "Output")


d <- read_excel(file.path(output, "filtered_db_230801.xlsx"))

d_checks <- d %>% mutate(qc_eval_v_PD_SDshort_PR_CR = ifelse(CRC_specific==1, #checkt of CRC specifieke getallen genomen zijn
                                          ifelse(N_eva_eff_crc == N_PD+N_SD_short+N_PR+N_CR, 1, 0), # som van alle evalueerbare crc gevallen vergeleken met som van evaluaties
                                          ifelse(N_eva_eff == N_PD+N_SD_short+N_PR+N_CR, 1, 0)), # som van alle evalueerbare crc gevallen vergeleken met som van evaluaties
                         qc_ninc_v_eva_eff = ifelse(N_inc >= N_eva_eff, 1, 0), 
                         qc_eva_eff_v_eva_eff_crc = ifelse(N_eva_eff >= N_eva_eff_crc, 1, 0), 
                         qc_ninc_v_ninc_crc = ifelse(N_inc >= N_inc_crc, 1, 0), 
                         qc_nsafety = ifelse(N_inc >= N_pat_safety, 1, 0), 
                         qc_maxage = ifelse(Max_age > Med_age, 1, 0), 
                         qc_maxlines = ifelse(Max_lines > Med_lines, 1, 0),
                         qc_gr3_event_v_cat = ifelse(`N_events_Gr3+` >= `N_Gr3+_cat`, 1, 0),
                         qc_gr3_event_v_pat = ifelse(`N_events_Gr3+` >= `N_pat_GR3+`, 1, 0),
                         qc_gr3_event_v_top5 = ifelse(`N_events_Gr3+` >= `N_events_top5_Gr3+`, 1, 0)
                  )  

write_xlsx(d_checks, file.path(output, "filtered_db_230801_QC2.xlsx"))
