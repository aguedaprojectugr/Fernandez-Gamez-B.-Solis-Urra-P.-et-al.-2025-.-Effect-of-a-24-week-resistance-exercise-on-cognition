# ---- 1. Load Functions and libraries----
library(here)   # Helps build file paths relative to project root
source(here("02_scripts/00_functions.R"))   # Load custom functions
options(scipen = 999)   # Avoid scientific notation in numeric output
pacman::p_load(
  # Data Manipulation
  dplyr, data.table, readr, readxl, openxlsx, tidyverse, magrittr, gtools,
  reshape2, sjmisc, sjlabelled, zoo, tidyverse,
  
  # Statistical Analysis
  afex, car, tableone, lme4, lmerTest, nlme, survival, rms, emmeans,
  simstudy, lavaan, mediation, multilevelmediation, sandwich, RNOmni,
  ppcor, LMMstar, psych, broom, checkmate,
  
  # Visualization
  ggplot2, hrbrthemes, ggsci, ggpmisc, ggpubr, ggrepel, cowplot, patchwork,
  grid, gridExtra, corrplot, MetBrewer, ggforce, ggprism, ragg,
  
  # Reporting
  sjPlot, ggeffects, forestplot, DescTools,
  
  # Miscellaneous
  berryFunctions, languageR, rlang, scales
)

# ---- 2. Load & Prepare Data ----

# Main dataset
data <- readRDS("01_data/raw/request_data_BFG_02_12_2024.rds")

# Cognitive dataset (remove columns 3–11)
data_cog <- read_excel("01_data/raw/data_cognitive_processed.xlsx") %>%
  dplyr::select(-c(3:11))

# Remove overlapping cognitive variables from main dataset
data <- data %>%
  dplyr::select(-all_of(intersect(names(data), names(data_cog)[-c(1, 2)])))

# Physical function dataset
data_pf <- read_excel("01_data/raw/data_strength_pf_all.xlsx")

# Additional PF variables (power metrics)
data_pf2 <- read_excel("01_data/raw/data_strenght_pf_new_variables_power_CODEBOOK.xlsx") %>%
  dplyr::select(record_id, redcap_event_name,
         sft30s_mean_power, sppb5_mean_power,
         sft30s_mean_power_z, sppb5_mean_power_z)

# Merge PF datasets
data_pf <- data_pf %>%
  left_join(data_pf2, by = c("record_id", "redcap_event_name")) %>%
  dplyr::select(-c(3:7)) %>%                     # Remove old PF columns
  mutate(across(3:119, as.numeric))       # Ensure numeric format

# Remove overlapping PF variables from main dataset
data <- data %>%
  dplyr::select(-all_of(intersect(names(data), names(data_pf)[-c(1, 2)])))

# Merge PF + cognitive data into main dataset
data <- data %>%
  left_join(data_pf,  by = c("record_id", "redcap_event_name")) %>%
  left_join(data_cog, by = c("record_id", "redcap_event_name"))


# ---- 3. Clean and Transform Data ----
data <- data %>%
  # Fix misspelled variable names
  rename_with(~ str_replace_all(., "strenght", "strength")) %>%
  
  # Recode event names and create categorical variables
  mutate(
    redcap_event_name = recode_factor(
      redcap_event_name,
      pre_intervention_arm_1 = "Pre",
      mid_point_arm_1 = "Mid",
      post_intervention_arm_1 = "Post"
    ),
    screen_years_edu_cat = factor(ifelse(screen_years_edu > 12, "High", "Low"),
                                  levels = c("High", "Low")),
    rand_group = factor(rand_group, levels = c(2, 1),
                        labels = c("Control", "Exercise")),
    rand_age_group = factor(rand_age_group, levels = c(1, 2),
                            labels = c("Youngers", "Olders")),
    screen_gender = factor(screen_gender, levels = c(0, 1),
                           labels = c("Male", "Female")),
    gen_apoee4_carrier = factor(gen_apoee4_carrier, levels = c(0, 1),
                                labels = c("Non-carrier", "Carrier")),
    pet_amyloid_status_mni = factor(pet_amyloid_status_mni, levels = c(0, 1),
                                    labels = c("Negative", "Positive")),
    dxa_bmi_cat = factor(ifelse(dxa_bmi_cat %in% c(1, 2), "Normal-weight", "Ow/Ob")),
    
    # Remove POST values for biomarkers not measured at POST
    pet_amyloid_status_mni = if_else(redcap_event_name == "Post", NA, pet_amyloid_status_mni),
    screen_smds4_score = if_else(redcap_event_name == "Post", NA, screen_smds4_score),
    dxa_bmi_cat = if_else(redcap_event_name == "Post", NA, dxa_bmi_cat),
    
    # Numeric version of event name (Pre=1, Mid=2, Post=3)
    redcap_event_name_num = as.numeric(redcap_event_name)
  )


## ---- Fill Missing Values Within Participants ----
fill_vars <- c(
  "screen_gender", "screen_age", "screen_years_edu_cat", "rand_age_group",
  "ethic_com_age", "rand_group", "gen_apoee4_carrier",
  "pet_amyloid_status_mni", "screen_smds4_score", "dxa_bmi_cat"
)

data <- data %>%
  group_by(record_id) %>%
  fill(!!!syms(fill_vars), .direction = "downup") #Important to cheack the direction of the fill regarding the data.

## ---- Creating Baseline Adjustment variable (rand_group2) ----
data$rand_group2 <- baselineAdjustment(
  data,
  variable = "rand_group",
  repetition = ~ redcap_event_name | record_id,
  constrain = c("Pre"),
  new.level = "All"
) %>%
  relevel(ref = "Control")


## ---- Prepare Physical Function Variables ----
data$pf_sppb_repchair_time_best_revert <- data$pf_sppb_repchair_time_best * -1

outcome_pf <- c(
  "pf_sft_arm_score", "pf_sft_chair_score", "pf_sppb_repchair_time_best_revert",
  "handgrip_score_mean", "iso_upper_peak", "iso_lower_peak",
  "pf_sft_2min_step", "pf_sft_timeup_score_revert",
  "pf_sft_6minutewalk_corr", "pf_2km_walk_time_revert",
  "sft30s_mean_power", "sppb5_mean_power"
)

## ---- Z‑score PF outcomes using PRE values ----
data <- zscore_pre(outcome_pf, data)
rm(outcome_pf)

## ---- Compute Strength & Composite Scores ----

pf_comp_vars <- paste0(c("pf_sft_2min_step", "pf_sft_timeup_score_revert"), "_z")
crf_comp_vars <- paste0(c("pf_sft_6minutewalk_corr", "pf_2km_walk_time_revert"), "_z")
strength_vars <- paste0(
  c("pf_sft_arm_score", "pf_sft_chair_score", "pf_sppb_repchair_time_best_revert",
    "handgrip_score_mean", "iso_upper_peak", "iso_lower_peak"), "_z"
)

data <- data %>%
  ungroup() %>%
  mutate(
    strength_mean = rowMeans(dplyr::select(., all_of(strength_vars)), na.rm = TRUE),
    pf_comp = rowMeans(dplyr::select(., all_of(pf_comp_vars)), na.rm = TRUE),
    crf_comp = rowMeans(dplyr::select(., all_of(crf_comp_vars)), na.rm = TRUE)
  )

# Z‑score composite variables
data <- zscore_pre(
  c("strength_mean", "pf_comp", "crf_comp"),
  data
)

## ---- Create High/Low Moderator Groups ----
subgroup <- c(
  "comp_ex_func_mean_z",
  paste0(
    c("episodic_mem_mean", "processing_speed_mean", "working_mem_mean",
      "attentional_control_mean", "visuospatial_mean"), "_z"
  ),
  "disease_advance_comor", "screen_smds4_score"
)


# High/Low classification based on PRE median
data <- high_low_pre(subgroup, data)

# Ensure consistent factor ordering
data <- data %>%
  mutate(
    comp_ex_func_mean_z_cat = factor(comp_ex_func_mean_z_cat, levels = c("High", "Low")),
    episodic_mem_mean_z_cat = factor(episodic_mem_mean_z_cat, levels = c("High", "Low")),
    processing_speed_mean_z_cat = factor(processing_speed_mean_z_cat, levels = c("High", "Low")),
    working_mem_mean_z_cat = factor(working_mem_mean_z_cat, levels = c("High", "Low")),
    attentional_control_mean_z_cat = factor(attentional_control_mean_z_cat, levels = c("High", "Low")),
    visuospatial_mean_z_cat = factor(visuospatial_mean_z_cat, levels = c("High", "Low")),
    disease_advance_comor_cat = factor(disease_advance_comor_cat, levels = c("Low", "High")),
    screen_smds4_score_cat = factor(screen_smds4_score_cat, levels = c("Low", "High"))
  )


# ---- 4. Save Processed Data ----
saveRDS(data, "dataused.Rds")

