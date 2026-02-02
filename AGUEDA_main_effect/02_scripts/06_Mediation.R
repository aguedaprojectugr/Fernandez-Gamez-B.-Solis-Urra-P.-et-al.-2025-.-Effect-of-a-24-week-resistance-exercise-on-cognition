
# ---- 1. Setup for mediation analysis ----
library(here)
source("02_scripts/01_data.R")
set.seed(123)   # Ensure reproducibility for bootstrapping

# setting variables
outcomes <- paste0(
  c("episodic_mem_mean", "processing_speed_mean", "working_mem_mean",
    "attentional_control_mean", "visuospatial_mean"),
  "_z"
)

outcomes <- c("comp_ex_func_mean_z", outcomes)

med <- "strength_mean_z"
pred <- "rand_group_Pre"

# ----2. Prepare dataset for mediation analysis ----
# Compute change in mediator (Post – Pre)

data2 <- data %>%
  dplyr::select(
    record_id, redcap_event_name, rand_group,med,
   outcomes
  ) %>%
  filter(redcap_event_name != "Mid")   # Remove mid-intervention timepoint


# Convert long → wide (Pre/Post columns)
data2 <- data2 %>%
  pivot_wider(
    id_cols = record_id,
    names_from = redcap_event_name,
    values_from = 3:10
  )

data2$change_med <- data2$strength_mean_z_Post - data2$strength_mean_z_Pre #individual mediation change

# Remove participants without mediator change
data2 <- data2 %>% filter(!is.na(change_med))

pred <- "rand_group_Pre"
bootstraping <- 100000
med <- "change_med"
# ----3. Run mediation for all outcomes ----

# Nested mapping: outcome → predictor → mediator, simple mediation analysis, 1 mediator, several outcomes (separately)
sim_med <- map(outcomes, function(out) {
  map(pred, function(pr) {
    map(med, function(md) {
      model1 <- med_func(out, pr, md, bootstraping, data = data2)
      list(model1 = model1)
    }) %>% set_names(med)
  }) %>% set_names(pred)
}) %>%
  set_names(outcomes) %>%
  do.call(c, .) %>%
  do.call(c, .) %>%
  do.call(c, .)


# ----4. Extract summary tables ----

# Clean summary table (total/direct/indirect/prop mediated)
tablecoef_med_prop <- map_df(sim_med, ~ .x$mediation_results, .id = "model")

# Raw mediation effects (a, b, c, c', indirect, etc.)
tablecoef_sim_med <- map_df(sim_med, ~ .x$mediation_results_raw, .id = "model")
