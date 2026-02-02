library(here)

# ---- 1.Compatibility between libraries ----

## To active compatibility between emmeans and LMMStar

recover_data.lmm <- function(object, ...){
  fcall <- object$call
  ff <- stats::formula(object, effects = "mean")
  data <- object$data
  oterms <- stats::terms(stats::model.frame(ff, data = data))
  out <- recover_data(fcall, trms = stats::delete.response(oterms), na.action = NULL, frame = data)
  return(out)
}

emm_basis.lmm <- function(object, trms, xlev, grid, ...){
  out <- list()
  m  <-  stats::model.frame(trms, grid, na.action = stats::na.pass, xlev = xlev)
  out$X <- stats::model.matrix(object, newdata = m, effects = "mean")
  ## out$X  <-  stats::model.matrix(trms, m, contrasts.arg = object$contrasts)
  out$bhat  <- stats::coef(object, effects = "mean")
  out$nbasis  <-  matrix(NA)  ## no rank deficiency
  out$V  <- stats::vcov(object, effects = "mean")
  if(!is.null(object$dVcov)){
    out$dffun <- function(k,dfargs){
      if(is.vector(k)){k <- rbind(k)}
      if(is.null(colnames(k))){colnames(k) <- dfargs$name.meanparam}
      do.call(dfargs$FUN, args = list(X.beta = k, vcov.param = dfargs$vcov.param, dVcov.param = dfargs$dVcov.param))
    }    
  }else{
    out$dffun <- function(k,dfargs){
      return(Inf)
    }
  }
  out$dfargs <- list(FUN = LMMstar:::.dfX,
                     name.meanparam = colnames(out$X),
                     vcov.param = object$vcov,
                     dVcov.param = object$dVcov)
  return(out)
}
# ---- 2. Scoring & Grouping Functions ----


## ---- 1. zscore_pre(): Compute PRE-based z-scores ----
# Standardizes selected variables using only PRE event values.
# PRE mean and PRE SD define the reference distribution.
zscore_pre = function(outcome, data) {
  data %>%
    ungroup() %>% 
    mutate(
      across(
        all_of(outcome),
        ~ (
          (. - mean(.[redcap_event_name == "Pre"], na.rm = TRUE)) /
            sd(.[redcap_event_name == "Pre"], na.rm = TRUE)
        ),
        .names = "{.col}_z"   # New columns: variable_z
      )
    )
}



## ---- 2. high_low_pre(): Categorize PRE values into High/Low ----
# Assigns High/Low groups based on the PRE median for each variable.
# Category is created at PRE, then filled across all events for each participant.
high_low_pre = function(subgroup, data) {
  data %>% 
    ungroup() %>% 
    mutate(
      across(
        all_of(subgroup),
        ~ case_when(
          . > median(.[redcap_event_name == "Pre"], na.rm = TRUE) &
            redcap_event_name == "Pre" ~ "High",
          
          . <= median(.[redcap_event_name == "Pre"], na.rm = TRUE) &
            redcap_event_name == "Pre" ~ "Low",
          
          redcap_event_name != "Pre" ~ NA_character_
        ),
        .names = "{.col}_cat"   # New columns: variable_cat
      )
    ) %>% 
    group_by(record_id) %>% 
    fill(paste0(subgroup, "_cat"))   # Carry PRE category to all timepoints
}


# ---- 3. Main Effect Functions ----

##---- 1. effects_agueda(): Main effects for group × time ----
effects_agueda <- function(outcome, data) {
  
  # Fit linear mixed‑effects model (group × time)
  model <- lmm(
    formula = as.formula(paste(outcome, "~ rand_group2*redcap_event_name")),
    data = data,
    repetition = ~ redcap_event_name | record_id,
    structure = "UN"
  )
  
  # Estimated marginal means for each group × time combination
  model_emm <- as.data.frame(
    emmeans(model, specs = ~ c(redcap_event_name, rand_group2))
  )
  
  # Pairwise comparisons between groups at each time point
  model_pair <- as.data.frame(
    pairs(emmeans(model, specs = ~ rand_group2 | redcap_event_name),
          reverse = FALSE, infer = TRUE)
  )
  
  # PRE confidence interval for the combined group
  All_ci_pre <- model_emm %>% 
    filter(rand_group2 == "All") %>% 
    dplyr::select(emmean, lower.CL, upper.CL) %>% 
    round(2) %>% 
    paste0(" [", .["lower.CL"], ";", .["upper.CL"], "]") %>% 
    .[1]
  
  # POST confidence interval for Exercise group
  Exe_ci_post <- model_emm %>% 
    filter(rand_group2 == "Exercise") %>% 
    dplyr::select(emmean, lower.CL, upper.CL) %>% 
    round(2) %>% 
    paste0(" [", .["lower.CL"], ";", .["upper.CL"], "]") %>% 
    .[1]
  
  # POST confidence interval for Control group
  Con_ci_post <- model_emm %>% 
    filter(rand_group2 == "Control") %>% 
    dplyr::select(emmean, lower.CL, upper.CL) %>% 
    round(2) %>% 
    paste0(" [", .["lower.CL"], ";", .["upper.CL"], "]") %>% 
    .[1]
  
  # Model summary and group difference at POST
  model_summ <- summary(model)
  group_diff_post <- model_summ$mean[3,] %>% round(2) %>% mutate(n = model$cluster$n)
  
  # Confidence interval for group difference
  emm_group_diff_post <- model_summ$mean[3,] %>% 
    round(4) %>% 
    paste0(" [", .["lower"], ";", .["upper"], "]") %>% 
    .[1]
  
  # Final summary table for this outcome
  outcome_final <- data.frame(
    outcome = outcome,
    All_ci_pre = All_ci_pre,
    Exe_ci_post = Exe_ci_post,
    Con_ci_post = Con_ci_post,
    emm_group_diff_post = emm_group_diff_post,
    p.value = model_summ$mean$p.value[3],
    stringsAsFactors = FALSE
  )
  
  # Return model + extracted summaries
  results <- list(
    model = model,
    model_emm = model_emm,
    group_diff_post = group_diff_post,
    outcome_final = outcome_final
  )
  
  return(results)
}


##---- 2. effects_agueda_int(): Interaction with subgroup variable ----
effects_agueda_int <- function(outcome, int, data) {
  
  # Fit model with 3‑way interaction (group × time × subgroup)
  model <- lmm(
    formula = as.formula(paste(outcome, "~ rand_group2*redcap_event_name*", int)),
    data = data,
    repetition = ~ redcap_event_name | record_id,
    structure = "UN"
  )
  
  # Extract p‑value for the interaction term
  p_int <- summary(model)$mean$p.value %>% last() %>% round(3)
  
  # Output table
  outcome_final <- data.frame(
    outcome = outcome,
    p.value = p_int
  )
  names(outcome_final)[2] <- paste0("p.value_", int)
  
  results <- list(
    model = model,
    outcome_final = outcome_final
  )
  
  return(results)
}

##---- 3. med_func(): Function for mediation analysis ----
# Define mediation function wrapper

# This function runs PROCESS mediation for:
#   X = predictor (rand_group_Pre)
#   M = mediator (change_med)
#   Y = outcome (Post cognitive score)
#   Covariate = baseline cognitive score (Pre)
# It returns:
#   - full PROCESS model
#   - cleaned summary table
#   - raw mediation effects table

med_func <- function(outcome, pred, med, bootstraping, data) {
  
  # Covariate = baseline outcome
  covariates <- paste0(outcome, "_Pre")
  
  # Outcome = Post score
  outcome <- paste0(outcome, "_Post")
  
  # Run PROCESS mediation model
  mediation_model <- bruceR::PROCESS(
    data,
    y = outcome,
    x = pred,
    meds = med,
    covs = covariates,
    digits = 4,
    std = TRUE,
    ci = "boot",
    nsim = bootstraping,
    seed = 123
  )
  
  # ---- Extract raw mediation effects table ----
  mediation_results_raw <- mediation_model$results[[1]]$mediation %>%
    rownames_to_column() %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    janitor::clean_names() %>%
    rename(estimate = "effect", effect = "rowname")
  
  mediation_results_raw$predictor <- pred
  mediation_results_raw$med <- med
  mediation_results_raw$outcome <- outcome
  
  # ---- Create cleaned summary table ----
  mediation_results <- mediation_model$results[[1]]$mediation %>%
    rownames_to_column() %>%
    .[c(1, 2, 9, 6)] %>%   # Select total, direct, indirect, p-value
    add_row(
      rowname = "prop_med",
      Effect = .$Effect[1] / .$Effect[3] * 100,   # % mediated
      `[Boot 95% CI]` = NA
    ) %>%
    mutate(
      Effect = round(Effect, 4),
      p = sprintf("%.2f", p)
    ) %>%
    unite("effect", 2:3, sep = " ", na.rm = TRUE) %>%
    pivot_wider(names_from = rowname, values_from = c(2, 3)) %>%
    janitor::clean_names() %>%
    mutate(
      model = "All",
      effect_prop_med = round(as.numeric(effect_prop_med), 1)
    ) %>%
    dplyr::select(model, 1:7)
  
  mediation_results$predictor <- pred
  mediation_results$med <- med
  mediation_results$outcome <- outcome
  
  # Return all components
  results <- list(
    mediation_model = mediation_model,
    mediation_results = mediation_results,
    mediation_results_raw = mediation_results_raw
  )
  
  names(results) <- c(
    "mediation_model",
    "mediation_results",
    "mediation_results_raw"
  )
  
  return(results)
}

