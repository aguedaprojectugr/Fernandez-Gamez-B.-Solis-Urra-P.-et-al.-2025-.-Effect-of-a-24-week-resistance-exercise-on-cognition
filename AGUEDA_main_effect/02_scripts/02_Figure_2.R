# ---- 1. Run Main Effects and Prepare Tables ----

library(here)
source("02_scripts/01_data.R")

# Outcome(s) to analyze
outcomes <- c("comp_ex_func_mean_z")

## ---- Fit models for each outcome (excluding Mid timepoint)
results <- map(
  outcomes,
  ~ effects_agueda(.x, data %>% filter(redcap_event_name != "Mid"))
) %>%
  purrr::set_names(outcomes)


# Extract Group Differences (Main Effects)
table_plot_main <- map_df(results, ~ .x$group_diff_post, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))


# Extract Estimated Marginal Means
table_plot <- map_df(results, ~ .x$model_emm, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))


# Duplicate “All” group rows so each timepoint has a matching row
table_plot <- bind_rows(
  table_plot %>% filter(rand_group2 != "All"),
  table_plot %>%
    group_by(outcome) %>%
    filter(rand_group2 == "All") %>%
    slice(rep(which(rand_group2 == "All"), each = 3))
)


# Assign Group Labels for Plotting
table_plot <- table_plot %>%
  group_by(outcome) %>%
  mutate(
    # First pass: duplicated "All" rows → Exercise
    rand_group = case_when(
      duplicated(rand_group2) & rand_group2 == "All" ~ "Exercise"
    )
  ) %>%
  mutate(
    # Second pass: remaining "All" rows → Control
    rand_group = case_when(
      rand_group2 == "All" & is.na(rand_group) ~ "Control",
      redcap_event_name %in% c("Post", "Mid") ~ rand_group2,
      !is.na(rand_group) ~ "Exercise"
    )
  )

# Merge EMM Table with Group Differences
table_plot <- table_plot %>%
  arrange(factor(outcome, levels = outcomes)) %>%
  left_join(table_plot_main, by = "outcome") %>%
  mutate(
    outcome = case_when(
      startsWith(outcome, "comp") ~ "Executive function"
    )
  )

# ---- 2. Fig_2a: Estimated Marginal Means Plot ----

Fig_2a = ggplot(table_plot, aes(x = redcap_event_name, y = emmean)) +
  
  # Error bars for confidence intervals
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL, color = rand_group2),
    position = position_dodge(0.07), width = 0.07
  ) +
  
  # Lines connecting group means across time
  geom_line(
    aes(group = rand_group, col = rand_group),
    position = position_dodge(0.05)
  ) +
  
  # Mean points
  geom_point(
    aes(fill = rand_group2, color = rand_group2),
    size = 3, shape = 21,
    position = position_dodge(0.07)
  ) +
  
  # Labels for POST means (value + CI)
  ggrepel::geom_text_repel(
    data = table_plot %>% filter(redcap_event_name == "Post"),
    aes(label = paste0(
      format(round(emmean, 2)),
      "\n(",
      round(lower.CL, 2), ";", round(upper.CL, 2),
      ")"
    ),color = rand_group2),
    nudge_x = 0.2,
    max.overlaps = 2,
    size = 3,
    segment.color = NA,
    direction = "y"
  ) +
  
  # Facet by outcome
  facet_wrap(~ outcome) +
  theme_classic() +
  
  # Manual color scales
  scale_color_manual(
    limits = c("All", "Control", "Exercise"),
    values = pal_npg("nrc")(10)[c(4, 10, 2)]
  ) +
  scale_fill_manual(
    limits = c("All", "Control", "Exercise"),
    values = pal_npg("nrc")(10)[c(4, 10, 2)]
  ) +
  
  # Theme adjustments
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = rel(0.9)),
    strip.placement = "outside",
    strip.background.x = element_rect(fill = "white", color = "white"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = rel(0.9)),
    text = element_text(family = "Arial", color = "black", size = 10),
    legend.position = "bottom",
    axis.line = element_line(size = 0.1),
    axis.ticks = element_line(size = 0.1),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  
  labs(y = "Executive function (z-score)") +
  
  # Make legend points larger
  guides(color = guide_legend(override.aes = list(size = 3))) +
  
  # Add group difference annotation inside each panel
  geom_text(
    data = table_plot %>%
      ungroup() %>%
      group_by(outcome, redcap_event_name) %>%
      top_n(2, emmean) %>%     # pick one row per panel
      distinct(outcome, .keep_all = TRUE),
    aes(
      x = 1.5, y = 0.5,
      label = paste0(
        "Group diff = ", estimate,
        " (", round(lower, 2), ";", round(upper, 2), ")",
        "\np = ", p.value
      )
    ),
    size = 3,
    color = "black",
    show.legend = FALSE
  ) +
  
  scale_x_discrete(guide = "prism_offset")

# ---- 3. Fig_2b: Individual Trajectories + Raincloud Plot ----

# Load custom raincloud geometry
devtools::source_url(
  "https://raw.githubusercontent.com/yjunechoe/geom_paired_raincloud/master/geom_paired_raincloud.R"
)

Fig_2b = ggplot(
  data %>% filter(redcap_event_name != "Mid"),
  aes(redcap_event_name, comp_ex_func_mean_z, fill = rand_group)
) +
  
  # Raincloud distribution + half violin + boxplot
  geom_paired_raincloud(
    position = position_nudge(0),
    alpha = 0.6,
    show.legend = FALSE
  ) +
  
  # Individual points
  geom_point(
    aes(group = record_id, col = rand_group),
    position = position_nudge(c(.05, -.05)),
    alpha = 0.5, shape = 16,
    show.legend = FALSE
  ) +
  
  # Individual trajectories
  geom_line(
    aes(group = record_id, col = rand_group),
    position = position_nudge(c(.05, -.05))
  ) +
  
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = rel(0.9)),
    strip.placement = "outside",
    axis.title.x =  element_blank(),
    axis.text = element_text(size = rel(0.9)),
    text = element_text(family = "Arial", color = "black", size = 10),
    legend.position = "none",
    axis.line = element_line(size = 0.1),
    strip.background = element_blank(),
    axis.ticks = element_line(size = 0.1),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  
  labs(y = "Executive function (individual values)") +
  
  # Manual colors
  scale_color_manual(values = pal_npg("nrc")(10)[c(10, 2)]) +
  scale_fill_manual(values = pal_npg("nrc")(10)[c(10, 2)]) +
  
  scale_x_discrete(guide = "prism_offset")

# ---- 4. Moderation analysis ----


# Includes demographic, biomarker, and cognitive moderators
int = c(
  "screen_gender", "rand_age_group", "screen_years_edu_cat",
  "gen_apoee4_carrier", "pet_amyloid_status_mni",
  paste0(subgroup, "_cat")   # cognitive + clinical moderators
)


# Create new dataframe, removing Midpoint
datas = data %>% 
  filter(redcap_event_name != "Mid") %>% 
  ungroup()

# Split dataset into subgroups for each moderator
datas <- map(int, ~ split(datas, datas[[.x]])) %>% 
  purrr::set_names(int)


# Fit Models Within Each Moderator Level
results_mod = list()

for (i in int) {
  level1 = names(datas[[i]])[1]
  level2 = names(datas[[i]])[2]
  
  # Fit effects_agueda() separately for each level
  results_mod[[i]][[level1]] =
    map(outcomes, ~ effects_agueda(.x, datas[[i]][[level1]])) %>%
    purrr::set_names(outcomes)
  
  results_mod[[i]][[level2]] =
    map(outcomes, ~ effects_agueda(.x, datas[[i]][[level2]])) %>%
    purrr::set_names(outcomes)
}

# Extract Group Differences for Plotting
table_mod_plot = do.call(c, unlist(results_mod, recursive = FALSE))

results_tableS11 = map_df(table_mod_plot, ~ .x$outcome_final, .id = "outcome") #Data for table S11

table_mod_plot = map_df(table_mod_plot, ~ .x$group_diff_post, .id = "outcome")

# Separate moderator, level, and outcome names
table_mod_plot = separate(
  table_mod_plot, outcome, sep = "\\.",
  into = c("mod", "level", "outcome")
)

# Add numeric level index
table_mod_plot = table_mod_plot %>%
  group_by(mod, outcome) %>%
  mutate(level2 = row_number())

table_mod_plot$level3 = table_mod_plot$level2
table_mod_plot$level2 = as.factor(table_mod_plot$level2)


# Interaction p-values (effects_agueda_int)
outcomes_list <- setNames(outcomes, outcomes)

results_int <- setNames(
  lapply(int, function(x)
    map(outcomes_list, ~ effects_agueda_int(.x, x, data %>% filter(redcap_event_name != "Mid")))
  ),
  int
)

table_int = map_df(flatten(results_int), ~ .x$outcome_final)

table_int = table_int %>%
  bind_rows() %>%
  group_by(outcome) %>%
  fill(everything(), .direction = "downup") %>%
  distinct(.keep_all = TRUE) %>%
  filter(endsWith(outcome, "_z"))

table_int = table_int %>%
  pivot_longer(2:length(table_int), names_to = "mod", values_to = "p_int")

table_int$mod = str_remove(table_int$mod, "p.value_")


# Merge Interaction p-values with Group Differences
table_mod_plot = table_mod_plot %>%
  left_join(table_int, by = c("mod", "outcome")) %>%
  mutate(mod2 = if_else(mod %in% subgroup, 1, 2))


# Clean Moderator Labels
table_mod_plot = table_mod_plot %>%
  mutate(
    mod = case_when(
      startsWith(mod, "screen_gender") ~ "Sex",
      startsWith(mod, "rand_age_group") ~ "Age\n (cut-off ≥ 72 years)",
      startsWith(mod, "screen_years_edu_cat") ~ "Educational level\n (cut-off > 12 years)",
      startsWith(mod, "gen_apoee4_carrier") ~ "APOE carrier",
      startsWith(mod, "pet_amyloid_status_mni") ~ "Amyloid burden\n (cut-off > 12 CL)",
      startsWith(mod, "comp_ex_func_mean_z_cat") ~ "Executive function\n (median)",
      startsWith(mod, "screen_smds4_") ~ "Subjective cognitive decline\n (median)",
      startsWith(mod, "disease_advance") ~ "Comorbidities\n (cut-off n ≥ 3)",
      TRUE ~ mod
    )
  )

# Order moderators in plot
table_mod_plot$mod = factor(
  table_mod_plot$mod,
  levels = c(
    "Sex",
    "Educational level\n (cut-off > 12 years)",
    "APOE carrier",
    "Executive function\n (median)",
    "Age\n (cut-off ≥ 72 years)",
    "Comorbidities\n (cut-off n ≥ 3)",
    "Amyloid burden\n (cut-off > 12 CL)",
    "Subjective cognitive decline\n (median)"
  )
)

# Add sample size to level labels
table_mod_plot$level = paste0(table_mod_plot$level, "\n (n=", table_mod_plot$n, ")")

table_mod_plot = table_mod_plot %>% filter(!is.na(mod))


# p-value Adjustment (BH)
table_mod_plot %>%
  mutate(p_adj = round(p.adjust(p.value, method = "BH"), 3)) %>%
  dplyr::select(mod, level, p.value, p_adj) %>%
  View()

# ---- 5. Fig_2c: Moderator Forest Plot ----
# This plot visualizes moderation effects:
#  - Each moderator appears in its own facet panel.
#  - Within each moderator, levels (e.g., High/Low) are shown with their
#    estimated group differences and confidence intervals.
#  - The vertical line at 0 indicates no group difference.
#  - Labels show the estimate and CI for each subgroup.


# Facet Layout for Fig_2c 
design = "
AE
BF
CG
DH
"

# plot
Fig_2c = ggplot(
  table_mod_plot,
  aes(
    x = estimate,   # Effect size (Exercise – Control)
    y = tidytext::reorder_within(level, level3, within = mod),  # Order levels within each moderator
    col = level2    # Color by subgroup level (e.g., High vs Low)
  )
) +
  
  # Point estimate for each subgroup
  geom_point(position = position_dodge(0.8), size = rel(2)) +
  
  # Confidence interval bars
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0,
    position = position_dodge(0.8)
  ) +
  
  # Fix y-axis ordering after using reorder_within()
  tidytext::scale_y_reordered() +
  
  # Facet each moderator into its own panel using a custom layout
  ggh4x::facet_manual(
    mod ~ .,          # One moderator per row
    design = design,  # Custom facet layout (A–H)
    scales = "free_y" # Allow different numbers of levels per moderator
  ) +
  
  # Clean base theme
  theme_classic() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    legend.position = "none",   # Remove legend (colors are self-explanatory)
    strip.text.x = element_text(size = rel(0.9), face = "bold"),  # Moderator titles
    axis.text.y = element_text(size = rel(0.9), angle = 45),      # Tilt labels for readability
    axis.text.x = element_text(size = rel(0.9)),
    axis.title.x = element_text(hjust = 0.5, size = rel(0.7)),
    axis.line = element_line(size = 0.1),
    strip.background = element_blank(),
    axis.ticks = element_line(size = 0.1)
  ) +
  
  # Colors for subgroup levels
  scale_color_manual(values = pal_npg("nrc")(10)[c(3, 5)]) +
  
  # Add p-value for the interaction at the top of each panel
  geom_label(
    aes(
      x = -0.5, y = 1.5,
      label = paste0("p for int = ", table_mod_plot$p_int)
    ),
    colour = "black",
    size = rel(3),
    label.size = NA   # No border around label
  ) +
  
  # Add estimate + CI text next to each point
  geom_text_repel(
    aes(label = paste0(
      format(round(estimate, 3), digits = 3),
      " (", lower, ";", upper, ")"
    )),
    nudge_y = 0.2, nudge_x = 0.2,
    size = rel(3)
  ) +
  
  # Reference line at 0 (no difference between groups)
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    alpha = 0.5
  ) +
  
  # Axis labels
  labs(
    x = "Favors control   Favors Exercise",
    y = NULL
  ) +
  
  # Offset x-axis labels slightly for better spacing
  scale_x_continuous(guide = "prism_offset")


# ---- 6. Merge All Plots ----

# Combine Fig_2a (EMM plot) and Fig_2b (individual trajectories) vertically.
# rel_heights controls how much vertical space each plot takes:
#   - Fig_2a gets slightly more space (1.2) because it contains more information.
#   - Fig_2b gets standard space (1).
Fig_2a_2b = cowplot::plot_grid(
  Fig_2a, Fig_2b,
  labels = "AUTO",      # Automatically label panels as A, B
  label_size = 12,      # Size of panel labels
  ncol = 1,             # Stack plots vertically
  rel_heights = c(1.2, 1)
)

# ----7. Save Figure 2 ----
# Open a high‑resolution TIFF device for publication-quality output.
# 600 dpi is standard for journals; width/height in inches.
tiff(
  "03_outputs/figures/Figure_2_600.tiff",
  width = 10,
  height = 8,
  units = "in",
  res = 600,
  compression = "none"
)

# Combine the left panel (Fig_2a + Fig_2b) with the moderator forest plot (Fig_2c).
# rel_widths adjusts how much horizontal space each side gets:
#   - Left side (Fig_2a_2b) is narrower (0.8)
#   - Right side (Fig_2c) is wider (1) to accommodate long moderator labels.
cowplot::plot_grid(
  Fig_2a_2b, Fig_2c,
  labels = c("", "C"),   # Only label the right panel as "C"
  label_size = 12,
  ncol = 2,              # Two columns: left (A+B), right (C)
  rel_widths = c(0.8, 1)
)

# Close the TIFF device and write the file.
dev.off()
