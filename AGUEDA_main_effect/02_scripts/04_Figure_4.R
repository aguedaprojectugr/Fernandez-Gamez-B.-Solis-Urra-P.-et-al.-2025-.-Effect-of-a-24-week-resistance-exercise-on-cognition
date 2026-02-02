# ---- 1. Run Main Effects on physical function and Prepare Tables ----

library(here)
source("02_scripts/01_data.R")

# Three composite z‑scored outcomes:
# - Muscular strength
# - Physical function
# - Cardiorespiratory fitness
outcomes <- paste0(c("strength_mean", "pf_comp", "crf_comp"), "_z")

# Fit models for each outcome (excluding Mid timepoint)

results <- map(
  outcomes,
  ~ effects_agueda(.x, data %>% filter(redcap_event_name != "Mid"))
) %>% purrr::set_names(outcomes)

# Extract final model results (Table 2 equivalent)
results_table2 <- map_df(results, ~ .x$outcome_final, .id = "outcome")

# Extract Group Differences (Main Effects)
table_plot_main <- map_df(results, ~ .x$group_diff_post, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))

# Extract Estimated Marginal Means
table_plot <- map_df(results, ~ .x$model_emm, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))

# Duplicate “All” rows so each outcome has Control + Exercise rows
table_plot <- bind_rows(
  table_plot %>% filter(rand_group2 != "All"),
  table_plot %>%
    group_by(outcome) %>%
    filter(rand_group2 == "All") %>%
    slice(rep(which(rand_group2 == "All"), each = 2))
)

# Assign Group Labels for Plotting
table_plot <- table_plot %>%
  group_by(outcome) %>%
  mutate(
    # First pass: duplicated “All” rows → Exercise
    rand_group = case_when(
      duplicated(rand_group2) & rand_group2 == "All" ~ "Exercise"
    )
  ) %>%
  mutate(
    # Second pass: remaining "All" rows → Control
    rand_group = case_when(
      rand_group2 == "All" & is.na(rand_group) ~ "Control",
      redcap_event_name == "Post" ~ rand_group2,
      !is.na(rand_group) ~ "Exercise"
    )
  )

# Order outcomes consistently
table_plot <- table_plot %>%
  arrange(factor(outcome, levels = outcomes))

# Merge group differences into plotting table
table_plot <- table_plot %>% left_join(table_plot_main, by = "outcome")

# Replace outcome variable names with human-readable labels
table_plot <- table_plot %>%
  mutate(
    outcome = case_when(
      startsWith(outcome, "episodic_mem_mean") ~ "Episodic memory",
      startsWith(outcome, "processing_speed_mean") ~ "Processing speed",
      startsWith(outcome, "working_mem_mean") ~ "Working memory",
      startsWith(outcome, "attentional_control_mean") ~ "Attentional/inhibitory control",
      startsWith(outcome, "visuospatial_mean") ~ "Visuospatial processing",
      startsWith(outcome, "strength_mean") ~ "Muscular strength\n composite score",
      startsWith(outcome, "pf_comp") ~ "Physical function\n composite score",
      startsWith(outcome, "crf_comp") ~ "Cardiorespitaroy fitness\n composite score",
      TRUE ~ outcome
    )
  )

# Set plotting order for outcomes
table_plot$outcome <- factor(
  table_plot$outcome,
  levels = c(
    "Muscular strength\n composite score",
    "Physical function\n composite score",
    "Cardiorespitaroy fitness\n composite score"
  )
)

# ---- 2. Fig_4a: Estimated Marginal Means Plot ----

Fig_4a <- ggplot(table_plot, aes(x = redcap_event_name, y = emmean)) +
  
  # Confidence interval error bars
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL, color = rand_group2),
    position = position_dodge(0.07),
    width = 0.07
  ) +
  
  # Lines connecting timepoints for each group
  geom_line(
    aes(group = rand_group, col = rand_group),
    position = position_dodge(0.05)
  ) +
  
  # Points for estimated means
  geom_point(
    aes(fill = rand_group2, color = rand_group2),
    size = rel(2),
    shape = 21,
    position = position_dodge(0.07)
  ) +
  
  # Labels for Post means with CIs
  ggrepel::geom_text_repel(
    data = table_plot %>% filter(redcap_event_name == "Post"),
    aes(
      label = paste0(
        format(round(emmean, 2), nsmall = 2),
        "\n(",
        round(lower.CL, 2), ";", round(upper.CL, 2),
        ")"
      ),
      color = rand_group2
    ),
    nudge_x = 0.5,
    max.overlaps = 10,
    size = rel(2),
    segment.color = NA,
    direction = "y"
  ) +
  
  # Clean theme + manual color palette
  theme_classic() +
  scale_color_manual(
    limits = c("All", "Control", "Exercise"),
    values = pal_npg("nrc")(10)[c(4, 10, 2)]
  ) +
  scale_fill_manual(
    limits = c("All", "Control", "Exercise"),
    values = pal_npg("nrc")(10)[c(4, 10, 2)]
  ) +
  
  # One panel per outcome
  facet_wrap(outcome ~ ., ncol = 1) +
  
  # Theme adjustments
  theme(
    strip.text.x = element_text(size = 7, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(0.5), margin = margin(l = 0.1)),
    strip.placement = "outside",
    legend.key.size = unit(3, "mm"),
    legend.spacing.x = unit(3, "mm"),
    strip.background.x = element_rect(fill = "white", color = "white"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = rel(0.9)),
    axis.text.y = element_text(size = rel(0.8)),
    text = element_text(family = "Arial", color = "black", size = 8),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0.1, 0.8),
    legend.box.background = element_rect(fill = "white", color = "white"),
    axis.line = element_line(size = 0.1),
    strip.background = element_blank(),
    axis.ticks = element_line(size = 0.1),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 2))) +
  
  # Annotate group differences inside each facet
  geom_text(
    data = table_plot %>%
      ungroup() %>%
      group_by(outcome) %>%
      top_n(2, emmean) %>%
      distinct(outcome, .keep_all = TRUE),
    aes(
      x = 1.5,
      y = 0.75,
      label = paste(
        "Group diff =", sprintf("%.2f", estimate),
        ifelse(
          p.value < 0.001,
          "\np < 0.001",
          paste("\np =", format(round(p.value, 3)))
        )
      )
    ),
    size = rel(2),
    color = "black",
    show.legend = FALSE
  ) +
  
  labs(y = "z-score", x = NULL) +
  scale_x_discrete(guide = "prism_offset")

# ---- 3. Analysis for correlation plot ----

# Define raw cognitive outcomes and combine with strength variable


# Raw (non‑z) cognitive outcomes
outcomes_raw_cog <- paste0(c(
  "comp_ex_func_mean",
  "episodic_mem_mean",
  "processing_speed_mean",
  "working_mem_mean",
  "attentional_control_mean",
  "visuospatial_mean"
))

# Add muscular strength to the list
outcomes_raw2 <- c(outcomes_raw_cog, "strength_mean")


# Compute change scores (Post – Pre) for each participant

data2 <- data %>%
  group_by(record_id) %>%
  # Keep only participants with amyloid data
  filter(!is.na(gaaincrtx_gaainwcb_centiloid_mni)) %>%
  # Compute difference for each outcome
  summarise(
    across(outcomes_raw2, ~ diff(.), .names = "{.col}_diff"),
    rand_group = unique(rand_group)
  )

# Split into physical and cognitive change datasets


# Physical: only strength change
physical <- data2 %>% dplyr::select(record_id, strength_mean_diff)

# Cognitive: first 7 columns (all cognitive diffs)
cognitive <- data2 %>% dplyr::select(c(1:7))

# Convert to long format (one row per variable per participant)
long_data_physical <- physical %>%
  pivot_longer(
    cols = -record_id,
    names_to = "physical",
    values_to = "value_physical"
  )

long_data_cognitive <- cognitive %>%
  pivot_longer(
    cols = -record_id,
    names_to = "cognitive",
    values_to = "value_cognitive"
  )

# Merge physical and cognitive long datasets
combined_data <- full_join(long_data_physical, long_data_cognitive, by = "record_id")


# Assign human‑readable labels to cognitive and physical variables


combined_data <- combined_data %>%
  mutate(
    cognitive = case_when(
      startsWith(cognitive, "comp") ~ "Executive function",
      startsWith(cognitive, "episodic_mem_mean") ~ "Episodic memory",
      startsWith(cognitive, "processing_speed_mean") ~ "Processing speed",
      startsWith(cognitive, "working_mem_mean") ~ "Working memory",
      startsWith(cognitive, "attentional_control_mean") ~ "Attentional/inhibitory\n control",
      startsWith(cognitive, "visuospatial_mean") ~ "Visuospatial\n processing"
    ),
    physical = case_when(
      startsWith(physical, "iso_lower_peak") ~ "Knee extension",
      startsWith(physical, "iso_upper_peak") ~ "Elbow extension",
      startsWith(physical, "pf_sft_2min_step") ~ "2-Minute step",
      startsWith(physical, "pf_sft_timeup_score") ~ "Up and go test",
      startsWith(physical, "pf_sft_6minutewalk_corr") ~ "Six-Minute walk test",
      startsWith(physical, "pf_2km_walk_time") ~ "2 km walking test",
      startsWith(physical, "pf_sft_arm_score") ~ "Arm curl",
      startsWith(physical, "pf_sft_chair_score") ~ "30-seconds sit-to-stand test",
      startsWith(physical, "pf_sppb_repchair") ~ "5-times sit-to-stand test",
      startsWith(physical, "handgrip_score_mean") ~ "Handgrip strength",
      TRUE ~ physical
    )
  )

# Order cognitive domains for plotting
combined_data$cognitive <- factor(
  combined_data$cognitive,
  levels = c(
    "Executive function",
    "Attentional/inhibitory\n control",
    "Episodic memory",
    "Processing speed",
    "Visuospatial\n processing",
    "Working memory"
  )
)

# ---- 4. Create scatterplots (Fig_4b) of strength change vs cognitive change ----


Fig_4b <- ggplot(
  combined_data %>% group_by(record_id),
  aes(x = value_physical, y = value_cognitive, col = physical)
) +
  geom_point(size = 0.4, alpha = 0.4) +
  scale_color_manual(values = pal_npg("nrc")(10)[c(6)]) +
  
  # One panel per cognitive domain
  facet_wrap(cognitive ~ ., ncol = 2, scales = "free") +
  theme_classic() +
  
  # Add regression line
  geom_smooth(method = lm, aes(group = 1), se = FALSE, linewidth = 0.5) +
  
  # Theme adjustments
  theme(
    legend.position = "none",
    text = element_text(colour = "black"),
    strip.text = element_text(size = rel(0.6), face = "bold"),
    axis.text = element_blank(),
    axis.title = element_text(size = rel(0.6)),
    axis.ticks = element_blank(),
    axis.line = element_line(size = 0.1),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.switch.pad.grid = unit(-0.2, "cm")
  ) +
  
  labs(
    x = "Changes in Muscular strength",
    y = "Changes in cognitive function"
  ) +
  
  # Add Pearson correlation per facet
  ggpubr::stat_cor(
    method = "pearson",
    size = 2.2,
    r.accuracy = 0.01,
    p.accuracy = 0.01,
    col = "black",
    aes(group = 1)
  )


# ---- 5. Save Figure 4 (Panels A + B) ----
library(ragg)

tiff(
  "03_outputs/figures/Figure_4_600.tiff",
  width = 5,
  height = 5,
  units = "in",
  res = 600,
  compression = "none"
)

cowplot::plot_grid(
  Fig_4a, Fig_4b,
  labels = c("A", "B"),
  label_size = 12,
  ncol = 2,
  rel_widths = c(1.5, 2)
)

dev.off()



