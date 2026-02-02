# ---- 1. Load Data and Remove Excluded Participants for per protocol analysis ----

library(here)
source("02_scripts/01_data.R")

# Remove participants flagged for exclusion
data <- data %>%
  filter(
    record_id != 155,
    record_id != 211,
    record_id != 264,
    record_id != 318,
    record_id != 350,
    record_id != 372
  )


# ---- 2. Define Outcomes and Fit Models (excluding Mid timepoint) ----

# Cognitive domain outcomes (z-scored)
outcomes <- paste0(
  c(
    "episodic_mem_mean",
    "processing_speed_mean",
    "working_mem_mean",
    "attentional_control_mean",
    "visuospatial_mean"
  ),
  "_z"
)

# Add composite executive function
outcomes <- c("comp_ex_func_mean_z", outcomes)

# Fit models for each outcome (excluding Mid)
results <- map(
  outcomes,
  ~ effects_agueda(.x, data %>% filter(redcap_event_name != "Mid"))
) %>%
  purrr::set_names(outcomes)

# Extract final model summaries (Table 2)
results_table2 <- map_df(results, ~ .x$outcome_final, .id = "outcome")


# ---- 3. Extract Group Differences (Post only) ----

table_plot_main <- map_df(results, ~ .x$group_diff_post, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))


# ---- 4. Extract Estimated Marginal Means ----

table_plot <- map_df(results, ~ .x$model_emm, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))

# Duplicate “All” rows so each outcome has matching Control/Exercise rows
table_plot <- bind_rows(
  table_plot %>% filter(rand_group2 != "All"),
  table_plot %>%
    group_by(outcome) %>%
    filter(rand_group2 == "All") %>%
    slice(rep(which(rand_group2 == "All"), each = 2))
)


# ---- 5. Assign Group Labels (Control vs Exercise) ----

table_plot <- table_plot %>%
  group_by(outcome) %>%
  mutate(
    # First pass: duplicated “All” rows → Exercise
    rand_group = case_when(
      duplicated(rand_group2) & rand_group2 == "All" ~ "Exercise"
    )
  ) %>%
  mutate(
    # Second pass: remaining “All” rows → Control
    rand_group = case_when(
      rand_group2 == "All" & is.na(rand_group) ~ "Control",
      redcap_event_name == "Post" ~ rand_group2,
      !is.na(rand_group) ~ "Exercise"
    )
  )


# ---- 6. Merge EMM with Group Differences ----

table_plot <- table_plot %>%
  arrange(factor(outcome, levels = outcomes)) %>%
  left_join(table_plot_main, by = "outcome")


# ---- 7. Relabel Outcomes for Plotting ----

table_plot <- table_plot %>%
  mutate(
    outcome = case_when(
      startsWith(outcome, "comp") ~ "Executive function",
      startsWith(outcome, "episodic_mem_mean") ~ "Episodic memory",
      startsWith(outcome, "processing_speed_mean") ~ "Processing speed",
      startsWith(outcome, "working_mem_mean") ~ "Working memory",
      startsWith(outcome, "attentional_control_mean") ~ "Attentional/inhibitory control",
      startsWith(outcome, "visuospatial_mean") ~ "Visuospatial processing",
      TRUE ~ outcome
    )
  )

# Set facet order
table_plot$outcome <- factor(
  table_plot$outcome,
  levels = c(
    "Executive function",
    "Attentional/inhibitory control",
    "Episodic memory",
    "Processing speed",
    "Visuospatial processing",
    "Working memory"
  )
)


# ---- 8. Fig_S5: Estimated Marginal Means Across Cognitive Domains, per protocol ----

Fig_S5 <- ggplot(table_plot, aes(x = redcap_event_name, y = emmean)) +
  
  # Confidence interval error bars
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL, color = rand_group2),
    position = position_dodge(0.07), width = 0.07
  ) +
  
  # Lines connecting group means
  geom_line(
    aes(group = rand_group, col = rand_group),
    position = position_dodge(0.05)
  ) +
  
  # Mean points
  geom_point(
    aes(fill = rand_group2, color = rand_group2),
    size = rel(2), shape = 21,
    position = position_dodge(0.07)
  ) +
  
  # Labels for POST means
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
  
  # Facet by cognitive domain
  facet_wrap(outcome ~ ., ncol = 2, scales = "free_x") +
  
  # Theme adjustments
  theme(
    strip.text.x = element_text(size = 8, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(0.5), margin = margin(l = 0.1)),
    legend.key.size = unit(3, "mm"),
    legend.spacing.x = unit(3, "mm"),
    strip.placement = "outside",
    strip.background.x = element_rect(fill = "white", color = "white"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = rel(0.75)),
    axis.text.y = element_text(size = rel(0.75)),
    text = element_text(family = "Arial", color = "black", size = 10),
    legend.position = "top",
    legend.box.background = element_rect(fill = "white", color = "white"),
    axis.line = element_line(size = 0.1),
    axis.ticks = element_line(size = 0.1),
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 2))) +
  
  # Group difference annotation inside each facet
  geom_text(
    data = table_plot %>%
      ungroup() %>%
      group_by(outcome) %>%
      top_n(2, emmean) %>%
      distinct(outcome, .keep_all = TRUE),
    aes(
      x = 1.5, y = 0.75,
      label = paste(
        "Group diff =", sprintf("%.2f", estimate),
        "\np =", p.value
      )
    ),
    size = rel(2),
    color = "black",
    show.legend = FALSE
  ) +
  
  labs(y = "z-score", x = NULL) +
  scale_x_discrete(guide = "prism_offset")


# ---- 9. Save Figure S5 (Panels A + B) ----

library(ragg)

agg_tiff(
  "04_supplementary/figures/Figure_S5_600.tiff",
  width = 4, height = 6, units = "in", res = 600
)

Fig_S5

dev.off()
