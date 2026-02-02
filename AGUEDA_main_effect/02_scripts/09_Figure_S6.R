# ---- 1. Load Data and Fit Model ----

source("02_scripts/01_data.R")

# Outcome to analyze
outcomes = "comp_ex_func_mean_z"

# Fit model for the outcome (includes all timepoints)
results = map(outcomes, ~ effects_agueda(.x, data)) %>%
  purrr::set_names(outcomes)


# ---- 2. Extract Model Summary and Group Differences ----

# Full model summary
model_summ = results$comp_ex_func_mean_z$model %>% summary()

# Extract mean estimates for Mid and Post (rows 4 and 5)
table_plot_main = model_summ$mean[4:5, ] %>%
  round(2) %>%
  mutate(n = results$comp_ex_func_mean_z$model$cluster$n)

# Convert rownames to a column
data.table::setDT(table_plot_main, keep.rownames = TRUE)[]
table_plot_main = table_plot_main %>% rename(outcomes = "rn")

# Add timepoint labels
table_plot_main$redcap_event_name = c("Mid", "Post")


# ---- 3. Extract Estimated Marginal Means ----

table_plot = map_df(results, ~ .x$model_emm, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))


# ---- 4. Duplicate “All” rows so each timepoint has matching rows ----

table_plot <- bind_rows(
  table_plot %>% filter(rand_group2 != "All"),
  table_plot %>%
    filter(rand_group2 == "All") %>%
    slice(rep(which(rand_group2 == "All"), each = 2))
)


# ---- 5. Assign Group Labels (Control vs Exercise) ----

table_plot = table_plot %>%
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
      !is.na(rand_group) ~ "Exercise",
      redcap_event_name == "Mid" ~ rand_group2,
      !is.na(rand_group) ~ "Exercise"
    )
  )


# ---- 6. Merge EMM with Group Differences ----

table_plot = table_plot %>%
  left_join(table_plot_main, by = "redcap_event_name") %>%
  mutate(
    outcome = case_when(
      startsWith(outcome, "comp") ~ "Executive function"
    )
  )

# Remove unused “All” rows
table_plot = table_plot %>% filter(rand_group != "All")

# Keep only Pre, Mid, Post rows for plotting
table_plot = table_plot[1:6, ]

# Set timepoint order
table_plot$redcap_event_name = factor(
  table_plot$redcap_event_name,
  levels = c("Pre", "Mid", "Post")
)


# ---- 7. Fig_S6a: Estimated Marginal Means Plot ----

Fig_S6a = ggplot(table_plot, aes(x = redcap_event_name, y = emmean)) +
  
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
    size = 3, shape = 21,
    position = position_dodge(0.07)
  ) +
  
  # Labels for Mid/Post means
  ggrepel::geom_text_repel(
    data = table_plot %>% filter(redcap_event_name != "Pre"),
    aes(
      label = paste0(
        format(round(emmean, 2)),
        "\n(",
        round(lower.CL, 2), ";", round(upper.CL, 2),
        ")"
      ),
      color = rand_group2
    ),
    position = position_nudge_center(
      y = 0.12, center_y = -0.2, x = 0.35
    ),
    min.segment.length = 0,
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
  
  # Theme adjustments
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    strip.placement = "outside",
    strip.background.x = element_rect(fill = "white", color = "white"),
    axis.title = element_blank(),
    axis.title.y = element_text(size = 8, angle = 90),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    text = element_text(family = "Arial", color = "black", size = 10),
    legend.position = "bottom",
    axis.line = element_line(size = 0.1),
    axis.ticks = element_line(size = 0.1)
  ) +
  
  labs(y = "Executive function (z-score)") +
  
  guides(color = guide_legend(override.aes = list(size = 2))) +
  
  # Group difference annotations
  geom_text(
    data = table_plot %>%
      filter(redcap_event_name == "Mid") %>%
      distinct(outcome, .keep_all = TRUE),
    aes(
      x = 1.5, y = 0.6,
      label = paste0(
        "Group diff = ", estimate,
        " (", round(lower, 2), ";", round(upper, 2), ")",
        "\np = ", p.value
      )
    ),
    size = rel(2.5),
    color = "black",
    show.legend = FALSE
  ) +
  geom_text(
    data = table_plot %>%
      filter(redcap_event_name == "Post") %>%
      distinct(outcome, .keep_all = TRUE),
    aes(
      x = 3, y = 0.6,
      label = paste0(
        "Group diff = ", estimate,
        " (", round(lower, 2), ";", round(upper, 2), ")",
        "\np = ", p.value
      )
    ),
    size = rel(2.5),
    color = "black",
    show.legend = FALSE
  ) +
  
  scale_x_discrete(guide = "prism_offset")


# ---- 8. Fig_S6b: Individual Trajectories + Raincloud Plot ----

devtools::source_url(
  "https://raw.githubusercontent.com/yjunechoe/geom_paired_raincloud/master/geom_paired_raincloud.R"
)

Fig_S6b = ggplot(data, aes(redcap_event_name, comp_ex_func_mean_z, fill = rand_group)) +
  
  # Raincloud distribution
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
    legend.text = element_text(size = 20),
    strip.placement = "outside",
    legend.key.size = unit(6, "mm"),
    axis.title = element_blank(),
    axis.title.y = element_text(size = 8, angle = 90),
    axis.text = element_text(size = 8),
    text = element_text(family = "Arial", color = "black", size = 10),
    legend.position = "none",
    axis.line = element_line(size = 0.1),
    strip.background = element_blank(),
    axis.ticks = element_line(size = 0.1),
    plot.title = element_text(hjust = 0.5, size = 14)
  ) +
  
  labs(y = "Executive function (individual values)") +
  
  scale_color_manual(values = pal_npg("nrc")(10)[c(10, 2)]) +
  scale_fill_manual(values = pal_npg("nrc")(10)[c(10, 2)]) +
  scale_x_discrete(guide = "prism_offset")


# ---- 9. Save Figure S6 (Panels A + B) ----

library(ragg)

agg_tiff(
  "04_supplementary/figures/Figure_S6_600.tiff",
  width = 4, height = 6, units = "in", res = 600
)

cowplot::plot_grid(
  Fig_S6a, Fig_S6b,
  labels = c("A", "B"),
  label_size = 12,
  ncol = 1,
  rel_widths = c(1, 1)
)

dev.off()
