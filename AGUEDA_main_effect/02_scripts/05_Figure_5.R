# ---- 1. Code for attendance plot ----

# Load data
source("02_scripts/01_data.R")


# Select exercise‑group participants at POST and extract attendance variables


data_exercise <- data %>%
  dplyr::select(1:4, rand_group, feeling_pre_ave:total_attendance) %>% 
  filter(redcap_event_name == "Post" & rand_group == "Exercise")


# Reshape attendance data to long format (one row per attendance type)


data_exercise_long <- data_exercise %>%
  pivot_longer(
    cols = c(
      onsite_attendance,
      missing_sessions,
      onsite_recovered,
      online_recovered
    ),
    names_to = "attendance_type",
    values_to = "results"
  ) %>%
  mutate(
    # Order attendance categories logically
    attendance_type = factor(attendance_type, levels = c(
      "missing_sessions",
      "online_recovered",
      "onsite_recovered",
      "onsite_attendance"
    )),
    # Reorder participants by attendance values
    record_id = reorder(record_id, results)
  )


# Compute total number of recovered sessions (online + onsite)


order <- data_exercise_long %>%
  summarise(
    recover = sum(
      results[data_exercise_long$attendance_type %in%
                c("online_recovered", "onsite_recovered")],
      na.rm = TRUE
    )
  )

# Merge recovery summary back into long dataset
data_exercise_long <- data_exercise_long %>%
  left_join(order, by = "record_id")

# Reorder participants by total attendance and recovered sessions
data_exercise_long$record_id <- fct_reorder2(
  data_exercise_long$record_id,
  data_exercise_long$total_attendance,
  -data_exercise_long$recover
)

# Rename attendance categories for plotting
data_exercise_long$attendance_type <- factor(
  data_exercise_long$attendance_type,
  labels = c("Missing", "Online", "At-home", "In-person")
)


# ----  2. Create attendance compliance plot (Figure 5A) ----


Fig_5a <- ggplot(
  data_exercise_long,
  aes(
    x = reorder(record_id, -total_attendance),
    y = results,
    fill = attendance_type
  )
) +
  geom_col(position = "fill") +  # Stacked proportional bars
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1),
    expand = c(0, 0),
    breaks = seq(0, 1, by = 0.2)
  ) +
  scale_fill_manual(values = pal_npg("nrc")(10)[c(5, 6, 2, 3)]) +
  scale_color_manual(values = pal_npg("nrc")(10)[c(5, 6, 2, 3)]) +
  theme_classic() +
  geom_hline(
    yintercept = 0.8,
    linetype = "dashed",
    color = "black",
    linewidth = 0.2
  ) +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", size = 0.1),
    axis.ticks = element_line(size = 0.1),
    axis.text.y = element_text(family = "Arial", size = 6, color = "black"),
    text = element_text(family = "Arial", size = 8, color = "black"),
    axis.text.x = element_blank(),
    axis.title.x = element_text(vjust = 0),
    axis.ticks.x = element_blank(),
    legend.spacing = unit(10, "mm"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(3, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-1, -1, -5, -1)
  ) +
  labs(x = "Individual subjects", y = "Attendance") +
  guides(fill = guide_legend(reverse = TRUE))


# ---- 3. Load full exercise dataset for RPE analysis ----


data_all_exercise <- read_xlsx("01_data/raw/data_all_exercise.xlsx")


# Compute mean RPE per week (session-level and exercise-level)

data_all_exercise <- data_all_exercise %>%
  group_by(week_target) %>%
  mutate(
    mean_rpe_achieved_session = mean(rpe_sesion, na.rm = TRUE),
    mean_rpe_achieved_exercise = mean(rpe_exercise, na.rm = TRUE)
  )

# ----  2. Create RPE progression plot (Figure 5B)

Fig_5b <- ggplot(data_all_exercise, aes(x = week_target)) +
  geom_line(
    aes(y = rpe_target, color = "Mean RPE Target"),
    size = 1
  ) +
  geom_line(
    aes(y = mean_rpe_achieved_session,
        color = "Mean RPE achieved per training sesion"),
    size = 1
  ) +
  geom_line(
    aes(y = mean_rpe_achieved_exercise,
        color = "Mean RPE achieved per training exercise"),
    size = 1
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2)
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) +
  labs(
    x = "Week",
    y = "Rate of Perceived Exertion",
    color = ""
  ) +
  scale_color_manual(
    values = colorRampPalette(pal_npg("nrc")(10))(30)[c(7, 4, 2)]
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black", size = 0.1),
    axis.text = element_text(family = "Arial", size = 6, color = "black"),
    axis.ticks = element_line(size = 0.1),
    axis.title.y = element_text(size = 8),
    text = element_text(family = "Arial", size = 6, color = "black"),
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, -10, -10),
    legend.text = element_text(size = 6)
  )


# ---- 4. Save Figure 5 (Panels A + B) ----


tiff(
  "03_outputs/figures/Figure_5_600.tiff",
  width = 6,
  height = 6,
  units = "in",
  res = 600,
  compression = "none"
)

cowplot::plot_grid(
  Fig_5a, Fig_5b,
  labels = c("A", "B"),
  label_size = 12,
  ncol = 1,
  rel_widths = c(1, 1)
)

dev.off()
