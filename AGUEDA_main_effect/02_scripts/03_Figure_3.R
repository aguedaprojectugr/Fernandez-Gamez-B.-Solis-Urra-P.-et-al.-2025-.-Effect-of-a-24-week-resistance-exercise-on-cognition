

# ---- 1. Run Main Effects and Prepare Tables ----

# Load data and functions
source("02_scripts/01_data.R")

# Define the cognitive outcomes of interest (all z-scored)
outcomes <- paste0(
  c("episodic_mem_mean", "processing_speed_mean", "working_mem_mean",
    "attentional_control_mean", "visuospatial_mean"),
  "_z"
)

## Run the effects_agueda() function for each outcome ----
# Excluding the 'Mid' timepoint from the dataset
results <- map(
  outcomes,
  ~ effects_agueda(.x, data %>% filter(redcap_event_name != "Mid"))
) %>% purrr::set_names(outcomes)

# Extract final outcome results for Table 2
results_table2 <- map_df(results, ~ .x$outcome_final, .id = "outcome")

# Extract post-intervention group differences
table_plot_main <- map_df(results, ~ .x$group_diff_post, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))

# Extract estimated marginal means (EMMs)
table_plot <- map_df(results, ~ .x$model_emm, .id = "outcome") %>%
  filter(endsWith(outcome, "_z"))

# Duplicate the "All" group rows so that each outcome has two rows
# (likely for plotting Control vs Exercise comparisons)
table_plot <- bind_rows(
  table_plot %>% filter(rand_group2 != "All"),
  table_plot %>%
    group_by(outcome) %>%
    filter(rand_group2 == "All") %>%
    slice(rep(which(rand_group2 == "All"), each = 2))
)

# Recode group labels to create a clean "rand_group" variable
table_plot <- table_plot %>%
  group_by(outcome) %>%
  mutate(
    # First pass: mark duplicated "All" rows as Exercise
    rand_group = case_when(
      duplicated(rand_group2) & rand_group2 == "All" ~ "Exercise"
    )
  ) %>%
  mutate(
    # Second pass: assign remaining "All" rows as Control
    # Otherwise use original group labels for Post
    rand_group = case_when(
      rand_group2 == "All" & is.na(rand_group) ~ "Control",
      redcap_event_name == "Post" ~ rand_group2,
      !is.na(rand_group) ~ "Exercise"
    )
  )

# Order outcomes in the original order defined above
table_plot <- table_plot %>%
  arrange(factor(outcome, levels = outcomes))

# Merge with post-intervention group differences
table_plot <- table_plot %>%
  left_join(table_plot_main, by = "outcome")

# Replace outcome variable names with human-readable labels
table_plot <- table_plot %>%
  mutate(
    outcome = case_when(
      startsWith(outcome, "episodic_mem_mean") ~ "Episodic memory",
      startsWith(outcome, "processing_speed_mean") ~ "Processing speed",
      startsWith(outcome, "working_mem_mean") ~ "Working memory",
      startsWith(outcome, "attentional_control_mean") ~ "Attentional/inhibitory control",
      startsWith(outcome, "visuospatial_mean") ~ "Visuospatial processing",
      TRUE ~ outcome
    )
  )


# ---- 2. Fig_3a: Estimated Marginal Means Plots ----

design = "
AABB
CCDD
#EE#
"
Fig_3a = ggplot(table_plot, aes(x = redcap_event_name, y = emmean)) +
  # Error bars
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL, color = rand_group2), 
                position = position_dodge(0.07), width = 0.07) +
  # Lines connecting the points
  geom_line(aes(group= rand_group, y = emmean, col = rand_group), position = position_dodge(0.05)) +
  # Points with dodge and custom shape
  geom_point(aes(fill = rand_group2, color = rand_group2), size = rel(2), shape = 21, 
             position = position_dodge(0.07)) +
  # Labels for Post-event means
  ggrepel::geom_text_repel(
    data = table_plot %>% filter(redcap_event_name == "Post"), 
    aes(label = paste0(format(round(emmean, 2),nsmall = 2),'\n(',round(lower.CL,2),';',round(upper.CL,2),')'),
        color = rand_group2),
    nudge_x = 0.5, nudge_y = 0,
    max.overlaps = 10,
    size = rel(3),  # Adjusted label size
    segment.color = NA,
    direction = "y") +
  # Use a clean theme and color scales
  theme_classic() +
  scale_color_manual(limits = c("All", "Control", "Exercise"), values = pal_npg("nrc")(10)[c(4,10,2)]) +
  scale_fill_manual(limits = c("All", "Control", "Exercise"), values = pal_npg("nrc")(10)[c(4,10,2)]) +
  facet_wrap(outcome~., ncol = 5) +
  # Theme adjustments for better visual appearance
  theme(
    strip.text.x = element_text(size = 8, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(0.5), margin = margin(l = 0.1)),  # Adjusted legend text size
    strip.placement = 'outside',
    legend.key.size = unit(3, "mm"),
    legend.spacing.x = unit(3, "mm"),
    strip.background.x = element_rect(fill = 'white', color = "white"),
    axis.title.x = element_blank(),  # Remove axis titles for simplicity
    axis.text.x= element_text(size = rel(0.9)),  # Standardized axis text size
    axis.text.y= element_text(size = rel(0.9)),  # Standardized axis text size
    text = element_text(family = "Arial", color = "black", size = 10),  # Standardized general text size
    legend.position = c(0.01, .9),
    legend.justification = c(0.1, .8),
    legend.box.background = element_rect(fill = 'white', color = "white"),
    axis.line = element_line(size=0.1),
    strip.background = element_blank(),
    axis.ticks = element_line(size = 0.1),
    plot.title = element_text(hjust = 0.5, size = 14)  # Centered and larger title
  ) +  
  guides(color = guide_legend(override.aes = list(size = 2))) +
  
  # Group difference annotations
  geom_text(
    data = table_plot %>% 
      ungroup() %>% 
      group_by(outcome) %>% 
      top_n(2, emmean) %>% 
      distinct(outcome, .keep_all = TRUE),
    aes(x = 1.5, y = 0.75, 
        label = paste("Group diff =", sprintf('%.2f', estimate), ifelse(p.value < 0.001, '\np < 0.001',paste('\np =',format(round(p.value, digits = 3)))))),
        size = rel(3),  # Adjusted annotation label size
    color = "black", 
    show.legend = FALSE
  ) + labs(y = 'z-score',x = NULL) +
  scale_x_discrete(#limits = c(0, 6), guide = "prism_offset_minor"

        guide = "prism_offset") 

# ---- 3. Moderation analysis ----
# This section evaluates whether intervention effects differ across demographic,
# biomarker, and cognitive subgroups (moderators).

# Define moderator variables:
# sex, age group, education, APOE4 status, amyloid status, and cognitive subgroups
int = c(
  "screen_gender",
  "rand_age_group",
  "screen_years_edu_cat",
  "gen_apoee4_carrier",
  "pet_amyloid_status_mni",
  paste0(subgroup, '_cat')
)

# Create high/low categories for each cognitive outcome
data <- high_low_pre(outcomes, data)
outcomes_cat = paste0(outcomes, '_cat')

# Convert outcome categories to ordered factors (High > Low)
data <- data %>%
  mutate_at(vars(outcomes_cat), ~ factor(., levels = c("High", "Low")))

# Remove Mid timepoint and split dataset by each moderator
datas = data %>% filter(redcap_event_name != 'Mid') %>% ungroup()

# For each moderator, split data into its levels (e.g., Male/Female)
datas <- map(int, ~ split(datas, datas[[.x]])) %>% purrr::set_names(int)

# Run effects_agueda() separately for each moderator level and each outcome
results_mod = list()
for (i in int) {
  level1 = names(datas[[i]])[1]
  level2 = names(datas[[i]])[2]
  
  # Model for level 1
  results_mod[[i]][[level1]] =
    map(outcomes, ~ effects_agueda(.x, datas[[i]][[level1]])) %>%
    purrr::set_names(outcomes)
  
  # Model for level 2
  results_mod[[i]][[level2]] =
    map(outcomes, ~ effects_agueda(.x, datas[[i]][[level2]])) %>%
    purrr::set_names(outcomes)
}

# Flatten nested list into a single list of model outputs
table_mod_plot = do.call(c, unlist(results_mod, recursive = FALSE))

# Extract final model results (Table 3)
results_table3 = map_df(table_mod_plot, ~ .x$outcome_final, .id = "outcome")

# Extract post-intervention group differences
table_mod_plot = map_df(table_mod_plot, ~ .x$group_diff_post, .id = "outcome")

# Separate combined ID into moderator, level, and outcome
table_mod_plot = separate(
  table_mod_plot,
  outcome,
  sep = '\\.',
  into = c("mod", "level", "outcome")
)

# Create numeric level index (1, 2)
table_mod_plot = table_mod_plot %>%
  group_by(mod, outcome) %>%
  mutate(level2 = row_number())

table_mod_plot$level3 = table_mod_plot$level2
table_mod_plot$level2 = as.factor(table_mod_plot$level2)

## ---- Interaction p-values (moderator × group) ----

# Run interaction models for each moderator × outcome
outcomes_list <- setNames(outcomes, outcomes)
results_int <- setNames(
  lapply(int, function(x)
    map(outcomes_list,
        ~ effects_agueda_int(.x, x, data %>% filter(redcap_event_name != 'Mid'))
    )),
  int
)

# Extract interaction p-values
table_int = map_df(flatten(results_int), ~ .x$outcome_final)

# Clean and deduplicate
table_int = table_int %>%
  bind_rows() %>%
  group_by(outcome) %>%
  fill(everything(), .direction = "downup") %>%
  distinct(.keep_all = TRUE) %>%
  filter(endsWith(outcome, "_z"))

# Convert wide → long (one row per moderator)
table_int = table_int %>%
  pivot_longer(2:length(table_int), names_to = 'mod', values_to = 'p_int')

table_int$mod = str_remove(table_int$mod, 'p.value_')

# Merge interaction p-values into main table
table_mod_plot = table_mod_plot %>% left_join(table_int, by = c('mod', 'outcome'))

## ---- Cognitive-domain moderator (special case) ----

table_mod_plot2 = table_mod_plot %>%
  filter(mod %in% paste0(outcomes, '_cat'))

table_mod_plot2$mod = str_remove(table_mod_plot2$mod, '_cat')

# Keep only matching outcome/moderator pairs
table_mod_plot2 = table_mod_plot2 %>% filter(mod == outcome)

# Rename moderator label
table_mod_plot2$mod = rep('Cognitive\n domain', nrow(table_mod_plot2))
table_mod_plot2$n = rep(45, nrow(table_mod_plot2))

## ---- Keep only selected moderators and append cognitive domain ----

table_mod_plot = table_mod_plot %>%
  filter(mod %in% int[c(1:5, 12, 13)]) %>%  # adjust if moderator order changes
  bind_rows(table_mod_plot2)

# Rename moderators and outcomes for plotting
table_mod_plot = table_mod_plot %>%
  mutate(
    mod = case_when(
      startsWith(mod, "screen_gender") ~ "Sex",
      startsWith(mod, "rand_age_group") ~ "Age",
      startsWith(mod, "screen_years_edu_cat") ~ "Educational\n level",
      startsWith(mod, "gen_apoee4_carrier") ~ "APOE\n carrier",
      startsWith(mod, "pet_amyloid_status_mni") ~ "Amyloid\n burden",
      startsWith(mod, "disease_advance") ~ "Comorbidities",
      startsWith(mod, "screen_smds4_score_cat") ~ "Subjective\n cognitive decline",
      TRUE ~ mod
    ),
    outcome = case_when(
      startsWith(outcome, 'episodic_mem_mean') ~ "Episodic memory",
      startsWith(outcome, 'processing_speed_mean') ~ "Processing speed",
      startsWith(outcome, 'working_mem_mean') ~ "Working memory",
      startsWith(outcome, 'attentional_control_mean') ~ "Attentional/inhibitory control",
      startsWith(outcome, 'visuospatial_mean') ~ "Visuospatial processing"
    )
  )

# Order moderator factor levels
table_mod_plot$mod = factor(
  table_mod_plot$mod,
  levels = c(
    "Sex", "Age", "Educational\n level", "Comorbidities",
    "APOE\n carrier", "Amyloid\n burden",
    "Cognitive\n domain", "Subjective\n cognitive decline"
  )
)

# Add sample size to level labels
table_mod_plot$level = paste0(table_mod_plot$level, '\n (n=', table_mod_plot$n, ')')

# Remove missing moderators
table_mod_plot = table_mod_plot %>% filter(!is.na(mod))

## ---- Adjust p-values (BH correction) ----
table_mod_plot = table_mod_plot %>%
  ungroup() %>%
  group_by(outcome) %>%
  mutate(p_adj = round(p.adjust(p.value, method = "BH"), 3))

# ---- 4. Plotting (Figure 3B) ----

Fig_3b = ggplot(
  table_mod_plot,
  aes(x = estimate,
      y = tidytext::reorder_within(level, level3, within = mod),
      col = level2)
) +
  # Background shading for selected moderators
  geom_rect(
    data = subset(table_mod_plot,
                  mod %in% c("Sex", "Educational\n level", "APOE\n carrier", "Cognitive\n domain")),
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "gray70", col = NA, alpha = 0.1
  ) +
  geom_rect(
    data = subset(table_mod_plot, mod == "Age"),
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill = "white", col = NA, alpha = 0.2
  ) +
  
  # Points and error bars
  geom_point(position = position_dodge(width = 0.8), size = rel(1)) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    width = 0,
    position = position_dodge(width = 0.8)
  ) +
  
  tidytext::scale_y_reordered() +
  facet_grid(mod ~ outcome, scales = "free_y") +
  theme_classic() +
  
  # Theme adjustments
  theme(
    text = element_text(family = "Arial", color = "black"),
    legend.position = "none",
    strip.background.y = element_rect(fill = 'white', color = "white", linewidth = 0.1),
    strip.text.x = element_text(size = 8, face = "bold"),
    strip.text.y.left = element_text(size = 6, face = "bold"),
    strip.text.y.right = element_text(size = 7, face = "bold"),
    axis.text.y = element_text(size = rel(0.7), angle = 45),
    axis.text.x = element_text(size = rel(0.8)),
    axis.title.x = element_text(size = rel(0.8)),
    axis.line = element_line(size = 0.1),
    panel.spacing = unit(0, 'lines'),
    axis.ticks = element_line(size = 0.1),
    strip.placement = 'outside',
    strip.switch.pad.grid = unit(-0.2,'cm'),
    strip.background = element_blank()
    
  ) +
  
  # Add interaction p-values
  geom_label(
    label = paste0('p for int = ', table_mod_plot$p_int),
    x = -0.5, y = 1.5,
    colour = 'black',
    size = rel(2),
    label.size = NA,
    fill = NA
  ) +
  
  # Add estimate labels with significance stars
  geom_text_repel(
    aes(label = paste0(
      format(round(estimate, 3)),
      ' (', lower, ';', upper, ')',
      ifelse(p_adj < 0.05, "**", "")
    ),
    color = level2),
    nudge_y = 0.05,
    nudge_x = 0.2,
    size = rel(2.5),
    min.segment.length = 3
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.3) +
  labs(x = "Favors control   Favors Exercise", y = NULL) +
  scale_color_manual(values = pal_npg("nrc")(10)[c(3, 5)])

Fig_3b = Fig_3b + scale_x_continuous(guide = "prism_offset_minor")

# ----5. Save Figure 3 ----

tiff(
  "03_outputs/figures/Figure_3_600.tiff",
  width = 12, height = 10, units = "in", res = 600, compression = "none"
)

cowplot::plot_grid(
  Fig_3a, Fig_3b,
  labels = c('A', 'B'),
  label_size = 12,
  ncol = 1,
  rel_heights = c(1, 3)
)

dev.off()

