# ============================================================
# PART 1: NIDI Netherlands Municipality Pilot
# Topic: Obesity, area-level SES, physical activity,
#        and chronic-health burden in selected Dutch municipalities
# ============================================================

# -----------------------------
# 0. Setup
# -----------------------------

base_dir <- "E:/PHD/NIDI"

packages <- c("tidyverse", "broom", "ggrepel", "janitor")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(broom)
library(ggrepel)
library(janitor)

output_dir <- file.path(base_dir, "outputs_part1")
table_dir  <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")

dir.create(output_dir, showWarnings = FALSE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1. Find files automatically
# -----------------------------

get_latest_file <- function(pattern) {
  files <- list.files(
    base_dir,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0) {
    stop(paste("No file found matching pattern:", pattern))
  }
  
  files[which.max(file.info(files)$mtime)]
}

health_file <- get_latest_file("Gezondheidsmonitor.*regio.*2024.*\\.csv$")
ses_file    <- get_latest_file("Sociaal.*economische.*status.*\\.csv$")

cat("Health file used:\n", health_file, "\n\n")
cat("SES-WOA file used:\n", ses_file, "\n\n")

# -----------------------------
# 2. Helper functions
# -----------------------------

normalise_region <- function(x) {
  x %>%
    str_replace_all("[’‘`]", "'") %>%
    str_replace_all(" \\(gemeente\\)", "") %>%
    str_replace_all(" \\(Z\\.\\)", "") %>%
    str_squish()
}

parse_cbs_number <- function(x) {
  
  if (is.numeric(x)) {
    return(x)
  }
  
  readr::parse_number(
    as.character(x),
    locale = locale(decimal_mark = ",", grouping_mark = " ")
  )
}

make_three_groups <- function(x, labels) {
  
  group_number <- dplyr::ntile(x, 3)
  
  factor(
    dplyr::case_when(
      is.na(group_number) ~ NA_character_,
      group_number == 1 ~ labels[1],
      group_number == 2 ~ labels[2],
      group_number == 3 ~ labels[3]
    ),
    levels = labels
  )
}

# -----------------------------
# 3. Clean CBS Health Monitor 2024
# -----------------------------

clean_health_86137 <- function(path) {
  
  lines <- readr::read_lines(path, locale = locale(encoding = "UTF-8"))
  lines <- lines[lines != ""]
  data_lines <- lines[-1]
  
  parsed <- map_dfr(data_lines, function(line) {
    
    m <- str_match(line, '^"([^;]+);""([^"]+)"";""([^"]+)"";(.*)$')
    
    if (is.na(m[1, 1])) {
      return(NULL)
    }
    
    age_group    <- m[1, 2]
    measure_type <- m[1, 3]
    region       <- m[1, 4]
    
    values_raw <- m[1, 5] %>%
      str_remove_all('"') %>%
      str_split(";", simplify = FALSE) %>%
      .[[1]]
    
    values_raw <- c(values_raw, rep(NA_character_, 9))[1:9]
    
    values_num <- parse_cbs_number(values_raw)
    
    tibble(
      age_group = age_group,
      measure_type = measure_type,
      region = region,
      good_very_good_health_pct = values_num[1],
      one_or_more_longterm_conditions_pct = values_num[2],
      moderate_overweight_pct = values_num[3],
      obesity_pct = values_num[4],
      meets_physical_activity_guideline_pct = values_num[5],
      weekly_sport_pct = values_num[6],
      informal_caregiver_pct = values_num[7],
      smokers_pct = values_num[8],
      meets_alcohol_guideline_pct = values_num[9]
    )
  })
  
  parsed
}

health <- clean_health_86137(health_file)

write_csv(
  health,
  file.path(table_dir, "01_cleaned_health_monitor_2024.csv")
)

# -----------------------------
# 4. Clean CBS SES-WOA file
# -----------------------------

clean_ses_85900 <- function(path) {
  
  raw <- read_delim(
    path,
    delim = ";",
    locale = locale(decimal_mark = ",", grouping_mark = " "),
    show_col_types = FALSE,
    trim_ws = TRUE
  )
  
  names(raw) <- names(raw) %>% str_squish()
  
  find_col <- function(pattern, required = TRUE) {
    col <- names(raw)[str_detect(names(raw), regex(pattern, ignore_case = TRUE))]
    
    if (length(col) == 0 && required) {
      stop(paste("Required SES column not found:", pattern))
    }
    
    if (length(col) == 0) {
      return(NA_character_)
    }
    
    col[1]
  }
  
  period_col     <- find_col("^Perioden$|Period")
  region_col     <- find_col("Wijken en buurten|Regio")
  household_col  <- find_col("Particuliere huishoudens")
  ses_score_col  <- find_col("Totaalscore.*Gemiddelde score")
  dispersion_col <- find_col("Spreiding.*totaal.*Waarde", required = FALSE)
  
  tibble(
    period = raw[[period_col]],
    region = raw[[region_col]],
    private_households = parse_cbs_number(raw[[household_col]]),
    ses_woa_total_score = parse_cbs_number(raw[[ses_score_col]]),
    ses_woa_dispersion = if (!is.na(dispersion_col)) {
      parse_cbs_number(raw[[dispersion_col]])
    } else {
      NA_real_
    }
  )
}

ses <- clean_ses_85900(ses_file)

write_csv(
  ses,
  file.path(table_dir, "02_cleaned_ses_woa.csv")
)

# -----------------------------
# 5. Keep municipality-level health rows
# -----------------------------

health_municipality <- health %>%
  filter(age_group == "Totaal") %>%
  filter(measure_type == "Waarde") %>%
  mutate(region_key = normalise_region(region)) %>%
  filter(
    region_key != "Nederland",
    !str_detect(region_key, "^GGD"),
    !str_detect(region_key, "^Veiligh"),
    !str_detect(region_key, "^Dienst Gezondheid")
  )

ses_municipality <- ses %>%
  mutate(region_key = normalise_region(region)) %>%
  filter(region_key %in% health_municipality$region_key) %>%
  arrange(region_key, desc(private_households)) %>%
  distinct(region_key, .keep_all = TRUE)

# -----------------------------
# 6. Merge Health Monitor + SES-WOA
# -----------------------------

merged <- health_municipality %>%
  inner_join(
    ses_municipality %>%
      select(region_key, private_households, ses_woa_total_score, ses_woa_dispersion),
    by = "region_key"
  ) %>%
  transmute(
    region = region_key,
    obesity_pct,
    moderate_overweight_pct,
    overweight_or_obesity_pct = moderate_overweight_pct + obesity_pct,
    good_very_good_health_pct,
    one_or_more_longterm_conditions_pct,
    meets_physical_activity_guideline_pct,
    weekly_sport_pct,
    smokers_pct,
    meets_alcohol_guideline_pct,
    private_households,
    ses_woa_total_score,
    ses_woa_dispersion
  ) %>%
  arrange(ses_woa_total_score)

merged <- merged %>%
  mutate(
    area_ses_category = make_three_groups(
      ses_woa_total_score,
      c("Lower area SES", "Middle area SES", "Higher area SES")
    ),
    physical_activity_category = make_three_groups(
      meets_physical_activity_guideline_pct,
      c("Lower activity", "Middle activity", "Higher activity")
    )
  )
cat("\nSES-WOA check:\n")
print(summary(merged$ses_woa_total_score))
print(merged %>% select(region, ses_woa_total_score))

if (n_distinct(merged$ses_woa_total_score) < 2) {
  stop("SES-WOA was not parsed correctly: all values are identical.")
}
write_csv(
  merged,
  file.path(table_dir, "03_merged_netherlands_municipality_pilot.csv")
)

cat("Merged rows:", nrow(merged), "\n\n")

# -----------------------------
# 7. Main descriptive table
# -----------------------------

table_1_main <- merged %>%
  select(
    region,
    obesity_pct,
    moderate_overweight_pct,
    overweight_or_obesity_pct,
    ses_woa_total_score,
    meets_physical_activity_guideline_pct,
    weekly_sport_pct,
    one_or_more_longterm_conditions_pct,
    good_very_good_health_pct,
    smokers_pct,
    meets_alcohol_guideline_pct
  )

write_csv(
  table_1_main,
  file.path(table_dir, "table_1_main_municipality_results.csv")
)

# -----------------------------
# 8. Correlations and effect sizes
# -----------------------------

calc_relation <- function(data, xvar, label) {
  
  d <- data %>%
    filter(!is.na(.data[[xvar]]), !is.na(obesity_pct))
  
  if (n_distinct(d[[xvar]]) < 2) {
    return(
      tibble(
        comparison = label,
        n = nrow(d),
        correlation_with_obesity = NA_real_,
        slope_per_1_unit = NA_real_,
        slope_per_10_units = NA_real_,
        r_squared = NA_real_,
        interpretation = "Cannot estimate: predictor has no variation"
      )
    )
  }
  
  model <- lm(reformulate(xvar, response = "obesity_pct"), data = d)
  model_summary <- summary(model)
  
  corr_value <- cor(d[[xvar]], d$obesity_pct)
  
  tibble(
    comparison = label,
    n = nrow(d),
    correlation_with_obesity = corr_value,
    slope_per_1_unit = coef(model)[2],
    slope_per_10_units = coef(model)[2] * 10,
    r_squared = model_summary$r.squared,
    interpretation = case_when(
      abs(corr_value) < 0.20 ~ "Weak / almost no linear association",
      abs(corr_value) < 0.50 ~ "Moderate association",
      abs(corr_value) < 0.70 ~ "Moderate-to-strong association",
      TRUE ~ "Strong association"
    )
  )
}
table_2_correlations <- bind_rows(
  calc_relation(
    merged,
    "ses_woa_total_score",
    "Area SES-WOA score vs obesity"
  ),
  calc_relation(
    merged,
    "meets_physical_activity_guideline_pct",
    "Physical activity guideline adherence vs obesity"
  ),
  calc_relation(
    merged,
    "weekly_sport_pct",
    "Weekly sport participation vs obesity"
  ),
  calc_relation(
    merged,
    "one_or_more_longterm_conditions_pct",
    "Long-term conditions vs obesity"
  ),
  calc_relation(
    merged,
    "good_very_good_health_pct",
    "Good/very good health vs obesity"
  ),
  calc_relation(
    merged,
    "overweight_or_obesity_pct",
    "Overweight or obesity BMI >= 25 vs obesity"
  )
) %>%
  mutate(
    interpretation = case_when(
      abs(correlation_with_obesity) < 0.20 ~ "Weak / almost no linear association",
      abs(correlation_with_obesity) < 0.50 ~ "Moderate association",
      abs(correlation_with_obesity) < 0.70 ~ "Moderate-to-strong association",
      TRUE ~ "Strong association"
    )
  )

write_csv(
  table_2_correlations,
  file.path(table_dir, "table_2_correlations_and_effect_sizes.csv")
)

# -----------------------------
# 9. Area SES summary
# -----------------------------

table_3_area_ses <- merged %>%
  group_by(area_ses_category) %>%
  summarise(
    municipalities = n(),
    mean_obesity_pct = mean(obesity_pct, na.rm = TRUE),
    mean_overweight_or_obesity_pct = mean(overweight_or_obesity_pct, na.rm = TRUE),
    mean_physical_activity_pct = mean(meets_physical_activity_guideline_pct, na.rm = TRUE),
    mean_longterm_conditions_pct = mean(one_or_more_longterm_conditions_pct, na.rm = TRUE),
    mean_good_very_good_health_pct = mean(good_very_good_health_pct, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  table_3_area_ses,
  file.path(table_dir, "table_3_obesity_by_area_ses_category.csv")
)

# -----------------------------
# 10. Physical activity summary
# -----------------------------

table_4_physical_activity <- merged %>%
  group_by(physical_activity_category) %>%
  summarise(
    municipalities = n(),
    mean_obesity_pct = mean(obesity_pct, na.rm = TRUE),
    mean_overweight_or_obesity_pct = mean(overweight_or_obesity_pct, na.rm = TRUE),
    mean_area_ses_score = mean(ses_woa_total_score, na.rm = TRUE),
    mean_longterm_conditions_pct = mean(one_or_more_longterm_conditions_pct, na.rm = TRUE),
    mean_good_very_good_health_pct = mean(good_very_good_health_pct, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  table_4_physical_activity,
  file.path(table_dir, "table_4_obesity_by_physical_activity_category.csv")
)

# -----------------------------
# 11. Highest and lowest obesity municipalities
# -----------------------------

table_5_highest_lowest <- bind_rows(
  merged %>%
    arrange(desc(obesity_pct)) %>%
    slice_head(n = 5) %>%
    mutate(rank = row_number(), group = "Highest obesity"),
  
  merged %>%
    arrange(obesity_pct) %>%
    slice_head(n = 5) %>%
    mutate(rank = row_number(), group = "Lowest obesity")
) %>%
  select(
    group,
    rank,
    region,
    obesity_pct,
    ses_woa_total_score,
    meets_physical_activity_guideline_pct,
    one_or_more_longterm_conditions_pct,
    good_very_good_health_pct
  )

write_csv(
  table_5_highest_lowest,
  file.path(table_dir, "table_5_highest_lowest_obesity.csv")
)

# -----------------------------
# 12. Exploratory ecological regression models
# Important: these are municipality-level OLS models,
# not individual-level causal models.
# -----------------------------

model_1 <- lm(obesity_pct ~ ses_woa_total_score, data = merged)

model_2 <- lm(
  obesity_pct ~ meets_physical_activity_guideline_pct,
  data = merged
)

model_3 <- lm(
  obesity_pct ~ ses_woa_total_score + meets_physical_activity_guideline_pct,
  data = merged
)

model_4 <- lm(
  obesity_pct ~ one_or_more_longterm_conditions_pct,
  data = merged
)

table_6_models <- bind_rows(
  tidy(model_1, conf.int = TRUE) %>% mutate(model = "Model 1: obesity ~ area SES"),
  tidy(model_2, conf.int = TRUE) %>% mutate(model = "Model 2: obesity ~ physical activity"),
  tidy(model_3, conf.int = TRUE) %>% mutate(model = "Model 3: obesity ~ area SES + physical activity"),
  tidy(model_4, conf.int = TRUE) %>% mutate(model = "Model 4: obesity ~ long-term conditions")
) %>%
  select(model, term, estimate, conf.low, conf.high, std.error, statistic, p.value)

write_csv(
  table_6_models,
  file.path(table_dir, "table_6_ecological_regression_models.csv")
)

model_fit <- tibble(
  model = c(
    "Model 1: obesity ~ area SES",
    "Model 2: obesity ~ physical activity",
    "Model 3: obesity ~ area SES + physical activity",
    "Model 4: obesity ~ long-term conditions"
  ),
  n = c(nobs(model_1), nobs(model_2), nobs(model_3), nobs(model_4)),
  r_squared = c(
    summary(model_1)$r.squared,
    summary(model_2)$r.squared,
    summary(model_3)$r.squared,
    summary(model_4)$r.squared
  ),
  adjusted_r_squared = c(
    summary(model_1)$adj.r.squared,
    summary(model_2)$adj.r.squared,
    summary(model_3)$adj.r.squared,
    summary(model_4)$adj.r.squared
  )
)

write_csv(
  model_fit,
  file.path(table_dir, "table_7_model_fit_statistics.csv")
)

# -----------------------------
# 13. Figures
# -----------------------------

fig1 <- ggplot(
  merged,
  aes(x = ses_woa_total_score, y = obesity_pct, label = region)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(size = 3, max.overlaps = 20) +
  labs(
    title = "Obesity and area-level SES in selected Dutch municipalities",
    x = "SES-WOA total score",
    y = "Obesity prevalence among adults 18+ (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_1_obesity_vs_area_ses.png"),
  fig1,
  width = 8,
  height = 5,
  dpi = 300
)

fig2 <- ggplot(
  merged,
  aes(x = meets_physical_activity_guideline_pct, y = obesity_pct, label = region)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(size = 3, max.overlaps = 20) +
  labs(
    title = "Physical activity and obesity in selected Dutch municipalities",
    x = "Adults meeting physical activity guidelines (%)",
    y = "Obesity prevalence among adults 18+ (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_2_obesity_vs_physical_activity.png"),
  fig2,
  width = 8,
  height = 5,
  dpi = 300
)

fig3 <- ggplot(
  merged,
  aes(x = one_or_more_longterm_conditions_pct, y = obesity_pct, label = region)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(size = 3, max.overlaps = 20) +
  labs(
    title = "Obesity and long-term conditions in selected Dutch municipalities",
    x = "Adults with one or more long-term conditions (%)",
    y = "Obesity prevalence among adults 18+ (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_3_obesity_vs_longterm_conditions.png"),
  fig3,
  width = 8,
  height = 5,
  dpi = 300
)

fig4 <- ggplot(
  table_4_physical_activity,
  aes(x = physical_activity_category, y = mean_obesity_pct)
) +
  geom_col() +
  labs(
    title = "Mean obesity prevalence by physical activity category",
    x = "Physical activity category",
    y = "Mean obesity prevalence (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_4_obesity_by_physical_activity_category.png"),
  fig4,
  width = 7,
  height = 5,
  dpi = 300
)

# -----------------------------
# 14. Print key results
# -----------------------------

cat("\n============================================================\n")
cat("PART 1 COMPLETE: Netherlands municipality pilot\n")
cat("============================================================\n\n")

cat("Number of merged municipalities:", nrow(merged), "\n\n")

cat("Main correlation results:\n")
print(table_2_correlations)

cat("\nArea SES summary:\n")
print(table_3_area_ses)

cat("\nPhysical activity summary:\n")
print(table_4_physical_activity)

cat("\nEcological regression model fit:\n")
print(model_fit)

cat("\nOutput tables saved in:\n")
cat(table_dir, "\n\n")

cat("Output figures saved in:\n")
cat(figure_dir, "\n\n")

cat("Suggested key paragraph:\n")
cat(
  "Across selected Dutch municipalities, obesity prevalence was examined in relation to area-level SES-WOA, physical activity, and chronic-health burden. Area-level SES alone showed limited explanatory value, while physical activity and long-term conditions were more strongly associated with obesity. These preliminary ecological results suggest that Dutch obesity inequalities should be studied through a combined health-demographic framework linking social living environment, behavioural context, and chronic disease burden.\n"
)
##### test$$

# Partial correlation: obesity and physical activity,
# controlling for long-term conditions

df <- merged

# Residualise physical activity on long-term conditions
pa_model <- lm(
  meets_physical_activity_guideline_pct ~ one_or_more_longterm_conditions_pct,
  data = df
)

pa_resid <- resid(pa_model)

# Residualise obesity on long-term conditions
obesity_model <- lm(
  obesity_pct ~ one_or_more_longterm_conditions_pct,
  data = df
)

obesity_resid <- resid(obesity_model)

# Correlate residuals
cor.test(pa_resid, obesity_resid)