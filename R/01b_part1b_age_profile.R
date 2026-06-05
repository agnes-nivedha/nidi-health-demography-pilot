# ============================================================
# PART 1B: Age-profile analysis — fixed minimal version
# Uses only 3 CBS files: Total, 18-65, 65+
# ============================================================

rm(list = ls())

# Set project directory
base_dir <- getwd()

# Raw data should be placed locally in data_raw/.
# Raw data are not redistributed in this GitHub repository.
data_dir <- file.path(base_dir, "data_raw")

packages <- c("tidyverse", "janitor", "ggplot2", "broom", "ggrepel")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(tidyverse)
library(janitor)
library(broom)
library(ggrepel)

output_dir <- file.path(base_dir, "outputs")
table_dir  <- file.path(output_dir, "tables", "part1b_age_profile")
figure_dir <- file.path(output_dir, "figures", "part1b_age_profile")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. Select only the correct files
# -----------------------------

all_files <- list.files(
  data_dir,
  pattern = "Gezondheidsmonitor__regio__2024_04062026_(201221|200710|201039)(\\(1\\))?\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(all_files) < 3) {
  stop("Could not find the three required files: 201221 total, 200710 18-65, 201039 65+.")
}

# If both original and (1) copies exist, keep one file per age code
file_info <- tibble(file = all_files) %>%
  mutate(
    age_code = case_when(
      str_detect(file, "201221") ~ "total_adults",
      str_detect(file, "200710") ~ "age_18_65",
      str_detect(file, "201039") ~ "age_65plus",
      TRUE ~ NA_character_
    ),
    modified = file.info(file)$mtime
  ) %>%
  filter(!is.na(age_code)) %>%
  arrange(age_code, desc(modified)) %>%
  group_by(age_code) %>%
  slice(1) %>%
  ungroup()

health_files <- file_info$file

cat("Files used:\n")
print(health_files)

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

read_one_health_file <- function(path) {
  
  raw <- readr::read_delim(
    path,
    delim = ";",
    locale = locale(decimal_mark = ",", grouping_mark = " "),
    col_types = cols(.default = col_character()),
    trim_ws = TRUE
  )
  
  names(raw) <- names(raw) %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
  
  tibble(
    source_file = basename(path),
    age_group = raw[["Leeftijd"]],
    measure_type = raw[["Marges"]],
    region = raw[["Regio's"]],
    good_very_good_health_pct = parse_number(raw[["Ervaren gezondheid (goed/zeer goed) (%)"]], locale = locale(decimal_mark = ",")),
    one_or_more_longterm_conditions_pct = parse_number(raw[["Eén of meer langdurige aandoeningen (%)"]], locale = locale(decimal_mark = ",")),
    moderate_overweight_pct = parse_number(raw[["Onder- en overgewicht/Mate van overgewicht/Matig overgewicht (%)"]], locale = locale(decimal_mark = ",")),
    obesity_pct = parse_number(raw[["Onder- en overgewicht/Mate van overgewicht/Ernstig overgewicht (obesitas) (%)"]], locale = locale(decimal_mark = ",")),
    meets_physical_activity_guideline_pct = parse_number(raw[["Bewegen en sport/Voldoet aan beweegrichtlijnen (%)"]], locale = locale(decimal_mark = ",")),
    weekly_sport_pct = parse_number(raw[["Bewegen en sport/Wekelijkse sporters (%)"]], locale = locale(decimal_mark = ",")),
    informal_caregiver_pct = parse_number(raw[["Mantelzorg geven/Mantelzorger (%)"]], locale = locale(decimal_mark = ",")),
    smokers_pct = parse_number(raw[["Rokers (%)"]], locale = locale(decimal_mark = ",")),
    meets_alcohol_guideline_pct = parse_number(raw[["Alcoholgebruik/Voldoet aan richtlijn alcoholgebruik (%)"]], locale = locale(decimal_mark = ","))
  )
}

safe_cor <- function(data, x, y, label) {
  d <- data %>% filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
  test <- cor.test(d[[x]], d[[y]])
  tibble(
    comparison = label,
    n = nrow(d),
    r = unname(test$estimate),
    p_value = test$p.value,
    ci_low = test$conf.int[1],
    ci_high = test$conf.int[2]
  )
}

# -----------------------------
# 3. Create all_health
# -----------------------------

all_health <- map_dfr(health_files, read_one_health_file) %>%
  mutate(
    age_group = str_squish(age_group),
    measure_type = str_squish(measure_type),
    region = normalise_region(region),
    age_code = case_when(
      age_group == "Totaal" ~ "total_adults",
      age_group == "18 tot 65 jaar" ~ "age_18_65",
      age_group == "65 jaar of ouder" ~ "age_65plus",
      TRUE ~ NA_character_
    ),
    age_label = case_when(
      age_code == "age_18_65" ~ "18-65 years",
      age_code == "age_65plus" ~ "65+ years",
      age_code == "total_adults" ~ "Total adults",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(measure_type == "Waarde") %>%
  filter(!is.na(age_code)) %>%
  filter(
    region != "Nederland",
    !str_detect(region, "\\(PV\\)"),
    !str_detect(region, "^GGD"),
    !str_detect(region, "^Veiligh"),
    !str_detect(region, "^Dienst Gezondheid")
  )

cat("\nRows in all_health:", nrow(all_health), "\n")
cat("\nAge groups found:\n")
print(table(all_health$age_group))

write_csv(all_health, file.path(table_dir, "01_all_health_age_groups_clean.csv"))

# -----------------------------
# 4. Keep municipalities with all three age groups
# -----------------------------

complete_regions <- all_health %>%
  distinct(region, age_code) %>%
  count(region, name = "n_age_groups") %>%
  filter(n_age_groups == 3) %>%
  pull(region)

age_long <- all_health %>%
  filter(region %in% complete_regions) %>%
  distinct(region, age_code, .keep_all = TRUE)

cat("\nMunicipalities with all 3 age groups:", length(complete_regions), "\n")
print(sort(complete_regions))

write_csv(age_long, file.path(table_dir, "02_age_long_complete_regions.csv"))

# -----------------------------
# 5. Wide format and gaps
# -----------------------------

age_wide <- age_long %>%
  select(
    region,
    age_code,
    obesity_pct,
    one_or_more_longterm_conditions_pct,
    meets_physical_activity_guideline_pct,
    good_very_good_health_pct,
    weekly_sport_pct
  ) %>%
  pivot_wider(
    names_from = age_code,
    values_from = c(
      obesity_pct,
      one_or_more_longterm_conditions_pct,
      meets_physical_activity_guideline_pct,
      good_very_good_health_pct,
      weekly_sport_pct
    ),
    names_glue = "{.value}_{age_code}"
  ) %>%
  mutate(
    total_obesity_pct = obesity_pct_total_adults,
    obesity_gap_65plus_minus_18_65 = obesity_pct_age_65plus - obesity_pct_age_18_65,
    ltc_gap_65plus_minus_18_65 =
      one_or_more_longterm_conditions_pct_age_65plus -
      one_or_more_longterm_conditions_pct_age_18_65,
    pa_gap_65plus_minus_18_65 =
      meets_physical_activity_guideline_pct_age_65plus -
      meets_physical_activity_guideline_pct_age_18_65,
    good_health_gap_65plus_minus_18_65 =
      good_very_good_health_pct_age_65plus -
      good_very_good_health_pct_age_18_65
  ) %>%
  arrange(desc(total_obesity_pct))

write_csv(age_wide, file.path(table_dir, "03_age_wide_with_gaps.csv"))

# -----------------------------
# 6. Correlations
# -----------------------------

correlations <- bind_rows(
  safe_cor(age_wide, "total_obesity_pct", "obesity_pct_age_65plus",
           "Total adult obesity vs 65+ obesity"),
  safe_cor(age_wide, "total_obesity_pct", "meets_physical_activity_guideline_pct_age_65plus",
           "Total adult obesity vs 65+ physical activity"),
  safe_cor(age_wide, "total_obesity_pct", "one_or_more_longterm_conditions_pct_age_65plus",
           "Total adult obesity vs 65+ long-term conditions"),
  safe_cor(age_wide, "total_obesity_pct", "good_very_good_health_pct_age_65plus",
           "Total adult obesity vs 65+ good/very good health"),
  safe_cor(age_wide, "total_obesity_pct", "obesity_gap_65plus_minus_18_65",
           "Total adult obesity vs 65+ minus 18-65 obesity gap"),
  safe_cor(age_wide, "total_obesity_pct", "ltc_gap_65plus_minus_18_65",
           "Total adult obesity vs 65+ minus 18-65 LTC gap"),
  safe_cor(age_wide, "total_obesity_pct", "pa_gap_65plus_minus_18_65",
           "Total adult obesity vs 65+ minus 18-65 physical activity gap")
)

write_csv(correlations, file.path(table_dir, "table_1_age_profile_correlations.csv"))

# -----------------------------
# 7. Group municipalities by obesity tertile
# -----------------------------

age_wide <- age_wide %>%
  mutate(
    obesity_group = ntile(desc(total_obesity_pct), 3),
    obesity_group = case_when(
      obesity_group == 1 ~ "Higher-obesity municipalities",
      obesity_group == 2 ~ "Middle-obesity municipalities",
      obesity_group == 3 ~ "Lower-obesity municipalities"
    ),
    obesity_group = factor(
      obesity_group,
      levels = c("Lower-obesity municipalities", "Middle-obesity municipalities", "Higher-obesity municipalities")
    )
  )

age_long_grouped <- age_long %>%
  left_join(age_wide %>% select(region, obesity_group), by = "region") %>%
  mutate(
    age_label = factor(age_label, levels = c("18-65 years", "65+ years", "Total adults"))
  )

summary_by_group <- age_long_grouped %>%
  group_by(obesity_group, age_label) %>%
  summarise(
    municipalities = n_distinct(region),
    mean_obesity_pct = mean(obesity_pct, na.rm = TRUE),
    mean_ltc_pct = mean(one_or_more_longterm_conditions_pct, na.rm = TRUE),
    mean_physical_activity_pct = mean(meets_physical_activity_guideline_pct, na.rm = TRUE),
    mean_good_health_pct = mean(good_very_good_health_pct, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_by_group, file.path(table_dir, "table_2_summary_by_obesity_group_and_age.csv"))

gap_summary <- age_wide %>%
  group_by(obesity_group) %>%
  summarise(
    municipalities = n(),
    mean_total_obesity_pct = mean(total_obesity_pct, na.rm = TRUE),
    mean_18_65_obesity_pct = mean(obesity_pct_age_18_65, na.rm = TRUE),
    mean_65plus_obesity_pct = mean(obesity_pct_age_65plus, na.rm = TRUE),
    mean_obesity_gap_65plus_minus_18_65 = mean(obesity_gap_65plus_minus_18_65, na.rm = TRUE),
    mean_ltc_gap_65plus_minus_18_65 = mean(ltc_gap_65plus_minus_18_65, na.rm = TRUE),
    mean_pa_gap_65plus_minus_18_65 = mean(pa_gap_65plus_minus_18_65, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(gap_summary, file.path(table_dir, "table_3_gap_summary_by_obesity_group.csv"))

# -----------------------------
# 8. Figures
# -----------------------------

p1 <- ggplot(
  summary_by_group %>% filter(age_label != "Total adults"),
  aes(x = age_label, y = mean_obesity_pct, group = obesity_group, color = obesity_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Obesity by age group and municipal obesity profile",
    x = "Age group",
    y = "Mean obesity prevalence (%)",
    color = "Municipality group"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure_1_obesity_age_profile.png"), p1, width = 8, height = 5, dpi = 300)

p2 <- ggplot(
  summary_by_group %>% filter(age_label != "Total adults"),
  aes(x = age_label, y = mean_physical_activity_pct, group = obesity_group, color = obesity_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Physical activity by age group and municipal obesity profile",
    x = "Age group",
    y = "Mean adults meeting physical activity guidelines (%)",
    color = "Municipality group"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure_2_physical_activity_age_profile.png"), p2, width = 8, height = 5, dpi = 300)

p3 <- ggplot(
  summary_by_group %>% filter(age_label != "Total adults"),
  aes(x = age_label, y = mean_ltc_pct, group = obesity_group, color = obesity_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Long-term conditions by age group and municipal obesity profile",
    x = "Age group",
    y = "Mean adults with one or more long-term conditions (%)",
    color = "Municipality group"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure_3_ltc_age_profile.png"), p3, width = 8, height = 5, dpi = 300)

p4 <- ggplot(age_wide, aes(x = total_obesity_pct, y = obesity_pct_age_65plus, label = region)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text_repel(size = 3, max.overlaps = 30) +
  labs(
    title = "Total adult obesity and obesity among adults aged 65+",
    x = "Total adult obesity prevalence (%)",
    y = "65+ obesity prevalence (%)"
  ) +
  theme_minimal()

ggsave(file.path(figure_dir, "figure_4_total_obesity_vs_65plus_obesity.png"), p4, width = 8, height = 5, dpi = 300)

# -----------------------------
# 9. Print results
# -----------------------------

cat("\n============================================================\n")
cat("PART 1B COMPLETE\n")
cat("============================================================\n\n")

cat("Municipalities included:", nrow(age_wide), "\n\n")

cat("Correlation results:\n")
print(correlations)

cat("\nSummary by obesity group:\n")
print(summary_by_group)

cat("\nGap summary:\n")
print(gap_summary)

cat("\nTables saved in:\n", table_dir, "\n")
cat("Figures saved in:\n", figure_dir, "\n")
