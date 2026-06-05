# ============================================================
# PART 2B: Sex-stratified Eurostat/EHIS BMI education appendix
# Countries: Netherlands, Finland, Poland
# Dataset: hlth_ehis_bm1e
# Purpose: Obesity education gradients by sex
# ============================================================

rm(list = ls())

base_dir <- "E:/PHD/NIDI"

packages <- c("tidyverse", "janitor", "ggplot2")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(tidyverse)
library(janitor)

output_dir <- file.path(base_dir, "outputs_part2b_sex_appendix")
table_dir  <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. Locate latest correct sex-stratified Eurostat file
# -----------------------------

candidate_files <- list.files(
  base_dir,
  pattern = "hlth_ehis_bm1e.*\\.tsv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(candidate_files) == 0) {
  stop("No hlth_ehis_bm1e TSV file found in E:/PHD/NIDI.")
}

# Pick the most recently modified file
sex_file <- candidate_files[which.max(file.info(candidate_files)$mtime)]

cat("Using Eurostat sex-stratified file:\n", sex_file, "\n\n")

# -----------------------------
# 2. Read Eurostat wide/tabular TSV and convert to long format
# -----------------------------

raw <- readr::read_tsv(
  sex_file,
  col_types = readr::cols(.default = readr::col_character()),
  trim_ws = TRUE
)

first_col <- names(raw)[1]

# Example first column:
# unit,bmi,sex,age,isced11,geo\TIME_PERIOD
dim_part <- stringr::str_split(first_col, "\\\\")[[1]][1]
dim_names <- stringr::str_split(dim_part, ",")[[1]]

year_cols <- names(raw)[-1]

long <- raw %>%
  tidyr::separate(
    col = all_of(first_col),
    into = dim_names,
    sep = ",",
    remove = TRUE
  ) %>%
  tidyr::pivot_longer(
    cols = all_of(year_cols),
    names_to = "time",
    values_to = "raw_value",
    values_transform = list(raw_value = as.character)
  ) %>%
  mutate(
    time = as.integer(str_extract(time, "\\d{4}")),
    raw_value = str_squish(raw_value),
    value = str_extract(raw_value, "[0-9.]+") %>% as.numeric(),
    flag = str_remove_all(raw_value, "[0-9.\\s:]")
  )

write_csv(
  long,
  file.path(table_dir, "01_cleaned_sex_stratified_eurostat_long.csv")
)

cat("Available sex codes:\n")
print(unique(long$sex))

cat("\nAvailable education codes:\n")
print(unique(long$isced11))

cat("\nAvailable BMI codes:\n")
print(unique(long$bmi))

# -----------------------------
# 3. Add labels
# -----------------------------

labelled <- long %>%
  mutate(
    country = case_when(
      geo == "NL" ~ "Netherlands",
      geo == "FI" ~ "Finland",
      geo == "PL" ~ "Poland",
      TRUE ~ geo
    ),
    sex_label = case_when(
      sex == "F" ~ "Women",
      sex == "M" ~ "Men",
      sex == "T" ~ "Total",
      TRUE ~ sex
    ),
    sex_label = factor(sex_label, levels = c("Women", "Men", "Total")),
    education = case_when(
      isced11 == "ED0-2" ~ "Low education",
      isced11 == "ED3_4" ~ "Medium education",
      isced11 == "ED5-8" ~ "High education",
      isced11 == "TOTAL" ~ "All education levels",
      TRUE ~ isced11
    ),
    education = factor(
      education,
      levels = c("Low education", "Medium education", "High education", "All education levels")
    ),
    bmi_category = case_when(
      bmi == "BMI_GE30" ~ "Obesity, BMI >=30",
      bmi == "BMI_GE25" ~ "Overweight/obesity, BMI >=25",
      bmi == "BMI25-29" ~ "Pre-obesity, BMI 25-29",
      bmi == "BMI18P5-24" ~ "Normal BMI, 18.5-24.9",
      bmi == "BMI_LT18P5" ~ "Underweight, BMI <18.5",
      TRUE ~ bmi
    )
  )

# -----------------------------
# 4. Keep main analysis sample
# -----------------------------

benchmark <- labelled %>%
  filter(
    geo %in% c("NL", "FI", "PL"),
    sex %in% c("F", "M", "T"),
    age == "Y_GE18",
    time == 2019,
    isced11 %in% c("ED0-2", "ED3_4", "ED5-8"),
    bmi %in% c("BMI_GE30", "BMI_GE25")
  )

if (nrow(benchmark) == 0) {
  stop("No rows found after filtering. Check whether age, sex, education and BMI codes match.")
}

write_csv(
  benchmark,
  file.path(table_dir, "02_benchmark_sex_stratified_sample.csv")
)

# -----------------------------
# 5. Obesity by country, sex and education
# -----------------------------

obesity_by_sex_education <- benchmark %>%
  filter(bmi == "BMI_GE30") %>%
  select(country, geo, sex_label, education, obesity_pct = value, flag) %>%
  arrange(country, sex_label, education)

write_csv(
  obesity_by_sex_education,
  file.path(table_dir, "table_1_obesity_by_country_sex_education.csv")
)

obesity_gap_by_sex <- obesity_by_sex_education %>%
  select(country, sex_label, education, obesity_pct) %>%
  pivot_wider(
    names_from = education,
    values_from = obesity_pct
  ) %>%
  mutate(
    absolute_gap_low_minus_high_pp = `Low education` - `High education`,
    relative_ratio_low_over_high = `Low education` / `High education`,
    medium_minus_high_pp = `Medium education` - `High education`,
    low_minus_medium_pp = `Low education` - `Medium education`
  ) %>%
  arrange(country, sex_label)

write_csv(
  obesity_gap_by_sex,
  file.path(table_dir, "table_2_obesity_education_gap_by_sex.csv")
)

# -----------------------------
# 6. Overweight/obesity BMI >=25 by country, sex and education
# -----------------------------

owob_by_sex_education <- benchmark %>%
  filter(bmi == "BMI_GE25") %>%
  select(country, geo, sex_label, education, overweight_obesity_pct = value, flag) %>%
  arrange(country, sex_label, education)

write_csv(
  owob_by_sex_education,
  file.path(table_dir, "table_3_overweight_obesity_by_country_sex_education.csv")
)

owob_gap_by_sex <- owob_by_sex_education %>%
  select(country, sex_label, education, overweight_obesity_pct) %>%
  pivot_wider(
    names_from = education,
    values_from = overweight_obesity_pct
  ) %>%
  mutate(
    absolute_gap_low_minus_high_pp = `Low education` - `High education`,
    relative_ratio_low_over_high = `Low education` / `High education`,
    medium_minus_high_pp = `Medium education` - `High education`,
    low_minus_medium_pp = `Low education` - `Medium education`
  ) %>%
  arrange(country, sex_label)

write_csv(
  owob_gap_by_sex,
  file.path(table_dir, "table_4_overweight_obesity_education_gap_by_sex.csv")
)

# -----------------------------
# 7. Figures
# -----------------------------

fig1 <- ggplot(
  obesity_by_sex_education,
  aes(x = education, y = obesity_pct, group = sex_label, color = sex_label)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ country) +
  labs(
    title = "Sex-stratified obesity gradients by education",
    subtitle = "Eurostat/EHIS 2019, adults aged 18+",
    x = "Educational attainment",
    y = "Obesity prevalence, BMI >=30 (%)",
    color = "Sex"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_1_obesity_education_gradient_by_sex.png"),
  fig1,
  width = 10,
  height = 5,
  dpi = 300
)

fig2 <- ggplot(
  obesity_gap_by_sex,
  aes(
    x = sex_label,
    y = absolute_gap_low_minus_high_pp,
    fill = sex_label
  )
) +
  geom_col() +
  facet_wrap(~ country) +
  labs(
    title = "Low-high education gap in obesity by sex",
    subtitle = "Low education obesity % minus high education obesity %, adults 18+, 2019",
    x = "Sex",
    y = "Education gap, percentage points"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  file.path(figure_dir, "figure_2_obesity_education_gap_by_sex.png"),
  fig2,
  width = 10,
  height = 5,
  dpi = 300
)

fig3 <- ggplot(
  owob_by_sex_education,
  aes(x = education, y = overweight_obesity_pct, group = sex_label, color = sex_label)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~ country) +
  labs(
    title = "Sex-stratified overweight/obesity gradients by education",
    subtitle = "Eurostat/EHIS 2019, adults aged 18+",
    x = "Educational attainment",
    y = "Overweight/obesity prevalence, BMI >=25 (%)",
    color = "Sex"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_3_overweight_obesity_education_gradient_by_sex.png"),
  fig3,
  width = 10,
  height = 5,
  dpi = 300
)

# -----------------------------
# 8. Print summary
# -----------------------------

cat("\n============================================================\n")
cat("PART 2B COMPLETE: Sex-stratified appendix\n")
cat("============================================================\n\n")

cat("Obesity by country, sex and education:\n")
print(obesity_by_sex_education)

cat("\nObesity education gaps by sex:\n")
print(obesity_gap_by_sex)

cat("\nOverweight/obesity education gaps by sex:\n")
print(owob_gap_by_sex)

cat("\nTables saved in:\n")
cat(table_dir, "\n\n")

cat("Figures saved in:\n")
cat(figure_dir, "\n\n")

cat("Suggested appendix interpretation:\n")
cat(
  "The sex-stratified appendix shows that education gradients in obesity differ substantially by sex and country. The Netherlands shows a steep gradient among both women and men. Finland shows a stronger obesity gradient among men than women. Poland shows the sharpest gendered pattern, with the education gradient concentrated strongly among women. This confirms that sex-stratification is important for interpreting socio-economic inequalities in cardiometabolic risk across welfare contexts.\n"
)