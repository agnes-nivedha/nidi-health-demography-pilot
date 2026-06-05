# ============================================================
# PART 2: Eurostat/EHIS BMI-by-Education Benchmark
# Countries: Netherlands, Finland, Poland
# Dataset: hlth_ehis_bm1e
# ============================================================

# -----------------------------
# 0. Setup
# -----------------------------

base_dir <- "E:/PHD/NIDI"

packages <- c("tidyverse", "janitor", "ggrepel")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(janitor)
library(ggrepel)

output_dir <- file.path(base_dir, "outputs_part2")
table_dir  <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")

dir.create(output_dir, showWarnings = FALSE)
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1. Locate Eurostat TSV file
# -----------------------------

get_latest_file <- function(pattern) {
  files <- list.files(
    base_dir,
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0) {
    stop(paste("No file found matching pattern:", pattern))
  }
  
  files[which.max(file.info(files)$mtime)]
}

eurostat_file <- get_latest_file("hlth_ehis_bm1e.*\\.tsv$")

cat("Eurostat BMI file used:\n", eurostat_file, "\n\n")

# -----------------------------
# 2. Read and reshape Eurostat file
# -----------------------------

raw <- read_tsv(
  eurostat_file,
  show_col_types = FALSE,
  trim_ws = TRUE
)

# First column contains all Eurostat dimensions
dimension_col <- names(raw)[1]
value_col <- names(raw)[2]

dimension_names <- str_split(dimension_col, "\\\\")[[1]][1] %>%
  str_split(",") %>%
  .[[1]]

part2 <- raw %>%
  separate(
    col = all_of(dimension_col),
    into = dimension_names,
    sep = ",",
    remove = TRUE
  ) %>%
  rename(raw_value = all_of(value_col)) %>%
  mutate(
    raw_value = as.character(raw_value),
    value = str_extract(raw_value, "[0-9.]+") %>% as.numeric(),
    flag = str_remove_all(raw_value, "[0-9.\\s:]")
  )

write_csv(
  part2,
  file.path(table_dir, "01_cleaned_eurostat_bmi_education_long.csv")
)

# -----------------------------
# 3. Add labels
# -----------------------------

part2 <- part2 %>%
  mutate(
    country = case_when(
      geo == "NL" ~ "Netherlands",
      geo == "FI" ~ "Finland",
      geo == "PL" ~ "Poland",
      TRUE ~ geo
    ),
    education = case_when(
      isced11 == "ED0-2" ~ "Low education",
      isced11 == "ED3_4" ~ "Medium education",
      isced11 == "ED5-8" ~ "High education",
      isced11 == "TOTAL" ~ "All education levels",
      TRUE ~ isced11
    ),
    education = factor(
      education,
      levels = c(
        "Low education",
        "Medium education",
        "High education",
        "All education levels"
      )
    ),
    bmi_category = case_when(
      bmi == "BMI18P5-24" ~ "Normal BMI, 18.5-24.9",
      bmi == "BMI25-29" ~ "Pre-obesity, BMI 25-29",
      bmi == "BMI_GE25" ~ "Overweight/obesity, BMI >=25",
      bmi == "BMI_GE30" ~ "Obesity, BMI >=30",
      TRUE ~ bmi
    )
  )

# -----------------------------
# 4. Keep main adult benchmark sample
# -----------------------------

benchmark <- part2 %>%
  filter(
    geo %in% c("NL", "FI", "PL"),
    sex == "T",
    age == "Y_GE18",
    isced11 %in% c("ED0-2", "ED3_4", "ED5-8", "TOTAL"),
    bmi %in% c("BMI18P5-24", "BMI25-29", "BMI_GE25", "BMI_GE30")
  )

write_csv(
  benchmark,
  file.path(table_dir, "02_benchmark_sample_adults_18plus.csv")
)

# -----------------------------
# 5. Table: BMI indicators by country and education
# -----------------------------

table_bmi_by_education <- benchmark %>%
  select(country, education, bmi_category, value) %>%
  pivot_wider(
    names_from = bmi_category,
    values_from = value
  ) %>%
  arrange(country, education)

write_csv(
  table_bmi_by_education,
  file.path(table_dir, "table_1_bmi_by_country_education.csv")
)

# -----------------------------
# 6. Main obesity education gradient
# -----------------------------

obesity_education <- benchmark %>%
  filter(
    bmi == "BMI_GE30",
    isced11 %in% c("ED0-2", "ED3_4", "ED5-8")
  ) %>%
  select(country, geo, education, obesity_pct = value, flag)

write_csv(
  obesity_education,
  file.path(table_dir, "table_2_obesity_by_country_education.csv")
)

obesity_gap <- obesity_education %>%
  select(country, education, obesity_pct) %>%
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
  arrange(desc(absolute_gap_low_minus_high_pp))

write_csv(
  obesity_gap,
  file.path(table_dir, "table_3_obesity_education_gap.csv")
)

# -----------------------------
# 7. Overweight/obesity BMI >=25 education gradient
# -----------------------------

overweight_education <- benchmark %>%
  filter(
    bmi == "BMI_GE25",
    isced11 %in% c("ED0-2", "ED3_4", "ED5-8")
  ) %>%
  select(country, geo, education, overweight_obesity_pct = value, flag)

write_csv(
  overweight_education,
  file.path(table_dir, "table_4_overweight_obesity_by_country_education.csv")
)

overweight_gap <- overweight_education %>%
  select(country, education, overweight_obesity_pct) %>%
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
  arrange(desc(absolute_gap_low_minus_high_pp))

write_csv(
  overweight_gap,
  file.path(table_dir, "table_5_overweight_obesity_education_gap.csv")
)

# -----------------------------
# 8. Overall obesity and overweight/obesity levels
# -----------------------------

overall_bmi <- benchmark %>%
  filter(
    isced11 == "TOTAL",
    bmi %in% c("BMI_GE30", "BMI_GE25")
  ) %>%
  select(country, bmi_category, value) %>%
  pivot_wider(
    names_from = bmi_category,
    values_from = value
  ) %>%
  arrange(country)

write_csv(
  overall_bmi,
  file.path(table_dir, "table_6_overall_obesity_overweight.csv")
)

# -----------------------------
# 9. Figures
# -----------------------------

fig1 <- ggplot(
  obesity_education,
  aes(x = education, y = obesity_pct, group = country, color = country)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Obesity prevalence by education level",
    subtitle = "Eurostat/EHIS, adults aged 18+, 2019",
    x = "Educational attainment",
    y = "Obesity prevalence, BMI >=30 (%)",
    color = "Country"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_1_obesity_by_education_three_countries.png"),
  fig1,
  width = 8,
  height = 5,
  dpi = 300
)

fig2 <- ggplot(
  obesity_gap,
  aes(
    x = reorder(country, absolute_gap_low_minus_high_pp),
    y = absolute_gap_low_minus_high_pp
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Absolute education gap in obesity",
    subtitle = "Low education obesity % minus high education obesity %, adults 18+, 2019",
    x = "Country",
    y = "Low-high obesity gap, percentage points"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_2_obesity_education_gap.png"),
  fig2,
  width = 7,
  height = 5,
  dpi = 300
)

fig3 <- ggplot(
  overweight_education,
  aes(x = education, y = overweight_obesity_pct, group = country, color = country)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Overweight/obesity prevalence by education level",
    subtitle = "Eurostat/EHIS, adults aged 18+, 2019",
    x = "Educational attainment",
    y = "Overweight/obesity prevalence, BMI >=25 (%)",
    color = "Country"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_3_overweight_obesity_by_education_three_countries.png"),
  fig3,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------
# 10. Print summary
# -----------------------------

cat("\n============================================================\n")
cat("PART 2 COMPLETE: Eurostat/EHIS BMI education benchmark\n")
cat("============================================================\n\n")

cat("Obesity by country and education:\n")
print(obesity_education)

cat("\nObesity education gaps:\n")
print(obesity_gap)

cat("\nOverweight/obesity education gaps:\n")
print(overweight_gap)

cat("\nOverall BMI indicators:\n")
print(overall_bmi)

cat("\nOutput tables saved in:\n")
cat(table_dir, "\n\n")

cat("Output figures saved in:\n")
cat(figure_dir, "\n\n")

cat("Suggested interpretation:\n")
cat(
  "In the Eurostat/EHIS three-country benchmark, obesity was socially patterned by education in all three countries. The Netherlands had the lowest overall obesity prevalence but the steepest relative education gradient, with low-educated adults having more than twice the obesity prevalence of high-educated adults. Poland showed the largest absolute low-high education gap, while Finland had the smallest education gradient among the three countries.\n"
)