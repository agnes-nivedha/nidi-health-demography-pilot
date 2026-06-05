# ============================================================
# PART 3B: Urban green infrastructure context
# Source: European Environment Agency
# Dataset: percentage-of-total-green-infrastructure.csv
# Countries/capitals:
#   Netherlands = Amsterdam
#   Finland     = Helsinki
#   Poland      = Warsaw
# ============================================================

rm(list = ls())

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

output_dir <- file.path(base_dir, "outputs_part3b_green_infrastructure")
table_dir  <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. Locate EEA green infrastructure file
# -----------------------------

green_file <- list.files(
  base_dir,
  pattern = "percentage-of-total-green-infrastructure.*\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(green_file) == 0) {
  stop("Could not find percentage-of-total-green-infrastructure.csv inside E:/PHD/NIDI")
}

green_file <- green_file[which.max(file.info(green_file)$mtime)]

cat("Green infrastructure file used:\n")
cat(green_file, "\n\n")

# -----------------------------
# 2. Read and clean file
# -----------------------------

green_raw <- read_csv(
  green_file,
  show_col_types = FALSE
)

green_clean <- green_raw %>%
  clean_names() %>%
  rename(
    city = city_name_text,
    total_green_infrastructure_pct = total_green_infrastructure_number,
    urban_green_space_pct = urban_green_space_number,
    urban_tree_cover_pct = urban_tree_cover_number
  )

write_csv(
  green_clean,
  file.path(table_dir, "01_eea_green_infrastructure_all_capitals_clean.csv")
)

# -----------------------------
# 3. Keep Netherlands, Finland, Poland capitals
# -----------------------------

capital_lookup <- tibble(
  country = c("Netherlands", "Finland", "Poland"),
  city = c("Amsterdam", "Helsinki", "Warsaw")
)

green_3countries <- green_clean %>%
  inner_join(capital_lookup, by = "city") %>%
  select(
    country,
    city,
    total_green_infrastructure_pct,
    urban_green_space_pct,
    urban_tree_cover_pct
  ) %>%
  arrange(desc(total_green_infrastructure_pct))

write_csv(
  green_3countries,
  file.path(table_dir, "table_1_green_infrastructure_three_capitals.csv")
)

# -----------------------------
# 4. Add Part 3 existing context manually
# PM2.5 and renewable energy values from earlier Part 3
# -----------------------------

part3_context <- tibble(
  country = c("Netherlands", "Finland", "Poland"),
  pm25_deaths_absolute_2023 = c(3847, 34, 25268),
  pm25_deaths_per_100k_2023 = c(21.5, 0.6, 67.2),
  renewable_energy_share_2024 = c(20.2, 52.1, 17.8)
)

part3b_combined <- part3_context %>%
  left_join(green_3countries, by = "country") %>%
  select(
    country,
    city,
    pm25_deaths_absolute_2023,
    pm25_deaths_per_100k_2023,
    renewable_energy_share_2024,
    total_green_infrastructure_pct,
    urban_green_space_pct,
    urban_tree_cover_pct
  ) %>%
  arrange(country)

write_csv(
  part3b_combined,
  file.path(table_dir, "table_2_part3b_combined_environment_green_context.csv")
)

# -----------------------------
# 5. Simple ranking table
# -----------------------------

ranking_table <- part3b_combined %>%
  mutate(
    rank_pm25_burden = min_rank(pm25_deaths_per_100k_2023),
    rank_renewable_energy = min_rank(desc(renewable_energy_share_2024)),
    rank_green_infrastructure = min_rank(desc(total_green_infrastructure_pct)),
    rank_tree_cover = min_rank(desc(urban_tree_cover_pct))
  ) %>%
  select(
    country,
    city,
    rank_pm25_burden,
    rank_renewable_energy,
    rank_green_infrastructure,
    rank_tree_cover
  )

write_csv(
  ranking_table,
  file.path(table_dir, "table_3_environment_indicator_rankings.csv")
)

# -----------------------------
# 6. Figures
# -----------------------------

fig1 <- ggplot(
  green_3countries,
  aes(
    x = reorder(city, total_green_infrastructure_pct),
    y = total_green_infrastructure_pct
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total green infrastructure in selected European capitals",
    subtitle = "European Environment Agency, Urban Atlas-based capital city indicator",
    x = "Capital city",
    y = "Total green infrastructure (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_1_total_green_infrastructure_capitals.png"),
  fig1,
  width = 7,
  height = 5,
  dpi = 300
)

fig2 <- ggplot(
  green_3countries,
  aes(
    x = reorder(city, urban_tree_cover_pct),
    y = urban_tree_cover_pct
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Urban tree cover in selected European capitals",
    subtitle = "European Environment Agency, Urban Atlas-based capital city indicator",
    x = "Capital city",
    y = "Urban tree cover (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_2_urban_tree_cover_capitals.png"),
  fig2,
  width = 7,
  height = 5,
  dpi = 300
)

fig3 <- ggplot(
  part3b_combined,
  aes(
    x = renewable_energy_share_2024,
    y = total_green_infrastructure_pct,
    label = paste0(country, "\n", city)
  )
) +
  geom_point(size = 3) +
  geom_text_repel(size = 3.5) +
  labs(
    title = "Renewable energy and capital-city green infrastructure",
    subtitle = "Country-level renewable energy share and capital-city green infrastructure",
    x = "Renewable energy share, 2024 (%)",
    y = "Capital-city total green infrastructure (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_3_renewables_vs_green_infrastructure.png"),
  fig3,
  width = 7,
  height = 5,
  dpi = 300
)

fig4 <- ggplot(
  part3b_combined,
  aes(
    x = pm25_deaths_per_100k_2023,
    y = total_green_infrastructure_pct,
    label = paste0(country, "\n", city)
  )
) +
  geom_point(size = 3) +
  geom_text_repel(size = 3.5) +
  labs(
    title = "PM2.5 health burden and capital-city green infrastructure",
    subtitle = "PM2.5 deaths per 100k and EEA capital-city green infrastructure",
    x = "PM2.5-related premature deaths per 100,000, 2023",
    y = "Capital-city total green infrastructure (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_4_pm25_vs_green_infrastructure.png"),
  fig4,
  width = 7,
  height = 5,
  dpi = 300
)

# -----------------------------
# 7. Print results
# -----------------------------

cat("\n============================================================\n")
cat("PART 3B COMPLETE: Green infrastructure context\n")
cat("============================================================\n\n")

cat("Green infrastructure table:\n")
print(green_3countries)

cat("\nCombined Part 3B environmental context:\n")
print(part3b_combined)

cat("\nRanking table:\n")
print(ranking_table)

cat("\nTables saved in:\n")
cat(table_dir, "\n\n")

cat("Figures saved in:\n")
cat(figure_dir, "\n\n")

cat("Suggested interpretation:\n\n")
cat(
  "Adding EEA capital-city green infrastructure data strengthens Part 3 by adding a direct health-pathway environmental indicator. Helsinki had the highest total green infrastructure share at 62%, followed by Warsaw at 47% and Amsterdam at 31%. Urban tree cover was also highest in Helsinki and Warsaw, while Amsterdam had much lower tree cover at 14%. This suggests that green-transition context is not captured by renewable energy alone: countries and cities differ across air pollution, energy transition, and urban green infrastructure pathways. Because this is a capital-city indicator, it should be treated as contextual and hypothesis-generating rather than as a national causal predictor of obesity.\n"
)