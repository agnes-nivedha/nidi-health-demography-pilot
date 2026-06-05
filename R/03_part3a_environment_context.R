# ============================================================
# PART 3: Environmental / green-transition context
# Countries: Netherlands, Finland, Poland
# Files:
#   sdg_11_52_page_tabular.tsv
#   nrg_ind_ren$defaultview_tabular.tsv
# ============================================================

# Set project directory
base_dir <- getwd()

# Raw data should be placed locally in data_raw/.
# Raw data are not redistributed in this GitHub repository.
data_dir <- file.path(base_dir, "data_raw")

packages <- c("tidyverse", "janitor")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(tidyverse)
library(janitor)

output_dir <- file.path(base_dir, "outputs")
table_dir  <- file.path(output_dir, "tables", "part3a_environment_context")
figure_dir <- file.path(output_dir, "figures", "part3a_environment_context")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1. Helper: read Eurostat wide TSV and convert to long
# -----------------------------

read_eurostat_wide_tsv <- function(path) {
  
  raw <- readr::read_tsv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    trim_ws = TRUE
  )
  
  first_col <- names(raw)[1]
  
  # Example first column:
  # freq,airpol,effect,unit,geo\TIME_PERIOD
  dim_part <- stringr::str_split(first_col, "\\\\")[[1]][1]
  dim_names <- stringr::str_split(dim_part, ",")[[1]]
  
  year_cols <- names(raw)[-1]
  
  out <- raw %>%
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
      time = as.integer(stringr::str_extract(time, "\\d{4}")),
      raw_value = stringr::str_squish(raw_value),
      value = stringr::str_extract(raw_value, "[0-9.]+") %>% as.numeric(),
      flag = stringr::str_remove_all(raw_value, "[0-9.\\s:]")
    )
  
  out
}
# -----------------------------
# 2. Locate files
# -----------------------------

pm25_file <- list.files(
  data_dir,
  pattern = "sdg_11_52.*\\.tsv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)[1]

ren_file <- list.files(
  data_dir,
  pattern = "nrg_ind_ren.*\\.tsv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)[1]

if (is.na(pm25_file)) {
  stop("PM2.5 file not found. Put the sdg_11_52 TSV file inside data_raw/.")
}

if (is.na(ren_file)) {
  stop("Renewable energy file not found. Put the nrg_ind_ren TSV file inside data_raw/.")
}

cat("PM2.5 file used:\n", pm25_file, "\n\n")
cat("Renewable energy file used:\n", ren_file, "\n\n")

# -----------------------------
# 3. Read files
# -----------------------------

pm25_long <- read_eurostat_wide_tsv(pm25_file)
ren_long  <- read_eurostat_wide_tsv(ren_file)

write_csv(pm25_long, file.path(table_dir, "01_pm25_long_clean.csv"))
write_csv(ren_long, file.path(table_dir, "02_renewable_energy_long_clean.csv"))

# -----------------------------
# 4. Country labels
# -----------------------------

country_lookup <- tibble(
  geo = c("NL", "FI", "PL"),
  country = c("Netherlands", "Finland", "Poland")
)

# -----------------------------
# 5. PM2.5 latest common year
# -----------------------------

pm25_3 <- pm25_long %>%
  filter(
    geo %in% c("NL", "FI", "PL"),
    airpol == "PM2_5",
    effect == "PMD",
    unit == "NR"
  )

pm25_latest_year <- pm25_3 %>%
  filter(!is.na(value)) %>%
  group_by(time) %>%
  summarise(n_countries = n_distinct(geo), .groups = "drop") %>%
  filter(n_countries == 3) %>%
  summarise(latest_year = max(time)) %>%
  pull(latest_year)

pm25_latest <- pm25_3 %>%
  filter(time == pm25_latest_year) %>%
  left_join(country_lookup, by = "geo") %>%
  transmute(
    country,
    geo,
    year = time,
    pm25_premature_deaths = value
  ) %>%
  arrange(desc(pm25_premature_deaths))

write_csv(
  pm25_latest,
  file.path(table_dir, "table_1_pm25_premature_deaths_latest.csv")
)

# -----------------------------
# 6. Renewable energy latest common year
# -----------------------------

ren_3 <- ren_long %>%
  filter(
    geo %in% c("NL", "FI", "PL"),
    nrg_bal == "REN",
    unit == "PC"
  )

ren_latest_year <- ren_3 %>%
  filter(!is.na(value)) %>%
  group_by(time) %>%
  summarise(n_countries = n_distinct(geo), .groups = "drop") %>%
  filter(n_countries == 3) %>%
  summarise(latest_year = max(time)) %>%
  pull(latest_year)

ren_latest <- ren_3 %>%
  filter(time == ren_latest_year) %>%
  left_join(country_lookup, by = "geo") %>%
  transmute(
    country,
    geo,
    year = time,
    renewable_energy_share_pct = value
  ) %>%
  arrange(desc(renewable_energy_share_pct))

write_csv(
  ren_latest,
  file.path(table_dir, "table_2_renewable_energy_share_latest.csv")
)

# -----------------------------
# 7. Combined Part 3 table
# -----------------------------

environment_context <- country_lookup %>%
  left_join(
    pm25_latest %>%
      select(geo, year_pm25 = year, pm25_premature_deaths),
    by = "geo"
  ) %>%
  left_join(
    ren_latest %>%
      select(geo, year_renewables = year, renewable_energy_share_pct),
    by = "geo"
  ) %>%
  arrange(country)

write_csv(
  environment_context,
  file.path(table_dir, "table_3_environment_context_combined.csv")
)

# -----------------------------
# 8. Figures
# -----------------------------

fig1 <- ggplot(
  pm25_latest,
  aes(x = reorder(country, pm25_premature_deaths), y = pm25_premature_deaths)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "PM2.5-related premature deaths",
    subtitle = paste0("Eurostat sdg_11_52, ", pm25_latest_year),
    x = "Country",
    y = "Premature deaths"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_1_pm25_premature_deaths.png"),
  fig1,
  width = 7,
  height = 5,
  dpi = 300
)

fig2 <- ggplot(
  ren_latest,
  aes(x = reorder(country, renewable_energy_share_pct), y = renewable_energy_share_pct)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Share of energy from renewable sources",
    subtitle = paste0("Eurostat nrg_ind_ren, ", ren_latest_year),
    x = "Country",
    y = "Renewable energy share (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(figure_dir, "figure_2_renewable_energy_share.png"),
  fig2,
  width = 7,
  height = 5,
  dpi = 300
)

# -----------------------------
# 9. Print results
# -----------------------------

cat("\n============================================================\n")
cat("PART 3 COMPLETE\n")
cat("============================================================\n\n")

cat("PM2.5 results:\n")
print(pm25_latest)

cat("\nRenewable energy results:\n")
print(ren_latest)

cat("\nCombined environment context:\n")
print(environment_context)

cat("\nTables saved in:\n")
cat(table_dir, "\n\n")

cat("Figures saved in:\n")
cat(figure_dir, "\n\n")
