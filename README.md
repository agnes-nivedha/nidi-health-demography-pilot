# Health Demography Pilot: Cardiometabolic Inequalities, Living Environments and Green-Transition Context

This repository contains reproducible R scripts and non-sensitive outputs for an exploratory health demography pilot prepared for a PhD application to the SEHealth/SoGreen project at NIDI-KNAW.

## Aim

The pilot examines how cardiometabolic health inequalities are patterned across:

1. Dutch municipal living environments
2. Adult age profiles
3. Education and sex gradients across the Netherlands, Finland and Poland
4. Environmental-health and green-transition contexts

## Data sources

The analysis uses publicly available aggregate data from:

* CBS/RIVM Gezondheidsmonitor 2024
* CBS SES-WOA
* Eurostat/EHIS 2019
* Eurostat SDG and energy indicators
* European Environment Agency green-infrastructure indicators

Raw data are not redistributed in this repository. Users should download source datasets directly from the original providers and place them locally in a `data_raw/` folder.

## Repository contents

* `R/` contains reproducible R scripts
* `outputs/figures/` contains selected generated figures
* `outputs/tables/` contains non-sensitive aggregate output tables
* `docs/` contains the supplementary pilot summary report

## Script overview

* `01_part1a_municipal_pilot.R` — Dutch municipal obesity, SES, physical activity and chronic-health burden
* `01b_part1b_age_profile.R` — age-profile analysis using 18–65, 65+ and total adult groups
* `02_part2a_education_gradients.R` — Eurostat/EHIS education gradients in obesity and overweight/obesity
* `02b_part2b_sex_stratified_gradients.R` — sex-stratified education gradients by country
* `03_part3a_environment_context.R` — PM2.5-related premature deaths and renewable-energy context
* `03b_part3b_green_infrastructure.R` — EEA capital-city green infrastructure and urban tree-cover context

## Reproducibility note

The scripts document the data-cleaning and analysis workflow. Some file paths may need to be adapted depending on where source data are stored locally. Raw source datasets are intentionally excluded from the repository.

## Status

This is an exploratory pilot portfolio. Results should be interpreted as descriptive and hypothesis-generating, not as causal estimates.

## Author

Agnes Nivedha Selvaraj
PhD applicant — Health Demography / Population Health / Environmental Inequality
