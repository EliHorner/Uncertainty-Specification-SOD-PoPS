## Setup validation
# devtools::install_github("ncsu-landscape-dynamics/rpops")
library(PoPS)
library(terra)
library(folderfun)
library(doParallel)
# library(plyr)

setff("In", "C:/Users/cmjone25/Desktop/SOD_OR/")

## cauchy only
infected_years_file <- ffIn("Cumulative Infections/cum_inf_2019_eu1.tif")
infected_file <- ffIn("End of Year Infections/end_inf_2018_eu1.tif")
host_file <- ffIn("Hosts/hosts_2019.tif")
total_populations_file <- ffIn("Hosts/lemma_max100m.tif")
temp <- TRUE
temperature_coefficient_file <- ffIn("Weather/weather_coef_2019.tif")
precip <- FALSE
precipitation_coefficient_file <- ""
model_type <- "SI"
latency_period <- 0
time_step <- "week"
season_month_start <- 1
season_month_end <- 12
start_date <- '2020-01-01'
end_date <- '2020-12-31'
use_lethal_temperature <- FALSE
temperature_file <- ''
lethal_temperature <- -15
lethal_temperature_month <- 1
mortality_on <- FALSE
mortality_rate <- 0
mortality_time_lag <- 0
mortality_frequency = "year"
mortality_frequency_n = 1
management <- FALSE
treatment_dates <- c('2020-12-24')
treatments_file <- ""
treatment_method <- "ratio"
natural_kernel_type <- "cauchy"
anthropogenic_kernel_type <- "cauchy"
natural_dir <- "NONE"
natural_kappa <- 0
anthropogenic_dir <- "NONE"
anthropogenic_kappa <- 0
pesticide_duration <- c(0)
pesticide_efficacy <- 1.0
mask <- ffIn('mask.tif')
output_frequency <- "year"
movements_file = ""
use_movements <- FALSE
number_of_iterations <- 10
number_of_cores <- 5
success_metric <- "quantity"
means <- read.table('parameters/eu1_2019_means.csv', header = F)
parameter_means <- t(means)
parameter_means <- parameter_means[1,]
parameter_means[1] <- 2.5
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
parameter_cov_matrix[5:8, ] <- 0
parameter_cov_matrix[ , 5:8] <- 0
output_frequency <- "year"
output_frequency_n <- 1
movements_file <- ""
use_movements <- FALSE
start_exposed <- FALSE
generate_stochasticity <- TRUE
establishment_stochasticity <- TRUE
movement_stochasticity <- TRUE
dispersal_stochasticity <- TRUE
establishment_probability <- 0.5
dispersal_percentage <- 0.99
quarantine_areas_file <- ""
use_quarantine <- FALSE
use_spreadrates <- FALSE
use_overpopulation_movements <- FALSE
overpopulation_percentage <- 0.75
leaving_percentage <- .50
leaving_scale_coefficient <- 5
exposed_file <- ""
write_outputs <- "None"
output_folder_path <- ""
point_file <- ""
network_filename <- ""
use_distance <- FALSE
use_configuration <- FALSE
network_movement <- "walk"
use_survival_rates <- FALSE
survival_rate_month <- 3
survival_rate_day <- 15
survival_rates_file <- ""


eu1_val <- PoPS::validate(infected_years_file,
                          number_of_iterations,
                          number_of_cores,
                          parameter_means,
                          parameter_cov_matrix,
                          infected_file,
                          host_file,
                          total_populations_file,
                          temp,
                          temperature_coefficient_file,
                          precip,
                          precipitation_coefficient_file,
                          model_type,
                          latency_period,
                          time_step,
                          season_month_start,
                          season_month_end,
                          start_date,
                          end_date,
                          use_survival_rates,
                          survival_rate_month,
                          survival_rate_day,
                          survival_rates_file,
                          use_lethal_temperature,
                          temperature_file,
                          lethal_temperature,
                          lethal_temperature_month,
                          mortality_on,
                          mortality_rate,
                          mortality_time_lag,
                          mortality_frequency,
                          mortality_frequency_n,
                          management,
                          treatment_dates,
                          treatments_file,
                          treatment_method,
                          natural_kernel_type,
                          anthropogenic_kernel_type,
                          natural_dir,
                          anthropogenic_dir,
                          pesticide_duration,
                          pesticide_efficacy,
                          mask,
                          output_frequency,
                          output_frequency_n,
                          movements_file,
                          use_movements,
                          start_exposed,
                          generate_stochasticity,
                          establishment_stochasticity,
                          movement_stochasticity,
                          dispersal_stochasticity,
                          establishment_probability,
                          dispersal_percentage,
                          quarantine_areas_file,
                          use_quarantine,
                          use_spreadrates,
                          use_overpopulation_movements,
                          overpopulation_percentage,
                          leaving_percentage,
                          leaving_scale_coefficient,
                          exposed_file,
                          write_outputs,
                          output_folder_path,
                          point_file = "",
                          network_filename,
                          network_movement,
                          use_distance,
                          use_configuration)


eu1_2019_val <- eu1_val$output_step_1
