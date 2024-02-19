# remotes::install_github("ncsu-landscape-dynamics/rpops")
library(PoPS)
library(terra)
library(folderfun)
library(doParallel)

# Change this to the directory that you save the folder to and you will good to go
setff("In", "C:/Users/cmjone25/Desktop/SOD_OR/")

infected_years_file <- ffIn("Cumulative Infections/cum_inf_2021_na1.tif")
number_of_observations <- 1
prior_number_of_observations <- 0
prior_means <- c(0, 0, 0, 0, 0, 0, 0, 0)
prior_cov_matrix <- matrix(0, 8, 8)
params_to_estimate <- c(T, T, T, T, F, F)
number_of_generations <- 7
generation_size <- 1000
infected_file <- ffIn("End of Year Infections/end_inf_2020_na1.tif")
host_file <- ffIn("Hosts/hosts_2021.tif")
total_populations_file <- ffIn("Hosts/lemma_max100m.tif")
temp <- TRUE
temperature_coefficient_file <- ffIn("Weather/weather_coef_2021.tif")
precip <- FALSE
precipitation_coefficient_file <- ""
model_type <- "SI"
latency_period <- 0
time_step <- "week"
season_month_start <- 1
season_month_end <- 12
start_date <- "2016-01-01"
end_date <- "2016-12-31"
use_survival_rates <- FALSE
survival_rate_month <- 3
survival_rate_day <- 15
survival_rates_file <- ""
use_lethal_temperature <- FALSE
temperature_file <- ""
lethal_temperature <- -12.87
lethal_temperature_month <- 1
mortality_on <- FALSE
mortality_rate <- 0
mortality_time_lag <- 0
mortality_frequency <- "year"
mortality_frequency_n <- 1
management <- FALSE
treatment_dates <- c("")
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
mask <- NULL
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
overpopulation_percentage <- 0
leaving_percentage <- 0
leaving_scale_coefficient <- 1
calibration_method <- "ABC"
number_of_iterations <- 100000
exposed_file <- ""
verbose = TRUE
write_outputs <- "None"
output_folder_path <- ""
network_filename <- ""
network_movement <- "walk"
success_metric <- "mcc and RMSE"

sod_2021 <- calibrate(infected_years_file,
                      number_of_observations,
                      prior_number_of_observations,
                      prior_means,
                      prior_cov_matrix,
                      params_to_estimate,
                      number_of_generations,
                      generation_size,
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
                      natural_kappa,
                      anthropogenic_dir,
                      anthropogenic_kappa,
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
                      calibration_method,
                      number_of_iterations,
                      exposed_file,
                      verbose,
                      write_outputs,
                      output_folder_path,
                      network_filename,
                      network_movement,
                      success_metric)


save(sod_2021, file = "rdata/na1_2021.RData")

sod_2021_m <- sod_2021$posterior_means
sod_2021_cm <- sod_2021$posterior_cov_matrix

write.table(x = sod_2021_m, file = 'parameters/na1_2021_means.csv', row.names = FALSE, col.names = FALSE)
write.table(sod_2021_cm, 'parameters/na1_2021_cov_mat.csv', row.names = FALSE, col.names = FALSE)
