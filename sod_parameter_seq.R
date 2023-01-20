## Setup multirun
#remotes::install_github("ncsu-landscape-dynamics/rpops")
library(PoPS)
library(terra)
library(folderfun)
library(doParallel)
# library(plyr)

setff("In", "H:/Shared drives/Data/Raster/Regional/SOD_OR/")

infected_file <- ffIn("End of Year Infections/mean_sd_end_inf_2021_eu1.tif")
host_file <- ffIn("Hosts/mean_sd_hosts_sum.tif")
total_populations_file <- ffIn("Hosts/lemma_max100m.tif")
means <- read.table('parameters/eu1_2019_means.csv', header = F)
parameter_means <- t(means)
parameter_means <- parameter_means[1,]
parameter_means[5] <- 2
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
temp <- TRUE
temperature_coefficient_file <- ffIn("Weather/weather_coef_2021.tif")
precip <- FALSE
precipitation_coefficient_file <- ""
model_type <- "SI"
latency_period <- 0
time_step <- "week"
season_month_start <- 1
season_month_end <- 12
start_date <- '2022-01-01'
end_date <- '2022-12-31'
use_survival_rates <- FALSE
survival_rate_month <- 3
survival_rate_day <- 15
survival_rates_file <- ""
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
anthropogenic_dir <- "NONE"
number_of_iterations <- 1000
number_of_cores <- 1
pesticide_duration <- c(0)
pesticide_efficacy <- 1.0
random_seed <- NULL
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
mask <- NULL
write_outputs <- "None"
network_filename <- ""
network_movement <- "walk"
output_folder_path = ""

initial_condition_uncertainty <- TRUE
host_uncertainty <- TRUE
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
parameter_cov_matrix[2:8,] <- 0
parameter_cov_matrix[,2:8] <- 0
eu1_par1 <- pops_multirun(infected_file,
                         host_file,
                         total_populations_file,
                         parameter_means,
                         parameter_cov_matrix,
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
                         number_of_iterations,
                         number_of_cores,
                         pesticide_duration,
                         pesticide_efficacy,
                         random_seed,
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
                         mask,
                         write_outputs,
                         output_folder_path,
                         network_filename,
                         network_movement,
                         initial_condition_uncertainty,
                         host_uncertainty)
initial_condition_uncertainty <- TRUE
host_uncertainty <- TRUE
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
parameter_cov_matrix[3:8,] <- 0
parameter_cov_matrix[,3:8] <- 0
parameter_cov_matrix[1,] <- 0
parameter_cov_matrix[,1] <- 0
eu1_par2 <- pops_multirun(infected_file,
                          host_file,
                          total_populations_file,
                          parameter_means,
                          parameter_cov_matrix,
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
                          number_of_iterations,
                          number_of_cores,
                          pesticide_duration,
                          pesticide_efficacy,
                          random_seed,
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
                          mask,
                          write_outputs,
                          output_folder_path,
                          network_filename,
                          network_movement,
                          initial_condition_uncertainty,
                          host_uncertainty)
initial_condition_uncertainty <- TRUE
host_uncertainty <- TRUE
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
parameter_cov_matrix[3:8,] <- 0
parameter_cov_matrix[,3:8] <- 0
parameter_cov_matrix[1:2,] <- 0
parameter_cov_matrix[,1:2] <- 0
eu1_par3 <- pops_multirun(infected_file,
                          host_file,
                          total_populations_file,
                          parameter_means,
                          parameter_cov_matrix,
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
                          number_of_iterations,
                          number_of_cores,
                          pesticide_duration,
                          pesticide_efficacy,
                          random_seed,
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
                          mask,
                          write_outputs,
                          output_folder_path,
                          network_filename,
                          network_movement,
                          initial_condition_uncertainty,
                          host_uncertainty)
initial_condition_uncertainty <- TRUE
host_uncertainty <- TRUE
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
parameter_cov_matrix[1:3,] <- 0
parameter_cov_matrix[,1:3] <- 0
parameter_cov_matrix[5:8,] <- 0
parameter_cov_matrix[,5:8] <- 0
eu1_par4 <- pops_multirun(infected_file,
                          host_file,
                          total_populations_file,
                          parameter_means,
                          parameter_cov_matrix,
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
                          number_of_iterations,
                          number_of_cores,
                          pesticide_duration,
                          pesticide_efficacy,
                          random_seed,
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
                          mask,
                          write_outputs,
                          output_folder_path,
                          network_filename,
                          network_movement,
                          initial_condition_uncertainty,
                          host_uncertainty)
initial_condition_uncertainty <- TRUE
host_uncertainty <- TRUE
parameter_cov_matrix <- matrix(0,8,8)
eu1_par_none <- pops_multirun(infected_file,
                          host_file,
                          total_populations_file,
                          parameter_means,
                          parameter_cov_matrix,
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
                          number_of_iterations,
                          number_of_cores,
                          pesticide_duration,
                          pesticide_efficacy,
                          random_seed,
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
                          mask,
                          write_outputs,
                          output_folder_path,
                          network_filename,
                          network_movement,
                          initial_condition_uncertainty,
                          host_uncertainty)

par_df <- data.frame(Parameter =  c('Par 1' , 'Par 2', 'Par 3', 'Par 4', 'None'),
                     Infected_Area = c(eu1_par1$infected_areas[1], eu1_par2$infected_areas[1], eu1_par3$infected_areas[1], eu1_par4$infected_areas[1], eu1_par_none$infected_areas[1]),
                     Variance = c(eu1_par1$infected_areas[2], eu1_par2$infected_areas[2], eu1_par3$infected_areas[2], eu1_par4$infected_areas[2], eu1_par_none$infected_areas[2]))
par_df$PctCh <- round((((par_df$Variance / par_df$Variance[5]) - 1) * 100), 2)

library(ggplot2)
parplot <- ggplot(data = par_df, aes(x = Parameter, y = Variance, fill = Parameter, label = PctCh)) +
         geom_bar(stat = 'identity') +
         scale_fill_brewer(palette = 'Set3') +
         theme_minimal() +
         labs(title = 'PoPS Uncertainty Parameter Partitioning (1 year)', x = '', y = 'Variance in Infected Area', fill = 'Uncertainty Type') +
         geom_hline(yintercept = eu1_par_none$infected_areas[2], linetype = 'dashed') +
         geom_text(size = 3, position = position_stack(vjust = 0.9))