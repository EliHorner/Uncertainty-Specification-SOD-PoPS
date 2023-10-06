## Setup multirun
#remotes::install_github("ncsu-landscape-dynamics/rpops")
library(PoPS)
library(terra)
library(folderfun)
library(doParallel)
# library(plyr)

setff("In", "H:/Shared drives/Data/Raster/Regional/SOD_OR/")

infected_file_on <- ffIn("End of Year Infections/mean_sd_end_inf_2021_eu1.tif")
host_file_on <- ffIn("Hosts/mean_sd_hosts_sum.tif")
infected_file_off <- ffIn("End of Year Infections/end_inf_2021_eu1.tif")
host_file_off <- ffIn("Hosts/mean_hosts_sum.tif")
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
number_of_cores <- 2
pesticide_duration <- c(0)
pesticide_efficacy <- 1.0
random_seed <- NULL
output_frequency <- "month"
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
eu1_all <- pops_multirun(infected_file_on,
                               host_file_on,
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
host_uncertainty <- FALSE
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
eu1_nohost <- pops_multirun(infected_file_on,
                                host_file_off,
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

initial_condition_uncertainty <- FALSE
host_uncertainty <- TRUE
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
eu1_noic <- pops_multirun(infected_file_off,
                              host_file_on,
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

initial_condition_uncertainty <- FALSE
host_uncertainty <- FALSE
parameter_cov_matrix <- matrix(0,8,8)
eu1_none <- pops_multirun(infected_file_off,
                                host_file_off,
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
eu1_nopar <- pops_multirun(infected_file_on,
                          host_file_on,
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

# area_mean_sd_monthly <- array(0, c(12, 2, 5))
# area_mean_sd[,1] <- eu1_all$infected_areas
# area_mean_sd[,2] <- eu1_none$infected_areas
# area_mean_sd[,3] <- eu1_host$infected_areas
# area_mean_sd[,4] <- eu1_ic$infected_areas
# area_mean_sd[,5] <- eu1_par$infected_areas
# colnames(area_mean_sd) <- c('All', 'None', 'Host', 'IC', 'Parameter')
# 
# area_uncert_contributions <- matrix(0, nrow = 2, ncol = 4)
# for (i in 1:3){
#   #Subtract sd with no uncertainties included from sd with ONLY this type included
#   area_uncert_contributions[1,i] <- area_mean_sd[2, (i+2)] - area_mean_sd[2,2]
# }
# area_uncert_contributions[1,4] <- area_mean_sd[2,2]
# colnames(area_uncert_contributions) <- c('Host', 'IC', 'Parameter', 'Process')
# #If no nonlinear behavior or interactions, Row 1 of this should sum to sd with all uncertainties
# #Second row will allow us to check where interactions occur (rule out where they don't)
# for (i in 1:3){
#   #Subtract sd with ONLY this type included from sd with ALL uncertainties included
#   area_uncert_contributions[2,i] <- area_mean_sd[2, 1] - area_mean_sd[2,(i+2)]
# }

infected_areas_array <- array(0, c(12,2,5))
plot_uncert <- matrix(0, nrow = 12, ncol = 5)
colnames(plot_uncert) <- c('Host', 'IC', 'Parameter', 'Process', 'Interactions')

for (i in 1:12){
  for (j in 1:2){
    infected_areas_array[i,j,1] <- eu1_all$infected_areas[j,i]
    infected_areas_array[i,j,2] <- eu1_none$infected_areas[j,i]
    infected_areas_array[i,j,3] <- eu1_nohost$infected_areas[j,i]
    infected_areas_array[i,j,4] <- eu1_noic$infected_areas[j,i]
    infected_areas_array[i,j,5] <- eu1_nopar$infected_areas[j,i]
  }
}

for (i in 1:12){
  plot_uncert[i, 1] <- infected_areas_array[i,2,1] - infected_areas_array[i,2,3]
  plot_uncert[i, 2] <- infected_areas_array[i,2,1] - infected_areas_array[i,2,4]
  plot_uncert[i, 3] <- infected_areas_array[i,2,1] - infected_areas_array[i,2,5]
  plot_uncert[i, 4] <- infected_areas_array[i,2,2]
  plot_uncert[i, 5] <- infected_areas_array[i,2,1] - sum(plot_uncert[i, 1:4])
}
