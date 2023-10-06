## Setup multirun
library(PoPS)
library(terra)
library(parallel)
library(doParallel)

print('loaded libraries lol')

inpath <- "/share/rkmeente/eahorner/sod_inputs/"
outpath <- "/share/rkmeente/eahorner/outputs/"

print('set paths')

uncertRunsSobol <- function(case = 'all', years = 1){
  if (case == 'all'){
    initial_condition_uncertainty <- TRUE
    host_uncertainty <- TRUE
    parameter_cov_matrix <- read.table(paste0(inpath, 'parameters/eu1_2019_cov_mat.csv'), header = F)
    eu1_all <- PoPS::pops_multirun(infected_file,
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
    terra::writeRaster(eu1_all$simulation_mean, paste0(outpath, 'Rasters/All_Mean.tif'), overwrite = TRUE)
    terra::writeRaster(eu1_all$simulation_sd, paste0(outpath, 'Rasters/All_SD.tif'), overwrite = TRUE)
    out = cbind(eu1_all$number_infecteds, eu1_all$infected_areas)
    colnames(out) <- rep(c('num all', 'area all'), years)
    rm(eu1_all)
    return(out)
  }
  if (case == 'nohost'){
    initial_condition_uncertainty <- TRUE
    host_uncertainty <- FALSE
    parameter_cov_matrix <- read.table(paste0(inpath, 'parameters/eu1_2019_cov_mat.csv'), header = F)
    eu1_nohost <- PoPS::pops_multirun(infected_file,
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
    terra::writeRaster(eu1_nohost$simulation_mean, paste0(outpath, 'Rasters/NoHost_Mean.tif'), overwrite = TRUE)
    terra::writeRaster(eu1_nohost$simulation_sd, paste0(outpath, 'Rasters/NoHost_SD.tif'), overwrite = TRUE)
    out = cbind(eu1_nohost$number_infecteds, eu1_nohost$infected_areas)
    colnames(out) <- rep(c('num nohost', 'area nohost'), years)
    rm(eu1_nohost)
    return(out)
  }
  if (case == 'noic'){
    initial_condition_uncertainty <- FALSE
    host_uncertainty <- TRUE
    parameter_cov_matrix <- read.table(paste0(inpath, 'parameters/eu1_2019_cov_mat.csv'), header = F)
    eu1_noic <- PoPS::pops_multirun(infected_file,
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
    terra::writeRaster(eu1_noic$simulation_mean, paste0(outpath, 'Rasters/NoIC_Mean.tif'), overwrite = TRUE)
    terra::writeRaster(eu1_noic$simulation_sd, paste0(outpath, 'Rasters/NoIC_SD.tif'), overwrite = TRUE)
    out = cbind(eu1_noic$number_infecteds, eu1_noic$infected_areas)
    colnames(out) <- rep(c('num noic', 'area noic'), years)
    rm(eu1_noic)
    return(out)
  }
  if (case == 'nopar'){
    initial_condition_uncertainty <- TRUE
    host_uncertainty <- TRUE
    parameter_cov_matrix <- matrix(0,8,8)
    eu1_nopar <- PoPS::pops_multirun(infected_file,
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
    terra::writeRaster(eu1_nopar$simulation_mean, paste0(outpath, 'Rasters/NoPar_Mean.tif'), overwrite = TRUE)
    terra::writeRaster(eu1_nopar$simulation_sd, paste0(outpath, 'Rasters/NoPar_SD.tif'), overwrite = TRUE)
    out = cbind(eu1_nopar$number_infecteds, eu1_nopar$infected_areas)
    colnames(out) <- rep(c('num nopar', 'area nopar'), years)
    rm(eu1_nopar)
    return(out)
  }
  if (case == 'host'){
    initial_condition_uncertainty <- FALSE
    host_uncertainty <- TRUE
    parameter_cov_matrix <- matrix(0,8,8)
    eu1_host <- PoPS::pops_multirun(infected_file,
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
    terra::writeRaster(eu1_host$simulation_mean, paste0(outpath, 'Rasters/Host_Mean.tif'), overwrite = TRUE)
    terra::writeRaster(eu1_host$simulation_sd, paste0(outpath, 'Rasters/Host_SD.tif'), overwrite = TRUE)
    out = cbind(eu1_host$number_infecteds, eu1_host$infected_areas)
    colnames(out) <- rep(c('num host', 'area host'), years)
    rm(eu1_host)
    return(out)
  }
  if (case == 'ic'){
    initial_condition_uncertainty <- TRUE
    host_uncertainty <- FALSE
    parameter_cov_matrix <- matrix(0,8,8)
    eu1_ic <- PoPS::pops_multirun(infected_file,
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
    terra::writeRaster(eu1_ic$simulation_mean, paste0(outpath, 'Rasters/IC_Mean.tif'), overwrite = TRUE)
    terra::writeRaster(eu1_ic$simulation_sd, paste0(outpath, 'Rasters/IC_SD.tif'), overwrite = TRUE)
    out = cbind(eu1_ic$number_infecteds, eu1_ic$infected_areas)
    colnames(out) <- rep(c('num ic', 'area ic'), years)
    rm(eu1_ic)
    return(out)
  }
  if (case == 'par'){
    initial_condition_uncertainty <- FALSE
    host_uncertainty <- FALSE
    parameter_cov_matrix <- read.table(paste0(inpath, 'parameters/eu1_2019_cov_mat.csv'), header = F)
    eu1_par <- PoPS::pops_multirun(infected_file,
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
    terra::writeRaster(eu1_par$simulation_mean, paste0(outpath, 'Rasters/Par_Mean.tif'), overwrite = TRUE)
    terra::writeRaster(eu1_par$simulation_sd, paste0(outpath, 'Rasters/Par_SD.tif'), overwrite = TRUE)
    out = cbind(eu1_par$number_infecteds, eu1_par$infected_areas)
    colnames(out) <- rep(c('num par', 'area par'), years)
    rm(eu1_par)
    return(out)
  }
  if (case == 'none'){
    initial_condition_uncertainty <- FALSE
    host_uncertainty <- FALSE
    parameter_cov_matrix <- matrix(0,8,8)
    eu1_none <- PoPS::pops_multirun(infected_file,
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
    terra::writeRaster(eu1_none$simulation_mean, paste0(outpath, 'Rasters/None_Mean.tif'), overwrite = TRUE)
    terra::writeRaster(eu1_none$simulation_sd, paste0(outpath, 'Rasters/None_SD.tif'), overwrite = TRUE)
    out = cbind(eu1_none$number_infecteds, eu1_none$infected_areas)
    colnames(out) <- rep(c('num none', 'area none'), years)
    rm(eu1_none)
    return(out)
  }
}

print('function read')

setupList <- c('all', 'host', 'ic', 'par', 'nohost', 'noic', 'nopar', 'none')

infected_file <- paste0(inpath, "EOYInfections/new_method_mean_sd_end_inf_2021_eu1.tif")
host_file <- paste0(inpath, "Hosts/mean_sd_host_mapping_tanoak.tif")
total_populations_file <- paste0(inpath, "Hosts/lemma_max100m.tif")
means <- read.table(paste0(inpath, 'parameters/eu1_2019_means.csv'), header = F)
parameter_means <- t(means)
parameter_means <- parameter_means[1,]
parameter_cov_matrix <- read.table(paste0(inpath, 'parameters/eu1_2019_cov_mat.csv'), header = F)
temp <- TRUE
temperature_coefficient_file <- paste0(inpath, "Weather/weather_coef_2021.tif")
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
number_of_iterations <- 10
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
output_folder_path <- ''

print('ready to start mpi stuff')

# Parallel Method
nWorkers <- 8
mpi.spawn.Rslaves(nslaves = nWorkers)

#test that mpi is setting up right
mpi.bcast.cmd ( id <- mpi.comm.rank() )
mpi.bcast.cmd( nWorkers <- mpi.comm.size() )
mpi.remote.exec(paste('I am', mpi.comm.rank(), 'of', mpi.comm.size()))

#Sends all functions and input objects to workers
mpi.bcast.Rfun2slave()
mpi.bcast.Robj2slave(all = TRUE)

#Run PoPS on workers
mpi.remote.exec(uncertRunsSobol, setupList[mpi.comm.rank()], 1)

mpi.finalize()