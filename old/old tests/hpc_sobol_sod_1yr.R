## Setup multirun
library(PoPS)
library(terra)
library(Rmpi)
library(doParallel)

inpath <- "/share/rkmeente/eahorner/sod_inputs/"
outpath <- "/share/rkmeente/eahorner/outputs/"

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

sobolFirstOrder <- function(valuesList, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(valuesList[sortList == 1])
  numWithout <- mean(valuesList[sortList == 0])
  numerator <- var(c(numWith, numWithout))
  return(numerator/var(valuesList))
}

sobolFirstOrderRast <- function(rastersList, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(rastersList[[sortList == 1]])
  numWithout <- mean(rastersList[[sortList == 0]])
  numerator <- (stdev(numWith, numWithout))^2
  return(numerator/(stdev(rastersList))^2)
}

sobolTotalOrder <- function(valuesList, uMat, uSource){
  sortMat <- uMat[-uSource,]
  varlist <- c(rep(0, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varlist[i] <- var(valuesList[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])])
  }
  numerator <- mean(varlist)
  return(numerator/ var(valuesList))
}

sobolTotalOrderRast <- function(rastersList, uMat, uSource){
  sortMat <- uMat[-uSource,]
  varRastlist <- c(rep(0, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varRastlist[[i]] <- stdev(rastersList[[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])]])^2
  }
  numerator <- mean(varRastlist)
  return(numerator/ (stdev(RastersList)^2))
}

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

# Parallel Method
cl <- makeCluster(length(setupList), type = 'FORK')
registerDoParallel(cl)
eu1_uncert_outs <- foreach(i = 1:length(setupList), .combine = 'cbind') %dopar% {
  uncertRunsSobol(setupList[i], 1)
}

stopCluster(cl)

# Sequential Method
# eu1_uncert_outs <- matrix(nrow = 2, ncol = (2 * length(setupList)))
# for(i in 1:length(setupList)){
#   eu1_uncert_outs[,((2*i)-1):(2*i)] <- uncertRunsSobol(setupList[i], 1)
# }
# End Methods

num_sources <- 3

#Pulls every 4th value(area infected (to anlyze variance in these values))
out_vals_var <- eu1_uncert_outs[seq(4, length(eu1_uncert_outs), 4)]

out_u_mat <- matrix(0, nrow = num_sources, ncol = length(setupList))
#Rows = host, ic, par, Cols = Combinations
out_u_mat[,1] = c(1,1,1)
out_u_mat[,2] = c(1,0,0)
out_u_mat[,3] = c(0,1,0)
out_u_mat[,4] = c(0,0,1)
out_u_mat[,5] = c(0,1,1)
out_u_mat[,6] = c(1,0,1)
out_u_mat[,7] = c(1,1,0)


# Create Sobol output table/matrix for whole study area
sobol_mat <- matrix(0, nrow = 5, ncol = num_sources)
colnames(sobol_mat) = c('Host', 'IC', 'Par')
#1: First Order Index
#2: Total Order Index
#3: Percent of Total Order as First Order by source
#4: Percent of first order variance from source
#5: Percent of total order variance from source
for(i in 1:num_sources){
  sobol_mat[1,i] <- sobolFirstOrder(out_vals_var, out_u_mat, i)
  sobol_mat[2,i] <- sobolTotalOrder(out_vals_var, out_u_mat, i)
  sobol_mat[3,i] <- sobol_mat[1,i] / sobol_mat[2,i]
}

sobol_mat[4,] <- sobol_mat[1,] / sum(sobol_mat[1,])
sobol_mat[5,] <- sobol_mat[2,] / sum(sobol_mat[2,])

print(sobol_mat)

#barplot(sobol_mat[2,], col = pal2, main = 'Sobol Method', ylab = 'Sobol Index')
#barplot(sobol_mat[1,], col = pal, add = TRUE)


# Create Sobol index output rasters
# [[x]] indexes raster stacks

out_vals_rasts_var <- c(rast(paste0(outpath, 'Rasters/All_SD.tif'))^2, rast(paste0(outpath, 'Rasters/Host_SD.tif'))^2, rast(paste0(outpath, 'Rasters/IC_SD.tif'))^2, rast(paste0(outpath, 'Rasters/Par_SD.tif'))^2, rast(paste0(outpath, 'Rasters/NoHost_SD.tif'))^2, rast(paste0(outpath, 'Rasters/NoIC_SD.tif'))^2, rast(paste0(outpath, 'Rasters/NoPar_SD.tif'))^2, rast(paste0(outpath, 'Rasters/None_SD.tif'))^2)
#out_vals_rasts <- c(rast('All_Mean.tif'), rast('Host_Mean.tif'), rast('IC_Mean.tif'), rast('Par_Mean.tif'), rast('NoHost_Mean.tif'), rast('NoIC_Mean.tif'), rast('NoPar_Mean.tif'), rast('None_Mean.tif'))
#out_vals_rasts_prob <- c(rast('AllP.tif'), rast('HostP.tif'), rast('ICP.tif'), rast('ParP.tif'), rast('NoHostP.tif'), rast('NoICP.tif'), rast('NoParP.tif'), rast('NoneP.tif'))

for(i in 1:num_sources){
  writeRaster(sobolFirstOrderRast(out_vals_rasts_var, out_u_mat, i), paste0(outpath, paste('SobolOuts/SobolFirstOrder', setupList[i+1],'.tif', sep ='')), overwrite = TRUE)
}

sfoHost <- rast(paste0(outpath, 'SobolOuts/SobolFirstOrderhost.tif'))
sfoIC <- rast(paste0(outpath, 'SobolOuts/SobolFirstOrderic.tif'))
sfoPar <- rast(paste0(outpath, 'SobolOuts/SobolFirstOrderpar.tif'))

sfoTotal <- sfoHost + sfoIC + sfoPar
writeRaster(sfoTotal, paste0(outpath, 'SobolOuts/SobolFirstOrderSum.tif'), overwrite = TRUE)

#plot(sfoIC / sfoTotal, ext = multiWindow, col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0000001, 10))


sobolTotalOrderRast <- function(rastersList, uMat, uSource){
  sortMat <- uMat[-uSource,]
  r <- rast(nrow = 1073, ncol = 686, ext = ext(sfoTotal), crs = crs(sfoTotal))
  varRastlist <- c(rep(r, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varRastlist[[i]] <- stdev(rastersList[[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])]])^2
  }
  numerator <- mean(varRastlist)
  return(numerator/ (stdev(rastersList)^2))
}

for(i in 1:num_sources){
  writeRaster(sobolTotalOrderRast(out_vals_rasts_var, out_u_mat, i), paste0(outpath, paste('SobolOuts/SobolTotalOrder', setupList[i+1],'.tif', sep ='')), overwrite = TRUE)
}

stoHost <- rast(paste0(outpath, 'SobolOuts/SobolTotalOrderhost.tif'))
stoIC <- rast(paste0(outpath, 'SobolOuts/SobolTotalOrderic.tif'))
stoPar <- rast(paste0(outpath, 'SobolOuts/SobolTotalOrderpar.tif'))

stoTotal <- stoHost + stoIC + stoPar
writeRaster(stoTotal, paste0(outpath, 'SobolOuts/SobolTotalOrderSum.tif'), overwrite = TRUE)

#Plotting (to add)

#testpal <- c('#8B0000', "#FFFFD9", "#E0F3B2", "#97D6B8", "#41B6C4", "#1E80B8", "#24429A", "#081D58", "#702963")

# test_uncert <- rep(0, length(setupList))
# 
# for(i in 1:length(setupList)){
#   test_uncert[i] <- eu1_uncert_outs[i*4]
# }
# 
# plot_mat <- matrix(0, nrow = 5, ncol = 2)
# plot_mat[1,1] <- test_uncert[2] - test_uncert[5]
# plot_mat[2,1] <- test_uncert[3] - test_uncert[5]
# plot_mat[3,1] <- test_uncert[4] - test_uncert[5]
# plot_mat[4,1] <- test_uncert[5]
# plot_mat[5,1] <- test_uncert[1] - plot_mat[1,1] - plot_mat[2,1] - plot_mat[3,1] - test_uncert[5]
# for(i in 1:5){
#   plot_mat[i,2] <- round(plot_mat[i,1]/test_uncert[1], 4)
# }
# 
# plot_df <- data.frame(
#   area = plot_mat[,1],
#   pct = plot_mat[,2],
#   plot = rep('Uncertainty', 5),
#   type = c('Host', 'Initial Conditions', 'Parameter', 'Process', 'Interactions')
# )
# 
# 
# library(ggplot2)
# barvarpct <- ggplot(data = plot_df, aes(x = plot, y = area, fill = type, label = paste((100 * pct), '%', sep =''))) +
#   geom_bar(stat = 'identity') +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_minimal() +
#   labs(title = 'PoPS Uncertainty Partitioning (1 year)', x = '', y = 'Variance in Infected Area', fill = 'Uncertainty Type') +
#   geom_text(size = 3, position = position_stack(vjust = 0.5))
# 
# constbarvarpct <- ggplot(data = plot_df, aes(x = plot, y = pct, fill = type, label = paste((100 * pct), '%', sep =''))) +
#   geom_bar(stat = 'identity') +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_minimal() +
#   labs(title = 'PoPS Uncertainty Partitioning (1 year)', x = '', y = 'Variance in Infected Area', fill = 'Uncertainty Type') +
#   geom_text(size = 3, position = position_stack(vjust = 0.5))
# eTime <- proc.time() - sTime