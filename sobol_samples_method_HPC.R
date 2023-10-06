#Traditional Sobol method
#HPC Version

n <- 10000 #number of iterations (individual realizations of PoPS)
num_sources <- 3 #number of uncertainty sources to test

# HPC Paths
inpath <- "/share/rkmeente/eahorner/sod_inputs/" #Where inputs are stored
outpath <- "/share/rkmeente/eahorner/outputs/" #Where to store outputs

# Local Paths 
# inpath <- "H:/Shared drives/Data/Raster/Regional/SOD_OR/" #Where inputs are stored
# outpath <- "C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/Test_Trad_Outs/" #Where to store outputs

#Should guarantee that the first 2^(num_sources) values are each case
#Or first 2 times 2^(num_sources) values are each case to guarantee SD
sampleMat <- matrix(sample(0:1, num_sources*n, replace = TRUE), nrow = n, ncol = num_sources) #Create sample matrix
write.csv(sampleMat, paste0(outpath, 'sampleMat.csv'))

#PoPS function that takes in the sample row and runs the setup of PoPS appropriate for the given sample
runPoPS <- function(hostVal, icVal, parVal, outValsList, outRastsMeanList, outRastsSDList){
  if(hostVal == 1){
    host_uncertainty <- TRUE
  }
  else{
    host_uncertainty <- FALSE
  }
  if(icVal == 1){
    initial_condtion_uncertainty <- TRUE
  }
  else{
    initial_condition_uncertainty <- FALSE
  }
  if(parVal == 1){
    parameter_cov_matrix <- read.table(paste0(inpath, 'parameters/eu1_2019_cov_mat.csv'), header = F) #HPC
    # parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F) #Local
  }
  else{
    parameter_cov_matrix <- matrix(0,8,8)
  }
  
  eu1_run <- PoPS::pops_multirun(infected_file,
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
  return(eu1_run)
  
}

#PoPS Settings
infected_file <- paste0(inpath, "EOYInfections/new_method_mean_sd_end_inf_2021_eu1.tif") #HPC
# infected_file <- paste0(inpath, "End of Year Infections/new_method_mean_sd_end_inf_2021_eu1.tif") #Local
host_file <- paste0(inpath, "Hosts/mean_sd_host_mapping_tanoak.tif") #HPC
# host_file <- paste0(inpath, "Hosts/mean_sd_host_mapping_tanoak.tif") #Local
total_populations_file <- paste0(inpath, "Hosts/lemma_max100m.tif") #HPC
# total_populations_file <- paste0(inpath, "Hosts/lemma_max100m.tif") #Local
means <- read.table(paste0(inpath, 'parameters/eu1_2019_means.csv'), header = F) #HPC
# means <- read.table('parameters/eu1_2019_means.csv', header = F) #Local
parameter_means <- t(means)
parameter_means <- parameter_means[1,]
temp <- TRUE
temperature_coefficient_file <- paste0(inpath, "Weather/weather_coef_2021.tif") #HPC
# temperature_coefficient_file <- paste0(inpath, "Weather/weather_coef_2021.tif") #Local
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
number_of_iterations <- 1
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

#Run PoPS for first time to set up Lists
host_uncertainty <- FALSE
if(sampleMat[1,1] == 1){
  host_uncertainty <- TRUE
}

initial_condition_uncertainty <- FALSE
if(sampleMat[1,2] == 1){
  initial_condtion_uncertainty <- TRUE
}

parameter_cov_matrix <- matrix(0,8,8)
if(sampleMat[1,3] == 1){
  parameter_cov_matrix <- read.table(paste0(inpath, 'parameters/eu1_2019_cov_mat.csv'), header = F) #HPC
  # parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F) #Local
}

eu1_start1 <- PoPS::pops_multirun(infected_file,
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
outVals <- c(eu1_start1$number_infecteds, eu1_start1$infected_areas)
outRastsMean <- eu1_start1$simulation_mean

host_uncertainty <- FALSE
if(sampleMat[2,1] == 1){
  host_uncertainty <- TRUE
}

initial_condition_uncertainty <- FALSE
if(sampleMat[2,2] == 1){
  initial_condtion_uncertainty <- TRUE
}

parameter_cov_matrix <- matrix(0,8,8)
if(sampleMat[2,3] == 1){
  parameter_cov_matrix <- read.table(paste0(inpath, 'parameters/eu1_2019_cov_mat.csv'), header = F) #HPC
  # parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F) #Local
}

eu1_start2 <- PoPS::pops_multirun(infected_file,
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

outVals <- c(outVals, eu1_start2$number_infecteds, eu1_start2$infected_areas)
outVals[6] <- sd(c(outVals[1], outVals[5]))
outVals[8] <- sd(c(outVals[3], outVals[7]))
outRastsMean <- c(outRastsMean, eu1_start2$simulation_mean)
outRastsSD <- stdev(outRastsMean)

# outList <- list(outVals, outRastsMean, outRastsSD)

#Sobol Functions
sobolFirstOrder <- function(valuesList, uMat, uSource){
  sortList <- uMat[,uSource]
  numWith <- mean(valuesList[sortList == 1])
  numWithout <- mean(valuesList[sortList == 0])
  numerator <- var(c(numWith, numWithout))
  return(numerator/ var(valuesList))
}

sobolTotalOrder <- function(valuesList, uMat, uSource){
  sortMat <- uMat[,-uSource]
  varlist <- c(rep(0, nrow(unique(sortMat, MARGIN = 1))))
  for(i in 1:nrow(unique(sortMat, MARGIN = 1))){
    varlist[i] <- var(valuesList[apply(sortMat, 1, identical, unique(sortMat, MARGIN = 1)[i,])])
  }
  numerator <- mean(varlist)
  return(numerator/ var(valuesList))
}

#Initialize Sobol Matrices (to hold values)
SFOValsNum <- matrix(0, nrow = 3, ncol = n)
STOValsNum <- matrix(0, nrow = 3, ncol = n)
SFOValsArea <- matrix(0, nrow = 3, ncol = n)
STOValsArea <- matrix(0, nrow = 3, ncol = n)

#Loop (does all the important stuff)
for(i in 3:n){
  eu1_run <- runPoPS(sampleMat[i,1], sampleMat[i,2], sampleMat[i,3], outVals, outRastsMean, outRastsSD)
  # Convert out of list form (keep as three separate objects)
  outVals <- append(outVals, c(eu1_run$number_infecteds, eu1_run$infected_areas))
  outVals[length(outVals) - 2] <- sd(outVals[seq(1, length(outVals), 4)])
  outVals[length(outVals)] <- sd(outVals[seq(3, length(outVals), 4)])
  outRastsMean <- c(outRastsMean, eu1_run$simulation_mean)
  outRastsSD <- c(outRastsSD, stdev(outRastsMean))
  #Calculate Sobol Indices from that (just summary for now)
  numInf <- outVals[seq(1, length(outVals), 4)]
  areaInf <- outVals[seq(3, length(outVals), 4)]
  for(j in 1:num_sources){
    SFOValsNum[j,i] <- sobolFirstOrder(numInf, sampleMat, j)
    STOValsNum[j,i] <- sobolTotalOrder(numInf, sampleMat, j)
    SFOValsArea[j,i] <- sobolFirstOrder(areaInf, sampleMat, j)
    STOValsArea[j,i] <- sobolTotalOrder(areaInf, sampleMat, j)
  }
  print(paste0('Finished iteration number: ', i))
}

#Write output files
write.csv(outVals, paste0(outpath, 'outVals.csv'))
write.csv(SFOValsNum, paste0(outpath, 'SFOValsNum.csv'))
write.csv(SFOValsArea, paste0(outpath, 'SFOValsArea.csv'))
write.csv(STOValsNum, paste0(outpath, 'STOValsNum.csv'))
write.csv(STOValsArea, paste0(outpath, 'STOValsArea.csv'))

# write.csv(outVals, paste0(outpath, 'outVals.csv'))
# writeRaster(outRastsMean, paste0(outpath, 'outRastsMean.tif'))
# writeRaster(outRastsMean, paste0(outpath, 'outRastsSD.tif'))