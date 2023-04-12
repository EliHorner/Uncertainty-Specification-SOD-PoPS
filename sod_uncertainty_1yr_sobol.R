## Setup multirun
#remotes::install_github("ncsu-landscape-dynamics/rpops")
library(PoPS)
library(terra)
library(folderfun)
library(doParallel)
# library(plyr)

sTime <- proc.time()

setff("In", "H:/Shared drives/Data/Raster/Regional/SOD_OR/")

setupList <- c('all', 'host', 'ic', 'par', 'nohost', 'noic', 'nopar', 'none')

infected_file <- ffIn("End of Year Infections/new_method_mean_sd_end_inf_2021_eu1.tif")
host_file <- ffIn("Hosts/mean_sd_hosts_sum.tif")
total_populations_file <- ffIn("Hosts/lemma_max100m.tif")
means <- read.table('parameters/eu1_2019_means.csv', header = F)
parameter_means <- t(means)
parameter_means <- parameter_means[1,]
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
number_of_iterations <- 2000
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
# registerDoParallel(length(setupList))
# eu1_uncert_outs <- foreach(i = 1:length(setupList), .combine = 'cbind') %dopar% {
#   uncertRunsWrite(setupList[i], 1)
# }
# 
# stopImplicitCluster()

# Sequential Method
eu1_uncert_outs <- matrix(nrow = 2, ncol = (2 * length(setupList)))
for(i in 1:length(setupList)){
  eu1_uncert_outs[,((2*i)-1):(2*i)] <- uncertRunsSobol(setupList[i], 1)
}
# End Methods

num_sources <- 3

#Pulls every 4th value(area infected (to anlyze variance in these values))
out_vals_area <- eu1_uncert_outs[seq(4, length(eu1_uncert_outs), 4)]

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
  sobol_mat[1,i] <- sobolFirstOrder(out_vals_area, out_u_mat, i)
  sobol_mat[2,i] <- sobolTotalOrder(out_vals_area, out_u_mat, i)
  sobol_mat[3,i] <- sobol_mat[1,i] / sobol_mat[2,i]
}

sobol_mat[4,] <- sobol_mat[1,] / sum(sobol_mat[1,])
sobol_mat[5,] <- sobol_mat[2,] / sum(sobol_mat[2,])

barplot(sobol_mat[2,], col = pal2, main = 'Sobol Method', ylab = 'Sobol Index')
barplot(sobol_mat[1,], col = pal, add = TRUE)


# Create Sobol index output rasters
# [[x]] indexes raster stacks

out_vals_rasts_var <- c(rast('All_SD.tif')^2, rast('Host_SD.tif')^2, rast('IC_SD.tif')^2, rast('Par_SD.tif')^2, rast('NoHost_SD.tif')^2, rast('NoIC_SD.tif')^2, rast('NoPar_SD.tif')^2, rast('None_SD.tif')^2)
out_vals_rasts <- c(rast('All_Mean.tif'), rast('Host_Mean.tif'), rast('IC_Mean.tif'), rast('Par_Mean.tif'), rast('NoHost_Mean.tif'), rast('NoIC_Mean.tif'), rast('NoPar_Mean.tif'), rast('None_Mean.tif'))

for(i in 1:num_sources){
  writeRaster(sobolFirstOrderRast(out_vals_rasts, out_u_mat, i), paste('SobolFirstOrder', setupList[i+1],'.tif', sep =''), overwrite = TRUE)
}

sfoHost <- rast('SobolFirstOrderhost.tif')
sfoIC <- rast('SobolFirstOrderic.tif')
sfoPar <- rast('SobolFirstOrderpar.tif')

sfoTotal <- sfoHost + sfoIC + sfoPar

plot(sfoIC / sfoTotal, ext = multiWindow, col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0000001, 10))


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
  writeRaster(sobolTotalOrderRast(out_vals_rasts, out_u_mat, i), paste('SobolTotalOrder', setupList[i+1],'.tif', sep =''), overwrite = TRUE)
}

stoHost <- rast('SobolTotalOrderhost.tif')
stoIC <- rast('SobolTotalOrderic.tif')
stoPar <- rast('SobolTotalOrderpar.tif')

stoTotal <- stoHost + stoIC + stoPar

#Plotting (to add)

testpal <- c('#8B0000', "#FFFFD9", "#E0F3B2", "#97D6B8", "#41B6C4", "#1E80B8", "#24429A", "#081D58", "#702963")

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