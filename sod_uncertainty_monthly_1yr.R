## Setup multirun
#remotes::install_github("ncsu-landscape-dynamics/rpops")
library(PoPS)
library(terra)
library(folderfun)
library(doParallel)
# library(plyr)

setff("In", "H:/Shared drives/Data/Raster/Regional/SOD_OR/")

setupList <- c('all', 'host', 'ic', 'par', 'none')

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
number_of_cores <- 1
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
months = 12

registerDoParallel(length(setupList))
eu1_uncert_outs_monthly <- foreach(i = 1:length(setupList), .combine = 'cbind') %dopar% {
  uncertRunsMonthly(setupList[i], 12)
}

stopImplicitCluster()

uncert_m <- matrix(0, nrow = 12, ncol = length(setupList))

for(i in 1:length(setupList)){
  uncert_m[1,i] <- eu1_uncert_outs_monthly[(i*48) - 22]
  uncert_m[2,i] <- eu1_uncert_outs_monthly[(i*48) - 20]
  uncert_m[3,i] <- eu1_uncert_outs_monthly[(i*48) - 18]
  uncert_m[4,i] <- eu1_uncert_outs_monthly[(i*48) - 16]
  uncert_m[5,i] <- eu1_uncert_outs_monthly[(i*48) - 14]
  uncert_m[6,i] <- eu1_uncert_outs_monthly[(i*48) - 12]
  uncert_m[7,i] <- eu1_uncert_outs_monthly[(i*48) - 10]
  uncert_m[8,i] <- eu1_uncert_outs_monthly[(i*48) - 8]
  uncert_m[9,i] <- eu1_uncert_outs_monthly[(i*48) - 6]
  uncert_m[10,i] <- eu1_uncert_outs_monthly[(i*48) - 4]
  uncert_m[11,i] <- eu1_uncert_outs_monthly[(i*48) - 2]
  uncert_m[12,i] <- eu1_uncert_outs_monthly[(i*48)]
}

plot_area_m <- c(rep(0, 60))

for(i in 1:12){
  plot_area_m[(i*5) - 4] <- uncert_m[i,2] - uncert_m[i,5]
  plot_area_m[(i*5) - 3] <- uncert_m[i,3] - uncert_m[i,5]
  plot_area_m[(i*5) - 2] <- uncert_m[i,4] - uncert_m[i,5]
  plot_area_m[(i*5) - 1] <- uncert_m[i,5]
  plot_area_m[(i*5)] <- uncert_m[i,1] - plot_area_m[(i*5) - 4] - plot_area_m[(i*5) - 3] - plot_area_m[(i*5) - 2] - plot_area_m[(i*5) - 1]
}

plot_pct_m <- c(rep(0, 60))

for(j in 1:12){
  for (t in 1:5){
    plot_pct_m[(5*j) - (5 - t)] <- round(plot_area_m[(5*j) - (5 - t)]/uncert_m[j,1], 4)
  }
}

plot_df_m <- data.frame(
  area = plot_area_m,
  pct = plot_pct_m,
  plot = c(rep('Jan', 5), rep('Feb', 5), rep('Mar', 5), rep('Apr', 5), rep('May', 5), rep('Jun', 5), rep('Jul', 5), rep('Aug', 5), rep('Sep', 5), rep('Oct', 5), rep('Nov', 5), rep('Dec', 5)),
  type = c('Host', 'Initial Conditions', 'Parameter', 'Process', 'Interactions')
)

library(ggplot2)
mbarplot <- ggplot(data = plot_df_m, aes(x = factor(plot, level = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
                             y = area, fill = type, label = paste((100 * pct), '%', sep =''))) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal() +
  labs(title = 'PoPS Monthly Uncertainty Partitioning (1 year)', x = '', y = 'Variance in Infected Area', fill = 'Uncertainty Type') +
  geom_text(size = 3, position = position_stack(vjust = 0.5))