## Setup multirun
#remotes::install_github("ncsu-landscape-dynamics/rpops")
library(PoPS)
library(terra)
library(folderfun)
library(doParallel)
# library(plyr)

setff("In", "H:/Shared drives/Data/Raster/Regional/SOD_OR/")

setupList <- c('all', 'host', 'ic', 'par', 'none')

infected_file <- ffIn("End of Year Infections/mean_sd_end_inf_2021_eu1.tif")
host_file <- ffIn("Hosts/mean_sd_hosts_sum.tif")
total_populations_file <- ffIn("Hosts/lemma_max100m.tif")
means <- read.table('parameters/eu1_2019_means.csv', header = F)
parameter_means <- t(means)
parameter_means <- parameter_means[1,]
parameter_cov_matrix <- read.table('parameters/eu1_2019_cov_mat.csv', header = F)
temp <- TRUE
temperature_coefficient_file <- ffIn("Weather/weather_coef_2021_2yr.tif")
precip <- FALSE
precipitation_coefficient_file <- ""
model_type <- "SI"
latency_period <- 0
time_step <- "week"
season_month_start <- 1
season_month_end <- 12
start_date <- '2022-01-01'
end_date <- '2023-12-31'
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

registerDoParallel(length(setupList))
eu1_uncert_outs2 <- foreach(i = 1:length(setupList), .combine = 'cbind') %dopar% {
  uncertRuns(setupList[i], 2)
}

stopImplicitCluster()

test_uncert2 <- matrix(0, nrow = 2, ncol = length(setupList))

for(i in 1:length(setupList)){
  test_uncert2[1,i] <- eu1_uncert_outs2[(i*8) - 2]
  test_uncert2[2,i] <- eu1_uncert_outs2[(i*8)]
}

plot_area2 <- c(rep(0, 10))
plot_area2[1] <- test_uncert2[1,2] - test_uncert2[1,5]
plot_area2[2] <- test_uncert2[1,3] - test_uncert2[1,5]
plot_area2[3] <- test_uncert2[1,4] - test_uncert2[1,5]
plot_area2[4] <- test_uncert2[1,5]
plot_area2[5] <- test_uncert2[1,1] - plot_area2[1] - plot_area2[2] - plot_area2[3] - plot_area2[4]
plot_area2[6] <- test_uncert2[2,2] - test_uncert2[2,5]
plot_area2[7] <- test_uncert2[2,3] - test_uncert2[2,5]
plot_area2[8] <- test_uncert2[2,4] - test_uncert2[2,5]
plot_area2[9] <- test_uncert2[2,5]
plot_area2[10] <- test_uncert2[2,1] - plot_area2[6] - plot_area2[7] - plot_area2[8] - plot_area2[9]

plot_pct2 <- c(rep(0,10))

for(i in 1:5){
  plot_pct2[i] <- round(plot_area2[i]/test_uncert2[1,1], 4)
  plot_pct2[i+5] <- round(plot_area2[i+5]/test_uncert2[2,1], 4)
}

plot_df2 <- data.frame(
  area = plot_area2,
  pct = plot_pct2,
  plot = c(rep('Year 1', 5), rep('Year 2', 5)),
  type = c('Host', 'Initial Conditions', 'Parameter', 'Process', 'Interactions')
)


library(ggplot2)
barvarpct2 <- ggplot(data = plot_df2, aes(x = plot, y = area, fill = type, label = paste((100 * pct), '%', sep =''))) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal() +
  labs(title = 'PoPS Uncertainty Partitioning (2 year)', x = '', y = 'Variance in Infected Area', fill = 'Uncertainty Type') +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
constbarvarpct2 <- ggplot(data = plot_df2, aes(x = plot, y = pct, fill = type, label = paste((100 * pct), '%', sep =''))) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal() +
  labs(title = 'PoPS Uncertainty Partitioning (2 year)', x = '', y = 'Variance in Infected Area', fill = 'Uncertainty Type') +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
