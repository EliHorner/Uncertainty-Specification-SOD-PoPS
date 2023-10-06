library(terra)
inpath <- "P:/PoPS_Sobol_Analysis/outputs/case1/"


fullPath <- paste0(inpath, '/pops_runs/')
filelist <- list.files(fullPath, pattern = "inf*")
temp <- rast(paste0(fullPath, filelist))
assign(paste0("All_Rasts"), temp)

All_Mean <- mean(All_Rasts)

AllValues <- global(All_Rasts, sum, na.rm = TRUE)
MeanValue <- global(All_Mean, sum, na.rm = TRUE)

closestIndex <- (abs(AllValues$sum - MeanValue$sum))

testSeedsCSV <- read.csv("P:/PoPS_Sobol_Analysis/outputs/case1/forecast_random_seeds.csv")
newSeedsCSV <- read.csv("P:/PoPS_Sobol_Analysis/outputs/case2/forecast_random_seeds.csv")

newSeedsCSV$disperser_generation <- testSeedsCSV[864,2]
newSeedsCSV$natural_dispersal <- testSeedsCSV[864,3]
newSeedsCSV$anthropogenic_dispersal <- testSeedsCSV[864,4]
newSeedsCSV$establishment <- testSeedsCSV[864,5]

newSeedsCSV <- subset(newSeedsCSV, select = -c(X))
write.csv(newSeedsCSV, "P:/PoPS_Sobol_Analysis/sod_inputs/Process_Off_Seeds.csv")