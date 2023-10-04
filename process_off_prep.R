library(terra)
inpath <- "P:/PoPS_Sobol_Analysis/outputs/case1/"


fullPath <- paste0(inpath, '/pops_runs/')
filelist <- list.files(fullPath, pattern = "inf*")
temp <- rast(paste0(fullPath, filelist))
assign(paste0(i, "Rasts"), temp)
