#Load in Rasters (names same as file)
All_Mean_R <- terra::rast('All_Mean.tif')
All_SD_R <- terra::rast('All_SD.tif')
All_Prob_R <- terra::rast('All_Prob.tif')
None_Mean_R <- terra::rast('None_Mean.tif')
None_SD_R <- terra::rast('None_SD.tif')
None_Prob_R <- terra::rast('None_Prob.tif')
Host_Mean_R <- terra::rast('Host_Mean.tif')
Host_SD_R <- terra::rast('Host_SD.tif')
Host_Prob_R <- terra::rast('Host_Prob.tif')
IC_Mean_R <- terra::rast('IC_Mean.tif')
IC_SD_R <- terra::rast('IC_SD.tif')
IC_Prob_R <- terra::rast('IC_Prob.tif')
Par_Mean_R <- terra::rast('Par_Mean.tif')
Par_SD_R <- terra::rast('Par_SD.tif')
Par_Prob_R <- terra::rast('Par_Prob.tif')


e <- terra::ext(c(380000,400000,4678000,4711000))
All_Mean_E <- terra::crop(All_Mean_R, e)
All_SD_E <- terra::crop(All_SD_R, e)
All_Prob_E <- terra::crop(All_Prob_R, e)
None_Mean_E <- terra::crop(None_Mean_R, e)
None_SD_E <- terra::crop(None_SD_R, e)
None_Prob_E <- terra::crop(None_Prob_R, e)
Host_Mean_E <- terra::crop(Host_Mean_R, e)
Host_SD_E <- terra::crop(Host_SD_R, e)
Host_Prob_E <- terra::crop(Host_Prob_R, e)
IC_Mean_E <- terra::crop(IC_Mean_R, e)
IC_SD_E <- terra::crop(IC_SD_R, e)
IC_Prob_E <- terra::crop(IC_Prob_R, e)
Par_Mean_E <- terra::crop(Par_Mean_R, e)
Par_SD_E <- terra::crop(Par_SD_R, e)
Par_Prob_E <- terra::crop(Par_Prob_R, e)

par(mfrow = c(2,3))
terra::plot(All_Mean_E, main = 'All Mean')
terra::plot(None_Mean_E, main = 'None Mean')
terra::plot(Host_Mean_E, main = 'Host Mean')
terra::plot(IC_Mean_E, main = 'IC Mean')
terra::plot(Par_Mean_E, main = 'Par Mean')
par(mfrow = c(1,1))

par(mfrow = c(2,3))
terra::plot(All_SD_E, main = 'All SD')
terra::plot(None_SD_E, main = 'None SD')
terra::plot(Host_SD_E, main = 'Host SD')
terra::plot(IC_SD_E, main = 'IC SD')
terra::plot(Par_SD_E, main = 'Par SD')
par(mfrow = c(1,1))

par(mfrow = c(2,3))
terra::plot(All_Prob_E, main = 'All SD')
terra::plot(None_Prob_E, main = 'None SD')
terra::plot(Host_Prob_E, main = 'Host SD')
terra::plot(IC_Prob_E, main = 'IC SD')
terra::plot(Par_Prob_E, main = 'Par SD')
par(mfrow = c(1,1))

Process_Rast <- None_SD_E
Parameter_Rast <- Par_SD_E - None_SD_E
Host_Rast <- Host_SD_E - None_SD_E
IC_Rast <- IC_SD_E - None_SD_E
Interactions_Rast <- All_SD_E - IC_Rast - Parameter_Rast - Process_Rast - Host_Rast

Process_Mean <- None_Mean_E
Parameter_Mean <- Par_Mean_E - None_Mean_E
Host_Mean <- Host_Mean_E - None_Mean_E
IC_Mean <- IC_Mean_E - None_Mean_E
Interactions_Mean <- All_Mean_E - IC_Mean - Parameter_Mean - Process_Mean - Host_Mean

pro3 <- aggregate(Process_Rast, fact = 3, fun = 'mean')
par3 <- aggregate(Parameter_Rast, fact = 3, fun = 'mean')
h3 <- aggregate(Host_Rast, fact = 3, fun = 'mean') 
ic3 <- aggregate(IC_Rast, fact = 3, fun = 'mean') 
int3 <- aggregate(Interactions_Rast, fact = 3, fun = 'mean') 
all3 <- aggregate(All_SD_E, fact = 3, fun = 'mean')

Process_Prob <- None_Prob_E
Parameter_Prob <- Par_Prob_E - None_Prob_E
Host_Prob <- Host_Prob_E - None_Prob_E
IC_Prob <- IC_Prob_E - None_Prob_E
Interactions_Prob <- All_Prob_E - IC_Prob - Parameter_Prob - Process_Prob - Host_Prob

library(RColorBrewer)
num_cols <- 7
mypal <- colorRampPalette(brewer.pal(9, 'YlGnBu'))(num_cols)
plot(Parameter_Rast / All_SD_E, col = mypal)
testpal <- c('#8B0000', "#FFFFD9", "#E0F3B2", "#97D6B8", "#41B6C4", "#1E80B8", "#24429A", "#081D58", "#702963")

#Filter to only do division where we have standard deviation in the all scenario
#How -> Mask out others maybe?

No_Var_Mask <- subst(All_SD_E, NA, 0) != 0

singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

#Standard Deviation Rasters (full study area)
par(mfrow = c(2,3))
terra::plot(All_SD_E, main = 'Overall Uncertainty (Infections)')
terra::plot(Process_Rast / All_SD_E, main = 'Process', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Parameter_Rast / All_SD_E, main = 'Parameter', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Host_Rast / All_SD_E, main = 'Host', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(IC_Rast / All_SD_E, main = 'IC', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Interactions_Rast / All_SD_E, main = 'Interactions', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
par(mfrow = c(1,1))

#Probability Rasters (not working yet)
par(mfrow = c(2,3))
terra::plot(All_Prob_E, main = 'Overall Uncertainty (Infections)')
terra::plot(Process_Prob, main = 'Process', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Parameter_Prob, main = 'Parameter', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Host_Prob, main = 'Host', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(IC_Prob, main = 'IC', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Interactions_Prob, main = 'Interactions', col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
par(mfrow = c(1,1))

#Standard Deviation Rasters (single infection area)
par(mfrow = c(2,3))
terra::plot(All_SD_E, main = 'Overall Uncertainty (Infections)', ext = singleWindow)
terra::plot(Process_Rast / All_SD_E, main = 'Process', col = testpal, ext = singleWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Parameter_Rast / All_SD_E, main = 'Parameter', col = testpal, ext = singleWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Host_Rast / All_SD_E, main = 'Host', col = testpal, ext = singleWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(IC_Rast / All_SD_E, main = 'IC', col = testpal, ext = singleWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Interactions_Rast / All_SD_E, main = 'Interactions', col = testpal, ext = singleWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
par(mfrow = c(1,1))

#Standard Deviation Rasters (infection cluster area)
par(mfrow = c(2,3))
terra::plot(All_SD_E, main = 'Overall Uncertainty (Infections)', ext = multiWindow)
terra::plot(Process_Rast / All_SD_E, main = 'Process', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Parameter_Rast / All_SD_E, main = 'Parameter', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Host_Rast / All_SD_E, main = 'Host', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(IC_Rast / All_SD_E, main = 'IC', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(Interactions_Rast / All_SD_E, main = 'Interactions', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
par(mfrow = c(1,1))

par(mfrow = c(2,3))
terra::plot(all3, main = 'Overall Uncertainty (Infections)', ext = multiWindow)
terra::plot(pro3 / all3, main = 'Process', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(par3 / all3, main = 'Parameter', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(h3 / all3, main = 'Host', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(ic3 / all3, main = 'IC', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
terra::plot(int3 / all3, main = 'Interactions', col = testpal, ext = multiWindow, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1, 10))
par(mfrow = c(1,1))

par(mfrow = c(2,3))
terra::plot(All_SD_E, main = 'Overall Uncertainty (Infections)', ext = multiWindow)
terra::plot(None_SD_E, main = 'Process', ext = multiWindow)
terra::plot(Par_SD_E, main = 'Parameter', ext = multiWindow)
terra::plot(Host_SD_E, main = 'Host', ext = multiWindow)
terra::plot(IC_SD_E, main = 'IC', ext = multiWindow)
par(mfrow = c(1,1))

#Sobol Indices Muti-Plots

palovr <- colorRampPalette(brewer.pal(9, 'OrRd'))(7)
par(mfrow = c(2,2))
plot(stoTotal, ext = singleWindow, main = 'Sobol Total Index', col = palovr)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
plot(stoPar / stoTotal, ext = singleWindow, col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0000001, 10), main = 'Parameter')
plot(sodPoints, ext = singleWindow, col = 'red', add = TRUE)
plot(stoHost / stoTotal, ext = singleWindow, col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0000001, 10), main = 'Host')
plot(sodPoints, ext = singleWindow, col = 'red', add = TRUE)
plot(stoIC / stoTotal, ext = singleWindow, col = testpal, breaks = c(-10, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0000001, 10), main = 'Initial Condition')
plot(sodPoints, ext = singleWindow, col = 'red', add = TRUE)
par(mfrow = c(1,1))

palt <- colorRampPalette(brewer.pal(9, 'Greens'))(8)
pal <- colorRampPalette(brewer.pal(9, 'OrRd'))(10)

par(mfrow = c(2,2))
plot(stoTotalNew, ext = singleWindow, main = 'Sobol Total Indices Sum', col = palt)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
plot(stoHostNew, ext = singleWindow, main = 'Host Sobol Total Index', col = pal)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
plot(stoParNew, ext = singleWindow, main = 'Parameter Sobol Total Index', col = pal)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
plot(stoICNew, ext = singleWindow, main = 'Initial Condition Sobol Total Index', col = pal)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(stoTotalNew, ext = multiWindow, main = 'Sobol Total Indices Sum', col = palt)
plot(sodPoints, ext = multiWindow, col = 'black', add = TRUE)
plot(stoHost, ext = multiWindow, main = 'Host Sobol Total Index', col = pal)
plot(sodPoints, ext = multiWindow, col = 'black', add = TRUE)
plot(stoPar, ext = multiWindow, main = 'Parameter Sobol Total Index', col = pal)
plot(sodPoints, ext = multiWindow, col = 'black', add = TRUE)
plot(stoIC, ext = multiWindow, main = 'Initial Condition Sobol Total Index', col = pal)
plot(sodPoints, ext = multiWindow, col = 'black', add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(sfoTotalNew, ext = singleWindow, main = 'Sobol First Order Indices Sum', col = palt)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
plot(sfoHostNew, ext = singleWindow, main = 'Host Sobol First Order Index', col = pal)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
plot(sfoParNew, ext = singleWindow, main = 'Parameter Sobol First Order Index', col = pal)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
plot(sfoICNew, ext = singleWindow, main = 'Initial Condition Sobol First Order Index', col = pal)
plot(sodPoints, ext = singleWindow, col = 'black', add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(sfoTotal, ext = multiWindow, main = 'Sobol First Order Indices Sum', col = palt)
plot(sfoHost, ext = multiWindow, main = 'Host Sobol First Order Index', col = pal)
plot(sodPoints, ext = multiWindow, col = 'black', add = TRUE)
plot(sfoPar, ext = multiWindow, main = 'Parameter Sobol First Order Index', col = pal)
plot(sodPoints, ext = multiWindow, col = 'black', add = TRUE)
plot(sfoIC, ext = multiWindow, main = 'Initial Condition Sobol First Order Index', col = pal)
plot(sodPoints, ext = multiWindow, col = 'black', add = TRUE)
par(mfrow = c(1,1))