#Load in Rasters (names same as file)
All_Mean_R <- terra::rast('All_Mean.tif')
All_SD_R <- terra::rast('All_SD.tif')
None_Mean_R <- terra::rast('None_Mean.tif')
None_SD_R <- terra::rast('None_SD.tif')
Host_Mean_R <- terra::rast('Host_Mean.tif')
Host_SD_R <- terra::rast('Host_SD.tif')
IC_Mean_R <- terra::rast('IC_Mean.tif')
IC_SD_R <- terra::rast('IC_SD.tif')
Par_Mean_R <- terra::rast('Par_Mean.tif')
Par_SD_R <- terra::rast('Par_SD.tif')

e <- ext(c(380000,400000,4678000,4711000))
All_Mean_E <- crop(All_Mean_R, e)
All_SD_E <- crop(All_SD_R, e)
None_Mean_E <- crop(None_Mean_R, e)
None_SD_E <- crop(None_SD_R, e)
Host_Mean_E <- crop(Host_Mean_R, e)
Host_SD_E <- crop(Host_SD_R, e)
IC_Mean_E <- crop(IC_Mean_R, e)
IC_SD_E <- crop(IC_SD_R, e)
Par_Mean_E <- crop(Par_Mean_R, e)
Par_SD_E <- crop(Par_SD_R, e)

par(mfrow = c(2,3))
plot(All_Mean_E, main = 'All Mean')
plot(None_Mean_E, main = 'None Mean')
plot(Host_Mean_E, main = 'Host Mean')
plot(IC_Mean_E, main = 'IC Mean')
plot(Par_Mean_E, main = 'Par Mean')
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(All_SD_E, main = 'All SD')
plot(None_SD_E, main = 'None SD')
plot(Host_SD_E, main = 'Host SD')
plot(IC_SD_E, main = 'IC SD')
plot(Par_SD_E, main = 'Par SD')
par(mfrow = c(1,1))