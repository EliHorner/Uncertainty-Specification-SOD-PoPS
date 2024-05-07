#New plots
library(terra)
library(geodata)
library(RColorBRewer)

#Files
us <- gadm(country = "USA", level = 2, resolution = 1, path = '../maps/')
hostNew5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/test_current/yr5/STO_Host.tif")

#Project basemap
us_proj <- project(us, crs(hostNew5))

#Bounding box (extents)
e <- terra::ext(c(380000,400000,4678000,4711000))
singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

px <- c(372000, 442000)
py <- c(4649000, 4757000)

ex <- c(380000, 400000)
ey <- c(4676000, 4711000)

#Colors
pal <- colorRampPalette(brewer.pal(9, 'OrRd'))(25)
pal_pct <- colorRampPalette(brewer.pal(9, 'GnBu'))(25)

#Plot
plot(us_proj, xlim = px, ylim = py, border = 'grey', background = 'lightblue', col = 'white', box = TRUE, axes = FALSE, main = 'Host Index')
plot(hostNew5,  add = TRUE, col = pal, range = c(0,0.5), legend = TRUE, axes = FALSE)
plot(us_proj, border = 'grey', add = TRUE)

host1new <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_Host.tif")
host1old <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/1year10k/Lump4k/STO_lump_Host.tif")

ic1new <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_IC.tif")
ic1old <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/1year10k/Lump4k/STO_lump_IC.tif")

weather1new <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_Weather.tif")
weather1old <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/1year10k/Lump4k/STO_lump_Weather.tif")

par1new <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_Par.tif")
par1old <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/1year10k/Lump4k/STO_lump_Par.tif")

process1new <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_Process.tif")
process1old <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/1year10k/Lump4k/STO_lump_Process.tif")

tSumNew <- host1new +ic1new + weather1new + par1new + process1new
tSumOld <- host1old + ic1old + weather1old + par1old + process1old

leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addRasterImage(host5pct, colors = pal_pct, opacity = 0.85, group = 'host') %>% 
  addRasterImage(ic5pct, colors = pal_pct, opacity = 0.85, group = 'ic') %>% 
  addRasterImage(weather5pct, colors = pal_pct, opacity = 0.85, group = 'weather') %>% 
  addRasterImage(par5pct, colors = pal_pct, opacity = 0.85, group = 'par') %>% 
  addRasterImage(process5pct, colors = pal_pct, opacity = 0.85, group = 'process') %>% 
  addLayersControl(baseGroups = c('host', 'ic', 'weather', 'par', 'process')) %>%
  addLegend('bottomright',
            colors = pal_pct,
            labels = seq(2, 38, 50/34),
            title = 'Percent Uncertainty',
            opacity = 1)

map <- leaflet() %>% addProviderTiles(providers$ CartoDB.DarkMatter) %>% 
  addRasterImage(which.max(c(host1pct, ic1pct, weather1pct, par1pct, process1pct)), colors = c('#224b5e', '#6d2f20', '#df7e66', '#edc775', '#94b594'), opacity = 0.8, group = 'yr1') %>% 
  addRasterImage(which.max(c(host2pct, ic2pct, weather2pct, par2pct, process2pct)), colors = c('#224b5e', '#6d2f20', '#df7e66', '#edc775', '#94b594'), opacity = 0.8, group = 'yr2') %>%
  addRasterImage(which.max(c(host3pct, ic3pct, weather3pct, par3pct, process3pct)), colors = c('#224b5e', '#6d2f20', '#df7e66', '#edc775', '#94b594'), opacity = 0.8, group = 'yr3') %>%
  addRasterImage(which.max(c(host4pct, ic4pct, weather4pct, par4pct, process4pct)), colors = c('#224b5e', '#6d2f20', '#df7e66', '#edc775', '#94b594'), opacity = 0.8, group = 'yr4') %>%
  addRasterImage(which.max(c(host5pct, ic5pct, weather5pct, par5pct, process5pct)), colors = c('#224b5e', '#6d2f20', '#df7e66', '#edc775', '#94b594'), opacity = 0.8, group = 'yr5') %>%
  addCircleMarkers(data = project(sodPoints1, '+proj=longlat +datum=WGS84'), opacity = 0.75, group = 'yr1 inf', radius = 4, stroke = FALSE, color = 'white') %>%
  addCircleMarkers(data = project(sodPoints2, '+proj=longlat +datum=WGS84'), opacity = 0.75, group = 'yr2 inf', radius = 4, stroke = FALSE, color = 'white') %>%
  addCircleMarkers(data = project(sodPoints3, '+proj=longlat +datum=WGS84'), opacity = 0.75, group = 'yr3 inf', radius = 4, stroke = FALSE, color = 'white') %>%
  addCircleMarkers(data = project(sodPoints4, '+proj=longlat +datum=WGS84'), opacity = 0.75, group = 'yr4 inf', radius = 4, stroke = FALSE, color = 'white') %>%
  addCircleMarkers(data = project(sodPoints5, '+proj=longlat +datum=WGS84'), opacity = 0.75, group = 'yr5 inf', radius = 4, stroke = FALSE, color = 'white') %>%
  addLayersControl(baseGroups = c('yr1', 'yr2', 'yr3', 'yr4', 'yr5'), overlayGroups = c('yr1 inf', 'yr2 inf', 'yr3 inf', 'yr4 inf', 'yr5 inf')) %>%
  addLegend('bottomright',
            colors = c('#224b5e', '#6d2f20', '#df7e66', '#edc775', '#94b594'),
            labels = c('Host', 'IC','Weather', 'Par', 'Process'),
            title = 'Largest Source',
            opacity = 1)
map

#saveWidget(map, file = '5yrMap.html')