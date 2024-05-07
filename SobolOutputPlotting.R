#Plots for New (Lump Method) Results
library(terra)
library(RColorBrewer)

# Read in Sobol Rasters

# Read in Predicted Infection Points
infections1 <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/yr1Mean.tif")
Minf1 <- mean(infections1)
sodPoints1 <- as.points(Minf1)
sodPoints1 <- sodPoints1[sodPoints1$mean >= 1]

infections2 <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/yr2Mean.tif")
Minf2 <- mean(infections2)
sodPoints2 <- as.points(Minf2)
sodPoints2 <- sodPoints2[sodPoints2$mean >= 1]

infections3 <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/yr3Mean.tif")
Minf3 <- mean(infections3)
sodPoints3 <- as.points(Minf3)
sodPoints3 <- sodPoints3[sodPoints3$mean >= 1]

infections4 <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/yr4Mean.tif")
Minf4 <- mean(infections4)
sodPoints4 <- as.points(Minf4)
sodPoints4 <- sodPoints4[sodPoints4$mean >= 1]

infections5 <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/yr5Mean.tif")
Minf5 <- mean(infections5)
sodPoints5 <- as.points(Minf5)
sodPoints5 <- sodPoints5[sodPoints5$mean >= 1]

yr1Var <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/yr1Var.tif")
yr2Var <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/yr2Var.tif")
yr3Var <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/yr3Var.tif")
yr4Var <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/yr4Var.tif")
yr5Var <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/yr5Var.tif")

# Make Color Palette
pal <- colorRampPalette(brewer.pal(9, 'OrRd'))(25)
pal_pct <- colorRampPalette(brewer.pal(9, 'GnBu'))(25)
e <- terra::ext(c(380000,400000,4678000,4711000))
singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

#Load Rasters
fHost1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_Host.tif")
fIC1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_IC.tif")
fWeather1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_Weather.tif")
fPar1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_Par.tif")
fProcess1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_Process.tif")

tHost1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_Host.tif")
tIC1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_IC.tif")
tWeather1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_Weather.tif")
tPar1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_Par.tif")
tProcess1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_Process.tif")

fHost2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_Host.tif")
fIC2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_IC.tif")
fWeather2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_Weather.tif")
fPar2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_Par.tif")
fProcess2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_Process.tif")

tHost2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_Host.tif")
tIC2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_IC.tif")
tWeather2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_Weather.tif")
tPar2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_Par.tif")
tProcess2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_Process.tif")

#New 5yr 4K outputs
host1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_Host.tif")
ic1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_IC.tif")
weather1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_Weather.tif")
par1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_Par.tif")
process1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/STO_Process.tif")

tSum1 <- host1 + ic1 + weather1 + par1 + process1

host1pct <- host1 / tSum1
ic1pct <- ic1 / tSum1
weather1pct <- weather1 / tSum1
par1pct <- par1 / tSum1
process1pct <- process1 / tSum1

host2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/STO_Host.tif")
ic2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/STO_IC.tif")
weather2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/STO_Weather.tif")
par2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/STO_Par.tif")
process2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/STO_Process.tif")

tSum2 <- host2 + ic2 + weather2 + par2 + process2

host2pct <- host2 / tSum2
ic2pct <- ic2 / tSum2
weather2pct <- weather2 / tSum2
par2pct <- par2 / tSum2
process2pct <- process2 / tSum2

host3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/STO_Host.tif")
ic3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/STO_IC.tif")
weather3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/STO_Weather.tif")
par3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/STO_Par.tif")
process3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/STO_Process.tif")

tSum3 <- host3 + ic3 + weather3 + par3 + process3

host3pct <- host3 / tSum3
ic3pct <- ic3 / tSum3
weather3pct <- weather3 / tSum3
par3pct <- par3 / tSum3
process3pct <- process3 / tSum3

host4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/STO_Host.tif")
ic4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/STO_IC.tif")
weather4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/STO_Weather.tif")
par4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/STO_Par.tif")
process4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/STO_Process.tif")

tSum4 <- host4 + ic4 + weather4 + par4 + process4

host4pct <- host4 / tSum4
ic4pct <- ic4 / tSum4
weather4pct <- weather4 / tSum4
par4pct <- par4 / tSum4
process4pct <- process4 / tSum4

host5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/STO_Host.tif")
ic5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/STO_IC.tif")
weather5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/STO_Weather.tif")
par5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/STO_Par.tif")
process5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/STO_Process.tif")

tSum5 <- host5 + ic5 + weather5 + par5 + process5

host5pct <- host5 / tSum5
ic5pct <- ic5 / tSum5
weather5pct <- weather5 / tSum5
par5pct <- par5 / tSum5
process5pct <- process5 / tSum5

#First order rasters
fhost1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/SFO_Host.tif")
fic1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/SFO_IC.tif")
fweather1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/SFO_Weather.tif")
fpar1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/SFO_Par.tif")
fprocess1 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr1/SFO_Process.tif")

fSum1 <- fhost1 + fic1 + fweather1 + fpar1 + fprocess1

fhost1pct <- fhost1 / fSum1
fic1pct <- fic1 / fSum1
fweather1pct <- fweather1 / fSum1
fpar1pct <- fpar1 / fSum1
fprocess1pct <- fprocess1 / fSum1

fhost2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/SFO_Host.tif")
fic2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/SFO_IC.tif")
fweather2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/SFO_Weather.tif")
fpar2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/SFO_Par.tif")
fprocess2 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr2/SFO_Process.tif")

fSum2 <- fhost2 + fic2 + fweather2 + fpar2 + fprocess2

fhost2pct <- fhost2 / fSum2
fic2pct <- fic2 / fSum2
fweather2pct <- fweather2 / fSum2
fpar2pct <- fpar2 / fSum2
fprocess2pct <- fprocess2 / fSum2

fhost3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/SFO_Host.tif")
fic3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/SFO_IC.tif")
fweather3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/SFO_Weather.tif")
fpar3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/SFO_Par.tif")
fprocess3 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr3/SFO_Process.tif")

fSum3 <- fhost3 + fic3 + fweather3 + fpar3 + fprocess3

fhost3pct <- fhost3 / fSum3
fic3pct <- fic3 / fSum3
fweather3pct <- fweather3 / fSum3
fpar3pct <- fpar3 / fSum3
fprocess3pct <- fprocess3 / fSum3

fhost4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/SFO_Host.tif")
fic4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/SFO_IC.tif")
fweather4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/SFO_Weather.tif")
fpar4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/SFO_Par.tif")
fprocess4 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr4/SFO_Process.tif")

fSum4 <- fhost4 + fic4 + fweather4 + fpar4 + fprocess4

fhost4pct <- fhost4 / fSum4
fic4pct <- fic4 / fSum4
fweather4pct <- fweather4 / fSum4
fpar4pct <- fpar4 / fSum4
fprocess4pct <- fprocess4 / fSum4

fhost5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/SFO_Host.tif")
fic5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/SFO_IC.tif")
fweather5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/SFO_Weather.tif")
fpar5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/SFO_Par.tif")
fprocess5 <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/5year4k/yr5/SFO_Process.tif")

fSum5 <- fhost5 + fic5 + fweather5 + fpar5 + fprocess5

fhost5pct <- fhost5 / fSum5
fic5pct <- fic5 / fSum5
fweather5pct <- fweather5 / fSum5
fpar5pct <- fpar5 / fSum5
fprocess5pct <- fprocess5 / fSum5


#Plots
par(mfrow=c(2,3))
plot(yr1Var, col = pal, ext = multiWindow, range = c(0,75))
plot(sodPoints1, add  = TRUE)
plot(yr2Var, col = pal, ext = multiWindow, range = c(0,75))
plot(sodPoints2, add  = TRUE)
plot(yr3Var, col = pal, ext = multiWindow, range = c(0,75))
plot(sodPoints3, add  = TRUE)
plot(yr4Var, col = pal, ext = multiWindow, range = c(0,75))
plot(sodPoints4, add  = TRUE)
plot(yr5Var, col = pal, ext = multiWindow, range = c(0,75))
plot(sodPoints5, add  = TRUE)
par(mfrow = c(1,1))


# First Order Plots
par(mfrow = c(2,3))
plot(fHost1k, ext = multiWindow, main = 'Host', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fIC1k, ext = multiWindow, main = 'Initial Conditions', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fWeather1k, ext = multiWindow, main = 'Weather', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fPar1k, ext = multiWindow, main = 'Parameter', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fProcess1k, ext = multiWindow, main = 'Process', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(which.max(c(fHost1k, fIC1k, fWeather1k, fPar1k, fProcess1k)), ext = multiWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(fHost1k, ext = singleWindow, main = 'Host', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fIC1k, ext = singleWindow, main = 'Initial Conditions', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fWeather1k, ext = singleWindow, main = 'Weather', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fPar1k, ext = singleWindow, main = 'Parameter', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fProcess1k, ext = singleWindow, main = 'Process', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(which.max(c(fHost1k, fIC1k, fWeather1k, fPar1k, fProcess1k)), ext = singleWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(fHost1k, ext = e, main = 'Host', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(fIC1k, ext = e, main = 'Initial Conditions', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(fWeather1k, ext = e, main = 'Weather', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(fPar1k, ext = e, main = 'Parameter', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(fProcess1k, ext = e, main = 'Process', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(which.max(c(fHost1k, fIC1k, fWeather1k, fPar1k, fProcess1k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

# Total Order Plots
par(mfrow = c(2,3))
plot(tHost1k, ext = multiWindow, main = 'Host', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tIC1k, ext = multiWindow, main = 'Initial Conditions', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tWeather1k, ext = multiWindow, main = 'Weather', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tPar1k, ext = multiWindow, main = 'Parameter', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tProcess1k, ext = multiWindow, main = 'Process', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(which.max(c(tHost1k, tIC1k, tWeather1k, tPar1k, tProcess1k)), ext = multiWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(tHost1k, ext = singleWindow, main = 'Host', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(tIC1k, ext = singleWindow, main = 'Initial Conditions', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(tWeather1k, ext = singleWindow, main = 'Weather', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(tPar1k, ext = singleWindow, main = 'Parameter', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(tProcess1k, ext = singleWindow, main = 'Process', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(which.max(c(tHost1k, tIC1k, tWeather1k, tPar1k, tProcess1k)), ext = singleWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(tHost1k, ext = e, main = 'Host', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tIC1k, ext = e, main = 'Initial Conditions', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tWeather1k, ext = e, main = 'Weather', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tPar1k, ext = e, main = 'Parameter', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tProcess1k, ext = e, main = 'Process', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(which.max(c(tHost1k, tIC1k, tWeather1k, tPar1k, tProcess1k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

#Percentage plots
fSum = fHost1k + fIC1k + fWeather1k + fPar1k + fProcess1k
tSum = tHost1k + tIC1k + tWeather1k + tPar1k + tProcess1k

fSum2k = fHost2k + fIC2k + fWeather2k + fPar2k + fProcess2k
tSum2k = tHost2k + tIC2k + tWeather2k + tPar2k + tProcess2k

pctHost2k <- tHost2k / tSum2k
pctIC2k <- tIC2k / tSum2k
pctWeather2k <- tWeather2k / tSum2k
pctPar2k <- tPar2k / tSum2k
pctProcess2k <- tProcess2k / tSum2k

pctHost1k <- tHost1k / tSum
pctIC1k <- tIC1k / tSum
pctWeather1k <- tWeather1k / tSum
pctPar1k <- tPar1k / tSum
pctProcess1k <- tProcess1k / tSum

pal_pct <- colorRampPalette(brewer.pal(9, 'GnBu'))(25)

#first order percentages
par(mfrow = c(2,3))
plot(fHost1k / fSum, ext = e, main = 'Host %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(fIC1k / fSum, ext = e, main = 'Initial Conditions %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(fWeather1k / fSum, ext = e, main = 'Weather %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(fPar1k / fSum, ext = e, main = 'Parameter %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(fProcess1k / fSum, ext = e, main = 'Process %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
#plot(which.max(c(tHost1k, tIC1k, tWeather1k, tPar1k, tProcess1k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

#2k percentages
par(mfrow = c(2,3))
plot(tHost2k / tSum2k, ext = e, main = 'Host %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(tIC2k / tSum2k, ext = e, main = 'Initial Conditions %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(tWeather2k / tSum2k, ext = e, main = 'Weather %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(tPar2k / tSum2k, ext = e, main = 'Parameter %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(tProcess2k / tSum2k, ext = e, main = 'Process %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(which.max(c(tHost2k, tIC2k, tWeather2k, tPar2k, tProcess2k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

#1k and 2k differences
par(mfrow = c(2,3))
plot(pctHost2k - pctHost1k, ext = multiWindow, main = 'Host % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#(sodPoints, add = TRUE)
plot(pctIC2k - pctIC1k, ext = multiWindow, main = 'Initial Conditions % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#plot(sodPoints, add = TRUE)
plot(pctWeather2k - pctWeather1k, ext = multiWindow, main = 'Weather % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#plot(sodPoints, add = TRUE)
plot(pctPar2k - pctPar1k, ext = multiWindow, main = 'Parameter % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#plot(sodPoints, add = TRUE)
plot(pctProcess2k - pctProcess1k, ext = multiWindow, main = 'Process % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#plot(sodPoints, add = TRUE)
#plot(which.max(c(tHost2k, tIC2k, tWeather2k, tPar2k, tProcess2k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

#Summary Data frame
vals1k <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'), 
                     first_order_sum = c(global(fHost1k, sum, na.rm = TRUE)$sum, global(fIC1k, sum, na.rm = TRUE)$sum, global(fWeather1k, sum, na.rm = TRUE)$sum, global(fPar1k, sum, na.rm = TRUE)$sum, global(fProcess1k, sum, na.rm = TRUE)$sum), 
                     total_order_sum = c(global(tHost1k, sum, na.rm = TRUE)$sum, global(tIC1k, sum, na.rm = TRUE)$sum, global(tWeather1k, sum, na.rm = TRUE)$sum, global(tPar1k, sum, na.rm = TRUE)$sum, global(tProcess1k, sum, na.rm = TRUE)$sum),
                     first_order_mean = c(global(fHost1k, mean, na.rm = TRUE)$mean, global(fIC1k, mean, na.rm = TRUE)$mean, global(fWeather1k, mean, na.rm = TRUE)$mean, global(fPar1k, mean, na.rm = TRUE)$mean, global(fProcess1k, mean, na.rm = TRUE)$mean),
                     total_order_mean = c(global(tHost1k, mean, na.rm = TRUE)$mean, global(tIC1k, mean, na.rm = TRUE)$mean, global(tWeather1k, mean, na.rm = TRUE)$mean, global(tPar1k, mean, na.rm = TRUE)$mean, global(tProcess1k, mean, na.rm = TRUE)$mean)
)

vals2k <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'), 
                     first_order_sum = c(global(fHost2k, sum, na.rm = TRUE)$sum, global(fIC2k, sum, na.rm = TRUE)$sum, global(fWeather2k, sum, na.rm = TRUE)$sum, global(fPar2k, sum, na.rm = TRUE)$sum, global(fProcess2k, sum, na.rm = TRUE)$sum), 
                     total_order_sum = c(global(tHost2k, sum, na.rm = TRUE)$sum, global(tIC2k, sum, na.rm = TRUE)$sum, global(tWeather2k, sum, na.rm = TRUE)$sum, global(tPar2k, sum, na.rm = TRUE)$sum, global(tProcess2k, sum, na.rm = TRUE)$sum),
                     first_order_mean = c(global(fHost2k, mean, na.rm = TRUE)$mean, global(fIC2k, mean, na.rm = TRUE)$mean, global(fWeather2k, mean, na.rm = TRUE)$mean, global(fPar2k, mean, na.rm = TRUE)$mean, global(fProcess2k, mean, na.rm = TRUE)$mean),
                     total_order_mean = c(global(tHost2k, mean, na.rm = TRUE)$mean, global(tIC2k, mean, na.rm = TRUE)$mean, global(tWeather2k, mean, na.rm = TRUE)$mean, global(tPar2k, mean, na.rm = TRUE)$mean, global(tProcess2k, mean, na.rm = TRUE)$mean)
)

vals12 <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'), 
                     yr1_total_sum = c(global(host1new, sum, na.rm = TRUE)$sum, global(ic1new, sum, na.rm = TRUE)$sum, global(weather1new, sum, na.rm = TRUE)$sum, global(par1new, sum, na.rm = TRUE)$sum, global(process1new, sum, na.rm = TRUE)$sum),
                     yr1_pct = c(global(host1new, sum, na.rm = TRUE)$sum / sum(vals12$yr1_total_sum), global(ic1new, sum, na.rm = TRUE)$sum / sum(vals12$yr1_total_sum), global(weather1new, sum, na.rm = TRUE)$sum / sum(vals12$yr1_total_sum), global(par1new, sum, na.rm = TRUE)$sum / sum(vals12$yr1_total_sum), global(process1new, sum, na.rm = TRUE)$sum / sum(vals12$yr1_total_sum)),
                     yr2_total_sum = c(global(host2, sum, na.rm = TRUE)$sum, global(ic2, sum, na.rm = TRUE)$sum, global(weather2, sum, na.rm = TRUE)$sum, global(par2, sum, na.rm = TRUE)$sum, global(process2, sum, na.rm = TRUE)$sum),
                     yr2_pct = c(global(host2, sum, na.rm = TRUE)$sum / sum(vals12$yr2_total_sum), global(ic2, sum, na.rm = TRUE)$sum / sum(vals12$yr2_total_sum), global(weather2, sum, na.rm = TRUE)$sum / sum(vals12$yr2_total_sum), global(par2, sum, na.rm = TRUE)$sum / sum(vals12$yr2_total_sum), global(process2, sum, na.rm = TRUE)$sum / sum(vals12$yr2_total_sum))
)

vals3yr <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'), 
                     yr1_total_sum = c(global(host1, sum, na.rm = TRUE)$sum, global(ic1, sum, na.rm = TRUE)$sum, global(weather1, sum, na.rm = TRUE)$sum, global(par1, sum, na.rm = TRUE)$sum, global(process1, sum, na.rm = TRUE)$sum),
                     yr1_pct = c(global(host1, sum, na.rm = TRUE)$sum / sum(vals3yr$yr1_total_sum), global(ic1, sum, na.rm = TRUE)$sum / sum(vals3yr$yr1_total_sum), global(weather1, sum, na.rm = TRUE)$sum / sum(vals3yr$yr1_total_sum), global(par1, sum, na.rm = TRUE)$sum / sum(vals3yr$yr1_total_sum), global(process1, sum, na.rm = TRUE)$sum / sum(vals3yr$yr1_total_sum)),
                     yr2_total_sum = c(global(host2, sum, na.rm = TRUE)$sum, global(ic2, sum, na.rm = TRUE)$sum, global(weather2, sum, na.rm = TRUE)$sum, global(par2, sum, na.rm = TRUE)$sum, global(process2, sum, na.rm = TRUE)$sum),
                     yr2_pct = c(global(host2, sum, na.rm = TRUE)$sum / sum(vals3yr$yr2_total_sum), global(ic2, sum, na.rm = TRUE)$sum / sum(vals3yr$yr2_total_sum), global(weather2, sum, na.rm = TRUE)$sum / sum(vals3yr$yr2_total_sum), global(par2, sum, na.rm = TRUE)$sum / sum(vals3yr$yr2_total_sum), global(process2, sum, na.rm = TRUE)$sum / sum(vals3yr$yr2_total_sum)),
                     yr3_total_sum = c(global(host3, sum, na.rm = TRUE)$sum, global(ic3, sum, na.rm = TRUE)$sum, global(weather3, sum, na.rm = TRUE)$sum, global(par3, sum, na.rm = TRUE)$sum, global(process3, sum, na.rm = TRUE)$sum),
                     yr3_pct = c(global(host3, sum, na.rm = TRUE)$sum / sum(vals3yr$yr3_total_sum), global(ic3, sum, na.rm = TRUE)$sum / sum(vals3yr$yr3_total_sum), global(weather3, sum, na.rm = TRUE)$sum / sum(vals3yr$yr3_total_sum), global(par3, sum, na.rm = TRUE)$sum / sum(vals3yr$yr3_total_sum), global(process3, sum, na.rm = TRUE)$sum / sum(vals3yr$yr3_total_sum))
)

yr1_sum <- global(tSum1, sum, na.rm = TRUE)$sum
yr2_sum <- global(tSum2, sum, na.rm = TRUE)$sum
yr3_sum <- global(tSum3, sum, na.rm = TRUE)$sum
yr4_sum <- global(tSum4, sum, na.rm = TRUE)$sum
yr5_sum <- global(tSum5, sum, na.rm = TRUE)$sum

fyr1_sum <- global(fSum1, sum, na.rm = TRUE)$sum
fyr2_sum <- global(fSum2, sum, na.rm = TRUE)$sum
fyr3_sum <- global(fSum3, sum, na.rm = TRUE)$sum
fyr4_sum <- global(fSum4, sum, na.rm = TRUE)$sum
fyr5_sum <- global(fSum5, sum, na.rm = TRUE)$sum

vals4yr <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'),
                      yr1_total_sum = c(global(host1, sum, na.rm = TRUE)$sum, global(ic1, sum, na.rm = TRUE)$sum, global(weather1, sum, na.rm = TRUE)$sum, global(par1, sum, na.rm = TRUE)$sum, global(process1, sum, na.rm = TRUE)$sum),
                      yr1_total_pct = c(global(host1, sum, na.rm = TRUE)$sum / yr1_sum, global(ic1, sum, na.rm = TRUE)$sum / yr1_sum, global(weather1, sum, na.rm = TRUE)$sum / yr1_sum, global(par1, sum, na.rm = TRUE)$sum / yr1_sum, global(process1, sum, na.rm = TRUE)$sum / yr1_sum),
                      yr1_avg_pct = c(global(host1pct, mean, na.rm = TRUE)$mean, global(ic1pct, mean, na.rm = TRUE)$mean, global(weather1pct, mean, na.rm = TRUE)$mean, global(par1pct, mean, na.rm = TRUE)$mean, global(process1pct, mean, na.rm = TRUE)$mean),
                      yr2_total_sum = c(global(host2, sum, na.rm = TRUE)$sum, global(ic2, sum, na.rm = TRUE)$sum, global(weather2, sum, na.rm = TRUE)$sum, global(par2, sum, na.rm = TRUE)$sum, global(process2, sum, na.rm = TRUE)$sum),
                      yr2_total_pct = c(global(host2, sum, na.rm = TRUE)$sum / yr2_sum, global(ic2, sum, na.rm = TRUE)$sum / yr2_sum, global(weather2, sum, na.rm = TRUE)$sum / yr2_sum, global(par2, sum, na.rm = TRUE)$sum / yr2_sum, global(process2, sum, na.rm = TRUE)$sum / yr2_sum),
                      yr2_avg_pct = c(global(host2pct, mean, na.rm = TRUE)$mean, global(ic2pct, mean, na.rm = TRUE)$mean, global(weather2pct, mean, na.rm = TRUE)$mean, global(par2pct, mean, na.rm = TRUE)$mean, global(process2pct, mean, na.rm = TRUE)$mean),
                      yr3_total_sum = c(global(host3, sum, na.rm = TRUE)$sum, global(ic3, sum, na.rm = TRUE)$sum, global(weather3, sum, na.rm = TRUE)$sum, global(par3, sum, na.rm = TRUE)$sum, global(process3, sum, na.rm = TRUE)$sum),
                      yr3_total_pct = c(global(host3, sum, na.rm = TRUE)$sum / yr3_sum, global(ic3, sum, na.rm = TRUE)$sum / yr3_sum, global(weather3, sum, na.rm = TRUE)$sum / yr3_sum, global(par3, sum, na.rm = TRUE)$sum / yr3_sum, global(process3, sum, na.rm = TRUE)$sum / yr3_sum),
                      yr3_avg_pct = c(global(host3pct, mean, na.rm = TRUE)$mean, global(ic3pct, mean, na.rm = TRUE)$mean, global(weather3pct, mean, na.rm = TRUE)$mean, global(par3pct, mean, na.rm = TRUE)$mean, global(process3pct, mean, na.rm = TRUE)$mean),
                      yr4_total_sum = c(global(host4, sum, na.rm = TRUE)$sum, global(ic4, sum, na.rm = TRUE)$sum, global(weather4, sum, na.rm = TRUE)$sum, global(par4, sum, na.rm = TRUE)$sum, global(process4, sum, na.rm = TRUE)$sum),
                      yr4_total_pct = c(global(host4, sum, na.rm = TRUE)$sum / yr4_sum, global(ic4, sum, na.rm = TRUE)$sum / yr4_sum, global(weather4, sum, na.rm = TRUE)$sum / yr4_sum, global(par4, sum, na.rm = TRUE)$sum / yr4_sum, global(process4, sum, na.rm = TRUE)$sum / yr4_sum),
                      yr4_avg_pct = c(global(host4pct, mean, na.rm = TRUE)$mean, global(ic4pct, mean, na.rm = TRUE)$mean, global(weather4pct, mean, na.rm = TRUE)$mean, global(par4pct, mean, na.rm = TRUE)$mean, global(process4pct, mean, na.rm = TRUE)$mean))

vals5yr <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'),
                      yr1_total_sum = c(global(host1, sum, na.rm = TRUE)$sum, global(ic1, sum, na.rm = TRUE)$sum, global(weather1, sum, na.rm = TRUE)$sum, global(par1, sum, na.rm = TRUE)$sum, global(process1, sum, na.rm = TRUE)$sum),
                      yr1_total_pct = c(global(host1, sum, na.rm = TRUE)$sum / yr1_sum, global(ic1, sum, na.rm = TRUE)$sum / yr1_sum, global(weather1, sum, na.rm = TRUE)$sum / yr1_sum, global(par1, sum, na.rm = TRUE)$sum / yr1_sum, global(process1, sum, na.rm = TRUE)$sum / yr1_sum),
                      yr1_avg_pct = c(global(host1pct, mean, na.rm = TRUE)$mean, global(ic1pct, mean, na.rm = TRUE)$mean, global(weather1pct, mean, na.rm = TRUE)$mean, global(par1pct, mean, na.rm = TRUE)$mean, global(process1pct, mean, na.rm = TRUE)$mean),
                      yr2_total_sum = c(global(host2, sum, na.rm = TRUE)$sum, global(ic2, sum, na.rm = TRUE)$sum, global(weather2, sum, na.rm = TRUE)$sum, global(par2, sum, na.rm = TRUE)$sum, global(process2, sum, na.rm = TRUE)$sum),
                      yr2_total_pct = c(global(host2, sum, na.rm = TRUE)$sum / yr2_sum, global(ic2, sum, na.rm = TRUE)$sum / yr2_sum, global(weather2, sum, na.rm = TRUE)$sum / yr2_sum, global(par2, sum, na.rm = TRUE)$sum / yr2_sum, global(process2, sum, na.rm = TRUE)$sum / yr2_sum),
                      yr2_avg_pct = c(global(host2pct, mean, na.rm = TRUE)$mean, global(ic2pct, mean, na.rm = TRUE)$mean, global(weather2pct, mean, na.rm = TRUE)$mean, global(par2pct, mean, na.rm = TRUE)$mean, global(process2pct, mean, na.rm = TRUE)$mean),
                      yr3_total_sum = c(global(host3, sum, na.rm = TRUE)$sum, global(ic3, sum, na.rm = TRUE)$sum, global(weather3, sum, na.rm = TRUE)$sum, global(par3, sum, na.rm = TRUE)$sum, global(process3, sum, na.rm = TRUE)$sum),
                      yr3_total_pct = c(global(host3, sum, na.rm = TRUE)$sum / yr3_sum, global(ic3, sum, na.rm = TRUE)$sum / yr3_sum, global(weather3, sum, na.rm = TRUE)$sum / yr3_sum, global(par3, sum, na.rm = TRUE)$sum / yr3_sum, global(process3, sum, na.rm = TRUE)$sum / yr3_sum),
                      yr3_avg_pct = c(global(host3pct, mean, na.rm = TRUE)$mean, global(ic3pct, mean, na.rm = TRUE)$mean, global(weather3pct, mean, na.rm = TRUE)$mean, global(par3pct, mean, na.rm = TRUE)$mean, global(process3pct, mean, na.rm = TRUE)$mean),
                      yr4_total_sum = c(global(host4, sum, na.rm = TRUE)$sum, global(ic4, sum, na.rm = TRUE)$sum, global(weather4, sum, na.rm = TRUE)$sum, global(par4, sum, na.rm = TRUE)$sum, global(process4, sum, na.rm = TRUE)$sum),
                      yr4_total_pct = c(global(host4, sum, na.rm = TRUE)$sum / yr4_sum, global(ic4, sum, na.rm = TRUE)$sum / yr4_sum, global(weather4, sum, na.rm = TRUE)$sum / yr4_sum, global(par4, sum, na.rm = TRUE)$sum / yr4_sum, global(process4, sum, na.rm = TRUE)$sum / yr4_sum),
                      yr4_avg_pct = c(global(host4pct, mean, na.rm = TRUE)$mean, global(ic4pct, mean, na.rm = TRUE)$mean, global(weather4pct, mean, na.rm = TRUE)$mean, global(par4pct, mean, na.rm = TRUE)$mean, global(process4pct, mean, na.rm = TRUE)$mean),
                      yr5_total_sum = c(global(host5, sum, na.rm = TRUE)$sum, global(ic5, sum, na.rm = TRUE)$sum, global(weather5, sum, na.rm = TRUE)$sum, global(par5, sum, na.rm = TRUE)$sum, global(process5, sum, na.rm = TRUE)$sum),
                      yr5_total_pct = c(global(host5, sum, na.rm = TRUE)$sum / yr5_sum, global(ic5, sum, na.rm = TRUE)$sum / yr5_sum, global(weather5, sum, na.rm = TRUE)$sum / yr5_sum, global(par5, sum, na.rm = TRUE)$sum / yr5_sum, global(process5, sum, na.rm = TRUE)$sum / yr5_sum),
                      yr5_avg_pct = c(global(host5pct, mean, na.rm = TRUE)$mean, global(ic5pct, mean, na.rm = TRUE)$mean, global(weather5pct, mean, na.rm = TRUE)$mean, global(par5pct, mean, na.rm = TRUE)$mean, global(process5pct, mean, na.rm = TRUE)$mean))

fvals5yr <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'),
                      yr1_total_sum = c(global(fhost1, sum, na.rm = TRUE)$sum, global(fic1, sum, na.rm = TRUE)$sum, global(fweather1, sum, na.rm = TRUE)$sum, global(fpar1, sum, na.rm = TRUE)$sum, global(fprocess1, sum, na.rm = TRUE)$sum),
                      yr1_total_pct = c(global(fhost1, sum, na.rm = TRUE)$sum / fyr1_sum, global(fic1, sum, na.rm = TRUE)$sum / fyr1_sum, global(fweather1, sum, na.rm = TRUE)$sum / fyr1_sum, global(fpar1, sum, na.rm = TRUE)$sum / fyr1_sum, global(fprocess1, sum, na.rm = TRUE)$sum / fyr1_sum),
                      yr1_avg_pct = c(global(fhost1pct, mean, na.rm = TRUE)$mean, global(fic1pct, mean, na.rm = TRUE)$mean, global(fweather1pct, mean, na.rm = TRUE)$mean, global(fpar1pct, mean, na.rm = TRUE)$mean, global(fprocess1pct, mean, na.rm = TRUE)$mean),
                      yr2_total_sum = c(global(fhost2, sum, na.rm = TRUE)$sum, global(fic2, sum, na.rm = TRUE)$sum, global(fweather2, sum, na.rm = TRUE)$sum, global(fpar2, sum, na.rm = TRUE)$sum, global(fprocess2, sum, na.rm = TRUE)$sum),
                      yr2_total_pct = c(global(fhost2, sum, na.rm = TRUE)$sum / fyr2_sum, global(fic2, sum, na.rm = TRUE)$sum / fyr2_sum, global(fweather2, sum, na.rm = TRUE)$sum / fyr2_sum, global(fpar2, sum, na.rm = TRUE)$sum / fyr2_sum, global(fprocess2, sum, na.rm = TRUE)$sum / fyr2_sum),
                      yr2_avg_pct = c(global(fhost2pct, mean, na.rm = TRUE)$mean, global(fic2pct, mean, na.rm = TRUE)$mean, global(fweather2pct, mean, na.rm = TRUE)$mean, global(fpar2pct, mean, na.rm = TRUE)$mean, global(fprocess2pct, mean, na.rm = TRUE)$mean),
                      yr3_total_sum = c(global(fhost3, sum, na.rm = TRUE)$sum, global(fic3, sum, na.rm = TRUE)$sum, global(fweather3, sum, na.rm = TRUE)$sum, global(fpar3, sum, na.rm = TRUE)$sum, global(fprocess3, sum, na.rm = TRUE)$sum),
                      yr3_total_pct = c(global(fhost3, sum, na.rm = TRUE)$sum / fyr3_sum, global(fic3, sum, na.rm = TRUE)$sum / fyr3_sum, global(fweather3, sum, na.rm = TRUE)$sum / fyr3_sum, global(fpar3, sum, na.rm = TRUE)$sum / fyr3_sum, global(fprocess3, sum, na.rm = TRUE)$sum / fyr3_sum),
                      yr3_avg_pct = c(global(fhost3pct, mean, na.rm = TRUE)$mean, global(fic3pct, mean, na.rm = TRUE)$mean, global(fweather3pct, mean, na.rm = TRUE)$mean, global(fpar3pct, mean, na.rm = TRUE)$mean, global(fprocess3pct, mean, na.rm = TRUE)$mean),
                      yr4_total_sum = c(global(fhost4, sum, na.rm = TRUE)$sum, global(fic4, sum, na.rm = TRUE)$sum, global(fweather4, sum, na.rm = TRUE)$sum, global(fpar4, sum, na.rm = TRUE)$sum, global(fprocess4, sum, na.rm = TRUE)$sum),
                      yr4_total_pct = c(global(fhost4, sum, na.rm = TRUE)$sum / fyr4_sum, global(fic4, sum, na.rm = TRUE)$sum / fyr4_sum, global(fweather4, sum, na.rm = TRUE)$sum / fyr4_sum, global(fpar4, sum, na.rm = TRUE)$sum / fyr4_sum, global(fprocess4, sum, na.rm = TRUE)$sum / fyr4_sum),
                      yr4_avg_pct = c(global(fhost4pct, mean, na.rm = TRUE)$mean, global(fic4pct, mean, na.rm = TRUE)$mean, global(fweather4pct, mean, na.rm = TRUE)$mean, global(fpar4pct, mean, na.rm = TRUE)$mean, global(fprocess4pct, mean, na.rm = TRUE)$mean),
                      yr5_total_sum = c(global(fhost5, sum, na.rm = TRUE)$sum, global(fic5, sum, na.rm = TRUE)$sum, global(fweather5, sum, na.rm = TRUE)$sum, global(fpar5, sum, na.rm = TRUE)$sum, global(fprocess5, sum, na.rm = TRUE)$sum),
                      yr5_total_pct = c(global(fhost5, sum, na.rm = TRUE)$sum / fyr5_sum, global(fic5, sum, na.rm = TRUE)$sum / fyr5_sum, global(fweather5, sum, na.rm = TRUE)$sum / fyr5_sum, global(fpar5, sum, na.rm = TRUE)$sum / fyr5_sum, global(fprocess5, sum, na.rm = TRUE)$sum / fyr5_sum),
                      yr5_avg_pct = c(global(fhost5pct, mean, na.rm = TRUE)$mean, global(fic5pct, mean, na.rm = TRUE)$mean, global(fweather5pct, mean, na.rm = TRUE)$mean, global(fpar5pct, mean, na.rm = TRUE)$mean, global(fprocess5pct, mean, na.rm = TRUE)$mean))



par(mfrow = c(2,3))
plot(host1pct, ext = multiWindow, main = 'Host', col = pal_pct, range = c(0,0.45))
#plot(sodPoints, add = TRUE)
plot(ic1pct, ext = multiWindow, main = 'Initial Conditions', col = pal_pct, range = c(0,0.45))
#plot(sodPoints, add = TRUE)
plot(weather1pct, ext = multiWindow, main = 'Weather', col = pal_pct, range = c(0,0.45))
#plot(sodPoints, add = TRUE)
plot(par1pct, ext = multiWindow, main = 'Parameter', col = pal_pct, range = c(0,0.45))
#plot(sodPoints, add = TRUE)
plot(process1pct, ext = multiWindow, main = 'Process', col = pal_pct, range = c(0,0.45))
#plot(sodPoints, add = TRUE)
plot(which.max(c(host1pct, ic1pct, weather1pct, par1pct, process1pct)), ext = multiWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(host2pct, ext = e, main = 'Host', col = pal_pct, range = c(0,0.5))
#plot(sodPoints, add = TRUE)
plot(ic2pct, ext = e, main = 'Initial Conditions', col = pal_pct, range = c(0,0.5))
#plot(sodPoints, add = TRUE)
plot(weather2pct, ext = e, main = 'Weather', col = pal_pct, range = c(0,0.5))
#plot(sodPoints, add = TRUE)
plot(par2pct, ext = e, main = 'Parameter', col = pal_pct, range = c(0,0.5))
#plot(sodPoints, add = TRUE)
plot(process2pct, ext = e, main = 'Process', col = pal_pct, range = c(0,0.5))
#plot(sodPoints, add = TRUE)
plot(which.max(c(host2pct, ic2pct, weather2pct, par2pct, process2pct)), ext = multiWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(host2pct - host1pct, ext = e, main = 'Host', col = brewer.pal(9, 'Spectral'), range = c(-0.25,0.25))
#plot(sodPoints, add = TRUE)
plot(ic2pct - ic1pct, ext = e, main = 'Initial Conditions', col = brewer.pal(9, 'Spectral'), range = c(-0.25,0.25))
#plot(sodPoints, add = TRUE)
plot(weather2pct - weather1pct, ext = e, main = 'Weather', col = brewer.pal(9, 'Spectral'), range = c(-0.25,0.25))
#plot(sodPoints, add = TRUE)
plot(par2pct - par1pct, ext = e, main = 'Parameter', col = brewer.pal(9, 'Spectral'), range = c(-0.25,0.25))
#plot(sodPoints, add = TRUE)
plot(process2pct - process1pct, ext = e, main = 'Process', col = brewer.pal(9, 'Spectral'), range = c(-0.25,0.25))
#plot(sodPoints, add = TRUE)
#plot(which.max(c(host2pct, ic2pct, weather2pct, par2pct, process2pct)), col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))