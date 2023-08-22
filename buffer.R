#BUffer for short distance vs Long distance comparison
singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

inf <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/12k Test/All_Mean.tif")

critValInf <- 0.5
bufferRadius <- 100

infB <- as.points(inf)
infB <- infB[infB$mean >= critValInf]
infB <- buffer(infB, bufferRadius)
plot(infB)

r <- rast(nrow = 1073, ncol = 686, ext = ext(inf), crs = crs(inf))
shortDist <- rasterize(infB, r)
longDist <- 1 - rasterize(infB, r, background = 0)
longDist <- classify(longDist, rcl = cbind(0, NA))

shortDistP <- as.points(shortDist)
shortDistP <- shortDistP[shortDistP$layer >= 1]

#shortDistP <- as.polygons(shortDist)

longDistP <- as.points(longDist)
longDistP <- longDistP[longDistP$layer >= 1]

tS <- extract(k112, shortDistP)
summary(tS)
tL <- extract(k112, longDistP, function(x) mean(x, na.rm = TRUE))
summary(tL)