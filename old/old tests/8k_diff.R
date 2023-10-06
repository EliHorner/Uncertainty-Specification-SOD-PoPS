k18 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/8k Test/SobolTotalOrderpar.tif")
k28 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/8k Test 2/SobolTotalOrderpar.tif")
small <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/HPC_Outs (test)/SobolOuts/SobolTotalOrderpar.tif")

singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

par(mfrow=c(3,1))
plot(k18, ext = multiWindow)
plot(k28, ext = multiWindow)
plot(small, ext = multiWindow)
par(mfrow=c(1,1))

sb8k <- c(k18, k28)
plot(diff(sb8k), ext = multiWindow)

s <- c(k18, small)
plot(diff(s), ext = multiWindow)

sdt <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/8k Test/All_SD.tif")
sdt2 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/8k Test 2/All_SD.tif")
k18s <- k18 * sdt
k28s <- k28 * sdt2

sb8 <- c(k18s, k28s)
plot(diff(sb8), ext = multiWindow)
#plot(sodPoints, add = TRUE)

tf8 <- freq(diff(sb8), digits = 1)

#0.982 within 0.1

d8k <- density(values(diff(sb8k), na.rm = TRUE), bw = 'SJ')