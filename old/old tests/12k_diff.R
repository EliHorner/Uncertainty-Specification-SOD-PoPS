k112 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/12k Test/SobolTotalOrderpar.tif")
k212 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/12k Test 2/SobolTotalOrderpar.tif")
small <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/HPC_Outs (test)/SobolOuts/SobolTotalOrderpar.tif")

singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

par(mfrow=c(3,1))
plot(k112, ext = multiWindow)
plot(k212, ext = multiWindow)
plot(small, ext = multiWindow)
par(mfrow=c(1,1))

sb12k <- c(k112, k212)
plot(diff(sb12k), ext = multiWindow)

s <- c(k112, small)
plot(diff(s), ext = multiWindow)

sdt <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/12k Test/All_SD.tif")
sdt2 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/12k Test 2/All_SD.tif")
k112s <- k112 * sdt
k212s <- k212 * sdt2

sb12 <- c(k112s, k212s)
plot(diff(sb12), ext = multiWindow)
#plot(sodPoints, add = TRUE)

tf12 <- freq(diff(sb12), digits = 1)

#0.986 within 0.1

d12k <- density(values(diff(sb12k), na.rm = TRUE), bw = 'SJ')