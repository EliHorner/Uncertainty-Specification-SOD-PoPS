k14 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/4k Test/SobolTotalOrderpar.tif")
k24 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/4k test 2/SobolTotalOrderpar.tif")
small <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/HPC_Outs (test)/SobolOuts/SobolTotalOrderpar.tif")

singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

par(mfrow=c(3,1))
plot(k14, ext = multiWindow)
plot(k24, ext = multiWindow)
plot(small, ext = multiWindow)
par(mfrow=c(1,1))

sb4k <- c(k14, k24)
plot(diff(sb4k), ext = multiWindow)

s <- c(k14, small)
plot(diff(s), ext = multiWindow)

sdt <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/4k Test/All_SD.tif")
sdt2 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/4k test 2/All_SD.tif")

k14s <- k14 * sdt
k24s <- k24 * sdt2

sb4 <- c(k14s, k24s)
plot(diff(sb4), ext = multiWindow)
#plot(sodPoints, add = TRUE)

tf4 <- freq(diff(sb4), digits = 1)
# 
# par(mfrow=c(2,2))
# plot(diff(k2sb2), ext = multiWindow, breaks = c(-10, -0.5, -0.25, -0.1 , 0.1, 0.25, 0.5, 10), col = palDiff, main = '2k Par Total')
# plot(diff(sb4), ext = multiWindow, breaks = c(-10, -0.5, -0.25, -0.1 , 0.1, 0.25, 0.5, 10), col = palDiff, main = '4k Par Total')
# plot(diff(sb8), ext = multiWindow, breaks = c(-10, -0.5, -0.25, -0.1 , 0.1, 0.25, 0.5, 10), col = palDiff, main = '8k Par Total')
# plot(diff(sb12), ext = multiWindow, breaks = c(-10, -0.5, -0.25, -0.1 , 0.1, 0.25, 0.5, 10), col = palDiff, main = '12k Par Total')
# par(mfrow=c(1,1))

#density(diff(sb4k)$mean)

#0.970 within 0.1

d4k <- density(values(diff(sb4k), na.rm = TRUE), bw = 'SJ')

# plot(d8k2, lwd = 2)
# lines(d2k2, lwd = 2, col = 'red')
# lines(d4k2, lwd = 2, col = 'blue')
# abline(v=0, lty = 3)

# plot(d8k, lwd = 2)
# lines(d2k, lwd = 2, col = 'red')
# lines(d4k, lwd = 2, col = 'blue')
# abline(v=0, lty = 3)