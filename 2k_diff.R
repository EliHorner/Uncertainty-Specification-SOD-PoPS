b629 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/HPC_Outs (629)/SobolTotalOrderpar.tif")
big <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/HPC_Outs (bigTest)/SobolOuts/SobolTotalOrderpar.tif")
small <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/HPC_Outs (test)/SobolOuts/SobolTotalOrderpar.tif")

par(mfrow=c(3,1))
plot(b629, ext = multiWindow)
plot(big, ext = multiWindow)
plot(small, ext = multiWindow)
par(mfrow=c(1,1))

sb2k <- c(b629, big)
plot(diff(sb2k), ext = multiWindow)

k2s <- c(b629, small)
plot(diff(k2s), ext = multiWindow)

sdt <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/HPC_Outs (629)/All_SD.tif")
sdt2 <- rast("C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/HPC_Outs (bigTest)/Rasters/All_SD.tif")
  
bs629 <- b629 * sdt
bsbig <- big * sdt2

k2sb2 <- c(bs629, bsbig)
plot(diff(k2sb2), ext = multiWindow)
#plot(sodPoints, add = TRUE)

tf2 <- freq(diff(k2sb2), digits = 1)

#0.934 within 0.1

d2k <- density(values(diff(sb2k), na.rm = TRUE), bw = 'SJ')