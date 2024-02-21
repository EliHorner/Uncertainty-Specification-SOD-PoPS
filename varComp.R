#See differnces in 1k vs 2k samples where variance exceeds a critical value
varPoints <- as.points(sampleVar2k)
cv <- 0.5

hostDiff <- (pctHost2k-pctHost1k)[varPoints$std >= cv]
icDiff <- (pctIC2k-pctIC1k)[varPoints$std >= cv]
weatherDiff <- (pctWeather2k-pctWeather1k)[varPoints$std >= cv]
parDiff <- (pctPar2k-pctPar1k)[varPoints$std >= cv]
processDiff <- (pctProcess2k-pctProcess1k)[varPoints$std >= cv]

summary(hostDiff)
hist(hostDiff$mean, main = 'Host')

summary(icDiff)
hist(icDiff$mean, main = 'Initial Conditions')

summary(weatherDiff)
hist(weatherDiff$mean, main = 'Weather')

summary(parDiff)
hist(parDiff$mean, main = 'Parameter')

summary(processDiff)
hist(processDiff$mean, main = 'Process')