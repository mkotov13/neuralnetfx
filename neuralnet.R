###050415
getwd()
library(neuralnet)
fx<-read.csv(file="EURUSD_day.csv")

head(fx)
tail(fx)

fx.c <- fx$X.CLOSE.
head(fx.c)
tail(fx.c)

#i <- 5
n <- length(fx.c)
fx.ls <- vector()
for (i in 0:10) {
	fx.1 <- fx.c[c(n-i):1]
	fx.ls <- cbind(fx.ls, fx.1[1:c(n-10)])
}
head(fx.ls)

fx.lags<-as.data.frame(fx.ls)

names(fx.lags) <- c("fx.t", "fx.tm1", "fx.tm2", "fx.tm3", "fx.tm4", "fx.tm5", "fx.tm6", "fx.tm7", "fx.tm8", "fx.tm9", "fx.tm10")

head(fx.lags)

###Log differences

fx.c2 <- log(fx$X.CLOSE.)
head(fx.c2)
tail(fx.c2)

#i <- 5
n <- length(fx.c2)

###Clear fx.ls2 before running the loop every time!!

fx.ls2 <- vector()
for (i in 1:11) {
	fx.ptm1 <- fx.c2[c(n-i):1]
	fx.pt <- fx.c2[c((n-i)+1):2]
	fx.diff <- fx.pt - fx.ptm1
	fx.ls2 <- cbind(fx.ls2, fx.diff[1:c(n-10)])
}

head(fx.ls2)
head(fx.diff)

fx.lags<-as.data.frame(fx.ls2)

names(fx.lags) <- c("fx.t", "fx.tm1", "fx.tm2", "fx.tm3", "fx.tm4", "fx.tm5", "fx.tm6", "fx.tm7", "fx.tm8", "fx.tm9", "fx.tm10")

library(neuralnet)
fx.lags.300 <- fx.lags[1:300,]
fx.lags.100.300 <- fx.lags[101:300,]
fx.lags.100 <- fx.lags[1:100, 2:11]

nn <- neuralnet(fx.t~fx.tm1+fx.tm2+fx.tm3+fx.tm4+fx.tm5+fx.tm6+fx.tm7+fx.tm8+fx.tm9+fx.tm10, data=fx.lags.300, rep=5, hidden=8)
nn2 <- neuralnet(fx.t~fx.tm1+fx.tm2+fx.tm3+fx.tm4+fx.tm5+fx.tm6+fx.tm7+fx.tm8+fx.tm9+fx.tm10, data=fx.lags.100.300, hidden=8)

###nn <- neuralnet(fx.t~fx.tm1+fx.tm2+fx.tm3, data=fx.lags.300, hidden=2)
###nn2 <- neuralnet(fx.t~fx.tm1+fx.tm2+fx.tm3, data=fx.lags.100.300, hidden=2)

head(fx.lags)

nn
plot(nn2)

### running and saving all 5 repetitions
besttoworst<-c(3,2,4,5,1)
### don't forget to empty the outputs
outputs<-vector()

for(i in besttoworst){
pred <- compute(nn, fx.lags.100, rep=i)
#outputs<-list(outputs, list(pred$net.result))
outputs <- cbind(outputs, pred$net.result)
}

### plots for multiple repetitions
par(mfrow=c(3,1))

for(i in c(1,5)){
  plot(outputs[1:24, i], type="l", col="red")
  abline(v=c(1:25))
}

plot(pred$net.result[1:24], type="l", col="red")
abline(v=c(1:25))
plot(fx.lags[100:1,1][c(2:25)], type="l", col="blue")
abline(v=c(1:25))

fx.lags.300 <- fx.lags[1:300,2:4]
pred <- compute(nn2, fx.lags.300)
head(fx.lags.100)
?

plot(fx.lags[100:1,1], type="l", col="blue")
plot(pred$net.result, type="l", col="red")

lines(fx.lags[100:1,1], type="l", col="blue")
abline(v=200)

plot(fx.lags[300:1,1], type="l", col="blue")
abline(v=200)
plot(pred$net.result, type="l", col="red")
abline(v=200)

plot(fx.lags[300:101,1], type="l", col="blue")
plot(pred$net.result[101:300], type="l", col="red")

plot(fx.lags[300:251,1], type="l", col="blue")
plot(pred$net.result[251:300], type="l", col="red")



