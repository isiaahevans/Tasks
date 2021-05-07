setwd("C:\\Users\\isiaa\\Evolution\\Tasks\\Project")

orca <- read.csv("orcas.csv")

AllWhales <- unique(orca[,1])

Age <- c()
MaxSize <- c()
MinSize <- c()

i <- 1
for (Choose in AllWhales){
  YearsSeen <- orca[which(orca[,1]==Choose),"year"]
    
  Age[i] <- max(orca[which(orca[,1]==Choose),"survt"])
             
  MaxSize[i] <- max(orca[which(orca[,1]==Choose),"comm.size"])
  MinSize[i] <- min(orca[which(orca[,1]==Choose),"comm.size"])
  
    i <- i + 1
}


par(mfrow=c(1,2), mar=c(4,4,1,1), las=1, mgp=c(2.5, 0.25, 0), las=1, tck=-0.01)
plot(MaxSize, Age, xlab="largest community size", ylab="age", pch=21, bg="red")

#Model <- lm(Age~poly(MaxSize,2))
Model <- lm(log(Age)~MaxSize)
summary(Model)

pY <- exp(predict(Model))
lines(MaxSize[order(MaxSize)], pY[order(MaxSize)], col="darkblue", lwd=5, lty=2) # lty is line type. lty=1 is solid, lty=2 is dashed, lty=3 is dotted
# lwd is line width. lwd=5 is a super thick line, lwd=1 is a skinny line

plot(MinSize, Age, xlab="smallest community size", ylab="age", pch=21, bg="red")


