setwd("C:\\Users\\isiaa\\Evolution\\Tasks\\Project")

orca <- read.csv("orcas.csv")

Choose <- 50
AllWhales <- unique(orca[,1])


  YearsSeen <- orca[which(orca[,1]==Choose),"year"]
    
  Age <- (max(YearsSeen) - min(YearsSeen)) + min(orca[which(orca[,1]==Choose),"start"])
