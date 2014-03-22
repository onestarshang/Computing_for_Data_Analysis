outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], main="Heart Attack 30?day Death Rate", xlab="30?day Death Rate")