outcomes<-read.csv("outcome-of-care-measures.csv",colClasses = "character")

outcomes[, 11] <- as.numeric(outcomes[, 11])
hist(outcomes[,11])