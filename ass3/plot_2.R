outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
out.ha<-as.numeric(outcome[,11])
out.hf<-as.numeric(outcome[,17])
out.p<-as.numeric(outcome[,23])

#print(out.ha)

xn <- "30-day Death Rate"
name.vec<-c("Heart Attack", "Heart Failure", "Pneumonia")
data.vec<-data.frame(out.ha, out.hf, out.p)

par(mfcol=c(3,1))

for (i in 1:3){
hist(data.vec[,i],
     density=8,
     prob=TRUE,
     main=substitute(name.vec[i](bar(X)==x),list(x=mean(data.vec[,i], na.rm=TRUE))),  
     #main=name.vec[i],
     xlab=xn,
     xlim=range(out.ha, out.hf, out.p ,na.rm=TRUE)
     )
abline(v = mean(data.vec[,i], na.rm=TRUE), col = "blue" )
}
