new<-sims[sims$percent!=75 & sims$discard.D1=="no",]

new$rate<-round(new$rate,3)

new<-new[new$percent=="25",c(1,4,5)]

new<-reshape(new, v.names = "rate", idvar = "similarity", timevar = "mu", direction="wide")


library(htmlTable)
library(stringr)

htmlTable(new, rnames=FALSE, 
          header=str_pad(c("Similarity Measure", "0.0", 
                           " 0.1", " 0.2", " 0.3", " 0.4"), side="both",width=2))