library(viridis)

max_itr<-1000000

x <- rep(0, times=max_itr)
y <- x

for (i in 2:(max_itr)) {
  rand=runif(1, 0, 100)
  
  if (rand<1) {
    x[i]<-0
    y[i]<-0.16*y[i-1]
  }
  
  else if (rand<7){
    x[i]<- -0.15*x[i-1]+0.28*y[i-1]
    y[i]<-0.26*x[i-1]+0.24*y[i-1]+0.44
  }
  
  else if (rand<14){
    x[i]<-0.2*x[i-1]-0.26*y[i-1]
    y[i]<-0.23*x[i-1]+0.22*y[i-1]+1.6
  }
  
  else {
    x[i]<-0.85*x[i-1]+0.04*y[i-1]
    y[i]<- -0.04*x[i-1]+0.85*y[i-1]+1.6
  }
}

color <- viridis::viridis(40)

png(filename="fern.png", bg= "floralwhite",height = 1920, width = 1080, units = "px", res= 150) 
plot(x,y, pch='.', xlab= "", ylab = "", col= color)
title(xlab = "Span", ylab = "Arch", col.lab ="indianred4")

dev.off()