library(viridisLite)
library(viridis) #just for colouring purpose

max_itr<-1000000 #number of iterations
color <- viridis::viridis(40) #color palette

x <- rep(0, times=max_itr) #make vector of 0s
y <- x #copy the vector

for (n in 2:(max_itr)) { #run the iteration to get next points
  
  rand_num=runif(1, 0, 100) #generate a random number between 0-100
  # starting at x = 0, y = 0
  #f1: xn+1 = 0 (chosen 1% of the time)
  #    yn+1 = 0.16 yn
  #f2: xn+1 = -0.15 xn + 0.28 yn (chosen 7% of the time)
  #    yn+1 = 0.26 xn + 0.24 yn + 0.44
  #f3: xn+1 = 0.2 xn - 0.26 yn (chosen 14% of the time)
  #    yn+1 = 0.23 xn + 0.22 yn + 1.6
  #f4: xn+1 = 0.85 xn + 0.04 yn (chosen 85% of the time)
  #    yn+1 = -0.04 xn + 0.85 yn + 1.6
  
  # I have subtracted 1 at every iteration from n so I dont have to add
  # 1 to f(x), basically subtracted from both sides
  
  if (rand_num < 1) { 
    x[n]<- 0
    y[n]<- 0.16*y[n-1]
  }
  else if (rand_num < 7){
    x[n]<- -0.15*x[n-1]+0.28*y[n-1]
    y[n]<- 0.26*x[n-1]+0.24*y[n-1]+0.44
  }
  else if (rand_num < 14){
    x[n]<- 0.2*x[n-1]-0.26*y[n-1]
    y[n]<- 0.23*x[n-1]+0.22*y[n-1]+1.6
  }
  else {
    x[n]<- 0.85*x[n-1]+0.04*y[n-1]
    y[n]<- -0.04*x[n-1]+0.85*y[n-1]+1.6
  }
  
}

#Saving the plot
png(filename="fern.png", bg= "floralwhite",height = 1920, width = 1080, units = "px", res= 150) 
plot(x,y, pch='.', xlab= "", ylab = "", col= color, )
title(xlab = "Rachilla & Pinnule", ylab = "Rachis", col.lab ="indianred4", main= "the Barnsley fern", col.main= "mediumaquamarine", font.main= 4)

dev.off()
