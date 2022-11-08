#Generate clustered spatial point process

library(spatstat)

#define variables
#variables that control the size and strength of clusters
val.at.center=1
effect.range=10
background=0.0001

#variables that control the number of points and spatial dimensions
Pointnum=100
Xmin=-50
Xmax=50
Ymin=-50
Ymax=50

#define the center locations and set up the distance vector
centers=matrix(data=c(-25,-25,25,25,-25,25,-25,25),nrow=4,ncol=2)
dist=matrix(nrow=4,ncol=1)

#define outputs
output.X=matrix(nrow = Pointnum, ncol = 1)
output.Y=matrix(nrow = Pointnum, ncol = 1)

#precalcs - calculate the slope of the clustering effect
slope=(val.at.center-background)/effect.range

#set a counter
outcounter=0

#main for loop
for (i in 1:100000){
  #generate a random candidate point
  x.candidate=runif(1, min=Xmin, max=Xmax)
  y.candidate=runif(1, min=Ymin, max=Ymax)
  
  #calculate the distance between the candidate point and the nearest cluster center
  for (j in 1:4){
    dist[j]=sqrt((x.candidate-centers[j,1])^2+(y.candidate-centers[j,2])^2)
  }
  min.dist=min(dist)
  
  #calculate the probability of retaining the candidate point
  if(min.dist<effect.range){
    prob=val.at.center-slope*min.dist
  }
  else
    prob=background
  
  #roll the dice to see if you keep the candidate point
  testval=runif(1,min=0,max=1)  
  if (testval<prob){
    outcounter=outcounter+1
    keep=1
    output.X[outcounter]=x.candidate
    output.Y[outcounter]=y.candidate
  }
  
  #if you've reached your target number of points, break from the for loop
  if(outcounter==Pointnum){
    break
  }
}

#plot the points and the cluster centers
plot(output.X,output.Y)
points(centers,type="p", col="red", pch=21, bg="red")

#create a point pattern object for analysis using the spatstat library
output_ppp = ppp(output.X, output.Y, c(Xmin,Xmax), c(Ymin,Ymax))




#Q1

#CSR
background.csr=1
output.X.csr=matrix(nrow = Pointnum, ncol = 1)
output.Y.csr=matrix(nrow = Pointnum, ncol = 1)
slope.csr=(val.at.center-background.csr)/effect.range

#set a counter
outcounter=0

#main for loop
for (i in 1:100000){
  #generate a random candidate point
  x.candidate=runif(1, min=Xmin, max=Xmax)
  y.candidate=runif(1, min=Ymin, max=Ymax)
  
  #calculate the distance between the candidate point and the nearest cluster center
  for (j in 1:4){
    dist[j]=sqrt((x.candidate-centers[j,1])^2+(y.candidate-centers[j,2])^2)
  }
  min.dist=min(dist)
  
  #calculate the probability of retaining the candidate point
  if(min.dist<effect.range){
    prob=val.at.center-slope.csr*min.dist
  }
  else
    prob=background.csr
  
  #roll the dice to see if you keep the candidate point
  testval=runif(1,min=0,max=1)  
  if (testval<prob){
    outcounter=outcounter+1
    keep=1
    output.X.csr[outcounter]=x.candidate
    output.Y.csr[outcounter]=y.candidate
  }
  
  #if you've reached your target number of points, break from the for loop
  if(outcounter==Pointnum){
    break
  }
}

output_csr = ppp(output.X.csr, output.Y.csr, c(Xmin,Xmax), c(Ymin,Ymax))

#plot the points 
plot(output.X.csr,output.Y.csr)



#Clustered
background.cluster = 0.0001
output.X.cluster=matrix(nrow = Pointnum, ncol = 1)
output.Y.cluster=matrix(nrow = Pointnum, ncol = 1)
slope.cluster=(val.at.center-background.cluster)/effect.range

#set a counter
outcounter=0

#main for loop
for (i in 1:100000){
  #generate a random candidate point
  x.candidate=runif(1, min=Xmin, max=Xmax)
  y.candidate=runif(1, min=Ymin, max=Ymax)
  
  #calculate the distance between the candidate point and the nearest cluster center
  for (j in 1:4){
    dist[j]=sqrt((x.candidate-centers[j,1])^2+(y.candidate-centers[j,2])^2)
  }
  min.dist=min(dist)
  
  #calculate the probability of retaining the candidate point
  if(min.dist<effect.range){
    prob=val.at.center-slope.cluster*min.dist
  }
  else
    prob=background.cluster
  
  #roll the dice to see if you keep the candidate point
  testval=runif(1,min=0,max=1)  
  if (testval<prob){
    outcounter=outcounter+1
    keep=1
    output.X.cluster[outcounter]=x.candidate
    output.Y.cluster[outcounter]=y.candidate
  }
  
  #if you've reached your target number of points, break from the for loop
  if(outcounter==Pointnum){
    break
  }
}

output_cluster = ppp(output.X.cluster, output.Y.cluster, c(Xmin,Xmax), c(Ymin,Ymax))

#plot the points 
plot(output.X.cluster,output.Y.cluster)




#Q2
quadrat.test(output_csr)
quadrat.test(output_cluster)



#Q3
envelope_csr <- envelope(output_csr)
envelope_cluster <- envelope(output_cluster)

plot(envelope_csr)
plot(envelope_cluster)




#Q4
effect.range.q4 = 2
background.q4 = 0.009
output.X.q4=matrix(nrow = Pointnum, ncol = 1)
output.Y.q4=matrix(nrow = Pointnum, ncol = 1)
slope.q4=(val.at.center-background.q4)/effect.range.q4

#set a counter
outcounter=0

#main for loop
for (i in 1:100000){
  #generate a random candidate point
  x.candidate=runif(1, min=Xmin, max=Xmax)
  y.candidate=runif(1, min=Ymin, max=Ymax)
  
  #calculate the distance between the candidate point and the nearest cluster center
  for (j in 1:4){
    dist[j]=sqrt((x.candidate-centers[j,1])^2+(y.candidate-centers[j,2])^2)
  }
  min.dist=min(dist)
  
  #calculate the probability of retaining the candidate point
  if(min.dist<effect.range){
    prob=val.at.center-slope.q4*min.dist
  }
  else
    prob=background.q4
  
  #roll the dice to see if you keep the candidate point
  testval=runif(1,min=0,max=1)  
  if (testval<prob){
    outcounter=outcounter+1
    keep=1
    output.X.q4[outcounter]=x.candidate
    output.Y.q4[outcounter]=y.candidate
  }
  
  #if you've reached your target number of points, break from the for loop
  if(outcounter==Pointnum){
    break
  }
}

output_q4 = ppp(output.X.q4, output.Y.q4, c(Xmin,Xmax), c(Ymin,Ymax))


plot(output.X.q4,output.Y.q4)


envelope_q4 <- envelope(output_q4)
plot(envelope_q4)

