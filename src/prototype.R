## ---- prototype
## prototype1
library(MASS)
Sigma <- matrix(c(10,3,3,2),2,2)
p1 <- mvrnorm(n = 1000, rep(0, 2), Sigma)
p1 <- data.frame(p1)
for( i in 1:1000){
  if( (p1$X1[i] > 0) && (p1$X2[i] >0)){
    p1$X1[i]=p1$X1[i]+rnorm(1,5,1)
    p1$X2[i]=p1$X2[i]+rnorm(1,5,1)}}
for( i in 1:1000){
  if( (p1$X1[i] < 0) && (p1$X2[i] <0)){
    p1$X1[i]=p1$X1[i]+rnorm(1,0,1)
    p1$X2[i]=p1$X2[i]+rnorm(1,0,1)}}
plot(p1$X1, p1$X2)
p1$Source <- c()
for( i in 1:1000){
  if( (p1$X1[i] > 0) && (p1$X2[i] >0)){
    p1$Source[i]= "Simulated"} else {
    p1$Source[i]= "Real-data"}  
    }
save(p1, file="data/p1.rda")

## rototype 2
Sigma <- matrix(c(10,3,3,2),2,2)
p2 <- mvrnorm(n = 1000, rep(0, 2), Sigma)
p2 <- data.frame(p2)
for( i in 1:1000){
  if( (p2$X1[i] > -2) && (p2$X2[i] >-5)){
    p2$X1[i]=p2$X1[i]+rnorm(1,0,2)
    p2$X2[i]=p2$X2[i]+rnorm(1,0,2)}}
p2$Source <- c()
for( i in 1:1000){
  if( (p2$X1[i] > 0) && (p2$X2[i] >0)){
    p2$Source[i]= "Simulated"} else {
      p2$Source[i]= "Real-data"}  
}

for( i in 1:1000){
  if( (p2$X2[i] >1)){
    p2$Source[i]= "Simulated"}   
}
save(p2, file="data/p2.rda")
plot(p2$X1, p2$X2)
