data = read.csv(file.choose())
#initialization step
mu<-c(0.5,0.3,0.8)
sd<-c(0.7,0.6,0.5)
probx_c=data.frame(0)
probc_x=data.frame(0)
s<-c()
N<-c()
L<-vector()
L[2]<-2
L[1]<-1

probc<-c(0.33,0.34,0.33)
#expectation step
for(i in 2:1000){
if(abs(L[i]-L[i-1])>=0.00001)
{

    for(i in 1:nrow(data))
    {
      for(j in 1:length(mu)){
        
        
        probx_c[i,j] <- ((exp( -1*(data[i,1]-mu[j])^2 / (2*(sd[j])^2)))) /(sqrt(2*pi*(sd[j])^2))
      }
      
    }
    
    
    for(i in 1:nrow(data)){
      s[i]= 0
    }
    for(i in 1:nrow(data))
    {
      for(j in 1:length(mu)) {
        s[i]<-s[i]+(probc[j]*probx_c[i,j])}}
  
  for(i in 1:nrow(data)) {
    
    for(j in 1:length(mu)){
      probc_x[i,j] <- (probx_c[i,j]*probc[j])/s[i]
    }
  }
  
  for(j in 1:length(mu)){
    N[j] = 0
  }
  
  
  for(j in 1:length(mu))
    N[j] =sum(probc_x[,j]) 


#maximization step


  for(i in 1:length(mu))
  {
    
    mu[i]<- sum(probc_x[,i]*data)/N[i]
    
    sd[i]<- sqrt((sum(probc_x[,i]*(data)^2))/N[i]-( sum(probc_x[,i]*data)/N[i])^2)
    
    probc[i]<- (N[i]/nrow(data))
    
    
  }
  
  #calculating loglikelihood
  

loglkh =0
  for (j in 1:length(mu)){
    loglkh<- loglkh +(probx_c[,j]*probc[j])
    
  }
  L= append(L, sum(log(loglkh)))

}}

#plotting value of loglikelihood
plot(2:length(L),L[-1],main = "Log likelihood iterations",xlab = "number of iterations")
mu
sd
probc
max(L)
#runnig EM algorithm





