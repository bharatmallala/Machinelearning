input = matrix(c(0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0,1,0,1,1,1,1,0,0,1,1,0,1,1,1,1,0,1,1,1,1), byrow=4,16)
input = cbind(input,rep(1,16))
output = matrix(c(0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0),16,1)

set.seed(89)
w1=matrix(c(runif(20,-1,1)),5,4)
set.seed(89)
w2=matrix(c(runif(5,-1,1)),5,1)
n=0.5
sigfun = function(x){
  return(1/(1+exp(-x)))
  
}

#hiddenlayer
count= 0
deltaw1=0
deltaw2=0
alpha =0.9
vl1 = (input%*%w1)
out1 = sigfun(vl1)
out1=cbind(out1,c(rep(1,16)))
vl2 = (out1%*%w2)
out2 = sigfun(vl2)
error = output - out2


repeat{
  
  if(abs(error[1,1])>=0.05|abs(error[2,1])>=0.05|abs(error[3,1])>=0.05|abs(error[4,1])>=0.05|abs(error[5,1])>=0.05|abs(error[6,1])>=0.05|abs(error[7,1])>=0.05|abs(error[8,1])>=0.05|abs(error[9,1])>=0.05|abs(error[10,1])>=0.05|abs(error[11,1])>=0.05|abs(error[12,1])>=0.05|abs(error[13,1])>=0.05|abs(error[14,1])>=0.05|abs(error[15,1])>=0.05|abs(error[16,1])>=0.05){ 
    
    count = count +1
    deract = out2 *(1-out2)
    deltak = (error*deract)
    deltaw2 = (n*t(out1)%*%deltak) + (alpha*deltaw2)
    #output layer
    deltaj = (deltak%*%t(w2)*(out1*(1-out1)))
    deltaw1 = (((t(input)%*%deltaj) * n)[,1:4]) +(alpha*deltaw1)
    w2 = w2 + deltaw2
    w1 = w1 + deltaw1
    
    vl1 = (input%*%w1)
    out1 = sigfun(vl1)
    out1=cbind(out1,c(rep(1,16)))
    vl2 = (out1%*%w2)
    out2 = sigfun(vl2)
    error = output - out2
  }
  else{
    break
  }
  print(count)
}
