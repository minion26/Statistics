##A1

a1=function(lambda,p,n,k){
  x=0:n
  y=dbinom(x,n,p)
  #return(barplot(y,main="Binomiala",space=0.5,col='blue'))
  
  z=k:(k+n)
  t=dpois(z,lambda)
  #return(barplot(t,main="Poisson",space=0.5,col='pink'))
  
  b=k:(k+n)
  c=dgeom(b,p)
  #return(barplot(c,main="Geometric",space=0.5,col='green'))
  
  H1<-barplot(y,main="Binomiala",space=0.5,col='blue')
  H2<-barplot(t,main="Poisson",space=0.5,col='pink')
  H3<-barplot(c,main="Geometric",space=0.5,col='green')
  plot(H1)
  plot(H2,col="red")
  plot(H3,col="green")
}

##A2

a2a=function(nume_fisier){
  
  #nume=scan(nume_fisier)
  nume=scan(file.choose())
  
  y=vector()
  y[1]=mean(nume)
  y[2]=median(nume)
  y[3]=sd(nume)
  y[4]=as.vector(quantile(nume))[1+1] #Q1
  y[5]=as.vector(quantile(nume))[2+1] #Q2
  y[6]=as.vector(quantile(nume))[3+1] #Q3
  
  return(y)
}


a2b=function(nume_fisier){
  #nume=scan(nume_fisier)
  nume=scan(file.choose())
  
  n=length(nume)
  medie=mean(nume)
  s=sd(nume)
  
  x=vector()
  j=0
  
  left = medie - s * 2
  right = medie + s * 2
  
  for(i in 1:n) {
    if((nume[i]<left) | (nume[i]>right)) {
      j = j + 1
      x[j] = nume[i] #x are val aberante
    }
  }
  nume=nume[-x] #vectorul curatat
  
  #print(x)

  return(nume)
}

a2c=function(nume_fisier){
  x=a2b(nume_fisier) #vectorul curatat
  
  
  interval = seq(0, 100, 10)
  #interval[1] = 0 + 0.00000000000001
  
  hist(x, breaks = interval,right=T,col='pink')
}

tema1=function(){
  a1(5, 0.25, 10, 100)
  
  a2a("notePS.txt")
  a2b("notePS.txt")
  a2c("notePS.txt")
  
}
tema1()
