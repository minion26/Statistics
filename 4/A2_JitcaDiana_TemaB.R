b1=function(N,a,b,c,h){
  count=0
  
  f=h/c
  x1=-1*a*sqrt(f)
  x2=a*sqrt(f)
  y1=-1*b*sqrt(f)
  y2=b*sqrt(f)
  z1=0
  z2=h
  
  for(i in 1:N){
    x=runif(1,x1,x2)
    y=runif(1,y1,y2)
    z=runif(1,z1,z2)
    m1=(x*x)/(a*a)
    m2=(y*y)/(b*b)
    m3=z/c
    if(m1+m2<m3){
      count=count+1
    }
  }
  #volum paralelipiped
  val=((x2-x1)*(y2-y1)*(z2-z1)*count)/N
  
  #volum real
  real=(pi*a*b*h*h)/(2*c)
  
  valRelativa = abs( (real - val) / val)
  cat("Aria Aproximata: ", val)
  cat("\nDiferenta : ", real - val)
  cat("\nEroarea Relativa: ", valRelativa)
  
}

b1(20000,4,3,4,4)
b1(50000,4,3,4,4)
b1(100000,4,3,4,4)

#rezolvand inecuatiile => [1,7]x[0,2]
b2=function(N,a,b,c,d){
  count=0
  x=runif(1,a,b)
  y=runif(1,c,d)
  for(i in 1:N){
    if(x>=1 && x<=7 && y>=0 && y <=2){
      count=count+1
    }
  }
  
  return(((b-a)*(d-c)*count)/N)
}
b2(20000,1,7,0,2)


b3a=function(N,a,b){
  sum=0
  for(i in 1:N){
    u=runif(1,a,b)
    y=((u*u)+2)*((u*u)+2)*((u*u)+2)
    sum=sum+u/y
  }
  aria=(b-a)*sum/N
  cat("Aria aproximata intre", a,"si",b, ":", aria, "\n")
  cat("Valoarea exacta intre 1 si 2:", 1/48)
}
b3a(10000,1,2)

b3b=function(N,a,b){
  sum=0
  min=min(a,b)
  max=max(a,b)
  for(i in 1:N){
    u=runif(1,min,max)
    y=((u*u)+9)
    sum=sum+1/y
  }
  aria=(max-min)*sum/N
  cat("Aria aproximata intre", a,"si",b, ":", aria, "\n")
  cat("Valoarea exacta intre 1 si 2:", pi/6)
}
b3b(10000,3,-3)

b3c=function(N,a,b){
  sum=0
  min=min(a,b)
  max=max(a,b)
  for(i in 1:N){
    u=runif(1,min,max)
    y=u*exp(-1*(u*u))
    sum=sum+y
  }
  aria=(max-min)*sum/N
  cat("Aria aproximata intre", a,"si",b, ":", aria, "\n")
  cat("Valoarea exacta intre 0 si inf :", 1/2)
}
b3c(10000,0,10000)
b3c(10000,0,20000)
b3c(10000,0,10000)


b4=function(n){
  
  timp_servire=vector()
  i=0
  while(i<n){
    a=0
    a=rexp(1,4)
    prob=runif(1,0,1)
    
    if(prob<=0.25){
      a=a+rgamma(1,4,3)
    }
    else 
      if(prob<=0.5){
        a=a+rgamma(1,4,2)
      }
    else 
      if(prob<=0.8){
        a=a+rgamma(1,5,2)
      }
    else{
      a=a+rgamma(1,5,3)
    }
    
    timp_servire[i]=a
    i=i+1
  }
  
  return(mean(timp_servire))
}
b4(50000)





#############B5##############


nr_pc_infectate=function(vector,n) #cate computere am infectate
{
  nr=0
  for(i in 1:n)
    if(vector[i]==TRUE)
      nr=nr+1
  return(nr)
}

curatare_pc=function(vector,n)#curatare 8 computere
{
  if(nr_pc_infectate(vector,n)<=8){#daca<8 --> atunci se curata toate
    vector=vector(mode='logical',length=n)
    return(vector)
  }
  
  i=0
  while(i<=8)
  {
    j=sample(n,1)
    if(vector[j]){
      vector[j]=FALSE#este curatat
      i=i+1
    }
  }
  return(vector)
  
}

infectez_un_pc=function(n) #infectez un singur computer random
{
  i=sample(n,1)
  vector=vector(mode="logical",length=n)
  vector[i]=TRUE #un pc infectat
  return (vector)
}

infectez_pc=function(vector,n,k){#infectare cu probabilitate k
  for(i in 1:n){
    p=runif(1,0,1)
    if(p<=k) 
      vector[i]=TRUE #este infectat
  }
  return(vector)
}

infectez_toate_pc=function(n,zile){
  
  pc=infectez_un_pc(n) #pc are doar un pc infectat
  cel_putin_o_data=pc #copie la vector
  
  for(i in 2:zile){#incepem de la ziua 2
    
    probabilitati_de_infectare=c(0.05,0.1,0.2) #vector cu probabilitatile de infectare
    probabilitate=sample(3,1)#aleg o probabilitate din 3 pt a infecta
    
    #infectare PC
    pc=infectez_pc(pc,n,probabilitati_de_infectare[probabilitate]) #infectez toate calculatoarele
    
    for(j in 1:n)
      if(pc[j] == TRUE) #daca e infectat
          cel_putin_o_data[j]=TRUE
    
    #curatare PC
    pc=curatare_pc(pc,n)
    
  }
  
  if(nr_pc_infectate(cel_putin_o_data,n)==50)
    return (TRUE)
  return(FALSE)
}

b5_a=function(n,zile,verificari){
  nr=0
  for(i in 1:verificari)
    if(infectez_toate_pc(n,zile))
      nr=nr+1
  return(nr)
}

val=b5_a(50,99,50)
print(val)


