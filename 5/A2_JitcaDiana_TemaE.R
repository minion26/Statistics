e1=function(tip,alfa,n,sample_mean,mu0,sigma){
  
  #l - as la stg
  #r - as la drp
  #s - simetrica
  
  tscore=(sample_mean-mu0)/(sigma/sqrt(n))
  
  if(tip=='l'){
    tcritic=qt(alfa,n-1)
    if(tscore<tcritic){
      cat("Ip nula respinsa, se accepta ipoteza alternativa")
    }
    else{
      cat("Nu am suficiente dovezi")
    }
  }
  else
    if(tip=='r'){
      tcritic=qt(1-alfa,n-1)
      if(tscore>tcritic){
        cat("Ip nula respinsa, se accepta ipoteza alternativa")
      }
      else{
        cat("Nu am suficiente dovezi")
      }
    }
  else
    if(tip=='s'){
      tcritic=qt(1-alfa/2,n-1)
      if(abs(tscore)>abs(tcritic)){
        cat("Ip nula respinsa, se accepta ipoteza alternativa")
      }
      else{
        cat("Nu am suficiente dovezi")
      }
    }
  
  return(c(tscore,tcritic))
}

e1('l',1/100,125,418,420,2.75)



e2=function(tip,n,mu0,sample_mean,alfa,sigma){
  #left - as la stg
  #right - as la drp
  #sim - simetrica
  
  zscore=(sample_mean-mu0)/(sigma/sqrt(n))
  
  if(tip=='left'){
    zcritic=qnorm(alfa,0,1)
    if(zscore<zcritic){
      cat("Ip nula respinsa, se accepta ipoteza alternativa", zscore, zcritic)
    }
    else{
      cat("Nu am suficiente dovezi",zscore,zcritic)
    }
  }
  else
    if(tip=="right"){
      zcritic=qnorm(1-alfa,0,1)
      if(zscore>zcritic){
        cat("Ip nula respinsa, se accepta ipoteza alternativa", zscore, zcritic)
      }
      else{
        cat("Nu am suficiente dovezi",zscore,zcritic)
      }
    }
  else
    if(tip=="sim"){
      zcritic=qnorm(1-alfa/2,0,1)
      if(abs(zscore)>abs(zcritic)){
        cat("Ip nula respinsa, se accepta ipoteza alternativa", zscore, zcritic)
      }
      else{
        cat("Nu am suficiente dovezi",zscore,zcritic)
      }
    }
  
  return(c(zscore,zcritic))
}
 e2('right',25,4.9,5.17,1/100,0.35)
 e2('right',25,4.9,5.17,5/100,0.35)

 
 e3=function(tip,alfa,sigma1,sigma2,n1,n2,sample_mean1,sample_mean2,m0){
   
   #left - as la stg
   #right - as la drp
   #sim - simetrica
   
   
   zscore=(sample_mean1-sample_mean2-m0)/sqrt(sigma1^2/n1+sigma2^2/n2)
   
   if(tip=='left'){
     zcritic=qnorm(alfa,0,1)
     if(zscore<zcritic){
       cat("Ip nula respinsa, se accepta ipoteza alternativa")
     }
     else{
       cat("Nu am suficiente dovezi")
     }
   }
   else
     if(tip=="right"){
       zcritic=qnorm(1-alfa,0,1)
       if(zscore>zcritic){
         cat("Ip nula respinsa, se accepta ipoteza alternativa")
       }
       else{
         cat("Nu am suficiente dovezi")
       }
     }
   else
     if(tip=="sim"){
       zcritic=qnorm(1-alfa/2,0,1)
       if(abs(zscore)>abs(zcritic)){
         cat("Ip nula respinsa, se accepta ipoteza alternativa")
       }
       else{
         cat("Nu am suficiente dovezi")
       }
     }
   
   return(c(zscore,zcritic))
 }
 
 e3('sim',1/100,1.31,0.93,25,28,5.48,6.12,0)
 e3('left',1/100,1.31,0.93,25,28,5.48,6.12,0)
 
 
 
 e4=function(tip,alfa,n1,n2,sigma1,sigma2){
   
   # r -> asimetrica la dreapta
   # s -> simetrica
   
   fscore=sigma1^2/sigma2^2
   
   if(tip=='r'){
     fcritic=qf(1-alfa,n1-1,n2-1)
     if(fscore>fcritic){
       cat("Se accepta ipoteza alternativa")
     }
     else{
       cat("Nu am dovezi")
     }
   }
   else
     if(tip=='s'){
       fscritic=qf(alfa/2,n1-1,n2-1)
       fdcritic=qf(1-alfa/2,n1-1,n2-1)
       if(fscore<fscritic | fscore>fdcritic){
         cat("Se accepta ipoteza alternativa")
       }
       else{
         cat("Nu am dovezi")
       }
     }
 }
 e4('r',1/100,25,28,1.24,0.87)
 
 
  