
likelihood  <- function(df, mu, sigma, V){
  if (V>1) {
    return(-Inf)
  }
  L <- data %>% 
    transmute(L = case_when(N==0 ~ 1-pnorm((S-mu)/sigma),
                            N==1 ~ pnorm((S-mu)/sigma) - pnorm((S*V-mu)/sigma),
                            N==2 ~ pnorm((S*V-mu)/sigma))) 
  return(sum(log(L)))
}

Probit <- function(df, init){
  f <- function(arg){
    return(-likelihood(data,
                       mu=arg[1],
                       sigma = arg[2], 
                       V = arg[3]))
  }
  r <- optim(init,f)
  return(r)
}



likelihood2  <- function(df, mu1, mu2, sigma, V){
  if (V>1) {
    return(-Inf)
  }
  L <- data %>% 
    transmute(L = case_when(N==0 ~ 1-pnorm((S-mu1)/sigma),
                            N==1 ~ pnorm((S-mu1)/sigma) - pnorm((S*V-mu2)/sigma),
                            N==2 ~ pnorm((S*V-mu2)/sigma))) 
  return(sum(log(L)))
}

Probit2 <- function(df, init){
  f <- function(arg){
    return(-likelihood2(data,
                       mu1=arg[1],
                       mu2=arg[2],
                       sigma = arg[3], 
                       V = arg[4]))
  }
  r <- optim(init,f)
  return(r)
}


KReg <- function(X,Y, sd){
  xgrid <- seq(from = 0, to = max(X), length.out = 100)
  ygrid <- 1:2
  Fxy <- matrix(vector(mode = "double", length = length(xgrid)*length(ygrid)), nrow = length(ygrid))
  
  for (y in 1:2) {
    for (x in 1:length(xgrid)) {
      denom <- sum(dnorm(X - xgrid[x], sd = sd))
      num <- sum(dnorm(X-xgrid[x], sd = sd)*if_else(Y>=y,1,0))
      Fxy[y,x] <- num/denom
    }
  }
  return(Fxy)
}

