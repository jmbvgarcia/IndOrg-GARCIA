

OLS <- function(X,Y){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  
  n = length(Y)
  k = dim(X)[2]
  
  beta <- solve(t(X) %*% X, t(X) %*% Y)
  
  fitted <- X %*% beta
  residual <- Y - fitted
  sigma <- sum(residual^2) / (n - k)
  var <- sigma * solve(t(X) %*% X)
  se <- sqrt(diag(var))
  
  results <- data.frame(beta = c(beta), se = se)
  
  return(results)
}


IV <- function(X,Y,Z){
  
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  Z <- as.matrix(Z)
  
  n = length(Y)
  k = dim(X)[2]
  
  beta_fs <- solve(t(Z) %*% Z) %*% t(Z) %*% X
  
  X_dot <- Z %*% beta_fs
  
  beta <- solve(t(X_dot) %*% X) %*% t(X_dot) %*% Y
  
  fitted <- X %*% beta
  residuals <- Y - fitted
  sigma <- sum(residuals^2) / (n - k)
  
  var <- sigma * solve(t(X_dot) %*% X_dot) 
  
  se <- sqrt(diag(var))
  
  results <- list(beta = beta,
                  se = se, 
                  beta_fs = beta_fs)
  return(results)
}

dummyGen <- function(df, x, label = "d_"){
  d <- tibble(.rows = length(df[[x]]))
  for(level in unique(df[[x]])){
    d[[str_c(label, level)]] <- if_else(df[[x]] == level,1,0) 
  }
  df <- bind_cols(df,d)
  return(df)
}


GMM <- function(X,Y,Z,W = "efficient"){
  X<-as.matrix(X)
  Z<-as.matrix(Z)
  Y<-as.matrix(Y)
  if (is_character(W) && W == "2SLS") {
    W <- solve(t(Z)%*%Z)
  }
  if (is_character(W) && W == "efficient") {
    beta <- IV(X,Y,Z)$beta
    e_hat <- Y - X %*% beta
    g <-  Z * (e_hat %*% rep(1,ncol(Z))) 
    gi <- g - colMeans(g)
    W <- solve(t(gi) %*% gi)
  }
  else{
    W<-as.matrix(W)
  }
  
  criterion <- function(beta,X,Y,Z,W){
    crit <- t((t(Z)%*%(Y-X%*%beta))) %*% W %*% (t(Z)%*%(Y-X%*%beta))
  }
  init <- rep(0,ncol(X))
  res <- optim(init, criterion, X=X, Y=Y, W=W, Z=Z)
  if (res$convergence == 0) {
    return(res$par)
  }
  if (res$convergence != 0) {
    return("No convergence")
  }
}

ComputeElasticities <- function(beta, alpha, price){
  P <- exp(alpha - beta*price)/sum(exp(alpha - beta*price))
  own_elast <- -beta*price*(1-exp(alpha - beta*price)/sum(exp(alpha - beta*price)))
  cross_elast <- beta*price*(exp(alpha - beta*price)/sum(exp(alpha - beta*price)))
  return(list(own = own_elast,
              cross = cross_elast))
}


FindDeltas <- function(S,prices, mu,sigma){
  
  nodes <- c(-sqrt(3), 0 , sqrt(3))
  weights <- c(1/6,2/3,1/6)
  
  delta_new <- matrix(0, ncol = ncol(S), nrow = nrow(S))
  difference <- Inf
  count <- 0
  while (difference > 1e-20 & count < 1000) {
    delta_old <- delta_new
    
    shares <- matrix(0, ncol = ncol(S), nrow = nrow(S))
    for (i in 1:length(weights)) {
      beta <- exp(mu + sigma*nodes[i])
      prob <- exp(delta_old - beta*prices)
      
      # Need a safety here to garantee that we are not dividing by zero if beta is too large
      # if that is the case, just put all probabilities to 1/J
      if(all(near(as.numeric(prob), rep(0, length(prob)) ))){
        prob <- matrix(ncol(S)^{-1},ncol = ncol(S), nrow = nrow(S))
      }
      else{
        prob <- prob/rowSums(prob, na.rm = T)
      }
      shares <- shares + prob*weights[i] 
    }
    
    delta_new <- delta_old + (S - shares)
    delta_new <- delta_new - delta_new[,1]
    difference <- sum(abs(delta_new - delta_old), na.rm = T)
    count=count+1
  }
  delta <- as.numeric(delta_new[,-1])
  return(delta)
}


getCriterion<- function(S, prices, X, Z, mu, sigma){
  deltas <- FindDeltas(S,prices, mu = mu, sigma = sigma)
  if (sigma<0) {
    return(Inf)
  }
  alphas <- GMM(X,deltas,Z, W = "2SLS")
  W <- solve(t(Z)%*%Z)
  e <- deltas - X %*% alphas
  crit <- t((t(Z)%*%(e))) %*% W %*% (t(Z)%*%(e))
  return(crit)
}

BLP <- function(S, prices, X, Z, init){
  f <- function(args){
    mu <- args[1]
    sigma <- args[2]
    crit <- getCriterion(S,prices,X,Z,mu,sigma)
    return(crit)
  }
  results<-optim(init,f)
  deltas<- FindDeltas(S,prices, results$par[1], results$par[2])
  alphas <- GMM(X,deltas,Z)
  par <- c(mu = results$par[1], 
           sigma = results$par[2], 
           alpha_heinz = alphas[1], 
           alpha_hunts = alphas[2], 
           alpha_dm = alphas[3])
  return(list(par = par, 
              deltas = deltas, 
              criterion = results$value))
}

ComputeElasticitiesBLP <- function(mu, sigma, alpha, meanprices){
  meanprices <- as.numeric(meanprices)
  nodes <- c(-sqrt(3), 0 , sqrt(3))
  weights <- c(1/6,2/3,1/6)
  
  E <- matrix(0, ncol = 4, nrow = 4)
  shares <- c(0,0,0,0)
  for (i in 1:length(weights)) {
    beta <- exp(mu + sigma*nodes[i])
    prob <- exp(c(0,alphas) - beta*meanprices)
    prob <- prob/sum(prob, na.rm = T)
    
    shares <- shares + prob * weights[i]
    
    for (j in 1:4) {
      E[j,j] <- E[j,j] - weights[i] * prob[j]*(1-prob[j]) * beta
      for (k in 1:4) {
        if (k!=j) {
          E[j,k] <- E[j,k] + weights[i] * prob[j]*prob[k] * beta
        }
      }
    }
  }
  E <- E / (shares %*% matrix(1, ncol = 4))
  E <- E * t(meanprices %*% matrix(1, ncol = 4))
  return(E)
}



BLPConstrained <- function(S, prices, X, Z, init){
  f <- function(args){
    mu <- args
    sigma <- 0
    crit <- getCriterion(S,prices,X,Z,mu,sigma)
    return(crit)
  }
  results<-optim(init,f, method = "Brent", lower = -5, upper = 10)
  deltas<- FindDeltas(S,prices, results$par[1], 0)
  alphas <- GMM(X,deltas,Z, W = "2SLS")
  return(c(exp(results$par),alphas))
}


SimulateShares <- function(splits, prices, mu, sigma, alphas){
  
  nodes <- c(-sqrt(3), 0 , sqrt(3))
  weights <- c(1/6,2/3,1/6)
  
  xi <- splits %>% 
    as.data.frame %>% 
    mutate(sb = 0) %>% 
    select(sb, everything()) %>% 
    as.matrix
  
  shares <- matrix(0, ncol = ncol(xi), nrow = nrow(xi))
  
  for (i in 1:length(weights)) {
    beta <- exp(mu + sigma*nodes[i])
    prob <- exp(matrix(1,nrow = nrow(xi)) %*% c(0,alphas) - beta*prices + xi)
    prob <- prob/rowSums(prob)
    shares <- shares + prob*weights[i]
  }
  return(shares)
}
