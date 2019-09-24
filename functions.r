

OLS <- function(X,Y,C=data.frame()){
  
  kx <- dim(X)[2]
  
  X <- as.matrix(bind_cols(X,C))
  Y <- as.matrix(Y)
  
  
  if (dim(Y)[2] != 1) {
    return("Error: Y must be a single variable")
  }
  
  n = length(Y)
  k = dim(X)[2]
  
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  fitted <- X %*% beta
  residual <- Y - fitted
  sigma <- sum(residual^2) / (n - k)
  var <- sigma * solve(t(X) %*% X)
  se <- sqrt(diag(var))
  
  results <- data.frame(beta = c(beta), se = se)[1:kx,]
  
  return(results)
}


IV <- function(X,Z,Y,C=data.frame()){
  
  X <- as.matrix(bind_cols(X,C))
  Y <- as.matrix(Y)
  Z <- as.matrix(bind_cols(Z,C))
  
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
                  fitted = as.numeric(fitted), 
                  residuals = as.numeric(residuals),
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

getCI <- function(reg){
  c(lower_bound = as.numeric(reg[["beta"]][1] - 1.98*reg[["se"]][1]),
    point_estimate = as.numeric(reg[["beta"]][1]),
    upper_bound = as.numeric(reg[["beta"]][1] + 1.98*reg[["se"]][1]))
}

computeArea <- function(start,end,fun,precision = 10000){
  step_grid <- (end-start)*(1:precision)/precision
  rectangle_area <- fun(start + step_grid)*(end-start)/precision
  area <- sum(rectangle_area)
  return(area)
}
  
