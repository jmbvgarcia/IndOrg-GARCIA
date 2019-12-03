

OLS <- function(X,Y){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  n = length(Y)
  k = dim(X)[2]
  
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  fitted <- X %*% beta
  residual <- Y - fitted
  sigma <- sum(residual^2) / (n - k)
  var <- sigma * solve(t(X) %*% X)
  se <- sqrt(diag(var))
  
  results <- data.frame(beta = c(beta), se = se)
  
  return(results)
}

getCI <- function(reg){
  c(lower_bound = as.numeric(reg[["beta"]][1] - 1.98*reg[["se"]][1]),
    point_estimate = as.numeric(reg[["beta"]][1]),
    upper_bound = as.numeric(reg[["beta"]][1] + 1.98*reg[["se"]][1]))
}

EfficientOutput <- function(X, beta, A){
  totalInputs <- sum(X)
  
  excessInputs <- function(lambda){
    xs <- sum((lambda/(A*beta))^(1/(beta-1))) - totalInputs
    return(abs(xs))
  }
  
  lambda <- optimize(excessInputs, interval = c(0,10), tol = 1e-8)$minimum
  
  Xnew <- (lambda/(A*beta))^(1/(beta-1))
  output <- sum(A*Xnew^beta)
  return(output)
}
