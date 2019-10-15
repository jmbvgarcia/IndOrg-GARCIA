library(tidyverse)
library(nleqslv)
rm(list=ls())

setwd("C:/Users/joaom/Desktop/IO/Assignment2")
data <- read_csv("clean_data.csv")

prices <- data %>% select(CTL, DEL, HEINZ, HUNT) %>% as.matrix()
choices <- data %>% select(choice_ctl, 
                           choice_del, 
                           choice_hei, 
                           choice_hun) %>% as.matrix()
N <- length(data$ID)

#a)
LL1 <- function(b) {
  U <- exp(-b*prices)
  U[is.na(U)] <- 0
  L <- (U/rowSums(U))^choices
  return(sum(log(L)))
}

(beta <- optimize(LL1, lower = -20, upper = 20, maximum = T)$maximum)

#b)
LL2 <- function(x) {
  alpha <- c(0,x[1:3])
  b <- x[4]
  A<- matrix(1,nrow = N) %*% alpha
  U <- exp(A-b*prices)
  U[is.na(U)] <- 0
  L <- rowSums(choices*U)/rowSums(U)
  return(-sum(log(L)))
}

(pars <- optim(c(0,0,0,0), LL2))

#c)
(pars$par[2]/pars$par[1])

#d)
beta <- pars$par[4]
alpha <- c(0,pars$par[1:3])

A<- matrix(1,nrow = length(data$ID)) %*% alpha
U <- exp(A-beta*prices)

Vitj0 <- mean(log(rowSums(U, na.rm = T)))
prices_no_del <- prices
prices_no_del[,2] <- NA
Vitj1 <- mean(log(rowSums(exp(A - beta*prices_no_del), na.rm = T)))

(Surplus <- (1/beta)*(Vitj1-Vitj0))

#e)

P <- U/rowSums(U, na.rm = T)

meandemand <- colSums(choices)/N
meanprices <- colMeans(prices, na.rm = T)

(Elast_at_average <- -beta*(1 - meandemand)*meanprices)

#f)

(c <- colMeans(prices, na.rm = T)*(1 + 1/Elast_at_average))

#h)

gamesolve <- function(p,c, alpha, beta){
  beta*(1-exp(alpha-beta*p)/sum(exp(alpha-beta*p)))*(p - c) - 1
}

(p_exit <- nleqslv(meanprices[-2], gamesolve, beta = beta, c = c[-2], alpha = alpha[-2])$x)

Vitj0 <- log(sum(exp(alpha - beta*meanprices)))
Vitj1 <- log(sum(exp(alpha[-2] - beta*p_exit)))

(Surplus2 <- (1/beta)*(Vitj1-Vitj0))




#############################################
# Exercise 2
############################################

#b)
rm(list=ls())

setwd("C:/Users/joaom/Desktop/IO/Assignment2")
data <- read_csv("clean_data.csv") %>%
  arrange(ID)

quad <- read_csv("quadrature_k3.csv")
W <- quad[["w"]]

nodes <- quad[,1:3] %>% as.matrix()

LL <- function(args, df){
  beta <- args[1]
  mu <- args[2:4]
  sigma <- args[5:7]
  if (any(sigma<0)) {
    return(Inf)
  }
  choices <- df %>%  
    select(starts_with("choice_")) %>% 
    as.matrix()
  prices <- df %>%
    select(starts_with("price_")) %>% 
    as.matrix()
  
  
  likelihoods <- matrix(0,nrow = nrow(df), ncol = length(W)) %>% 
    as.tibble() %>% bind_cols(ID = df$ID)
  for (i in 1:length(W)) {
    alphas <- c(0,nodes[i,]*sigma + mu)
    A <- matrix(1,ncol = 1,nrow = nrow(choices)) %*% alphas
    P <- exp(A - beta*prices)
    P <- (P/rowSums(P,na.rm = T))^choices
    P <- exp(rowSums(log(P)))
    likelihoods[,i] <-P
  } 
  likelihoods <- likelihoods %>% 
    group_by(ID) %>% 
    summarise_all(prod) 
  likelihoods[,-1] <- likelihoods[,-1] * (matrix(1,ncol = 1,nrow = nrow(likelihoods)) %*% W) 
  likelihoods$LLi <- rowSums(likelihoods[,-1])
  likelihoods$LLi <- if_else(likelihoods$LLi <= 0, 10^(-30), likelihoods$LLi)
  LL <- likelihoods %>% pull(LLi)
  return(-sum(log(LL)))
}



#solve for a subsample and use result as initial conditial for full sample
#initial value is solution for last model, with std errors = 1

subsample0 <- data %>% group_by(ID) %>% nest() %>% sample_frac(0.05) %>% unnest()
start <- optim(c(6.87,0.85,3.76,0.94,1,1,1), df = subsample0, LL, method = "BFGS")$par

#generate several different initial conditions around this point and solve for each
init <- list()
for (i in 1:20) {
  init[[i]] <- start * exp(runif(n=7,min = log(0.5), max = log(2)))
}

res <- map(init, safely(optim), df = data, LL, method = "BFGS")


# now select the parameters from the iteration with the lowest value

res <- res %>% transpose() %>% .$result %>% discard(is.null) %>% transpose()
LL_values <- as.double(res[["value"]])
parameters <- res[["par"]] %>% as.data.frame()
result <- parameters[,which(LL_values == min(LL_values))]


# Some functions to make graphs around the maximum for diagnosis
LL_parameter_test <- function(x,pos){
  if (pos==1) {
    return(LL(c(x,result[2:7]),data))
  }
  else if (pos==7) {
    return(LL(c(result[1:6],x),data))
  }
  else if (1<pos & pos<7) {
    return(LL(c(result[1:pos-1],x,result[pos+1:7]),data))
  }
  else {
    return("error: index out of bounds")
  }
}

grid <- seq(from = 0.9, to = 1.1, length =11)

graph_around_max <- function(n){
  (grid*result[n]) %>% 
    as.list() %>% 
    map_dbl(LL_parameter_test, pos = n) %>% 
    tibble(likelihood = ., parameter = grid) %>%
    ggplot() + geom_path(aes(x=parameter,y=likelihood))
}

graphs <- list(1,2,3,4,5,6,7) %>% map(graph_around_max)


#d)

quad <- read_csv("quadrature_k3.csv")
W <- quad[["w"]]

nodes <- quad[,1:3] %>% as.matrix()
mu <- result[2:4]
beta <- result[1]
sigma <- result[5:7]
prob <- 0
for (i in 1:length(W)) {
  alphas <- c(0,nodes[i,]*sigma + mu)
  P <- exp(alphas - beta*meanprices)
  P <- P/sum(P)
  prob <-prob+P*W[i]
}

E <- -beta*meanprices*(1-prob)
