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
