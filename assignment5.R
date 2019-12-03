library(tidyverse)
library(haven)
setwd("C:/Users/joaom/Desktop/IO/Assignment5")
source("functions.r")

#load data
data <- read_dta("Replication_File_JPE/stata/comprehensive.dta") %>%
  filter(industry == 1) %>%
  mutate(Y = c274a1y - c274b1y - c274e1y,
         y = log(Y),
         X = c281a1y,
         x = log(X),
         cons = 1) %>%
  filter(!is.na(x+y) & x+y>-Inf) %>%
  select(Y,X,y,x,cons,country) %>%
  add_count(country) %>%
  filter(n>=50)

#2a
res <- list()
ci <- list()
for (c in unique(data$country)) {
  
  datac <- data %>% filter(country == c)
  
  x <- datac %>% select(x,cons)
  y <- datac %>% select(y)
  
  res[[c]] <- OLS(x,y) %>% round(2)
  ci[[c]] <- getCI(res[[c]]) %>% round(2)
  
}

#2b
data2b <- data %>%
  mutate(beta = X/Y,
         A = Y/(X^beta))  %>% 
  group_by(country) %>% 
  summarise(beta = max(beta, na.rm = T))

#2c
beta <- data %>%
  filter(country == 'SriLanka2004') %>%
  summarise(beta = mean(X/Y) * 0.025) %>%
  as.numeric()

#2d
data2d <- data %>% 
  mutate(A = Y/(X^beta))

Y <- list()
EY <- list()
for (c in unique(data$country)) {
  d <- data2d %>% filter(country == c)
  X <- d %>% select(X)
  A <- d %>% select(A)
  Y[c] <- d %>% summarize(sum(Y)) %>% as.numeric()
  EY[c] <- EfficientOutput(X, beta, A)
}


Y2 <- list()
EY2 <- list()
for (c in unique(data$country)) {
  d <- data2d %>% 
    filter(country == c) %>%
    filter(A != max(A)) %>% 
    filter(A != max(A)) %>% 
    filter(A != max(A))
  
  X <- d %>% select(X)
  A <- d %>% select(A)
  Y2[c] <- d %>% summarize(sum(Y)) %>% as.numeric()
  EY2[c] <- EfficientOutput(X, beta, A)
}