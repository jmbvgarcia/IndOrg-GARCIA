library(tidyverse)


setwd("C:/Users/joaom/Desktop/IO/Assignment4")
data <- read_csv("static_games_clean.csv")

source("functions.R")

# a)
res1<- Probit(data, c(25,15,.3))$par


# c)
res2<- Probit2(data, c(25,25,15,.3))$par

# e)
data <- data %>% mutate(N1 = if_else(N>0,1,0),
                        N2 = if_else(N>1,1,0))

phi_tilde <- KReg(data$S,data$N, 6)

p <- tibble(p=(11:30)/100, s1 =0, s2 = 0) 
for (i in 1:20) {
  p$s1[i] = ngrid[max(which(phi_tilde[1,]<p$p[i]))]
  p$s2[i] = ngrid[max(which(phi_tilde[2,]<p$p[i]))]
}
p<-p%>% mutate(v2 = s1/s2)
v2 <- mean(p$v2)