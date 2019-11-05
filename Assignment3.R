library(tidyverse)
library(viridis)
library(rsample)

rm(list=ls())
source("functions.r")

setwd("C:/Users/joaom/Desktop/IO/Assignment3")

# Input data
datawide <- read_csv("ketchup_wide.csv") %>%
  mutate(p1 = price1-price0,
         p2 = price2-price0,
         p3 = price3-price0,
         po1 = price_othmkt1-price_othmkt0,
         po2 = price_othmkt2-price_othmkt0,
         po3 = price_othmkt3-price_othmkt0) %>%
  mutate_at(c("share1","share2","share3"),
            .funs = ~ log(./share0))

data <- read_csv("ketchup_long.csv") %>% 
  filter(brand != "store brand") %>% 
  select(-price_othmkt) %>%
  inner_join(datawide) %>%
  mutate(S = case_when(brand == "Heinz"  ~ share1,
                       brand == "Hunt's" ~ share2,
                       brand == "Del Monte" ~ share3),
         P = case_when(brand == "Heinz"  ~ -p1,
                       brand == "Hunt's" ~ -p2,
                       brand == "Del Monte" ~ -p3),
         P_other = case_when(brand == "Heinz"  ~ po1,
                             brand == "Hunt's" ~ po2,
                             brand == "Del Monte" ~ po3)) %>%
  dummyGen("brand", label = "brand_") 

# save mean prices for elasticities
meanprices = read_csv("ketchup_wide.csv") %>% 
  summarise(p0 = mean(price0), 
            p1 = mean(price1), 
            p2 = mean(price2), 
            p3 = mean(price3))

# Case 1
Y <- data %>% select(S) %>% as.matrix
X <- data %>% select(P, starts_with("brand_")) %>% as.matrix

# column 1
res1 <- GMM(X,Y,X, W = "2SLS")


Z <- data %>% select(price1,price2,price3, price0, starts_with("brand_")) %>% as.matrix
#column 2
res2 <- GMM(X,Y,Z, W = "2SLS")
#column 3
res3 <- GMM(X,Y,Z, W = "efficient")


elast <- ComputeElasticities(beta = res3[1], alpha = c(0,res3[-1]), price = meanprices)

# Case 2
Z <- data %>% select(P_other, starts_with("brand_")) %>% as.matrix

#column 1
res4 <- GMM(X,Y,Z, W = "2SLS")

Z<- data%>% select(starts_with("price_ot"), starts_with("brand_")) %>% as.matrix

#column 2
res5 <- GMM(X,Y,Z, W = "2SLS")
#column 3
res6 <- GMM(X,Y,Z, W = "efficient")

elast <- ComputeElasticities(beta = res6[1], alpha = c(0,res6[-1]), price = meanprices)


# Case 3
#Some more data inputing
datalong <- read_csv("ketchup_long.csv") %>% 
  dummyGen("brand", label = "brand_") %>% 
  select(-`brand_store brand`)
datawide <- read_csv("ketchup_wide.csv")
data <- datalong %>% inner_join(datawide)

S      <- datawide %>% 
  select(share0,share1,share2,share3) %>% as.matrix
prices <- datawide %>% 
  select(price0,price1,price2,price3)  %>% as.matrix

X <- datalong %>% 
  filter(brand != "store brand") %>% 
  select(brand_Heinz, `brand_Hunt's`, `brand_Del Monte` ) %>% as.matrix

Z<- data %>% filter(brand != "store brand") %>%
  select(price0, 
         price1, 
         price2, 
         price3, 
         brand_Heinz, 
         `brand_Hunt's`, 
         `brand_Del Monte`) %>% as.matrix

#Estimation
estim <- BLP(S, prices, X, Z, init = c(1,1))

mu <-estim[[1]][1]
sigma <- estim[[1]][2]
alphas <- estim[[1]][3:5]

elast<-ComputeElasticitiesBLP(mu, sigma, alphas, meanprices)

##################
#Checks and tests#
##################

#1. constrained estimation
estim_test <- BLPConstrained(S, prices, X, Z, init = 1)

#2. Graph
#set up grid
grid <- tibble(mu = seq(mu - 2,
                        mu + 2,
                        length = 41), 
               sigma = seq(sigma - 0.5,
                           sigma + 0.5,
                           length = 41)) %>%
  expand(mu, sigma) %>% mutate(value = NA_real_)

#compute criterion at grid
for (i in 1:nrow(grid)) {
  grid$value[i] <- getCriterion(S=S,
                                prices = prices, 
                                X=X,
                                Z=Z, 
                                mu = grid$mu[i], 
                                sigma = grid$sigma[i])
}

#plot
grid %>% ggplot() + 
  geom_tile(aes(x=mu, y = sigma, fill = log(value))) + 
  scale_fill_viridis() +
  annotate("point", x=mu, y = sigma, colour = "red")


#3. Bootstrap simulation

#recover xis
xis <- tibble(xi = estim[[2]]) %>% 
  mutate(brand = c(rep("heinz",123),rep("hunts",123),rep("delmonte",123)),
         week = c(seq(1,123),seq(1,123),seq(1,123)),
         xi = case_when(brand == "heinz"    ~ xi - alphas[1],
                        brand == "hunts"    ~ xi - alphas[2],
                        brand == "delmonte" ~ xi - alphas[3]))

#have a look at the distribution (not included in summary)
ggplot(xis) + geom_path(aes(x=week, y=xi, colour = brand)) + labs(title = "Xi")
xis <- xis %>% spread(key = brand, value = xi) %>% select(-week)


set.seed(1000)
boots <- bootstraps(xis, times=100)

boots <- boots %>% 
  mutate(shares = map(splits,
                      SimulateShares,
                      prices = prices, 
                      mu = mu, 
                      sigma = sigma, 
                      alphas = alphas),
         model = map(shares,
                     BLP,
                     prices = prices,
                     X = X,
                     Z = Z,
                     init = c(1.4,0.6)),
         par = map(model, "par"))

results <- boots$par %>% as.data.frame() %>% t() %>% as_tibble()

ggplot(results) + geom_density(aes(x=mu)) + 
  labs(title = "Mu") + geom_vline(xintercept = mu)
ggplot(results) + geom_density(aes(x=sigma)) + 
  labs(title = "Sigma") + geom_vline(xintercept = sigma)
ggplot(results) + geom_density(aes(x=alpha_heinz)) +
  labs(title = "Alpha - Heinz") + geom_vline(xintercept = alphas[1])
ggplot(results) + geom_density(aes(x=alpha_hunts)) +
  labs(title = "Alpha - Hunt's") + geom_vline(xintercept = alphas[2])
ggplot(results) + geom_density(aes(x=alpha_dm)) + 
  labs(title = "Alpha - Del Monte") + geom_vline(xintercept = alphas[3])



