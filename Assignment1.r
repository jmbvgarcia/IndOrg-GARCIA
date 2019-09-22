
library(tidyverse)
library(haven)
rm(list=ls())

setwd("C:/Users/joaom/Desktop/IO/Assignment1")

source("functions.r") #load self written functions
source("dataclean.r")

## a)

Cs <- sample %>% select(S_Alabama:S_Wyoming, time_trend, time_trend2, time_trend3, cons)
Xs <- sample %>% select(ln_price_supply)
Ys <- sample %>% select(ln_quantity)
Zs <- sample %>% select(road_mileage, railpop, urbanization, autos_capita)

supply_equation <- IV(Xs,Zs,Ys,Cs)
getCI(supply_equation)

## b)
Cd <- sample %>% select(S_Alabama:S_Wyoming, time_trend, time_trend2, time_trend3, cons)
Xd <- sample %>% select(ln_price_demand)
Yd <- sample %>% select(ln_quantity)
Zd <- sample %>% select(ln_oil)

demand_equation <- IV(Xd,Zd,Yd,Cd)
getCI(demand_equation)

## c and d)
sample <- tibble(residuals_supply = supply_equation[["residuals"]],
                 residuals_demand = demand_equation[["residuals"]]) %>%
  bind_cols(sample)

sample <- sample %>% group_by(year) %>% mutate(tau_s = supply_equation$beta[49]*time_trend  +
                                                 supply_equation$beta[50]*time_trend2 +
                                                 supply_equation$beta[51]*time_trend3,
                                               us_population = sum(pop_state_adj)) %>%
  ungroup()

supply_shocks <- sample %>% group_by(year) %>% 
  summarise(cum_shocks = sum((residuals_supply + tau_s)*pop_state_adj/us_population)) %>% 
  mutate(shock = cum_shocks - lag(cum_shocks))

supply_shocks %>% filter(!is.na(shock)) %>% ggplot() + geom_line(aes(y=shock,x=year))


## e)
sample <- sample %>% mutate(tau_d = demand_equation$beta[49]*time_trend  +
                              demand_equation$beta[50]*time_trend2 +
                              demand_equation$beta[51]*time_trend3)

demand_shocks <- sample %>% group_by(year) %>% 
  summarise(cum_shocks = sum((residuals_demand - tau_d)*pop_state_adj/us_population)) %>% 
  mutate(shock = cum_shocks -lag(cum_shocks)) %>% ungroup()

demand_shocks %>% filter(!is.na(shock)) %>% ggplot() + geom_line(aes(y=shock,x=year))

## f)
RI2008 <- sample %>% filter(state == "Rhode Island" & year == 2008)

quantity_old <- RI2008  %>% select(quantity) %>% as.numeric()
price_old_demand <- RI2008  %>% select(price_demand) %>% as.numeric()
price_old_supply<- RI2008  %>% select(price_supply) %>% as.numeric()

error_s <- RI2008 %>% select(residuals_supply) %>% as.numeric()
error_d <- RI2008 %>% select(residuals_demand) %>% as.numeric()

alpha_s <- supply_equation$beta[length(supply_equation$beta)] + error_s
beta_s <- supply_equation$beta[1]
alpha_d <- demand_equation$beta[length(demand_equation$beta)] + error_d
beta_d <- demand_equation$beta[1]

price_new <-  exp((alpha_d-alpha_s)/(beta_s-beta_d))

quantity_new <- exp(alpha_s) * price_new^beta_s

data.frame(name = c("price with taxes", "price net of taxes", "quantity with taxes", 
                    "price under tax holiday", "quantity under tax holiday"),
           value = c(price_old_demand, price_old_supply, quantity_old, 
                     price_new, quantity_new))


## g)
demand_function <- function(x){exp(alpha_d)*x^beta_d}
supply_function <- function(x){exp(alpha_s)*x^beta_s}

consumer_gains <- computeArea(price_new,price_old_demand,demand_function)
supplier_gains <- computeArea(price_old_supply,price_new,supply_function)
taxes_lost     <- (price_old_demand-price_old_supply)*quantity_old

data.frame(name = c("consumer gains", "supplier gains", "taxes lost", "total gains"),
           value = c(consumer_gains, supplier_gains, taxes_lost, 
                     consumer_gains + supplier_gains - taxes_lost))

## i)
sample %>% 
  filter(state == "Rhode Island") %>%
  ggplot() + geom_line(aes(x = year, y = unemployment))

sample %>% 
  filter(state == "Rhode Island") %>%
  mutate(unemployment = if_else(year>=1985 & year<=2000,100*unemployment,unemployment)) %>%
  ggplot() + geom_line(aes(x = year, y = unemployment))
