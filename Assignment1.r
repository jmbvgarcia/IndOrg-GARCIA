
library(tidyverse)
library(haven)
rm(list=ls())

setwd("C:/Users/joaom/Desktop/IO/Assignment1")
source("functions.r") #load self written functions
source("dataclean.r") #load and prepare data

# Gasoline

## a)

C1 <- sample %>% select(S_Alabama:S_Wyoming, time_trend, time_trend_sq, cons)
X1 <- sample %>% select(ln_price_supply)
Y1 <- sample %>% select(ln_quantity)
Z1 <- sample %>% select(road_mileage, railpop, urbanization)

supply_equation <- IV(X1,Z1,Y1,C1)
(supply_CI <- c(lower = as.numeric(supply_equation[["beta"]][1] - 1.98*supply_equation[["se"]][1]), 
                upper = as.numeric(supply_equation[["beta"]][1] + 1.98*supply_equation[["se"]][1])))

## b)
C2 <- sample %>% select(S_Alabama:S_Wyoming, time_trend, time_trend_sq, cons)
X2 <- sample %>% select(ln_price_demand)
Y2 <- sample %>% select(ln_quantity)
Z2 <- sample %>% select(ln_oil, ln_tax)

demand_equation <- IV(X2,Z2,Y2,C2)
(demand_CI <- c(lower = as.numeric(demand_equation[["beta"]][1] - 1.98*demand_equation[["se"]][1]), 
                upper = as.numeric(demand_equation[["beta"]][1] + 1.98*demand_equation[["se"]][1])))


## c)
sample <- sample %>% add_column(fitted_quantity_supplied = supply_equation[["fitted"]]) %>%
  mutate(residuals_supply = ln_quantity-fitted_quantity_supplied)

supply_shocks <- sample %>% group_by(year) %>% 
  summarise(cum_shocks = mean(residuals_supply)) %>% 
  mutate(shock = cum_shocks -lag(cum_shocks))

supply_shocks %>% filter(!is.na(shock)) %>% ggplot() + geom_line(aes(y=shock,x=year))

## e)
sample <- sample %>% add_column(fitted_quantity_demanded = demand_equation[["fitted"]]) %>%
  mutate(residuals_demand = ln_quantity-fitted_quantity_demanded)

demand_shocks <- sample %>% group_by(year) %>% 
  summarise(cum_shocks = mean(residuals_demand)) %>% 
  mutate(shock = cum_shocks -lag(cum_shocks))

demand_shocks %>% filter(!is.na(shock)) %>% ggplot() + geom_line(aes(y=shock,x=year))

## f)

RI2008 <- sample %>% filter(state == "Rhode Island" & year == 2008)

RI_pop <- RI2008  %>% select(apop_adj) %>% as.numeric()
quantity_old <- RI2008  %>% select(hug) %>% as.numeric()
price_old_demand <- RI2008  %>% select(taxin_gas_price) %>% as.numeric()
price_old_supply<- RI2008  %>% transmute(taxin_gas_price- gas_tax_all) %>% as.numeric()

epsilon_s <- RI2008 %>% select(residuals_supply) %>% as.numeric()
epsilon_d <- RI2008 %>% select(residuals_demand) %>% as.numeric()

alpha_s <- supply_equation$beta[length(supply_equation$beta)] + epsilon_s
beta_s <- supply_equation$beta[1]
alpha_d <- demand_equation$beta[length(demand_equation$beta)] + epsilon_d
beta_d <- demand_equation$beta[1]

price_new <-  exp((alpha_d -alpha_s)/(beta_s-beta_d))

quantity_new <- RI_pop * exp(alpha_s + beta_s*log(price_new))

c(price_with_taxes = price_old_demand, price_net_of_taxes = price_old_supply, quantity = quantity_old)
c(price_tax_holiday = price_new, quantity_tax_holiday = quantity_new)

## g)

c(consumer_gains = 0.5 * (price_old_demand - price_new)*(quantity_new-quantity_old),
  supplier_gains  = 0.5 * (price_new - price_old_supply)*(quantity_new-quantity_old),
  total_gains = 0.5 * (price_old_demand - price_old_supply)*(quantity_new-quantity_old))
