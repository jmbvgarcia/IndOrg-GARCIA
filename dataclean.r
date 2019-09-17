
sample <- read_dta("annual_regression_data.dta") %>% 
  filter(!is.na(hug + real_oilprice + taxin_gas_price)) %>%  #exclude missing values
  dummyGen("year", "Y_") %>%
  dummyGen("state", "S_") %>% 
  mutate(cons = 1, 
         ln_quantity = log(hug/apop_adj),
         ln_price_demand = log(taxin_gas_price),
         ln_price_supply = log((taxin_gas_price-gas_tax_all)),
         ln_oil = log(real_oilprice),
         ln_tax = log(gas_tax_all),
         time_trend = year-2008,
         time_trend_sq = time_trend^2) %>%
  select(-`S_Rhode Island`)
