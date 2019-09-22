
sample <- read_dta("annual_regression_data.dta") %>% 
  filter(!is.na(hug + real_oilprice + taxin_gas_price)) %>%  #exclude missing values
  dummyGen("state", "S_") %>% 
  mutate(cons = 1, 
         quantity = hug,
         ln_quantity = log(quantity), #consumption in thousand gallons
         price_demand = taxin_gas_price/cpi87, # price in dollars per gallon
         ln_price_demand = log(price_demand),
         price_supply = (taxin_gas_price-gas_tax_all)/cpi87,
         ln_price_supply = log(price_supply), 
         ln_oil = log(real_oilprice),
         time_trend = year-2008, # to make 2008 the reference year
         time_trend2 = time_trend^2,
         time_trend3 = time_trend^3) %>%
  select(-`S_Rhode Island`) #to make RI the reference state

