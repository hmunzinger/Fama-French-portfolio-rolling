## Loading package pacman and use its function p_load for loading all required packages
library(pacman)
p_load(tidyverse, tidyquant, timetk, broom, glue, tibbletime, highcharter, gt, gtsummary, formattable,
       htmlwidgets, webshot, modelr, estimatr)

## SPY = S%P 500, MCHI = iShares MSCI China ETF, EWY = iShares MSCI South Korea ETF, EWJ = iShares MSCI Japan ETF, 
## INDA = iShares MSCI India ETF
symbols <- c("SPY", "MCHI", "EWY", "EWJ", "INDA")

## Collect Data
prices <- getSymbols(symbols, src = 'yahoo', from = "2012-02-03", to = "2024-06-03",
    auto.assign = TRUE, warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

## Check imported data
head(prices)

## Calculate monthly lag returns of individual assets
monthly_asset_prices <- prices %>%  
  to.monthly(indexAt = "firstof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date")

head(monthly_asset_prices)

log_asset_returns  <- monthly_asset_prices %>% 
                      mutate(SPY_returns = (log(monthly_asset_prices$SPY) - log(lag(monthly_asset_prices$SPY))),
                             MCHI_returns = (log(monthly_asset_prices$MCHI) - log(lag(monthly_asset_prices$MCHI))),
                             EWY_returns = (log(monthly_asset_prices$EWY) - log(lag(monthly_asset_prices$EWY))),
                             EWJ_returns = (log(monthly_asset_prices$EWJ) - log(lag(monthly_asset_prices$EWJ))),
                             INDA_returns = (log(monthly_asset_prices$INDA) - log(lag(monthly_asset_prices$INDA)))) %>% 
                     na.omit() %>% 
                     select(date, SPY_returns:INDA_returns)
             
## Check on object created     
head(log_asset_returns)                  

## Plot asset log return of MCHI
MCHI_log_returns <- log_asset_returns %>% 
  ggplot(aes(x = date, y = MCHI_returns)) +
  geom_line() +
  labs(title = "MCHI log returns") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90))

## Plot the graph
MCHI_log_returns

## Allocate weights to individual assets for the portfolio
w <- c(0.25, 0.25, 0.20, 0.20, 0.1)

## Calculate monthly log returns of individual assets grouped by assets
asset_returns_long <- prices %>%  
  to.monthly(indexAt = "firstof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>% 
  gather(asset, returns, -date) %>% 
  group_by(asset) %>% 
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

## Check on calculated monthly asset returns
head(asset_returns_long)

## Calculate rebalanced monthly portfolio returns, using weights from above
portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
    returns_col = returns,
    weights = w,
    col_rename = "returns",
    rebalance_on = "months")

## Check on calculated rebalanced monthly portfolio returns
head(portfolio_returns_tq_rebalanced_monthly)

## Download Fama French Global 3 Factors csv file
## https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip

## Set working directory to folder where the csv file is saved
setwd()

## Import Global 3 Factors csv file
Global_3_Factors <- read.csv("F-F_Research_Data_Factors.csv", skip = 3) %>%
                    rename (date = X1) %>% 
                    mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>% 
                    mutate_at(vars(-date), as.numeric)
                  
## Check on object created
head(Global_3_Factors)
map_chr(Global_3_Factors, class)

## Merge portfolio returns and Global 3 Factors 
ff_portfolio_returns <- 
  portfolio_returns_tq_rebalanced_monthly %>% 
  left_join(Global_3_Factors, by = "date") %>% 
  na.omit()
  
## Turn variables from percent to decimal
ff_portfolio_returns <- ff_portfolio_returns %>%   
  mutate(Mkt.RF = ff_portfolio_returns$Mkt.RF/100,
         SMB = ff_portfolio_returns$SMB/100,
         HML = ff_portfolio_returns$HML/100,
         RF =  ff_portfolio_returns$RF/100,
         R_excess = round(returns - RF, 4))

## Check merged new object and adjust to decimal values into one object
head(ff_portfolio_returns)

## Set length for calculating the rolling linear regression models
window <- 24

## Set up the function for calculating the portfolio rolling linear regression models
rolling_lm_robust <-
  rollify(.f = function(R_excess, Mkt.RF, SMB, HML) {
    lm_robust(R_excess ~ Mkt.RF + SMB + HML)
  }, window = window, unlist = FALSE)

## Calculate rolling linear regression models and group by factors
rolling_ff_betas <- ff_portfolio_returns %>% 
    mutate(rolling_ff = rolling_lm_robust(R_excess, Mkt.RF, SMB, HML)) %>% 
    mutate(tidied = map(rolling_ff, tidy, conf.int = TRUE)) %>% 
    unnest(tidied) %>% 
    slice(-1:-23) %>% 
    select(date, term, estimate, conf.low, conf.high) %>% 
    filter(term != "(Intercept)") %>% 
    rename( beta = estimate, factor = term) %>% 
    group_by(factor)

## Check calculate rolling regression models
head(rolling_ff_betas)

## Extract date, rsquared, adj rsquared and p.values from regression results
rolling_ff_rsquared <- ff_portfolio_returns %>% 
  mutate(rolling_ff = rolling_lm_robust(R_excess, Mkt.RF, SMB, HML)) %>% 
  slice(-1:-23) %>% 
  mutate(glanced = map(rolling_ff, glance)) %>% 
  unnest(glanced) %>% 
  select(date, r.squared, adj.r.squared, p.value)

## Check on regression results
head(rolling_ff_rsquared)  

## Plot of 24-Month Rolling FF Factor Betas from regression results
Month_24_Rolling_FF_Factor_Betas <- rolling_ff_betas %>% 
  ggplot(aes(x = date, y = beta, color = factor)) +
  geom_line() +
  labs(title = "24-Month Rolling FF Factor Betas", x  = "rolling betas") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(angle = 90))

## Plot the graph
Month_24_Rolling_FF_Factor_Betas

## Save plot as pdf file
ggsave("Month_24_Rolling_FF_Factor_Betas.pdf")

## Turn data frame into xts object
rolling_ff_rsquared_xts <- rolling_ff_rsquared %>% 
  tk_xts(date_var = date, silent = TRUE)

## Plot of Rolling FF 3-Factor Adj. R-Squared from regression results using highcharter
Rolling_FF_3_Factor_Adj_R_Squared1 <- highchart(type = "stock") %>% 
  hc_add_series(rolling_ff_rsquared_xts$adj.r.squared,
    color = "cornflowerblue",
    name = " adj. r-squared") %>% 
  hc_title(text = "Rolling FF 3-Factor Adj. R-Squared") %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = TRUE)

## Save the graph
htmlwidgets::saveWidget(widget = Rolling_FF_3_Factor_Adj_R_Squared1, 
  file = "~/Rolling_FF_3_Factor_Adj_R_Squared1.html")
webshot::webshot(url = "Rolling_FF_3_Factor_Adj_R_Squared1.html",
  file = "Rolling_FF_3_Factor_Adj_R_Squared1.png")

## More precise plot of Rolling FF 3-Factor Adj. R-Squared from regression results using highcharter
Rolling_FF_3_Factor_Adj_R_Squared2 <- highchart(type = "stock") %>%
  hc_add_series(rolling_ff_rsquared_xts$adj.r.squared,
    color = "cornflowerblue",
    name = "Adj. r-squared") %>%
  hc_title(text = "Rolling FF 3-Factor Adj. R-Squared") %>%
  hc_yAxis( max = 1.5, min = 0) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE)

## Save the graph
htmlwidgets::saveWidget(widget = Rolling_FF_3_Factor_Adj_R_Squared2, 
                        file = "~/Rolling_FF_3_Factor_Adj_R_Squared2.html")
webshot::webshot(url = "Rolling_FF_3_Factor_Adj_R_Squared2.html",
                 file = "Rolling_FF_3_Factor_Adj_R_Squared2.png")

## Individual assets regress on Global 3 Factors
combined_returns <- 
  log_asset_returns %>% 
  left_join(Global_3_Factors, by = "date") %>% 
  na.omit()

## Check on object created
head(combined_returns)

## Calculate decimal values
combined_returns <- combined_returns %>% 
  mutate(Mkt.RF = Mkt.RF/100,
  SMB = SMB/100,
  HML = HML/100,
  RF = RF/100,
  SPY_excess = round(SPY_returns - RF, 4),
  MCHI_excess = round(MCHI_returns - RF, 4),
  EWY_excess = round(EWY_returns - RF, 4),
  EWJ_excess = round(EWJ_returns - RF, 4),
  INDA_excess = round(INDA_returns - RF, 4) %>% 
  na.omit())
    
## Delete asset returns which are not required for linear regression models
combined_returns <- combined_returns %>% 
  select(-SPY_returns:-INDA_returns, -RF)

## Check on object created
head(combined_returns)

## Rolling Linear Regressions for individual ETF's

## Set window length
window <- 24

## Set up the function for calculating the rolling linear regression models
rolling_lm_robust_MCHI <-
  rollify(.f = function(MCHI_excess, Mkt.RF, SMB, HML) {
    lm_robust(MCHI_excess ~ Mkt.RF + SMB + HML)
  }, window = window, unlist = FALSE)

##MCHI
rolling_MCHI_ff_betas <- combined_returns %>% 
  mutate(rolling_MCHI_ff = rolling_lm_robust_MCHI(MCHI_excess, Mkt.RF, SMB, HML)) %>% 
  mutate(tidied = map(rolling_MCHI_ff, broom::tidy, conf.int = TRUE)) %>% 
  unnest(tidied) %>% 
  slice(-1:-23) %>% 
  select(date, term, estimate, conf.low, conf.high) %>% 
  filter(term != "(Intercept)") %>% 
  rename( beta = estimate, factor = term) %>% 
  group_by(factor)

## Check on object created
head(rolling_MCHI_ff_betas)

## Set up the function for calculating the portfolio rolling linear regression models
rolling_lm_robust_EWY <-
  rollify(.f = function(EWY_excess, Mkt.RF, SMB, HML) {
    lm_robust(EWY_excess ~ Mkt.RF + SMB + HML)
  }, window = window, unlist = FALSE)

## EWY
rolling_EWY_ff_betas <- combined_returns %>% 
  mutate(rolling_EWY_ff = rolling_lm_robust_EWY(EWY_excess, Mkt.RF, SMB, HML)) %>% 
  mutate(tidied = map(rolling_EWY_ff, tidy, conf.int = TRUE)) %>% 
  unnest(tidied) %>% 
  slice(-1:-23) %>% 
  select(date, term, estimate, conf.low, conf.high) %>% 
  filter(term != "(Intercept)") %>% 
  rename( beta = estimate, factor = term) %>% 
  group_by(factor)

## Check on object created
head(rolling_EWY_ff_betas)

## Set up the function for calculating the portfolio rolling linear regression models
rolling_lm_robust_EWJ <-
  rollify(.f = function(EWJ_excess, Mkt.RF, SMB, HML) {
    lm_robust(EWJ_excess ~ Mkt.RF + SMB + HML)
  }, window = window, unlist = FALSE)

## EWJ
rolling_EWJ_ff_betas <- combined_returns %>% 
  mutate(rolling_EWJ_ff = rolling_lm_robust_EWJ(EWJ_excess, Mkt.RF, SMB, HML)) %>% 
  mutate(tidied = map(rolling_EWJ_ff, tidy, conf.int = TRUE)) %>% 
  unnest(tidied) %>% 
  slice(-1:-23) %>% 
  select(date, term, estimate, conf.low, conf.high) %>% 
  filter(term != "(Intercept)") %>% 
  rename( beta = estimate, factor = term) %>% 
  group_by(factor)

## Check on object created
head(rolling_EWJ_ff_betas)

## Set up the function for calculating the portfolio rolling linear regression models
rolling_lm_robust_INDA <-
  rollify(.f = function(INDA_excess, Mkt.RF, SMB, HML) {
    lm_robust(INDA_excess ~ Mkt.RF + SMB + HML)
  }, window = window, unlist = FALSE)

## INDA
rolling_INDA_ff_betas <- combined_returns %>% 
  mutate(rolling_INDA_ff = rolling_lm_robust_INDA(INDA_excess, Mkt.RF, SMB, HML)) %>% 
  mutate(tidied = map(rolling_INDA_ff, tidy, conf.int = TRUE)) %>% 
  unnest(tidied) %>% 
  slice(-1:-23) %>% 
  select(date, term, estimate, conf.low, conf.high) %>% 
  filter(term != "(Intercept)") %>% 
  rename( beta = estimate, factor = term) %>% 
  group_by(factor)

## Check on object created
head(rolling_INDA_ff_betas)

## Plot of graphs of individual rolling regression results

## MCHI
Month_24_MCHI_Rolling_FF_Factor_Betas <- rolling_MCHI_ff_betas %>% 
  ggplot(aes(x = date, y = beta, color = factor)) +
  geom_line() +
  labs(title = "24-Month MCHI Rolling FF Factor Betas", x  = "rolling MCHI betas") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90))

## Plot the graph
Month_24_MCHI_Rolling_FF_Factor_Betas

## Save plot as pdf file
ggsave("Month_24_MCHI_Rolling_FF_Factor_Betas.pdf")

## EWY
Month_24_EWY_Rolling_FF_Factor_Betas <- rolling_EWY_ff_betas %>% 
  ggplot(aes(x = date, y = beta, color = factor)) +
  geom_line() +
  labs(title = "24-Month EWY Rolling FF Factor Betas", x  = "rolling EWY betas") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90))

## Plot the graph
Month_24_EWY_Rolling_FF_Factor_Betas

## Save plot as pdf file
ggsave("Month_24_EWY_Rolling_FF_Factor_Betas.pdf")

## EWJ
Month_24_EWJ_Rolling_FF_Factor_Betas <- rolling_EWJ_ff_betas %>% 
  ggplot(aes(x = date, y = beta, color = factor)) +
  geom_line() +
  labs(title = "24-Month EWJ Rolling FF Factor Betas", x  = "rolling EWJ betas") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90))

## Save plot as pdf file
ggsave("Month_24_EWJ_Rolling_FF_Factor_Betas.pdf")

## INDA
Month_24_INDA_Rolling_FF_Factor_Betas <- rolling_INDA_ff_betas %>% 
  ggplot(aes(x = date, y = beta, color = factor)) +
  geom_line() +
  labs(title = "24-Month INDA Rolling FF Factor Betas", x  = "rolling INDA betas") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90))

## Save plot as pdf file
ggsave("Month_24_INDA_Rolling_FF_Factor_Betas.pdf")

## MCHI CAPM robust linear regression
MCHI_lm_robust <- lm_robust(MCHI_excess ~ Mkt.RF, data = combined_returns)

## Summary of regression result
tbl_regression(MCHI_lm_robust)

## MCHI Plot of regression
MCHI_lm <- combined_returns %>% 
ggplot(aes(Mkt.RF, MCHI_excess)) +
  geom_point() +
  stat_smooth(method = "lm_robust") +
  theme_bw()

## Plot graph
MCHI_lm

## Save plot as pdffile
ggsave("MCHI_lm.png")

## EWY CAPM robust linear regression
EWY_lm_robust <- lm_robust(EWY_excess ~ Mkt.RF, data = combined_returns)

## Summary of regression result
tbl_regression(EWY_lm_robust)

## Plot of regression
EWY_lm <- combined_returns %>% 
ggplot(aes(Mkt.RF, EWY_excess)) +
  geom_point() +
  stat_smooth(method = "lm_robust") +
  theme_bw()

## Plot graph
EWY_lm

## Save plot as pdffile
ggsave("EWY_lm.png")

## EWJ CAPM robust linear regression
EWJ_lm_robust <- lm_robust(EWJ_excess ~ Mkt.RF, data = combined_returns)

## Summary of regression result
tbl_regression(EWJ_lm_robust)

## Plot of regression
EWJ_lm <- combined_returns %>% 
  ggplot(aes(Mkt.RF, EWJ_excess)) +
  geom_point() +
  stat_smooth(method = "lm_robust") +
  theme_bw()

## Plot graph
EWJ_lm

## Save plot as pdffile
ggsave("EWJ_lm.png")

## INDA CAPM robust linear regression
INDA_lm_robust <- lm_robust(INDA_excess ~ Mkt.RF, data = combined_returns)

## Summary of regression result
tbl_regression(INDA_lm_robust)

## Plot of regression
INDA_lm <- combined_returns %>% 
  ggplot(aes(Mkt.RF, INDA_excess)) +
  geom_point() +
  stat_smooth(method = "lm_robust") +
  theme_bw()

## Plot graph
INDA_lm

## Save plot as pdffile
ggsave("INDA_lm.png")
