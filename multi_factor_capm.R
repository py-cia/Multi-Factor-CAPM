# rm(list = ls())
library(tidyverse)
library(quantmod)
# importing data
library(lubridate)
capm <- read.csv("C:/Users/valen/OneDrive/Desktop/capm.csv")
# capital asset pricing model: excess asset returns = alpha + Beta*excess return on the market + idiosyncratic risk(diversifiable risk)
# excess returns are equal to asset returns - risk free rate
# the variable "riskfree" is already a monthly rate
# data contains stock prices, riskfree rate, tbill rate, size factor, value factor, market returns, and date
# returns will be calculated as percentage change increase pt-pt-1/pt-1

# make date column
df <- separate(capm, col = dates, into = c("year", "month"), sep = "m", convert = TRUE) %>%
  mutate(date = make_datetime(year, month))
  
df2 <- df %>%
  select(-c(year, month)) %>%
  select(all_of(2:7), 10)

# get percentage change and drop first observation
pct_chg <- map(df2, pluck) %>% 
  map(~(.x - lag(.x))/lag(.x)) %>%
  map_dfc(~.x * 1) %>%
  drop_na()

# merge riskfree column and rename
pct_chg <-  cbind(pct_chg, capm[-1, 8], df[-1, 14]) %>% 
  rename("riskfree" = "capm[-1, 8]", "date" = "df[-1, 14]")

# clean dataset
View(pct_chg)

# sp500 returns
ggplot(data = pct_chg, mapping = aes(x = date, y = sp500)) +
  geom_line() +
  geom_hline(yintercept = 0)

# Microsoft
ggplot(data = pct_chg, mapping = aes(x = date, y = msoft)) +
  geom_line() +
  geom_hline(yintercept = 0)

# Gold
ggplot(data = pct_chg, mapping = aes(x = date, y = gold)) +
  geom_line() +
  geom_hline(yintercept = 0)


excss_rt <- pct_chg[ ,all_of(1:7)] - pct_chg$riskfree

capm_coef <- excss_rt %>%
  map(pluck) %>%
  map(~lm(.x ~ sp500, data = excss_rt)) %>% 
  map(summary) %>% 
  map(coefficients)

print(capm_coef)

# The beta risk for exxon and walmart suggest they are conservative stocks. They move with the market but the movements are weaker. 
# general electric, ibm, and microsoft are aggresive stocks. On average they move in excess of the market. The beta risk of one by sp500
# indicates this is a benchmark stock.
# Gold is a imperfect hedge, having a beta of -.09. Gold tends to move in the opposite direction of the market

###########################
                          #
# Working with real data  # 
                          #                         
###########################

getSymbols("NVDA", src = "yahoo")
getSymbols("ORCL", src = "yahoo")
getSymbols("META", src = "yahoo")

# Get monthly returns
nvda_monthly <- to.monthly(NVDA) %>% monthlyReturn()

orcl_monthly <- to.monthly(ORCL) %>% monthlyReturn()

meta_monthly <- to.monthly(META) %>% monthlyReturn()

# Last 10 years of Data
nvda_monthly_rtn <- nvda_monthly[c(-1:-72, -194, -195), ]
names(nvda_monthly_rtn)[1] <- "nvda_rtn"

orcl_monthly_rtn <- orcl_monthly[c(-1:-72, -194, -195), ]
names(orcl_monthly_rtn)[1] <- "orcl_rtn"

meta_monthly_rtn <- meta_monthly[c(-1:-8, -130, -131), ]
names(meta_monthly_rtn)[1] <- "meta_rtn"

# Import fama-frech 3 factor model: 
fama_french <- read.csv("C:/Users/valen/Downloads/fama_french.csv", na.strings="999") %>% 
  separate(col = X, into = c("year", "month"), sep = 4, convert = TRUE) %>% 
  mutate(date = make_datetime(year, month)) %>%
  select(3, 4, 5, 6) # was going to re-index with date but didnt really need to here

# cosolidate into one data frame
stock_rtn <- cbind(nvda_monthly_rtn, orcl_monthly_rtn, meta_monthly_rtn) %>% data.frame() %>% cbind(fama_french)

# calculate excess returns for assets
stock_rtn$nvda_rtn <- stock_rtn$nvda_rtn - stock_rtn$RF
stock_rtn$orcl_rtn <- stock_rtn$orcl_rtn - stock_rtn$RF
stock_rtn$meta_rtn <- stock_rtn$meta_rtn - stock_rtn$RF

# can now apply lm
# applying one factor
capm_nvda <- lm(nvda_rtn ~ Mkt.RF, data = stock_rtn)
capm_nvda %>% summary()

capm_orcl <- lm(orcl_rtn ~ Mkt.RF, data = stock_rtn)
capm_orcl %>% summary()

capm_meta <- lm(meta_rtn ~ Mkt.RF, data = stock_rtn)
capm_meta %>% summary()

# the beta risk is significant at the 5% level for all 3 stocks, showing that the excess return on the market
# helps determine excess asset returns. 
# I expected the Beta risk to be greater than 1 since tech stocks are more aggressive on average but 
# I beleive this is due to the difference in market benchmarks. I think if i calculate the beta risk for
# conservative stocks using the market bench mark in the fama-french data set they would have a lower beta risk than the ones
# depicted here for tech.

# multi-factor capm
multi_capm_nvda <- lm(nvda_rtn ~ Mkt.RF + SMB + HML, data = stock_rtn)
summary(multi_capm_nvda)

multi_capm_orcl <- lm(orcl_rtn ~ Mkt.RF + SMB + HML, data = stock_rtn)
summary(multi_capm_orcl)

multi_capm_meta <- lm(meta_rtn ~ Mkt.RF + SMB + HML, data = stock_rtn)
summary(multi_capm_meta)

# the performance of small stocks relative to big stocks and the performance of value stocks
# relative to growth stocks have no effect on the excess return of the assets above.
















