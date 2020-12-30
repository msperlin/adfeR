#' # Importing Data from the Internet {#importing-
#' 

#' 
#' It can be said that one of the great advantages
#' 
#' In this chapter, I will describe and give examp
#' 
#' `GetQuandlData` [@GetQuandlData]
#' :  Imports economical and financial data from t
#' 
#' `BatchGetSymbols` [@BatchGetSymbols]
#' : Imports adjusted and unadjusted stock price d
#' 
#' `simfinR` [@simfinR]
#' : Imports financial statements and adjusted sto
#' 
#' `tidyquant` [@tidyquant]
#' : Imports several financial information about s
#' 
#' `Rbitcoin` [@Rbitcoin] 
#' : Imports data for cryptocurrencies.
#' 
#' 
#' ## Package `GetQuandlData` {#quandl}
#' 
#' _Quandl_ is an established and comprehensive pl
#' 
#' In R, package `Quandl` [@quandl] is the officia
#' 
#' The **first and mandatory** step in using `GetQ
#' 

#' 
## ---- eval = FALSE-------------------------------------------------------------------------------------------
## # set FAKE api key to quandl
## my_api_key <- 'Asv8Ac7zuZzJSCGxynfG'

#' 
#' This API key is unique to each user, and the on
#' 
#' Now, with the API key and the Quandl symbol, we
#' 
## ------------------------------------------------------------------------------------------------------------
library(GetQuandlData)
library(tidyverse)

# set symbol and dates
my_symbol <- c('Gold Prices in EURO' = 'WGC/GOLD_DAILY_USD')
first_date <- as.Date('1980-01-01')
last_date <- as.Date('2019-01-01')

# get data!
df_gold <- get_Quandl_series(id_in = my_symbol,
                             api_key = my_api_key, 
                             first_date = first_date,
                             last_date = last_date)

# check it
glimpse(df_gold)

#' 
#' Notice how we set the name of the time series i
#' 
#' Worth knowing that other `Quandl` API options a
#' 
#' As an inspection check, let's plot the prices o
#' 
## ------------------------------------------------------------------------------------------------------------
library(ggplot2)

# plot prices with ggplot2
p <- ggplot(df_gold, aes(x = ref_date, y = value)) + 
  geom_line() + 
  labs(y = 'Prices (USD)', 
       x = '',
       title = 'Prices of Gold',
       subtitle = paste0(first_date, ' to ', last_date),
       caption = 'Data from Quandl') + 
   theme_bw()

# print it
print(p)

#' 
#' Overall, gold prices were fairly stable between
#' 
## ------------------------------------------------------------------------------------------------------------
# sort the rows
df_gold <- df_gold %>%
  arrange(ref_date)

total_ret <- last(df_gold$value)/first(df_gold$value) - 1
total_years <- as.numeric(max(df_gold$ref_date) - 
                          min(df_gold$ref_date) )/365

comp_ret_per_year <- (1 + total_ret)^(1/total_years) - 1

print(comp_ret_per_year)

#' 
## ---- include=FALSE------------------------------------------------------------------------------------------
# set symbol and dates
my_symbol <- c('US Inflation' = 'RATEINF/INFLATION_USA')
first_date <- as.Date('1980-01-01')
last_date <- as.Date('2019-01-01')

# get data!
df_inflation <- get_Quandl_series(id_in = my_symbol,
                                  api_key = my_api_key,
                                  first_date = first_date,
                                  last_date = last_date)

# check it
df_inflation <- df_inflation %>%
  arrange(ref_date) %>%
  mutate(infl_by_month = (1+value/100)^(1/12) - 1,
         idx_infl = cumprod(1+infl_by_month))

total_infl_ret <- last(df_inflation$idx_infl)/first(df_inflation$idx_infl) - 1
total_infl_years <- as.numeric(max(df_inflation$ref_date) - min(df_inflation$ref_date) )/365

inflation_comp_ret <- (1 + total_infl_ret)^(1/total_infl_years) - 1

#' 
#' We find the result that Gold prices in USD comp
#' 
#' 
#' #### Fetching many time series
#' 
#' When asking for multiple time series from Quand
#' 
## ------------------------------------------------------------------------------------------------------------
library(GetQuandlData)
library(tidyverse)

# databse to get info
db_id <- 'RATEINF'

# get info 
df_db <- get_database_info(db_id, my_api_key)

glimpse(df_db)

#' 
#' Column `name` contains the description of table
#' 
## ------------------------------------------------------------------------------------------------------------
print(unique(df_db$name))

#' 
#' What we want is the `'Inflation YOY - *'` datas
#' 
## ------------------------------------------------------------------------------------------------------------
selected_series <- c('Inflation YOY - USA',
                     'Inflation YOY - Canada',
                     'Inflation YOY - Euro Area',
                     'Inflation YOY - Australia')

# filter selected countries
idx <- df_db$name %in% selected_series
df_db <- df_db[idx, ]

#' 
#' Now we grab the data using `get_Quandl_series`:
#' 
## ------------------------------------------------------------------------------------------------------------
my_id <- df_db$quandl_code
names(my_id) <- df_db$name
first_date <- '2010-01-01'
last_date <- Sys.Date()

df_inflation <- get_Quandl_series(id_in = my_id, 
                                  api_key = my_api_key,
                                  first_date = first_date,
                                  last_date = last_date)

glimpse(df_inflation)

#' 
#' And, finally, we create an elegant plot to see 
#' 
## ------------------------------------------------------------------------------------------------------------
p <- ggplot(df_inflation, aes(x = ref_date, y = value/100)) + 
  geom_col() + 
  labs(y = 'Inflation YOY (%)', 
       x = '',
       title = 'Inflation in the World',
       subtitle = paste0(first_date, ' to ', last_date),
       caption = 'Data from Quandl') + 
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~series_name) + 
  theme_bw()

print(p)

#' 
#' As you can see, the `GetQuandlData` output is f
#' 
#' 
#' ## Package `BatchGetSymbols`
#' 
#' Package `BatchGetSymbols` [@BatchGetSymbols] is
#' 
#' **Cleanliness and organization**: All financial
#' 
#' **Control of import errors**: All download erro
#' 
#' **Comparison of dates to a benchmark**: Individ
#' 
#' **Caching system**: By default, all imported da
#' 
#' **Access to _tickers_ in market indices**: The 
#' 
#' **Use of multiple cores**:  If the user is down
#' 
#' **Flexible output format**: The package also of
#' 
#' As an example of usage, let's download the pric
#' 
#' In the call to function `BatchGetSymbols`, we s
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
library(BatchGetSymbols)

# set tickers
tickers <- c('MSFT','GOOGL','JPM','GE')

# set dates
first_date <- Sys.Date()-5*365
last_date <- Sys.Date()
thresh_bad_data <- 0.95   # sets percent threshold for bad data
bench_ticker <- '^GSPC'   # set benchmark as ibovespa
cache_folder <- 'data/BGS_Cache' # set folder for cache

l_out <- BatchGetSymbols(tickers = tickers,
                         first.date = first_date,
                         last.date = last_date,
                         bench.ticker = bench_ticker,
                         thresh.bad.data = thresh_bad_data,
                         cache.folder = cache_folder)


#' 
#' The output of `BatchGetSymbols` is an object of
#' 
#' Back to our example, object `l_out` has two ele
#' 
## ------------------------------------------------------------------------------------------------------------
# print result of download process
print(l_out$df.control)

#' 
#' Column `threshold.decision` from `df.control` s
#' 
#' As for the actual financial data, it is contain
#' 
## ------------------------------------------------------------------------------------------------------------
# print df.tickers
glimpse(l_out$df.tickers)

#' 
#' As expected, we find information about stock pr
#' 
#' To inspect the data, let's look at its prices w
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
library(ggplot2)

p <- ggplot(l_out$df.tickers, aes(x = ref.date, 
                                  y = price.adjusted)) + 
  geom_line() + facet_wrap(~ticker, scales = 'free_y') + 
  scale_y_continuous(labels = format.cash) + 
  labs(x = '', 
       y = 'Stock Adjusted Prices',
       title = 'Prices of four stocks',
       caption = 'Data from Yahoo Finance') + 
  theme_bw()

print(p)

#' 
#' 
#' We see that General Eletric (GE) stock was not 
#' 
#' Now, let's look at an example of a large batch 
#' 
## ---- cache=TRUE, message=FALSE, eval=FALSE------------------------------------------------------------------
## library(BatchGetSymbols)
## 
## # set tickers
## df_SP500 <- GetSP500Stocks()
## sp500_tickers <- df_SP500$Tickers
## 
## # set dates
## first_date <- '2010-01-01'
## last_date <- '2019-01-01'
## thresh_bad_data <- 0.95   # sets percent threshold for bad data
## bench_ticker <- '^GSPC'   # set benchmark as ibovespa
## cache_folder <- 'data/BGS_Cache' # set folder for cache
## 
## # set number of cores (half of available cores)
## future::plan(future::multisession,
##              workers = floor(parallel::detectCores()/2))
## 
## l_out <- BatchGetSymbols(tickers = sp500_tickers,
##                          first.date = first_date,
##                          last.date = last_date,
##                          bench.ticker = bench_ticker,
##                          thresh.bad.data = thresh_bad_data,
##                          cache.folder = cache_folder,
##                          do.parallel = TRUE)

#' 

#' 
#' And now we check the resulting data:
#' 
## ------------------------------------------------------------------------------------------------------------
glimpse(l_out$df.tickers)

#' 
#' We get a table with `r nrow(l_out$df.tickers)` 
#' 
#' 
#' ## Package `simfinR`
#' 
#' [SimFin](https://simfin.com/)^[https://simfin.c
#' 
#' > Our core goal is to make financial data as fr
#'     
#' The platform is free, with a daily limit of 200
#' 
#' Package `simfinR` facilitates importing data fr
#' 
#' The release version of the package `simfinR` is
#' 
#' 
#' ### Example 01 - Apple Inc Annual Profit
#' 
#' The first step in using `simfinR` is registerin
#' 

#' 
## ---- eval = FALSE-------------------------------------------------------------------------------------------
## my_api_key <- 'rluwSlN304NpyJeBjlxZPspfBBhfJR4o'

#' 
#' You need to be aware that the **API key in `my_
#' 
#' With the API key in hand, the second step is to
#' 
## ---- cache=TRUE---------------------------------------------------------------------------------------------
library(simfinR)
library(tidyverse)

# get info
df_info_companies <- simfinR_get_available_companies(my_api_key)

# check it
glimpse(df_info_companies)

#' 
#' Digging deeper into the `dataframe`, we find th
#' 
## ---- cache=TRUE---------------------------------------------------------------------------------------------
id_companies <- 111052 # id of APPLE INC
type_statements <- 'pl' # profit/loss
periods = 'FY' # final year
years = 2009:2018

df_fin_FY <- simfinR_get_fin_statements(
  id_companies,
  type_statements = type_statements,
  periods = periods,
  year = years,
  api_key = my_api_key)

glimpse(df_fin_FY)

#' 
#' And now we plot the results of the Net Income (
#' 
## ------------------------------------------------------------------------------------------------------------
net_income <- df_fin_FY %>% 
              dplyr::filter(acc_name == 'Net Income')

p <- ggplot(net_income,
            aes(x = ref_date, y = acc_value)) +
  geom_col()  + 
  labs(title = 'Yearly Profit of APPLE INC',
       x = '',
       y = 'Yearly Profit',
       subtitle = '',
       caption = 'Data from simfin <https://simfin.com/>') + 
  theme_bw()

print(p)

#' 
#' Not bad! Apple has been doing very well over th
#' 
## ---- cache=TRUE---------------------------------------------------------------------------------------------
type_statements <- 'pl' # profit/loss
periods = c('Q1', 'Q2', 'Q3', 'Q4') # final year
years = 2009:2018

df_fin_quarters <- simfinR_get_fin_statements(
  id_companies,
  type_statements = type_statements,
  periods = periods,
  year = years,
  api_key = my_api_key)

glimpse(df_fin_quarters)

#' 
#' And plot the results:
#' 
## ------------------------------------------------------------------------------------------------------------
net_income <- df_fin_quarters %>% 
              filter(acc_name == 'Net Income')

p <- ggplot(net_income,
            aes(x = period, y = acc_value)) +
  geom_col() + facet_grid(~year, scales = 'free') + 
  labs(title = 'Quarterly Profit of APPLE INC',
       x = 'Quarters',
       y = 'Net Profit',
       caption = 'Data from simfin') + 
  theme_bw()

print(p)

#' 
#' Nice and impressive profit record. The first qu
#' 
#' 
#' ### Example 02 - Quarterly Net Profit of Many C
#' 
#' Package `simfinR` can also fetch information fo
#' 
## ---- cache=TRUE---------------------------------------------------------------------------------------------
set.seed(5)
my_ids <- sample(df_info_companies$simId, 4)
type_statements <- 'pl' # profit/loss
periods = 'FY' # final year
years = 2010:2018

df_fin <- simfinR_get_fin_statements(
  id_companies = my_ids,
  type_statements = type_statements,
  periods = periods,
  year = years,
  api_key = my_api_key)

net_income <- df_fin %>% 
              filter(acc_name == 'Net Income')

p <- ggplot(net_income,
            aes(x = ref_date, y = acc_value)) +
  geom_col() + 
  labs(title = 'Annual Profit/Loss of Four Companies',
       x = '',
       y = 'Net Profit/Loss',
       caption = 'Data from simfin') + 
  facet_wrap(~company_name, scales = 'free_y') + 
  theme_bw()

print(p)

#' 
#' 
#' ### Example 03: Fetching price data
#' 
#' The simfin project also provides prices of stoc
#' 
## ---- cache=TRUE---------------------------------------------------------------------------------------------
set.seed(5)
my_ids <- sample(df_info_companies$simId, 4)
type_statements <- 'pl' # profit/loss
periods = 'FY' # final year
years = 2009:2018

df_price <- simfinR_get_price_data(id_companies = my_ids,
                                   api_key = my_api_key)

p <- ggplot(df_price,
            aes(x = ref_date, y = close_adj)) +
  geom_line() + 
  labs(title = 'Adjusted stock prices for four companies',
       x = '',
       y = 'Adjusted Stock Prices',
       caption = 'Price data from simfin') + 
  facet_wrap(~company_name, scales = 'free_y') + 
  theme_bw()

print(p)

#' 
#' As you can see, the data is comprehensive and s
#' 
#' 
#' ## Package `tidyquant`
#' 
#' Package `tidyquant` [@tidyquant] provides funct
#' 
#' The package includes functions for obtaining fi
#' 
#' In its current version, `tidyquant` has `r leng
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyquant)

# set stock and dates
ticker <- 'AAPL'
first_date <- '2019-01-01'
last_date <-  Sys.Date()

# get data with tq_get
df_prices <- tq_get(ticker,
                    get = "stock.prices", 
                    from = first_date, 
                    to = last_date)

glimpse(df_prices)

#' 
#' As we can see, except for column names, the pri
#' 
#' One interesting aspect of `tidyquant` is the sa
#' 
## ------------------------------------------------------------------------------------------------------------
# get stocks in NYSE
df_nyse <- tq_exchange("NYSE")

glimpse(df_nyse)

#' 
#' We find `r nrow(df_nyse)` stocks for `r length(
#' 
#' We can also get information about components of
#' 
## ------------------------------------------------------------------------------------------------------------
# print available indices
print(tq_index_options())

#' 
#' Let's get information for `"DOWGLOBAL"`.
#' 
## ------------------------------------------------------------------------------------------------------------
# get components of "DOWJONES"
print(tq_index("DOWGLOBAL"))

#' 
#' We only looked into a few functions from the pa
#' 
#' 
#' ## Package `Rbitcoin`
#' 
#' Given the lasting popularity of crypto-currenci
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
library(Rbitcoin)

# set mkt, currency pair and type of action
mkt <- "kraken"
currency <- c("BTC","EUR")
action <- 'trades'

# import data
my_l <- market.api.process(market = mkt,
                           currency_pair = currency,
                           action = action)

# print it
print(my_l)

#' 
#' The output of `market.api.process` is a `list` 
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
glimpse(my_l$trades)

#' 
#' It includes price and time information for the 
#' 
#' 
#' ## Other Packages
#' 
#' In CRAN, you'll find many more packages for imp
#' 
#' 
#' ## Accessing Data from Web Pages (_webscraping_
#' 
#' Packages from the previous section make it easy
#' 
#' The process of extracting information from web 
#' 
#' 
#' ### Scraping the Components of the SP500 Index 
#' 
#' In its website, Wikipedia offers a [section](ht
#' 
## ----SP500-wikipedia, echo = FALSE, out.width = '75%', fig.cap = 'Mirror of Wikipedia page on SP500 components'----
knitr::include_graphics('figs/SP500-Wikipedia.png')

#' 
#' The information on this web page is constantly 
#' 
#' The first step in webscraping is finding out th
#' 
## ----SP500-Wikipedia-webscraping, echo = FALSE, out.width = '75%', fig.cap = 'Finding xpath from website'----
knitr::include_graphics('figs/SP500-Wikipedia_webscraping.png')

#' 
#' Here, the copied _xpath_ is:
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## '//*[@id="mw-content-text"]/table[1]/thead/tr/th[2]'

#' 
#' This is the address of the header of the table.
#' 
#' Now that we have the location of what we want, 
#' 
## ---- tidy=FALSE, cache=TRUE---------------------------------------------------------------------------------
library(rvest)

# set url and xpath
my_url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
my_xpath <- '//*[@id="mw-content-text"]/div/table[1]'

# get nodes from html
out_nodes <- html_nodes(read_html(my_url),
                        xpath = my_xpath)

# get table from nodes (each element in 
# list is a table)
df_SP500_comp <- html_table(out_nodes)

# isolate it 
df_SP500_comp <- df_SP500_comp[[1]]

# change column names (remove space)
names(df_SP500_comp) <- make.names(names(df_SP500_comp))

# print it
glimpse(df_SP500_comp)

#' 
#' Object `df_SP500_comp` contains a mirror of the
#' 
#' 
#' ### Scraping the Website of the Reserve Bank of
#' 
#' As another example of webscraping with R, let’s
#' 
## ----RBA-website, echo = FALSE, out.width = '75%', fig.cap = 'Website for the Reserve Bank of Australia'-----
knitr::include_graphics('figs/website_RBA-webscrapping.png')

#' 
#' The website presents several information such a
#' 
#' The first step of _webscrapping_ is finding out
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## xpath_inflation <- '//*[@id="content"]/section[1]/div/div[2]/p'
## xpath_int_rate <- '//*[@id="content"]/section[1]/div/div[1]/p'

#' 
#' A difference from the previous example is we ar
#' 
## ---- cache = TRUE-------------------------------------------------------------------------------------------
library(rvest)

# set address of RBA
my_url <- 'https://www.rba.gov.au/'

# read html
html_code <- read_html(my_url)

# set xpaths
xpath_inflation <- '//*[@id="content"]/section[1]/div/div[2]/p'
xpath_int_rate <- '//*[@id="content"]/section[1]/div/div[1]/p'

# get inflation from html
my_inflation <- html_text(html_nodes(html_code,
                                     xpath = xpath_inflation ))

# get interest rate from html
my_int_rate <- html_text(html_nodes(x = html_code,
                                    xpath = xpath_int_rate ))

#' 
#' And now we print the result:
#' 

#' 
## ------------------------------------------------------------------------------------------------------------
# print result
cat("\nCurrent inflation in AUS:", my_inflation)
cat("\nCurrent interest rate AUS:", my_int_rate)

#' 
#' Using _webscraping_ techniques can become a str
#' 
#' Another problem is that the webscrapping code d
#' 
#' Readers interested in this topic should study t
#' 
#' 
#' ## Exercises 
#' 
#' 01. Using the `BatchGetSymbols` package, downlo
#' 
#' 02. Use `tidyquant::tq_get` to download histori
#' 
#' 03. Use the [Quandl search box](https://www.qua
#' 
#' 04. Create a profile on the [Quandl website](ht
#' 
#' 05. What is the latest value of EUR Bitcoin at 