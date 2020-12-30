#' # Creating and Saving Figures with `ggplot2` {#
#' 
## ---- include=FALSE------------------------------------------------------------------------------------------
source('Scripts/preamble_chapters.R')

#' 
## ---- include=FALSE------------------------------------------------------------------------------------------
my_fig_height <- 6
my_fig_width <- 7

knitr::opts_chunk$set(fig.align='center',
                      prompt = FALSE, 
                      warning=FALSE, 
                      tidy=FALSE)

#' 
#' It is a well-known fact that communication is o
#' 
#' The easiness of which you can create and presen
#' 
#' Visual attractiveness
#' : Always facilitate the analysis of your audien
#' 
#' A figure must be self-contained
#' : Any technical information such as origin and 
#' 
#' The graphical analysis should help your analysi
#' : Just because you can make a plot, doesn't mea
#' 
#' Check your references
#' : Science and data analysis evolves as building
#' 
#' Previous guidelines will help you create impact
#' 
#' 
#' ## The `ggplot2` Package
#' 
#' R has built-in functions for creating figures, 
#' 
#' This deficiency was remedied by users. In 2005,
#' 
#' In this book, we will not go deep into `ggplot2
#' 
#' For most examples given here, we will work with
#' 
#' First, let's load the data and check its conten
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# set file and load data
my_f  <- afedR::afedR_get_data_file('SP500-Stocks-WithRet.rds')
df_sp500 <- read_rds(my_f )

# print first 5 rows
glimpse(df_sp500)

#' 
#' It is a fairly common table used in previous ch
#' 
#' 
#' ## Using Graphics Windows
#' 
#' Before studying the use of `ggplot2`, we need t
#' 
#' A more intelligent approach to managing figures
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## x11()
## plot(1:10)

#' 
#' The visual result in RStudio should be similar 
#' 

#' 
#' Each call to `x11()` will create a new empty wi
#' 
#' After creating so many windows, it is best to c
#' 
#' 
#' ## Creating Figures with Function `qplot`
#' 
#' Package `ggplot2` has an introductory function,
#' 
#' To build a time series plot with the prices of 
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width---------------------------------------------
library(ggplot2)

# filter stock data
temp_df <- df_sp500 %>%
  filter(ticker == 'MMM')

# plot its prices
qplot(data = temp_df, 
      x = ref.date, 
      y = price.adjusted, 
      geom = 'line')

#' 
#' In the previous example, the name of the axis c
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width---------------------------------------------
qplot(data = temp_df, 
      x = ref.date, 
      y = price.adjusted, 
      geom = 'line', 
      xlab = 'Dates', 
      ylab = 'Adjusted closing prices', 
      main = 'Prices of MMM')

#' 
#' Much better! Notice how the horizontal axis of 
#' 
#' 
#' ## Creating Figures with Function `ggplot` {#gg
#' 
#' Using function `qplot` is recommended when you 
#' 
#' Before presenting examples using `ggplot`, let'
#' 
#' The distinction between the steps of creating a
#' 
#' Look at the syntax of the following example tha
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my_fig_height, fig.width=my_fig_width--------------------------------
p <- ggplot(data = temp_df, 
            mapping = aes(x = ref.date, 
                          y = price.adjusted))
p <- p + geom_line()
p <- p + labs(x = 'Dates', 
              y = 'Adjusted closing prices', 
              title = 'Prices of MMM')
print(p)

#' 
#' In using `ggplot`, it is always necessary to pr
#' 
#' After defining the input information in argumen
#' 

#' 
#' Once the data and axis are defined, we save it 
#' 
## ----eval=TRUE-----------------------------------------------------------------------------------------------
library(ggplot2)
library(stringr)

# get names of functions in ggplot2
fcts <- ls('package:ggplot2')

# select those that starts with geom_
idx <- str_sub(fcts, 1, 5) == 'geom_'
fcts <- fcts[idx]

# print result
print(fcts)

#' 
#' As you can see, the `ggplot2` package offers a 
#' 
#' Going back to our example, the third line of th
#' 
#' Using the **pipeline operator** is also possibl
#' 
## ------------------------------------------------------------------------------------------------------------
p <- temp_df %>%
  ggplot(aes(x = ref.date, y = price.adjusted)) +
  geom_line() +
  labs(x = 'Dates', 
       y = 'Adjusted Closing Prices', 
       title = 'Prices of MMM')

#' 
#' We advise that you make a note to show we have 
#' 
#' One of the great advantages of using `ggplot` i
#' 
## ----eval=TRUE-----------------------------------------------------------------------------------------------
# fix seed
set.seed(10)

# select 4 stocks randomly
tickers <- sample(unique(df_sp500$ticker), 4)

# create temporary df
temp_df <- df_sp500 %>%
  filter(ticker %in% tickers)

#' 
#' In this code, we use operator `%in%` to find ou
#' 
## ----fig.height=my_fig_height, fig.width=my_fig_width--------------------------------------------------------
p <- temp_df %>%
  ggplot(aes(x = ref.date, 
             y = price.adjusted, 
             color = ticker)) +
  geom_line() +
  labs(x = 'Dates', 
       y = 'Adjusted closing prices', 
       title = 'Prices of four random stocks',
       subtitle = paste0('Date from ', min(temp_df$ref.date), 
                         ' to ', max(temp_df$ref.date) ),
       caption = 'Data from Yahoo Finance')

print(p)

#' 
#' A difference from the previous examples is that
#' 
#' Notice how easy and quick it was to adjust the 
#' 
#' 
#' ### The US Yield Curve
#' 
#' Now, let's use what we learned so far to create
#' 
#' In the following code, we will first download t
#' 
## ---- include=FALSE------------------------------------------------------------------------------------------
my_api_key <- 'Esv7Ac7zuZzJSCGxynyF'

#' 
## ------------------------------------------------------------------------------------------------------------
library(Quandl)
library(tidyverse)

Quandl.api_key(my_api_key) # set yours api key here

# set symbol and dates
my.symbol <- 'USTREASURY/YIELD'
first.date <- as.Date('2010-01-01')
last_date <- Sys.Date()

# get data!
df_yc <- Quandl(code = my.symbol,
                type = 'raw', 
                start_date = first.date,
                end_date = last_date)

print(head(df_yc))

#' 
#' The result is a dataframe in the wide format: y
#' 
## ------------------------------------------------------------------------------------------------------------
# change to long format and convert to factor
df_yc <- gather(data = df_yc,
                key = 'maturity',
                value = 'rate',
                -Date) %>%
  mutate(maturity = factor(maturity))

# keep only longer term yields (names with YR)
idx <- str_detect(df_yc$maturity, 'YR')
df_yc <- df_yc[idx, ]

# change name to year number with regex
# obs: regex ([0-9]+) extracts all numbers within a string
out <- str_extract_all(string = df_yc$maturity,
                       pattern = '([0-9]+)')
df_yc$maturity <- as.numeric(out)

# glimpse result
glimpse(df_yc)

#' 
#' We have a dataframe in the long format with thr
#' 
## ------------------------------------------------------------------------------------------------------------
# keep only last date of each
last_date <- max(df_yc$Date)
df_yc_lastdate <- df_yc[df_yc$Date == last_date, ]

# plot it!
p <- ggplot(df_yc_lastdate, aes(x=maturity, y=rate)) + 
  geom_point(size = 2.5) + geom_line(size=1) + 
  labs(x = 'Maturity (years)', 
       y='Yield Rate (%)',
       title = paste0('US Yield Curve for ',last_date),
       caption = paste0('Data from Quandl table ', my.symbol, '\n',
                        'Access at ', Sys.time()))

print(p)

#' 
#' As expected, the current yield curve is upward 
#' 
## ------------------------------------------------------------------------------------------------------------
# set number of periods 
n_periods <- 5
my_year <- 2019

# filter for year 2019
df_yc_my_year <- df_yc %>%
  filter(lubridate::year(Date) == my_year )

# get unique dates in data
unique_dates <- unique(df_yc_my_year$Date)

# set sequence of observations
my_seq <- floor(seq(1, length(unique_dates), 
                    length.out = n_periods))

# get actual dates from sequence
my_dates <- unique_dates[my_seq]

# find rows for dates in df
idx <- df_yc_my_year$Date %in% my_dates
df_yc_periods <- df_yc_my_year[idx, ]

# plot it!
p <- ggplot(df_yc_periods, aes(x=maturity, 
                               y=rate, 
                               color= factor(Date))) + 
  geom_point(size = 2.5) + geom_line(size = 1) + 
  labs(x = 'Maturity (years)', 
       y='Yield Rate (%)',
       title = paste0('US Yield Curve for ', my_year),
       color = 'Dates',
       caption = paste0('Data from Quandl table ', 
                        my.symbol, '\n',
                        'Access at ', Sys.time()))

print(p)

#' 
#' The yield curve is not static and will change o
#' 
#' 
#' ## Using Themes
#' 
#' One way of customizing graphics in `ggplot2` is
#' 

#' 
#' 
#' Package `ggplot` has a pre-packaged collection 
#' 
## ----eval=TRUE-----------------------------------------------------------------------------------------------
library(ggplot2)
library(stringr)

# get all functions
fcts <- ls('package:ggplot2')

# find out those that start with theme_
idx <- str_sub(fcts, 1, 6) == 'theme_'
fcts <- fcts[idx]

# print result
print(fcts)

#' 
#' Let's try it with the theme from function `them
#' 
## ----fig.height=my_fig_height, fig.width=my_fig_width--------------------------------------------------------
p <- temp_df %>%
  ggplot(aes(x = ref.date, y = price.adjusted, color=ticker)) +
  geom_line() +
  labs(x = 'Dates', 
       y = 'Adjusted closing prices', 
       title = 'Prices of four random stocks',
       caption = 'Data from Yahoo Finance') + 
  theme_bw()

print(p)

#' 
#' As you can see, the new theme was a white backg
#' 
#' Let's now use package `gridExtra` to create a g
#' 
## ---- fig.height=9 , fig.width = 9---------------------------------------------------------------------------
require(gridExtra)

p1 <- p + 
  theme_bw() + 
  labs(title = 'Theme BW')

p2 <- p + 
  theme_dark() + 
  labs(title = 'Theme Dark')

p3 <- p + 
  theme_grey() + 
  labs(title = 'Theme Grey')

p4 <- p + 
  theme_light() + 
  labs(title = 'Theme Light')

p5 <- p + 
  theme_classic() + 
  labs(title = 'Theme Classic')

p6 <- p + 
  theme_minimal() + 
  labs(title = 'Theme Minimal')

grid.arrange(p1, p2, p3,
             p4, p5, p6,
             ncol=2, nrow = 3)

#' 
#' 
#' You can try other themes on your computer and s
#' 
#' In the previous example, notice how the structu
#' 
#' Digging deeper, the selection of the colors fol
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my_fig_height, fig.width=my_fig_width--------------------------------
p <- p + 
  theme_bw() + 
  scale_colour_grey(start = 0.0, end = 0.6)

print(p)  

#' 
#' The lines of the plot are now in grey. The inpu
#' 
#' 
#' ## Creating Panels with `facet_wrap`
#' 
#' Another possibility of creating graphics for di
#' 
#' Facets are possible with function `facet_wrap`,
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width, tidy=FALSE---------------------------------
library(dplyr)
# fix seed
set.seed(10)

# select 4 stocks randomly
tickers <- sample(unique(df_sp500$ticker), 4)

p <- df_sp500 %>%
  filter(ticker %in% tickers) %>%
  ggplot(aes(x = ref.date, y = price.adjusted)) + 
  geom_line() + 
  labs(x = 'Date', 
       y = 'Adjusted closing prices',
       title = 'Prices of four random stocks',
       caption = 'Data from Yahoo Finance') + 
  facet_wrap(facets = ~ticker) + 
  theme_bw()

print(p)

#' 
#' Using panels is recommended when the data of th
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width, tidy=FALSE---------------------------------
# fix seed
set.seed(25)

# select 4 stocks randomly
tickers <- sample(unique(df_sp500$ticker), 4)

p <- df_sp500 %>%
  filter(ticker %in% tickers) %>%
  ggplot(aes(x = ref.date, y = ret)) + 
  geom_line(size=1) + 
  labs(x = 'Date', 
       y = 'Returns',
       title = 'Daily returns of four random stocks',
       caption = 'Data from Yahoo Finance') +   
  facet_wrap(facets = ~ticker) + 
  theme_bw()

print(p)

#' 
#' Notice how the vertical axis of the panels is f
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width---------------------------------------------
p <- p + 
  facet_wrap(facets = ~ticker, scales = 'free_y')

print(p)

#' 
#' Here, both axis, x and y, have their own scale 
#' 
#' 
#' ## Using the Pipeline
#' 
#' We previously saw that `ggplot2` is a friend of
#' 
## ---- eval=TRUE, tidy=FALSE,fig.height=my_fig_height, fig.width=my_fig_width---------------------------------
library(tidyverse)
library(ggplot2)

# calculated mean and sd of returns, plot result
my_f <- afedR::afedR_get_data_file(
  'SP500_Stocks_long_by_year.rds'
  )

df_sp500 <- read_rds(my_f)

p <- df_sp500 %>%
  na.omit() %>%
  group_by(ticker) %>%
  summarise(mean_ret = mean(ret.adjusted.prices),
            std_ret = sd(ret.adjusted.prices)) %>%
  ggplot(aes(x = std_ret, y = mean_ret)) +
  geom_point() + 
  labs(x = 'Standard Deviation of Yearly returns', 
       y = 'Average Yearly Returns',
       title = 'Expected Return and Risk for SP500 Stocks',
       subtitle = paste0('Annual price data from 2010 to 2019, ',
                         length(unique(df_sp500$ticker)), 
                         ' stocks included'),
       caption = 'Data imported from Yahoo Finance') + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_bw()

print(p)

#' 
#' The previous code is self-contained, easy to re
#' 
#' This is another example of an elegant code prod
#' 
#' 
#' ## Creating Statistical Graphics
#' 
#' Package `ggplot` has several options for creati
#' 
#' 
#' ### Creating Histograms
#' 
#' A histogram shows the empirical distribution of
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width---------------------------------------------
# set file and load data
my_f  <- afedR::afedR_get_data_file('SP500-Stocks-WithRet.rds')
df_sp500 <- read_rds(my_f )

# remove outliers
my_prob_outliers <- 0.01
df_sp500$ret <- afedR::afedR_replace_outliers(
  df_sp500$ret, 
  my_prob = my_prob_outliers
  )

# plot the data
p <- ggplot(data = df_sp500, aes(x = ret)) + 
  geom_histogram(bins = 100) + 
  labs(y = 'Frequency', 
       x = 'Returns',
       title = paste0('Distribution of returns for ', 
                      'all stocks in the SP500 index'),
       subtitle = paste0('Data from 2010 to 2019\n',
                         'Distribution based quantiles at the ', 
                         scales::percent(my_prob_outliers), 
                         ' were removed'),
       caption = 'Data from Yahoo Finance'
  ) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_bw()

print(p)

#' 
#' Here, we only need to define the _x_ value, wit
#' 
#' We can also use groups and facets as we did for
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width, tidy=FALSE---------------------------------
# fix seed
set.seed(30)

# select 4 stocks randomly
tickers <- sample(unique(df_sp500$ticker), 4)

p <- df_sp500 %>%
  filter(ticker %in% tickers) %>%
  ggplot(aes(x = ret)) + 
  geom_histogram(bins = 50) + 
  labs(y = 'Frequency', 
       x = 'Returns',
       title = 'Distribution of returns for four random stocks',
       subtitle = paste0('Data from 2010 to 2019\n',
                         'Quantiles at the ', 
                         scales::percent(my_prob_outliers), 
                         ' of the distribution were removed'),
       caption = 'Data from Yahoo Finance'
  ) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_bw() + 
  facet_wrap(facets = ~ticker)

print(p)

#' 
#' A histogram with the empirical densities of the
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width, tidy=FALSE---------------------------------
p <- df_sp500 %>%
  filter(ticker %in% tickers) %>%
  ggplot(aes(x = ret)) + 
  geom_density() + 
  facet_wrap(facets = ~ticker) + 
  labs(y = 'Interpolated Frequency', 
       x = 'Returns',
       title = paste0('Interpolated distribution of returns',
                      'for all stocks in the SP500 index'),
       subtitle = paste0('Data from 2010 to 2019\n',
                         'Quantiles at the ', 
                         scales::percent(my_prob_outliers), 
                         ' of both sides of the distribution', 
                         'were removed'),
       caption = 'Data from Yahoo Finance'
  ) + 
  scale_x_continuous(labels = scales::percent) + 
  theme_bw()

print(p)

#' 
#' The previous figure allows a clear visual compa
#' 
#' 
#' ### Creating _boxplot_ Figures
#' 
#' Figures of type _boxplot_, or box and whisker d
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width, tidy=FALSE---------------------------------
# fix seed
set.seed(30)

# select 4 stocks randomly
tickers <- sample(unique(df_sp500$ticker), 4)

p <- df_sp500 %>%
  filter(ticker %in% tickers) %>%
  ggplot(aes(x = ticker, y = price.adjusted)) + 
  geom_boxplot() + 
  labs(y = 'Adjusted Price',
       x = 'Ticker',
       title = paste0('Distribution of daily prices', 
                      ' of four random stocks'),
       caption = paste0('Data imported from Yahoo Finance', 
                        ' (2010 - 2019)')
       ) + 
  theme_bw()

print(p)

#' 
#' We define a box plot by setting the `x` and `y`
#' 
#' Another interesting application of boxplot figu
#' 
## ------------------------------------------------------------------------------------------------------------
p <- ggplot(df_yc, aes(x = factor(maturity), y = rate)) + 
  geom_boxplot() + 
  labs(title = 'Distribution of Yield Rates over Maturities',
       x = 'Maturity (years)', 
       y = 'Yield Rate (% per year)',
       caption = paste0('Data from Quandl \n',
                        'Created at ', Sys.time()) ) + 
  theme_bw()

print(p)

#' 
#' The statistical plot shows the upward pattern f
#' 
#' 
#' ### Creating _QQ_ Plots
#' 
#' QQ plots show a comparison between the empirica
#' 
#' Let's try an example with some simulated data.
#' 
## ----eval=TRUE, fig.height=my_fig_height, fig.width=my_fig_width, tidy=FALSE---------------------------------
# fix seed
set.seed(40)

# set options
N <- 10000
my_mean <- 10
my_sd <- 2

# create tibble
temp_df <- tibble(y=rnorm(n = N, 
                          mean = my_mean, 
                          sd = my_sd))

# plot QQ
p <- ggplot(data = temp_df, aes(sample = y)) + 
  geom_qq(distribution = qnorm, 
          dparams = c(mean=my_mean, sd=my_sd)) + 
  labs(title = 'QQ plot of simulated Normal Distribution')

print(p)

#' 
#' In the previous code, we simulate random normal
#' 
#' Now, let's try it for our table of stock's retu
#' 
#' 
## ----eval=TRUE,fig.height=my_fig_height, fig.width=my_fig_width, tidy=FALSE----------------------------------
# fix seed
set.seed(10)

# select 4 stock randomly and filter
tickers <- sample(unique(df_sp500$ticker), 4)

temp_df <- df_sp500 %>%
  filter(ticker %in% tickers)

# set function for normalization
norm_vec <- function(y){
  # Normalizes a vector by subtracting mean and dividing
  # by the standard deviation
  #
  # Args:
  #   y - numerical vector
  #
  # Returns:
  #   A normalized vector
  
  y.norm <- (y-mean(y, na.rm = TRUE))/sd(y, na.rm = TRUE)
  return(y.norm)
}

# apply function  
my_l <- tapply(X = temp_df$ret, 
               INDEX = factor(temp_df$ticker), 
               FUN = norm_vec)

# reorder list (tapply sorts alphabetically)
my_l <- my_l[as.character(unique(temp_df$ticker))]

# save new column norm.ret
temp_df$norm_ret <- unlist(my_l)

# plot it!
p <- ggplot(data = temp_df, aes(sample = norm_ret)) + 
  geom_qq() + 
  facet_wrap(~ticker) + 
  labs(title = 'QQ plot for normalized returns',
       subtitle = 'Daily returns from 2010 to 2019', 
       x = 'Theoretical value (from Normal)',
       y = 'True/observed value',
       caption = 'Data from Yahoo Finance') + 
  theme_bw()

print(p)

#' 
#' 
#' As you can see, the result is not visually simi
#' 
#' 
#' ## Saving Graphics to a File
#' 
#' To save pictures created with `ggplot`, use fun
#' 
#' Consider the following example, where we create
#' 
## ----eval=TRUE, tidy=FALSE,fig.height=my_fig_height, fig.width=my_fig_width----------------------------------
library(tidyverse)

# fix seed
set.seed(40)

# select 4 stocks randomly
tickers <- sample(unique(df_sp500$ticker), 4)

p <- df_sp500 %>%
  filter(ticker %in% tickers) %>%
  ggplot(aes(x = ref.date, 
             y = price.adjusted, 
             color = ticker)) + 
  geom_line() + 
  labs(x = 'Date', 
       y = 'Adjusted closing prices',
       title = 'Prices of four random stocks',
       caption = 'Data from Yahoo Finance')

# save file
my_fig_file <- 'fig_ggplot/MyPrices.png'
ggsave(filename = my_fig_file, 
       plot=p,
       dpi = 600)

#' 
#' You can verify the creation of the file with fu
#' 
## ------------------------------------------------------------------------------------------------------------
print(list.files('fig_ggplot'))

#' 
#' As expected, the file is available in folder `f
#' 
#' 
#' ## Exercises 
#' 
#' 01. Download Facebook (FB) stock data with the 
#' 
#' - The x and y-axis is correctly named;
#' - The plot has a title ("Prices for 1 stock"), 
#' 
#' 02. Download Google (GOOG), Facebook (FB) and D
#' 
#' 03. For the previous chart, add points to the l
#' 
#' 04. For the same chart, separate stock prices i
#' 
#' 05. Modify the previous chart theme to greyscal
#' 
#' 06. For the previous data, create the histogram
#' 
#' 07. Use function `tidyquant::tq_exchange` to fi
#' 
#' 08. Head over to the [Kaggle data website](http