#' # Cleaning and Structuring Data {#cleaning}
#' 
## ---- include=FALSE------------------------------------------------------------------------------------------
source('Scripts/preamble_chapters.R')

#' 

#' 
#' So, now that we learned the basics of programmi
#' 
#' - Changing the format of a dataframe (long/wide
#' - Converting a `list` of `dataframes` into a si
#' - Identifying and treating extreme values (_out
#' - Price data deflation;
#' - Data aggregation based on a change of time-fr
#' 
#' 
#' ## The Format of a `dataframe`
#' 
#' The `dataframe` format discussion arose due to 
#' 
#' **In the wide format**, the rows of the table a
#' 
## ---- echo=FALSE---------------------------------------------------------------------------------------------
library(tidyverse)

set.seed(10)
N <- 4

temp_df <- tibble(refdate=Sys.Date()+1:N,
                  STOCK1 = 10+cumsum(rnorm(N, sd = 1.25)),
                  STOCK2 = 3+ cumsum(rnorm(N, sd = 0.5)),
                  STOCK3 = 6+ cumsum(rnorm(N, sd = 0.5)))

knitr::kable(temp_df, digits = 2)

#' 
#' The above table has three distinct pieces of in
#' 
#' **In the long format**, each row of the `datafr
#' 
## ---- echo=FALSE---------------------------------------------------------------------------------------------
wide_df <- tidyr::gather(data = temp_df,
                         key = 'ticker',
                         value = 'price',
                         - refdate)
colnames(wide_df) <- c('refdate', 'ticker', 'price')

knitr::kable(wide_df, digits = 2)

#' 
#' In comparison, the wide format is more intuitiv
#' 
#' This argument may seem trivial since the inform
#' 
#' It is worth noting that, in finance, the wide f
#' 
#' 
#' ### Converting a `dataframe` Structure (long an
#' 
#' The conversion from one format to the other is 
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
library(tidyr)
library(tidyverse)

# set dates and stock vectors
refdate <- as.Date('2015-01-01') + 0:3
STOCK1 <- c(10, 11, 10.5, 12)
STOCK2 <- c(3, 3.1, 3.2, 3.5)
STOCK3 <- c(6, 7, 7.5, 6)

# create wide dataframe
my_df_wide <- tibble(refdate, STOCK1, STOCK2, STOCK3)

# print it
print(my_df_wide)

# convert wide to long
my_df_long <- gather(data = my_df_wide,
                     key = 'ticker',
                     value = 'price',
                     - refdate)

# print result
print(my_df_long)

#' 
#' To perform the reverse conversion, _long_ to _w
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# convert from long to wide
my_df_wide_converted <- spread(data = my_df_long, 
                               key = 'ticker',
                               value = 'price')

# print result
print(my_df_wide_converted)

#' 
#' With more complex conversions, where it is nece
#' 
## ------------------------------------------------------------------------------------------------------------
library(reshape2)

# use melt to change from wide to long
my_df_long <- melt(data = my_df_wide, 
                   id.vars = 'refdate', 
                   variable.name = 'ticker', 
                   value.name = 'price')

# print result				   
print(my_df_long)

#' 
## ------------------------------------------------------------------------------------------------------------
# use melt to change from long to wide
my_df_wide_converted <- dcast(data = my_df_long, 
                              formula = refdate ~ ticker, 
                              value.var = 'price')
print(my_df_wide_converted)

#' 
#' Although, it is worth noting that it is importa
#' 
#' 
#' ## Converting `lists` into `dataframes`
#' 
#' Another important case in data structuring is t
#' 
#' For the first, let's use the `purrr` package as
#' 
## ---- include=FALSE------------------------------------------------------------------------------------------
# clean up files
file.remove(list.files('many_datafiles_2/', 
                       full.names = TRUE) )

#' 
## ------------------------------------------------------------------------------------------------------------
create_rnd_data <- function(n_obs = 100,
                            folder_out) {
  # function for creating random datasets
  #
  # ARGS: n_obs - number of observations
  #       folder_out - folder where to save files
  #
  # RETURN: TRUE, if sucessfull
  
  require(tidyverse)
  require(wakefield)
  
  # check if folder exists
  if (!dir.exists(folder_out)) dir.create(folder_out)
  
  # create extensive random data
  rnd_df <- r_data_frame(n = n_obs,
                         id,
                         race,
                         age,
                         sex) %>%
    r_na(prob = 0.1)
  
  # for 15% of the time, create a new column
  if (runif(1) < 0.15 ) {
    rnd_df$bad_column <- 'BAD COLUMN!'
  }
  
  # set file name
  f_out <- tempfile(fileext = '.csv', 
                    pattern = 'file_',
                    tmpdir = folder_out)
  
  write_csv(x = rnd_df, 
            path = f_out)
  
  return(TRUE)
}

#' 
#' Function `create_rnd_data` will create and writ
#' 
#' Going forward, let's use `purrr::pmap` to creat
#' 
## ------------------------------------------------------------------------------------------------------------
n_files <- 50
n_obs <- 100
folder_out <- 'many_datafiles_2'

# create random datasets
l_out <- pmap(.l = list(n_obs = rep(n_obs, n_files),
                        folder_out = rep(folder_out, n_files)), 
              .f = create_rnd_data) 

# check if files are there
print(head(list.files(folder_out)))

#' 
#' The files are available, as expected. Now, let'
#' 
## ------------------------------------------------------------------------------------------------------------
read_single_file <- function(f_in) {
  # Function for reading single csv file with random data
  #
  # ARGS: f_in - path of file
  #
  # RETURN: A dataframe with the data
  
  require(tidyverse)
  
  df <- read_csv(f_in, col_types = cols())
  
  return(df)
}

#' 
## ------------------------------------------------------------------------------------------------------------
library(purrr)

files_to_read <- list.files('many_datafiles_2/', 
                            full.names = TRUE)

l_out <- map(files_to_read, read_single_file)

#' 
#' And now we bind them all together with a simple
#' 
## ------------------------------------------------------------------------------------------------------------
compiled_df <- bind_rows(l_out)

glimpse(compiled_df)

#' 
#' It worked, as expected. We have `r nrow(compile
#' 
#' For the second example, let's take a case of da
#' 
## ---- cache=TRUE---------------------------------------------------------------------------------------------
library(BETS)

my_id <- 3785:3791

# set dates
first_date = '2010-01-01'
last_date  = as.character(Sys.Date())

# get data
l_out <- BETSget(code = my_id, data.frame = TRUE,
                 from = first_date, to = last_date)

# check data in first dataframe
glimpse(l_out[[1]])

#' 
#' In this example we gather data for unemployment
#' 
#' Now, if we want to structure all imported table
#' 
## ------------------------------------------------------------------------------------------------------------
my_countries <- c("Germany", "Canada", "United States", 
                  "France",  "Italy", "Japan", 
                  "United Kingdom")

#' 
#' The order of elements in vector `my_countries` 
#' 
#' Going further, we now create a function that wi
#' 
## ------------------------------------------------------------------------------------------------------------
clean_bets <- function(df_in, country_in) {
  # function for cleaning data from BETS
  #
  # ARGS: df_in - dataframe within a list
  #       country_in - name of country
  #
  # VALUE: a new dataframe with new column type
  
  #set column
  df_in$country <- country_in
  
  # return df
  return(df_in)
}

#' 
#' The next step is to use the previous function t
#' 
## ------------------------------------------------------------------------------------------------------------
library(purrr)

# set args
l_args <- list(df_in = l_out, 
               country_in = my_countries)
# format dfs
l_out_formatted <- pmap(.l = l_args, 
                        .f = clean_bets)

# check first element (all are the same structure)
glimpse(l_out_formatted[[1]])

#' 
#' From the output of `glimpse` we see that the co
#' 
## ------------------------------------------------------------------------------------------------------------
# bind all rows of dataframes in list
df_unemp <- bind_rows(l_out_formatted)

# check it
glimpse(df_unemp)

#' 
#' Done! The result is an organized `dataframe` in
#' 
#' 
#' ## Removing Outliers
#' 
#' A recurrent issue in data analysis is handling 
#' 
#' So, because we want to visualize the destructiv
#' 
#' The next example might be challenging if it is 
#' 
## ------------------------------------------------------------------------------------------------------------
# set seed for reproducibility
set.seed(100)

# set options
nT <- 100
sim_x <- rnorm(nT)
my_beta <- 0.5

# simulate x and y
sim_y <- sim_x*my_beta + rnorm(nT)
sim_y_with_outlier <- sim_y

# simulate y with outlier
sim_y_with_outlier[10] <- 50

#' 
#' Objects `sim_y` and `sim_y_with_outlier` are ex
#' 
## ------------------------------------------------------------------------------------------------------------
library(texreg)

# estimate models
model_no_outlier <- lm(formula = sim_y ~ sim_x)
model_with_outlier <- lm(formula = sim_y_with_outlier ~ sim_x)

# report them
screenreg(list(model_no_outlier, 
               model_with_outlier),
          custom.model.names = c('No Outlier', 'With Outlier'))

#' 
#' Here we report the models with the `texreg` pac
#' 
#' Notice from the estimation table that the slope
#' 
#' One way to accomplish this is to identify poten
#' 
## ------------------------------------------------------------------------------------------------------------
# find the value of vector that sets the 95% quantile
quantile95 <- quantile(x = abs(sim_y_with_outlier),
                       probs = 0.95)

print(quantile95)

#' 
#' Here, the value of `r quantile95` is higher tha
#' 
## ------------------------------------------------------------------------------------------------------------
# find cases higher than 95% quantile
idx <- which(sim_y_with_outlier > quantile95)
print(sim_y_with_outlier[idx])

#' 
#' We find the "artificial" outlier we've set in p
#' 
#' Finally, we need to treat outliers. We can eith
#' 
## ------------------------------------------------------------------------------------------------------------
# copy content
sim_y_without_outlier <- sim_y_with_outlier

# set NA in outlier
sim_y_without_outlier[idx] <- NA

# or remove it
sim_y_without_outlier <- sim_y_without_outlier[-idx]

#' 
#' An alternative for identifying extreme values i
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## library(outliers)
## 
## # find outlier
## my_outlier <- outlier(sim_y_with_outlier)
## 
## # print it
## print(my_outlier)

#' 

#' 
#' 
#' As expected, it correctly identified the outlie
#' 
#' 
#' ### Treating Outliers in `dataframes`
#' 
#' Let's go a bit deeper. In a real data analysis 
#' 
#' The first step is to define a function that acc
#' 
## ------------------------------------------------------------------------------------------------------------
replace_outliers <- function(col_in, my_prob = 0.05) {
  # Replaces outliers from a vector and returns a new
  # vector
  #
  # INPUTS: col_in The vector
  #         my_prob Probability of quantiles 
  #                 (will remove quantiles at p and 1-p)
  #
  # OUTPUT: A vector without the outliers
  
  # return if class is other than numeric
  if (!(class(col_in) %in% 
        c('numeric', 'integer'))) return(col_in)
  
  my_outliers <- stats::quantile(x = col_in,
                                 probs = c(my_prob, 1-my_prob),
                                 na.rm = TRUE)
  
  idx <- (col_in <= my_outliers[1])|(col_in >= my_outliers[2])
  col_in[idx] <- NA
  
  return(col_in)
  
}

#' 
#' Let's test it:
#' 
## ------------------------------------------------------------------------------------------------------------
# set test vector
my_x <- runif(25)

# find and replace outliers
print(replace_outliers(my_x, my_prob = 0.05))

#' 
#' As we can see, it performed correctly. The outp
#' 
## ------------------------------------------------------------------------------------------------------------
library(wakefield)
library(tidyverse)

# options
n_obs <- 100

# create extensive random data
my_df <- r_data_frame(n = n_obs,
                      race,
                      age,
                      birth, 
                      height_cm,
                      sex) 

# check it
glimpse(my_df)

#' 
#' Now, let's use `purrr::map` to iterate all elem
#' 
## ------------------------------------------------------------------------------------------------------------
library(purrr)

# remove outlivers from vectors
l_out <- map(my_df, replace_outliers)

#' 
#' Next, we regroup all vectors into a single data
#' 
## ------------------------------------------------------------------------------------------------------------
# rebuild dataframe
my_df_no_outlier <- as_tibble(l_out)

# check it
glimpse(my_df_no_outlier)

# summary of my_df_no_outlier
summary(my_df_no_outlier)

#' 
#' Note that, as expected, we find `NA` values for
#' 
#' For last, we remove all rows with outliers usin
#' 
## ------------------------------------------------------------------------------------------------------------
# remove outliers
my_df_no_outlier <- na.omit(my_df_no_outlier)

glimpse(my_df_no_outlier)

#' 
#' Notice, however, that some rows were lost. The 
#' 
#' 
#' ## Inflation and Price Data
#' 
#' A common effect in economic and financial data 
#' 
#' To offset the effect of inflation on price data
#' 
## ------------------------------------------------------------------------------------------------------------
library(GetQuandlData)
library(tidyverse)

# set api (you need your OWN from www.quandl.com)
my_api_key <- readLines(
  '~/Dropbox/98-pass_and_bash/.quandl_api.txt'
  )

# set symbol and dates
my_symbol <- 'RATEINF/INFLATION_USA'
first_date <- as.Date('2000-01-01')
last_date <- Sys.Date()

# get data!
df_inflation <- get_Quandl_series(id_in = my_symbol,
                                  api_key = my_api_key, 
                                  first_date = first_date, 
                                  last_date = last_date)

# sort by date
df_inflation <- df_inflation %>%
  arrange(ref_date)

# check content
glimpse(df_inflation)

#' 
#' Now, let's create a random dataframe with rando
#' 
## ------------------------------------------------------------------------------------------------------------
n_T <- nrow(df_inflation)

# create df with prices
my_df <- tibble(Date = df_inflation$ref_date,
                x = 100 + cumsum(rnorm(n_T)),
                y = 100 + cumsum(rnorm(n_T)))

# check it
glimpse(my_df)

#' 
#' The first step is to create a deflator index ba
#' 
## ------------------------------------------------------------------------------------------------------------
# accumulate: R_a = cumprod(r_t + 1)
my_df$infl_idx <- cumprod(df_inflation$value/100 +1)

# set inflation index
my_df$infl_idx <- my_df$infl_idx/my_df$infl_idx[nrow(my_df)]

#' 
#' And now we create the new variables:
#' 
## ------------------------------------------------------------------------------------------------------------
my_df$x_desinflated <- my_df$x*my_df$infl_idx
my_df$y_desinflated <- my_df$y*my_df$infl_idx

glimpse(my_df)

#' 
#' Done. Following the previous example, we could 
#' 
#' 
#' ## Modifying Time Frequency and Aggregating Dat
#' 
#' Sometimes we receive data with a mismatch of ti
#' 
#' Let's start with an example with the SP500 inde
#' 
## ------------------------------------------------------------------------------------------------------------
library(BatchGetSymbols)

df_SP500 <- BatchGetSymbols(tickers = '^GSPC',
                            first.date = '2010-01-01',
                            freq.data = 'daily',
                            last.date = '2018-01-01')[[2]]

#' 
#' Every time-frequency operation from higher to l
#' 
## ------------------------------------------------------------------------------------------------------------
# from daily to annual
df_SP500_annual <- df_SP500 %>%
  mutate(ref_year = lubridate::year(ref.date)) %>%
  group_by(ref_year) %>%
  summarise(last_value = last(price.adjusted)) 

# glimpse it
glimpse(df_SP500_annual)

#' 
#' We will create a new column with the years, gro
#' 
#' 
#' ## Exercises
#' 
#' 01. Consider the following `dataframe`:
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

my_N <- 100

df <- bind_rows(tibble(ticker = rep('STOCK 1', my_N),
                       ref_date = Sys.Date() + 1:my_N,
                       price = 100 + cumsum(rnorm(my_N))),
                tibble(ticker = rep('STOCK 2', my_N),
                       ref_date = Sys.Date() + 1:my_N,
                       price = 100 + cumsum(rnorm(my_N))) )

print(df)

#' 
#' The format is long or wide? Explain your answer
#' 
#' 02. Modify the format of the previous dataframe
#' 
#' 03. Consider the following `list`:
#' 
## ------------------------------------------------------------------------------------------------------------
my_l <- list(df1 = tibble(x = 1:100, 
                          y = runif(100)),
             df2 = tibble(x = 1:100, 
                          y = runif(100), 
                          v = runif(100)),
             df3 = tibble(x = 1:100, 
                          y = runif(100), 
                          z = runif(100)) )

#' 
#' Aggregate all elements of `my_l` into a single 
#' 
#' 04. Use package `BatchGetSymbols` to download S
#' 
#' 05. Use the function created in this chapter fo
#' 
#' 06. Use function `BatchGetSymbols::BatchGetSymb
#' 
#' 07. Use the same daily data from the previous e
#' 
#' 08. CHALLENGE - For the previously downloaded F