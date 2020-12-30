#' # Programming and Data Analysis {#programming}
#' 

#' 
#' In previous chapters, we have learned what the 
#' 
#' 
#' ## R Functions
#' 
#' **The use of functions is in the heart of R**. 
#' 
#' Moreover, the usability of the function only ge
#' 
#' A function always has three parts: input, proce
#' 
## ------------------------------------------------------------------------------------------------------------
example_fct <- function(arg1 = 1, arg2 = 'abc'){
  
  msg1 <- paste0('\nValue of arg1: ', arg1)
  cat(msg1)
  
  msg2 <- paste0('\nValue of arg2: ', arg2)
  cat(msg2)
  
  cat('\n')
  
  out <- c(msg1, msg2)
  
  return(out)
}

#' 
#' The definition of a function is similar to the 
#' 
#' Using the equality symbol in this setting, as i
#' 
#' After registering the function in the environme
#' 
## ------------------------------------------------------------------------------------------------------------
# first call
out1 <- example_fct(arg1 = 2, arg2 = 'bcd')

# second call
out2 <- example_fct(arg1 = 10, arg2 = 'dab')

#' 
#' Every function will return an object with the `
#' 
#' As for using the function, you'll first need to
#' 
#' The arguments of the function can be set by pos
#' 
#' Now, let's create a function that does somethin
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
my_mean_fct <- function(x = c(1, 1, 1, 1)){
  # Calculates the average of input x
  #
  # Args: 
  # 	x: a numerical vector
  #
  # Returns:
  #   The mean of x
  
  out <- sum(x)/length(x)
  
  return(out)
  
}

#' 
#' Notice how we've set a comment section after th
#' 
#' > "Functions should contain a comments section 
#' >
#' > --- Google's R style manual
#' 
#' After writing the function down, register it by
#' 
## ------------------------------------------------------------------------------------------------------------
# testing function my_mean_fct
my_mean <- my_mean_fct(x = 1:100)

# print result
print(my_mean)

#' 
#' The mean of a sequence from 1 to 100 is `r my_m
#' 
#' If function `my_mean_fct` is called without any
#' 
## ------------------------------------------------------------------------------------------------------------
# calling my_mean_fct without input
my_mean <- my_mean_fct()

# print result
print(my_mean)

#' 
#' Again, as expected, the returned value is corre
#' 
#' A note here. A simple strategy for setting defa
#' 
#' Although simple, the previous example can be fu
#' 
#' Correcting this problem is simple: you just nee
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
my_mean_fct <- function(x = c(1, 1, 1, 1)){
  # Calculates the average of input x
  #
  # Args: 
  #   x - a numerical vector
  #
  # Returns:
  #   The mean of x, as numeric
  
  if (!(class(x) %in% c('numeric', 'integer'))){
    stop('x is not a numeric class.')
  }
  
  out <- sum(x)/length(x)
  
  return(out)
}

#' 
#' In the previous code, we use the `class` functi
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## # using wrong inputs (ERROR)
## my_mean_fct(x = c('a', 'b'))

#' 

#' 
#' 
#' Another problem with our custom function is tha
#' 
#' To handle `NA` values in function `my_mean_fct`
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
my_mean_fct <- function(x = c(1, 1, 1, 1)){
  # Calculates the average of input x
  #
  # Args: 
  #   x: a numerical vector
  #
  # Returns:
  #   The mean of x, as numeric
  
  if (!(class(x) %in% c('numeric', 'integer'))){
    stop('Input error: x is not numeric or integer')
  }
  
  if (any(is.na(x))){
    warning('Warning: Found NA in x. Removing it.')
    x <- na.omit(x)
  }	
  
  out <- sum(x)/length(x)
  
  return(out)
}

#' 
#' We used function `warning` to issue a message i
#' 
## ----warning=TRUE--------------------------------------------------------------------------------------------
# set vector with NA
y <- c(1, 2, 3, NA, 1)

# test function
print(my_mean_fct(y))

#' 
#' As we can see, the function acknowledged the ex
#' 
#' Using comments and input testing is a good prog
#' 
#' Now, let's move to a more complete example of u
#' 

#' 

#' 
#' In R, this procedure takes as input a price vec
#' 
#' First, let's register a function for calculatin
#' 
## ------------------------------------------------------------------------------------------------------------
calc_ret <- function(P) {
  # Calculates arithmetic returns from a vector of prices
  #
  # Args:
  #   P - vector of prices (numeric)
  #
  # Returns:
  #   A vector of returns
  
  # ret = p_{t}/p_{t-1} - 1	
  my_length <- length(P)
  ret <- c(NA, P[2:my_length]/P[1:(my_length - 1)] - 1)
  return(ret)
}


#' 
#' The function is simple and direct. Notice how w
#' 
#' Although intuitive, note that the `calc_ret` fu
#' 
#' To solve this issue is straightforward: we need
#' 
## ------------------------------------------------------------------------------------------------------------
calc_ret <- function(P, 
                     tickers = rep('ticker', length(P))) {
  # Calculates arithmetic returns from a vector of prices
  #
  # Args:
  #   P - vector of prices (numeric)
  #   tickers - vector of tickers (optional)
  #
  # Returns:
  #   A vector of returns
  
  # ret_t = p_{t}/p_{t-1} - 1
  
  # error checking
  if ( !(class(P) %in% c('numeric', 'integer'))) {
    stop('P should be a numeric object.')
  }
  
  if ( !(class(tickers) %in% c('character', 'factor'))) {
    stop('tickers should be a character or factor object.')
  }
  
  if (length(P) != length(tickers)) {
    stop('The length of P and tickers does not match.')
  }
  
  if ( length(P) < 2) {
    stop('input P should have at least 2 elements.')
  }
  
  my_length <- length(P)
  ret <- c(NA, P[2:my_length]/P[1:(my_length - 1)] - 1)
  
  idx <- (tickers != c(NA, tickers[1:(my_length-1)]))
  ret[idx] <- NA
  
  return(ret)
}


#' 
#' That's a lengthy code! But remember, you only n
#' 
#' Now, let's use the function with the data for t
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

my_f <- afedR::afedR_get_data_file('SP500-Stocks_long.csv')

# import data
df_sp500 <- read_csv(my_f, col_types = cols())

# calculate return column
df_sp500 <- df_sp500 %>%
  mutate(ret = calc_ret(P = price.adjusted,
                        tickers = ticker))


#' 
#' Let's look at the result:
#' 
## ------------------------------------------------------------------------------------------------------------
glimpse(df_sp500)

summary(df_sp500)

#' 
#' It looks great! The return vector is available 
#' 
#' Going further, let's remove all `NA` rows with 
#' 
## ------------------------------------------------------------------------------------------------------------
df_sp500 <- df_sp500 %>%
  filter(complete.cases(.))

# check result
glimpse(df_sp500)
summary(df_sp500)

#' 
#' Finally, we save the resulting dataset as a _.r
#' 
## ------------------------------------------------------------------------------------------------------------
write_rds(x = df_sp500,
          path = 'data/SP500-Stocks-WithRet.rds')

#' 
#' As a final word on using functions, don't hesit
#' 
#' 
#' ## Using `for` Loops 
#' 
#' A _loop_ command is the most basic computer ins
#' 
#' The great thing about _loops_ is its length. Th
#' 
#' Back to the code, the structure of a _loop_ in 
#' 
## ----eval=FALSE----------------------------------------------------------------------------------------------
## for (i in i_vec) {
##   ...
## }

#' 
#' Command `for` indicates the beginning of a _loo
#' 
## ------------------------------------------------------------------------------------------------------------
# set seq
my_seq <- seq(-5, 5)

# do loop
for (i in my_seq){
  cat(paste('\nThe value of i is', i))
}

#' 
#' We created a sequence from -5 to 5 and presente
#' 
#' The iterated sequence in the _loop_ is not excl
#' 
## ------------------------------------------------------------------------------------------------------------
# set char vec
my_char_vec <- letters[1:5]

# loop it!
for (i_char in my_char_vec){
  cat(paste('\nThe value of i_char is', 
            i_char))
}

#' 
#' The same goes for `lists`:
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set list
my_l <- list(x = 1:5, 
             y = c('abc', 'dfg'), 
             z = factor('A', 'B', 'C', 'D'))

# loop list
for (i_l in my_l){
  
  cat(paste0('\nThe class of i_l is ', class(i_l), '. '))
  cat(paste0('The number of elements is ', length(i_l), '.'))
  
}

#' 
#' In the definition of _loops_, the iterator does
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set vec and iterators
my_vec <- seq(1:5)
my_x <- 5
my_z <- 10

for (i in my_vec){
  # increment "manually"
  my_x <- my_x + 1
  my_z <- my_z + 2
  
  cat('\nValue of i = ', i, 
      ' | Value of my_x = ', my_x, 
      ' | Value of my_z = ', my_z)
}

#' 
#' Using nested _loops_, that is, a _loop_ inside 
#' 
## ------------------------------------------------------------------------------------------------------------
# set matrix
my_mat <- matrix(1:9, nrow = 3)

# loop all values of matrix
for (i in seq(1, nrow(my_mat))){
  for (j in seq(1,ncol(my_mat))){
    cat(paste0('\nElement [', i, ', ', j, '] = ', 
               my_mat[i, j]))
  }
}

#' 
#' Let's do a more complex example using data file
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# set number of files to create
n_files <- 10

# set the first part of saved files
pattern_name <- 'myfiles_'

# set dir
out_dir <- 'many_datafiles/'

# test if out.dir exists -- if not, create it
if (!dir.exists(out_dir)) {
  dir.create(out_dir)	
} else {
  # clean up folder before creating new files
  file.remove(list.files(out_dir, 
                         full.names = TRUE))	
}

# set vec with filenames
file_names <- paste0(out_dir, 
                     pattern_name, 
                     seq(1, n_files), '.csv')

# loop it!
for (i_file in file_names){
  # create temp df
  temp_df <- tibble(x = runif(100))
  
  # write it!
  write_csv(x = temp_df, path = i_file)
}

#' 
#' In the previous example, we used function `if` 
#' In the _loop_, we used function `runif` to crea
#' 
#' Now, let's check if the files are in the folder
#' 
## ------------------------------------------------------------------------------------------------------------
# check files
print(list.files(out_dir))

#' 
#' As expected, the files are there. To complete t
#' 
## ------------------------------------------------------------------------------------------------------------
# set empty df
df_agg <- tibble()
for (i_file in file_names){
  # read file
  temp_df <- read_csv(i_file, col_types = cols())
  
  # row bind 
  df_agg <- bind_rows(df_agg, temp_df)
}

glimpse(df_agg)

#' 
#' Notice how we bind all `dataframes` within the 
#' 
#' Another practical example of using _loops_ is p
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# read data
my_f <- afedR::afedR_get_data_file('SP500-Stocks_long.csv')
df_SP500 <- read_csv(my_f,
                     col_types = cols())

# find unique tickers in column ticker
unique_tickers <- unique(df_SP500$ticker)

# create empty df for saving results
tab_out <- tibble()

# loop tickers
for (i_ticker in unique_tickers){
  
  # create temp df with ticker i.ticker
  temp <- df_SP500 %>%
    filter(ticker == i_ticker)
  
  # row bind i.ticker and mean_price
  temp_mean_price <- mean(temp$price.adjusted)
  tab_out <- bind_rows(tab_out,
                       tibble(ticker = i_ticker,
                              mean_price = temp_mean_price))
  
}

# print result
print(head(tab_out))

#' 
#' We used the function `unique` to discover the n
#' 
#' 
#' ## Conditional Statements (`if`, `else`, `switc
#' 
#' Making binary decisions of type _yes_ or _no_ i
#' 
## ----eval=FALSE----------------------------------------------------------------------------------------------
## # skeleton for if statement
## if (cond){
## 
##   CodeIfTRUE...
## 
## } else {
## 
##   CodeIfFALSE...
## 
## }

#' 
#' The placeholder `cond` is the condition to be e
#' 
## ------------------------------------------------------------------------------------------------------------
# set vec and threshold
my_x <- 1:10
my_thresh <- 5

for (i in my_x) {
  if (i > my_thresh){
    cat('\nValue of ', i, 
        ' is higher than ', 
        my_thresh)
  } else {
    cat('\nValue of ', 
        i, 
        ' is lower or equal than ', 
        my_thresh)
  }
}

#' 
#' If we want to apply more than one logical condi
#' 
## ------------------------------------------------------------------------------------------------------------
for (i in my_x){
  if (i > my_thresh){
    cat('\nValue of ', i, ' is higher than ', my_thresh)
  } else if (i==my_thresh) {
    cat('\nValue of ', i, ' is equal to ', my_thresh)
  } else {
    cat('\nValue of ', i, ' is lower than ', my_thresh)
  }
}

#' 
#' Another possibility of using conditional execut
#' 
## ------------------------------------------------------------------------------------------------------------
# set vec
my_vec <- c('A', 'D', 'B', 'A', 'C', 'B')

for (i_vec in my_vec){
  if (i_vec == 'A'){
    cat('\nGot an A!')
  } else if (i_vec == 'B') {
    cat('\nGot a B!')
  } else if (i_vec == 'C') {
    cat('\nGot a C!')
  } else if (i_vec == 'D') {
    cat('\nGot a D!')	
  }
}

#' 
#' While the previous code works, using several `e
#' 
## ------------------------------------------------------------------------------------------------------------
# set vec
my.vec <- c('A', 'D', 'B', 'A', 'C', 'B')

for (i_vec in my.vec){
  msg.out <- switch(i_vec, 
                    'A' = '\nGot an A!',
                    'B' = '\nGot a B!',
                    'C' = '\nGot a C!',
                    'D' = '\nGot a D!')
  
  cat(msg.out)
  
}

#' 
#' The main benefit of using `switch` is the code 
#' 
#' 
#' ## Using `apply` Functions
#' 
#' An alternative way of using _loops_ in R is to 
#' 
#' It is worth pointing out that all procedures us
#' 
#' Whenever it is possible, give preference to a f
#' 
#' 
#' ### Using `lapply`
#' 
#' Function `base::lapply` takes as input a `list`
#' 
## ------------------------------------------------------------------------------------------------------------
# set list
my_l <- list(c(1, 2, 2), 
             c(2:5, NA), 
             c(10:-20))

# use lapply with mean
my_mean_vec <- lapply(X = my_l, 
                      FUN = mean)

# print result
print(my_mean_vec)

#' 
#' The result shows the means of each vector in `m
#' 
## ------------------------------------------------------------------------------------------------------------
# set list
my_l <- list(c(1, 2, 2), c(2:5, NA), 10:-20)

# use lapply with mean
my_mean_vec <- lapply(X = my_l, 
                      FUN = mean, 
                      na.rm=TRUE)

# print result
print(my_mean_vec)

#' 
#' As we can see, the extra argument `na.rm = TRUE
#' 
#' In particular, using `lapply` is useful when us
#' 
## ------------------------------------------------------------------------------------------------------------
# function to generate files
create_rnd_file <- function(name_file, N = 100){
  # Generates a csv file with random content
  #
  # Args:
  # 	name.file - name of csv file (character)
  #	  N - number of rows in random dataframe (integer)
  #
  # Returns:
  # 	TRUE, if successful
  
  require(tidyverse)
  
  if (class(name_file) != 'character'){
    stop('ERROR: input name.file is not a character')
  }
  
  if ( !(class(N) %in% c('numeric', 'integer')) ){
    stop('ERROR: input N is not an integer or numeric!')
  }
  
  # create random df
  temp_df <- tibble(x = runif(N))
  
  # write it!
  write_csv(x = temp_df, 
            path = name_file)
  
  # return TRUE
  return(TRUE)
}

#' 
#' Now, we use the function with `lapply`: 
#' 
## ------------------------------------------------------------------------------------------------------------
# set options
n_files <- 5
pattern_name <- 'myfiles_with_lapply_'
out_dir <- 'many_datafiles/'

# set file names
file_names <- paste0(out_dir, 
                     pattern_name, 
                     seq(1, n_files), '.csv')

# test if out.dir exists, if not, create it
if (!dir.exists(out_dir)){
  dir.create(out_dir)	
} 

# clean up folder before creating new files
file.remove(list.files(out_dir, 
                       full.names = TRUE))	

# use lapply
out_l <- lapply(X = file_names, 
                FUN = create_rnd_file, 
                N = 100)

# print result
print(out_l)

#' 

#' 
#' As you can see, everything worked well, as expe
#' 
#' Notice the returned object of function `lapply`
#' 
#' 
#' ### Using `sapply`
#' 
#' Function `base::sapply` works similarly to `lap
#' 
## ------------------------------------------------------------------------------------------------------------
# create list
my_l <- list(1:10, 2:5, 10:-20)

# use sapply
my_mean_vec <- sapply(my_l, mean)

# print result
print(my_mean_vec)

#' 
#' Using `sapply` is recommended when the output o
#' 
#' An important aspect of using `sapply` is the un
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set list
my_l <- list(x1 = runif(10), 
             x2 = runif(15), 
             x3 = rnorm(1000))

my_mean_fct <- function(x){
  # Returns mean and standard deviation of a vector
  #
  # Args: 
  #	  x - numerical vector
  #
  # Returns:
  #	  Vector as c(mean(x), sd(x))
  
  if (!(class(x) %in% c('numeric','integer'))){
    stop('ERROR: Class of x is not numeric or integer.')
  }
  
  x <- na.omit(x)
  
  out <- c(Mean = mean(x), 
           StDev = sd(x))
  return(out)
  
}

# use sapply
my_vec <- sapply(my_l, my_mean_fct)

# check result
print(my_vec)

#' 
#' When there is more than one output in the under
#' 
#' A practical use of function `sapply` in data an
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
describe_vec <- function(x){
  # Describe numerical vector with mean and other stats
  #
  # Args:
  #	  x - numerical vector
  #
  # Returns:
  #   A vector with mean, maximum and minimum
  
  # error checking
  if (!(class(x) %in% c('numeric','integer'))){
    stop('ERROR: Class of x is not numeric or integer.')
  }
  
  x <- na.omit(x)
  
  # calc vec
  out <- c(mean_price = mean(x), 
           max_price = max(x), 
           min_price = min(x))
  
  return(out)
}

#' 
#' Now, let's load the data and apply the function
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
library(tidyverse)

# set file and read it
my_f <- afedR::afedR_get_data_file('SP500-Stocks_long.csv')
df_sp500 <- read_csv(my_f,
                     col_types = cols())

# use split to split prices by ticker
my_l <- split(x = df_sp500$price.adjusted, 
              f = df_sp500$ticker)

# use sapply
my_tab <- sapply(X = my_l, FUN = describe_vec)

# check result
print(head(t(my_tab)))

#' 
#' We used function `split` in `split(x = df_sp500
#' 
#' 
#' ### Using `tapply`
#' 
#' Function `tapply` is designed to perform group 
#' 
## ------------------------------------------------------------------------------------------------------------
# set numeric vec and factor
my_x <- 1:150
my_factor <- factor(c(rep('C',50), 
                      rep('B',50), 
                      rep('A',50)))

# use tapply
my_mean_vec <- tapply(X = my_x, INDEX = my_factor, FUN = mean)

# print result
print(my_mean_vec)

#' 
#' A very important point about using `tapply` is 
#' 
#' Going back to the previous example using stock 
#' 
## ------------------------------------------------------------------------------------------------------------
# use tapply for descriptive stats
my_l_out <- tapply(X = df_sp500$price.adjusted, 
                   INDEX = df_sp500$ticker, 
                   FUN = describe_vec)

# print result				   
print(my_l_out[1:5])

#' 
#' The output of `tapply` is a `list` of values. E
#' 
## ------------------------------------------------------------------------------------------------------------
# convert list to the dataframe
my_tab <- do.call(what = bind_rows, 
                  args = my_l_out)

# set ticker column
my_tab <- my_tab %>%
  mutate(ticker = names(my_l_out))

# print result
print(head(my_tab))

#' 
#' It is the first time that command `do.call` is 
#' 
#' Reverting to the example, we can see the result
#' 
#' 
#' ### Using `mapply`
#' 
#' Function `mapply` is a multivariate version of 
#' 
#' Assume we are interested in creating a `list` w
#' 
## ------------------------------------------------------------------------------------------------------------
# set size
N <- 10

# preallocate list
my_l <- list()

for (i in seq(1, N)){
  my_l[[i]] <- seq(1, i)
}

# print result
print(my_l)

#' 
#' Another, less verbose and more elegant solution
#' 
## ------------------------------------------------------------------------------------------------------------
# use mapply for creating list
my_l <- mapply(FUN = seq, 
               rep(1, N), 
               seq(1, N))

print(my_l)

#' 
#' Explaining the result, function `mapply` is cal
#' 
#' 
#' ### Using `apply`
#' 
#' The `apply` function follows the same logic as 
#' 
## ------------------------------------------------------------------------------------------------------------
# set matrix and print it
my_mat <- matrix(1:15, nrow = 5)
print(my_mat)

# sum rows with apply and print it
sum_rows <- apply(X = my_mat, MARGIN = 1, FUN = sum)
print(sum_rows)

# sum columns with apply and print it
sum_cols <- apply(X = my_mat, MARGIN = 2, FUN = sum)
print(sum_cols)

#' 
#' In the previous example, the `MARGIN` argument 
#' 
#' Expanding the example, we can use `apply` to fi
#' 
## ------------------------------------------------------------------------------------------------------------
# print max by row
print(apply(X = my_mat, MARGIN = 1, FUN = max))

# print max by column
print(apply(X = my_mat, MARGIN = 2, FUN = max))

#' 
#' 
#' ### Using `by`
#' 
#' The `by` function differentiates itself because
#' 
#' Look at the next example, where we create a mor
#' 
## ------------------------------------------------------------------------------------------------------------
# load data 
df_sp500 <- read_rds(afedR::afedR_get_data_file(
  'SP500-Stocks-WithRet.rds')
  )

# set function for processing df
describe_vec_with_ret <- function(df_in){
  
  P <- df_in$price.adjusted
  ret <- df_in$ret
  
  out <- c(ticker = df_in$ticker[1],
           MeanPrice= mean(P),
           MaxPrice = max(P),
           MinPrice = min(P),
           MeanRet = mean(ret),
           MaxRet = max(ret),
           MinRet = min(ret))
  
  return(out)
  
}

# apply example_fct for each ticker in df_sp500
my_l <- by(data = df_sp500, 
           INDICES = df_sp500$ticker, 
           FUN = describe_vec_with_ret)

# convert list to dataframe
my_tab <- do.call(what = bind_rows, args = my_l)

# print result
print(head(my_tab))


#' 
#' Function `describe_vec_with_ret` was a requirem
#' 
#' 
#' ## Using package `purrr`
#' 
#' The `tidyverse` universe also offers functions 
#' 
## ------------------------------------------------------------------------------------------------------------
library(purrr)

# set list
my_l <- list(vec1 = 1:10,
             vec2 = 1:50,
             vec3 = 1:5,
             char1 = letters[1:10])

# get length of objects
res_out <- my_l %>%
  map_int(length) %>%
  print()

# find character objects
res_out <- my_l %>%
  map_lgl(is.character) %>%
  print()

#' 
#' Another interesting point about the `purrr` fun
#' 
## ------------------------------------------------------------------------------------------------------------

# set list
my_l <- list(vec1 = c(elem1 = 10, elem2 = 20, elem3 = 5),
             char1 = c(elem1 = 40, elem2 = 50, elem3 = 15))

# get second element of each element in list, by position
res_out <- my_l %>% map(2)
print(res_out)

# get third element of each element in list, by name
res_out <- my_l %>% map('elem3')
print(res_out)

#' 
#' This functionality is very useful because in ma
#' 
#' The great innovation of `purrr` over `base` is 
#' 
#' Using function `safely` is simple. It encapsula
#' 
## ---- error=TRUE---------------------------------------------------------------------------------------------
library(purrr)

example_fct <- function(x) {
  return(x+1)
}

# ERROR
example_fct('a')

#' 
#' Now, let's use `safely` to enclose `example_fct
#' 
## ------------------------------------------------------------------------------------------------------------
# with safely
example_fct_safely <- safely(example_fct)

class(example_fct_safely('a'))

#' 
#' The code `print(example_fct_safely('a'))` resul
#' 
## ------------------------------------------------------------------------------------------------------------
my_l <- list(1:5,
             'a',
             1:4)

res_out <- my_l %>%
  map(safely(example_fct))

print(res_out)

#' 
#' We can easily see that the function had an erro
#' 
## ------------------------------------------------------------------------------------------------------------
# only print results without errors
print(res_out %>% map('result'))

#' 
#' Or just the error messages:
#' 
## ------------------------------------------------------------------------------------------------------------
# only print error messages
print(res_out %>% map('error'))

#' 
#' An interesting option of `safely` is the choice
#' 
## ------------------------------------------------------------------------------------------------------------
my_l <- list(1,
             'a',
             4)

# NA for errors
res_out <- my_l %>%
  map(safely(example_fct,
             otherwise = NA)) %>%
  map_dbl('result')

# print result
print(res_out)

#' 
#' Other functions for controlling errors in `purr
#' 
#' 
#' ### The `purrr::pmap` function
#' 
#' The `purrr::pmap` is one of the best functional
#' 
#' As an example, let's consider a function that b
#' 
## ------------------------------------------------------------------------------------------------------------
build_phrase <- function(name_in, fruit_in, verb_in) {
  my_msg <- paste0('My name is ', name_in,
                   ' and I like to eat ', fruit_in,
                   ' while ', verb_in, '.')
  
  return(my_msg)
}

build_phrase('Joe', 'apple', 'studying')

#' 
#' Function `build_phrase` has three text inputs: 
#' 
## ------------------------------------------------------------------------------------------------------------
names_vec <- c('Joe', 'Kate')
fruits_vec <- c('kiwi', 'apple')
verb_vec <- c('rowing', 'studying')

my_phrases <- character()
for (i_name in names_vec) {
  for (i_fruit in fruits_vec) {
    for (i_verb in verb_vec) {
      my_phrases <- c(my_phrases, 
                      build_phrase(i_name, i_fruit, i_verb))
    }
  }
}

print(my_phrases)

#' 
#' While the code works as expected, a better appr
#' 
## ------------------------------------------------------------------------------------------------------------
df_grid <- expand.grid(names_vec = names_vec,
                       fruits_vec = fruits_vec,
                       verb_vec = verb_vec)

l_args <- list(name_in = df_grid$names_vec,
               fruit_in = df_grid$fruits_vec,
               verb_in = df_grid$verb_vec)

my_phrases <- purrr::pmap(.l = l_args, 
                          .f = build_phrase)

print(my_phrases)

#' 
#' Do notice that the names in `l_args` match the 
#' 
## ------------------------------------------------------------------------------------------------------------
print(as.character(my_phrases))

#' 
#' If necessary, we can also set fixed arguments i
#' 
## ------------------------------------------------------------------------------------------------------------
l_args <- list(name_in = names_vec,
               fruit_in = 'orange',
               verb_in = 'studying')

my_phrases <- purrr::pmap(.l = l_args, 
                          .f = build_phrase)

print(my_phrases)

#' 
#' Whenever you have a situtation where a nested l
#' 
#' 
#' ## Data Manipulation with Package `dplyr`
#' 
#' Package `dplyr` [@dplyr] is very handy for data
#' 

#' 
#' 
#' In its current version, `r my_ver`, `dplyr` has
#' 
#' 
#' ### Group Operations with `dplyr`
#' 
#' The greatest functionality of `dplyr` is in per
#' 
#' To illustrate the use of the functions `group_b
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# load data
my_f <- 'data/SP500-Stocks-WithRet.rds'
df_sp500 <- readRDS(my_f)

# group data and calculate stats
my_tab <- df_sp500 %>%
  group_by(ticker) %>%
  summarise(mean_price = mean(price.adjusted),
            max_price = max(price.adjusted),
            min_price = min(price.adjusted),
            max_ret = max(ret),
            min_ret = min(ret))

# check result
print(my_tab)

#' 
#' The first step in using `dplyr` is to group the
#' 
#' After we group the data in line `group_by(ticke
#' 
#' Using `dplyr` is highly recommended when you ha
#' 
## ------------------------------------------------------------------------------------------------------------
# set new col week.day
df_sp500 <- df_sp500 %>%
  mutate(week_day = weekdays(ref.date))

# check result
glimpse(df_sp500)

#' 
#' Now, we proceed by adding column `week_day` in 
#' 
## ------------------------------------------------------------------------------------------------------------
# group by ticker and weekday, calculate stats
my_tab <- df_sp500 %>%
  group_by(ticker, week_day) %>%
  summarise(mean_price = mean(price.adjusted), 
            max_price = max(price.adjusted), 
            min_price = min(price.adjusted),
            max.ret = max(ret),
            min.ret = min(ret))

# print result						  
print(my_tab)

#' 
#' And that's it! To group the data to a new `fact
#' 
#' 
#' ### Complex Group Operations with `dplyr`
#' 
#' The previous example shows a simple case of gro
#' 
#' Package `dplyr` also supports more complex oper
#' 
#' Let’s look at the following example, where we u
#' 
## ------------------------------------------------------------------------------------------------------------
# simulated vector of returns
ret <- c(0, rnorm(4, sd= 0.05))

# vector of accumulated returns
acum_ret <- cumprod(1+ret)
print(acum_ret)

#' 
#' Vector `acum_ret` represents a multiplier of an
#' 
## ------------------------------------------------------------------------------------------------------------
library(dplyr)

# get acum ret of stocks
my_tab <- df_sp500 %>%
  group_by(ticker) %>%
  do(acum_ret = cumprod(1+.$ret)) %>%
  mutate(last_cumret = acum_ret[length(acum_ret)],
         min_cumret = min(acum_ret))

print(head(my_tab))

#' 
#' Notice how column `acum_ret` is not a single va
#' 
#' The greatest advantage of using complex group o
#' 
#' In sum, the `dplyr` package was a very signific
#' 
#' 
#' ## Exercises
#' 
#' 1.  Create a function called `say_my_name` that
#' 
#' 2.  Considering the previous `say_my_name` func
#' 
#' 3.  Download a database of popular Canadian bab
#' 
#' 4.  Redo the previous exercise 3 using `sapply`
#' 
#' 5.  Use package `BatchGetSymbols` to download v
#' 
#' 6.  Redo the previous exercise using the `dplyr
#' 
#' 7.  With the dataset of names from exercise 3, 
#' 
#' 8.  CHALLENGE - In [Rstudio CRAN logs](http://c