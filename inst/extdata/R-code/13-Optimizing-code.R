#' # Optimizing Code {#optimizing}
#' 

#' 
#' In this chapter, we will study how to be more e
#' 
#' However, before we start you need to understand
#' 
#' 
#' ## Optimizing your Programming Time
#' 
#' The first topic you must consider when thinking
#' 
#' DRY (don't repeat yourself)
#' : Whenever you find yourself writing similar co
#' 
#' KISS (keep it simple and _stunning_)
#' : The simplest and most-straightforward your co
#' 
#' Folder structure
#' : Organize all elements of the scripts in relat
#' 
#' Comments are your timeless friend
#' : Even when writing just to yourself, keep comm
#' 
#' Keep a clear notation
#' : Code notation is the personal touch you bring
#' 
#' **Filenames and paths:** Names and paths of fil
#' 
## ------------------------------------------------------------------------------------------------------------
# GOOD
my_f <- '01_Run_Research.R'
my_f <- '02_Import_and_Clean_Data.R'
my_f <- 'data/gdp.rds'
my_f <- 'fcts/report_functions.R'

# BAD
my_f <- 'functions.R'
my_f <- 'R/script.R'
my_f <- 'New folder/script_ver_03.R'
my_f <- 'New folder/script_ver_05_with_clean_data.R'


#' 
#' **Sections of code**: Use dashes within the scr
#' 
## ------------------------------------------------------------------------------------------------------------
# Import data ----


# Clean data ----


# Estimate models ----

#' 
#' **Variable and function names**: Use these guid
#' 
#' - Give preference to lowercase characters when 
#' - Keep names short, concise and intuitive (easi
#' - Use the first few words to identify the class
#' - Avoid the use of dots (`.`) when connecting w
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## # GOOD
## my_seq <- 1:100 # simple and clean
## df_prices <- tibble()  # for sure it is a dataframe with prices!
## fct_plot_prices <- function() # I know what it does before executing it!
##   l_args <- list() # also nice and clean
## 
## # BAD
## DF <- tibble() # all uppercase and generic
## DataFrame <- tibble() # camel case and same name as object
## list <- list() # Same name as constructor. Does it even work?
## # It does..
## Prices_of_Facebook <- tibble() # very informative,
## # but too long!
## DATAFRAME_WITH_SPECIAL_DATA <- tibble() # too long!
## # Why SHOUT in code?
## # be nice
## df.prices <- tibble() # use of dots

#' 
#' **Other code conventions**:
#' 
#' - Always put a space around operators (`=`, `>`
#' 
## ------------------------------------------------------------------------------------------------------------
# GOOD
x <- 10
flag <- (x > 0)

# BAD
x<-0
flag<-x>0

#' 
#' - Always set a space after using comma, just li
#' 
## ---- eval = FALSE-------------------------------------------------------------------------------------------
## # GOOD
## my_fct(1, 2, 3)
## my_x <- c(1, 2, 3)
## 
## # BAD
## my_fct(1,2,3)
## my_x <- c(1,2,3)

#' 
#' These are simple and important rules you can fo
#' 
#' 
#' ## Optimizing Code Speed
#' 
#' For most R users, code execution time is not mu
#' 
#' Moreover, time can also be abundant. For exampl
#' 
#' The execution time becomes an issue when the R 
#' 
#' Back to the code, optimizing speed in R scripts
#' 
#' 
#' ### Profiling Code
#' 
#' There are different routes to profile an R code
#' 
#' As an example, I'll first write a function that
#' 
## ------------------------------------------------------------------------------------------------------------
my_bench_fct <- function() {
  
  require(tictoc)
  require(tidyverse)
  
  cat('01-Set parameters\n')
  my_n <- 1000000
  
  cat('02-Build variables\n')
  x <- runif(my_n)
  y <- x + rnorm(my_n)
  
  cat('03-Pause for a while -- the bottleneck\n')
  profvis::pause(1)
  
  cat('04-Estimate a linear model\n')
  lm_model <- lm(data = tibble(x = x, y = y), 
                 formula = y ~ x)
  
  return(lm_model)
}

out <- my_bench_fct()

#' 
#' Whenever you make a call to function `my_bench_
#' 
#' For complex and extensive code, however, using 
#' 
#' Function `base::Rprof` works by first calling i
#' 
## ---- results='hold'-----------------------------------------------------------------------------------------
# set temporary file for results
profiling_file <-  tempfile(pattern = 'profiling_example', 
                            fileext = '.out')

# initialize profiling
Rprof(filename = profiling_file)

cat('01-Set parameters\n')
my_n <- 1000000

cat('02-Build variables\n')
x <- runif(my_n)
y <- x + rnorm(my_n)

cat('03-Pause for a while -- the bottleneck\n')
profvis::pause(1)

cat('04-Estimate a linear model\n')
lm_model <- lm(data = tibble(x = x, y = y), 
               formula = y ~ x)

# stop profiling
Rprof(NULL)

#' 
#' The actual results can be imported with `base::
#' 
## ------------------------------------------------------------------------------------------------------------
# check results
df_res <- summaryRprof(profiling_file)$by.total

# print it
print(head(df_res))

#' 
#' In this `dataframe` we see the top 5 lines of c
#' 
#' Another solution for profiling is package `prof
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## library(profvis)
## 
## # use profvis
## profiling <- profvis(expr = {
##   require(tictoc)
##   require(tidyverse)
## 
##   cat('01-Set parameters\n')
##   my_n <- 1000000
## 
##   cat('02-Build variables\n')
##   x <- runif(my_n)
##   y <- x + rnorm(my_n)
## 
##   cat('03-Pause for a while -- the bottleneck\n')
##   profvis::pause(1)
## 
##   cat('04-Estimate a linear model\n')
##   lm_model <- lm(data = tibble(x = x, y = y),
##                  formula = y ~ x)
## 
## })
## 
## # create visualization
## htmlwidgets::saveWidget(profiling , "profile.html")
## 
## # Can open in browser from R
## browseURL("profile.html")

#' 
#' The result will be similar to Figure \@ref(fig:
#' 

#' 
#' Again we find the same result -- line 13 (`prof
#' 
#' 
#' ### Simple Strategies to Improve Code Speed
#' 
#' Once you identify the bottleneck in your code, 
#' 
#' 
#' #### Use Vector Operations
#' 
#' Whenever you are working with atomic vectors in
#' 
#' As an example, let's look at the case of buildi
#' 
## ---- results='hold'-----------------------------------------------------------------------------------------
library(tictoc)

N <- 10000000
x <- 1:N

tic('Using loops without preallocation') # start timer
y <- numeric()
for (i in seq_along(x)) {
  y[i] <- x[i] + 1
}
toc() # end timer


tic('Using loops with preallocation') # start timer
y <- numeric(length = N)
for (i in seq_along(x)) {
  y[i] <- x[i] + 1
}
toc() # end timer

tic('Using vectors') # start timer
y <- x + 10
toc() # end timer

#' 
#' In the first version with loop, we set `y <- nu
#' 
#' The lesson here is: **always seek vectorized ve
#' 
#' 
#' #### Repetitive binding of `dataframes`
#' 
#' Another common mistake when it comes to R code 
#' 
#' For that, let's explore an example with some ra
#' 
## ---- results='hold'-----------------------------------------------------------------------------------------
library(tidyverse)

n_dfs <- 1000 # number of dataframes to bind
n_obs <- 1000 # number of observations in each dataframe

tic('Binding dataframes within the loop')
my_df <- tibble()
for (i in 1:n_dfs) {
  temp_df <- tibble(x = runif(n_obs),
                    y = rnorm(n_obs))
  
  my_df <- bind_rows(my_df, temp_df)
  
}
toc()

tic('Using lists within the loop, bind it later')
my_l <- list()
for (i in 1:n_dfs) {
  temp_df <- tibble(x = runif(n_obs),
                    y = rnorm(n_obs))
  
  my_l <- c(my_l, list(temp_df))
}

my_df <- bind_rows(my_l)
toc()

#' 
#' As you can see, the difference is significant. 
#' 
#' 
#' ### Using C++ code (package `Rcpp`)
#' 
#' Package `Rcpp` [@rcpp] is a great example of ho
#' 
#' Have a look in the next example, where we write
#' 
## ------------------------------------------------------------------------------------------------------------
library(Rcpp)
library(tictoc)

sum_R_looped <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  return(total)
}

sum_R_vector <- function(x) {
  total <- sum(x)
  return(total)
}

cppFunction('double sum_C(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

#' 
#' Using `cppFunction` is straightforward. Its inp
#' 
#' Now, let's test all three functions with a larg
#' 
## ---- results='hold'-----------------------------------------------------------------------------------------
x <- 1:5000000

tic('Sum with R loops')
out1 <- sum_R_looped(x)
toc()

tic('Sum with R (vectorized)')
out2 <- sum_R_vector(x)
toc()

tic('Sum with C++ (rcpp)')
out3 <- sum_C(x)
toc()

#' 
#' The results are as expected. The case with the 
#' 
#' Whenever you have a numerical bottleneck in you
#' 
#' 
#' ### Using cache (package `memoise`)
#' 
#' A very underestimated feature of R is the use o
#' 
#' Moreover, caching works perfectly with determin
#' 
#' Particularly, caching works very well in the im
#' 
#' While you can write your own caching system by 
#' 
## ------------------------------------------------------------------------------------------------------------
sleeping_beauty <- function(arg1, arg2) {
  # Simple example function that will sleep for one second
  #
  # ARGS: arg1 - anything 
  #       arg2 - anything
  # RETURNS: A list
  
  profvis::pause(1)
  
  return(list(arg1, arg2))
}

#' 
#' The first step in using `memoise` is setting th
#' 
## ---- include=FALSE------------------------------------------------------------------------------------------
my_dir <- 'mem_cache'
if (dir.exists(my_dir)) fs::dir_delete(my_dir)

#' 
## ------------------------------------------------------------------------------------------------------------
library(memoise)

my_cache_folder <- cache_filesystem(path = 'mem_cache')

#' 
#' The next step is telling `memoise` that we have
#' 
## ------------------------------------------------------------------------------------------------------------
mem_sleeping_beauty <- memoise(f = sleeping_beauty, 
                               cache = my_cache_folder)

#' 
#' Now, let's call the function with different arg
#' 
## ---- results='hold'-----------------------------------------------------------------------------------------
library(memoise)
library(tictoc)

tic('    sleeping_beauty:\t arg1 = 1, arg2 = 2')
out1 <- sleeping_beauty(1, 2)
toc()

tic('mem_sleeping_beauty:\t arg1 = 1, arg2 = 2')
out1 <- mem_sleeping_beauty(1, 2)
toc()

tic('    sleeping_beauty:\t arg1 = 1, arg2 = 2')
out1 <- sleeping_beauty(1, 2)
toc()

tic('mem_sleeping_beauty:\t arg1 = 1, arg2 = 2')
out1 <- mem_sleeping_beauty(1, 2)
toc()

#' 
#' Function `sleeping_beauty` is the original code
#' 
#' Going further, if we change the arguments of `m
#' 
## ---- results='hold'-----------------------------------------------------------------------------------------
tic('mem_sleeping_beauty:\t arg1 = 2, arg2 = 2')
out1 <- mem_sleeping_beauty(2, 2)
toc()

tic('mem_sleeping_beauty:\t arg1 = 2, arg2 = 2')
out2 <- mem_sleeping_beauty(2, 2)
toc()

tic('mem_sleeping_beauty:\t arg1 = 5, arg2 = 1')
out3 <- mem_sleeping_beauty(5, 1)
toc()

#' 
#' Looking at folder `mem_cache` we find the actua
#' 
## ------------------------------------------------------------------------------------------------------------
mem_files <- list.files('mem_cache/')

print(mem_files)

#' 
#' These are just _rds_ files with the content of 
#' 
#' Caching is one of the best techniques for impro
#' 
#' 
#' #### Using parallel processing (package `furrr`
#' 
#' Whenever you are running an R script, a single 
#' 
#' Parallel processing relates to using more than 
#' 
#' Before we start, understand that not every prob
#' 
#' A typical case in data analysis is importing a 
#' 
#' First, let's write a function that will create 
#' 
## ------------------------------------------------------------------------------------------------------------
create_file <- function(n_obs, folder_to_save) {
  # Create files in the computer
  #
  # ARGS: n_obs - Number of observations in dataframe
  #       folder_to_save - Where to save files
  # RETURNS: True, if successful
  
  require(tidyverse)
  
  
  temp_df <- tibble(x = runif(n_obs),
                    y = rnorm(n_obs))
  
  temp_file <- tempfile(pattern = 'file', tmpdir = folder_to_save, 
                        fileext = '.csv')
  
  write_csv(temp_df, 
            path = temp_file)
  
  return(TRUE)
}

#' 
#' So, with the function completed, its time to ca
#' 
## ------------------------------------------------------------------------------------------------------------
library(purrr)

n_files <- 1000
n_obs <- 10000
folder_to_save <- file.path(tempdir(), 'many files')

dir.create(folder_to_save)

pwalk(.l = list(n_obs = rep(n_obs, n_files), 
                folder_to_save = rep(folder_to_save, 
                                     n_files)), 
      create_file)

#' 
#' Now we read back those files with two strategie
#' 
#' Before we start, we need to set up our machine 
#' 
## ------------------------------------------------------------------------------------------------------------
n_cores_available <- future::availableCores()

print(n_cores_available)

#' 
#' The machine in which the book was compiled has 
#' 
#' This code will run both versions of the same op
#' 
## ------------------------------------------------------------------------------------------------------------
library(furrr)
library(tictoc)

# get files
my_files <- list.files(path = folder_to_save, 
                       full.names = TRUE)

# setup for multicore
n_cores <- 10

# set the number of cores and type of parallel
plan(strategy = multisession, workers = n_cores)

tic('Sequential with pmap (1 core)')
l_out_1 <- pmap(
  .l = list(file = my_files, 
            col_types = rep(list(cols()), 
                            length(my_files)) ), 
  .f = readr::read_csv
)
toc()

tic(paste0('Parallel with future_pmap (', 
           n_cores, ' cores)'))
l_out_2 <- future_pmap(
  .l = list(file = my_files,
            col_types = rep(list(cols()), 
                            length(my_files)) ), 
  
  .f = readr::read_csv
)
toc()

identical(l_out_1, l_out_2)

#' 
#' Notice the gain in speed is not tenfold. When c
#' 
#' In conclusion, parallel computing works better 
#' 
#' 
#' ## Exercises
#' 
#' 01. Consider this code:
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(forecast)
library(BatchGetSymbols)

ticker <- '^GSPC'
df_prices <- BatchGetSymbols(tickers = ticker, 
                             first.date = '2010-01-01')[[2]]

my_arima <- auto.arima(df_prices$ret.adjusted.prices)
summary(my_arima)

#' 
#' Use `Rprof` and `profvis` to identify the bottl
#' 
#' 02. Use package `Rcpp` to write and use a C++  
#' 
#' 03. Use package `tictoc` to compare the perform
#' 
#' 04. Use package `memoise` to create a memorized
#' 
#' 