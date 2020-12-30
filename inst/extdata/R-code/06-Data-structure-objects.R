#' # Dataframes and other objects {#data-structure
#' 

#' 
#' **In R, everything is an object with its own pr
#' 
#' The basic object classes in R include numeric v
#' 
#' To avoid that, a simpler way to organize our da
#' 
#' 
#' ## `Dataframes`
#' 
#' Without a doubt, the `dataframe` class is the m
#' 
#' **A `dataframe` can organize our work significa
#' 
#' Another positive aspect of using the `dataframe
#' 
#' 
#' ### Creating `dataframes`
#' 
#' The `dataframe` object is one of R's native cla
#' 
#' We call function `tibble::tibble` to create a `
#' 
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
library(tidyverse)

# set tickers
tickers <- c(rep('AAP',5), 
             rep('COG', 5), 
             rep('BLK', 5), 
             rep('CAM',5) )

# set a date vector
dates <- as.Date(rep(c("2010-01-04", "2010-01-05", "2010-01-06", 
                       "2010-01-07", "2010-01-08"), 4) )

# set prices					  
prices <- c(40.38,  40.14,  40.49,  40.48,  40.64,
            46.23,  46.17,  45.97,  45.56,  45.46,
            238.58, 239.61, 234.67, 237.25, 238.92,
            43.43,  43.96,  44.26,  44.5,   44.86)

# create tibble/dataframe
my_df <- tibble(tickers, dates, prices)

# print its first 5 rows
print(head(my_df))

#' 
#' We used the function `rep` to replicate and fac
#' 

#' 
#' The advantage of using the viewer is that you c
#' 
#' 
#' ### Inspecting a Dataframe
#' 
#' Once you have a dataframe in your R session, th
#' 
#' - Properly defined column's names and classes;
#' - Correct number of rows and columns;
#' - The existence (or not) of missing data (`NA`)
#' 
#' We often have no control over how we get our da
#' 
#' It is also very important to make sure that the
#' 
#' We should also check for the number of `NA` val
#' 
#' Back to the code, one of the most recommended f
#' 
## ------------------------------------------------------------------------------------------------------------
# check content of my_df
glimpse(my_df)

#' 
#' Usually, the use of `glimpse` is sufficient to 
#' 
## ------------------------------------------------------------------------------------------------------------
# check variation my_df
summary(my_df)

#' 
#' The objective of `summary` is to provide a gras
#' 
#' A more visual way of inspecting `dataframes` is
#' 
## ------------------------------------------------------------------------------------------------------------
library(inspectdf)

# inspect categories of columns
show_plot(inspect_cat(my_df))

# inspect NA values
show_plot(inspect_na(my_df))

# inspect memory size of columns
show_plot(inspect_mem(my_df))

#' 
#' If you prefer visual analysis, `inspectdf` can 
#'   
#' 
#' ### The _pipeline_ Operator (`%>%`)
#' 
#' An important feature of the `tidyverse` univers
#' 
#' Imagine a situation where we have three functio
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## my_tab <- my_df %>%
##   fct1(arg1) %>%
##   fct2(arg2) %>%
##   fct3(arg3)

#' 
#' We use symbol `%>%` at the end of each line to 
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## # version 1
## my_tab <- fct3(fct2(fct1(my_df,
##                          arg1),
##                     arg2),
##                arg1)
## 
## # version 2
## temp1 <- fct1(my_df, arg1)
## temp2 <- fct2(temp1, arg2)
## my_tab <- fct3(temp1, arg3)

#' 
#' Notice how the alternatives result in a messy c
#' 
#' 
#' ### Accessing Columns
#' 
#' To discover the names of the columns of a `data
#' 
## ------------------------------------------------------------------------------------------------------------
# get names of columns with names
names(my_df)
colnames(my_df)

#' 
#' Both can also modify column names:
#' 
## ------------------------------------------------------------------------------------------------------------
# set temp df
temp_df <- my_df

# change names
names(temp_df) <- paste('Col', 1:ncol(temp_df))

# check names
names(temp_df)

#' 
#' In this example, the way we use `names` differs
#' 
#' To access a particular column of a `dataframe`,
#' 
## ------------------------------------------------------------------------------------------------------------
# isolate columns of df
my_tickers <- my_df$tickers
my_prices <- my_df['prices']
my_dates <- my_df[ ,2]

# print the results
print(head(my_tickers))
print(head(my_prices))
print(head(my_dates))

#' 
#' It's worth knowing that, internally, dataframes
#' 
## ------------------------------------------------------------------------------------------------------------
# select column in dataframe with list notation
print(my_df[[2]])

#' 
#' To access specific rows and columns of a `dataf
#' 
## ------------------------------------------------------------------------------------------------------------
# accessing rows 1:5, column 2
print(my_df[1:5, 2])

# accessing rows 1:5, columns 1 and 2
print(my_df[1:5, c(1,2)])

# accessing rows 1:5, all columns
print(my_df[1:5, ])

#' 
#' Column selection can also be performed using na
#' 
## ------------------------------------------------------------------------------------------------------------
# selecting rows 1 to 3, columns 'ticker' and 'prices'
print(my_df[1:3, c('tickers', 'prices')])

#' 
#' Or, using the pipeline operator and function  `
#' 
## ------------------------------------------------------------------------------------------------------------
my.temp <- my_df %>%
  select(tickers, prices) %>%
  slice(1:3) %>%
  glimpse()

#' 
#' 
#' ### Modifying a `dataframe`
#' 
#' To create new columns in a dataframe, simply us
#' 
## ------------------------------------------------------------------------------------------------------------
# add columns with mutate
my_df <- my_df %>%
  mutate(ret = prices/lag(prices) -1,
         seq_1 = 1:nrow(my_df),
         seq_2 =  seq_1 +9) %>%
  glimpse()

#' 
#' All new columns are defined as arguments in `dp
#' 
#' Another, more traditional way of creating new c
#' 
## ------------------------------------------------------------------------------------------------------------
# add new column with base R
my_df$seq_3 <- 1:nrow(my_df)

# check it
glimpse(my_df)

#' 
#' Therefore, you can use `$` to either access or 
#' 
#' Going further, if we try to create a column wit
#' 
## ---- eval=FALSE, error=TRUE---------------------------------------------------------------------------------
## my_df <- my_df %>%
##   mutate(seq_3 =  1:100) %>%
##   glimpse()

#' 
## ---- echo=FALSE---------------------------------------------------------------------------------------------
cat('Error: Column `seq_3` must be length 20 (the number of rows) ...')

#' 
#' However, due to the simplified recycling rule, 
#' 
## ------------------------------------------------------------------------------------------------------------
my_df <- my_df %>%
  mutate(seq_3 =  1) %>%
  glimpse()

#' 
#' To remove columns from a `dataframe`, use funct
#' 
## ------------------------------------------------------------------------------------------------------------
# removing columns
my_df.temp <- my_df %>%
  select(-seq_1, -seq_2, -seq_3) %>%
  glimpse()

#' 
#' Using base R, the traditional way of removing c
#' 
## ------------------------------------------------------------------------------------------------------------
# set temp df
temp_df <- my_df

# remove cols
temp_df$prices <- NULL
temp_df$dates  <- NULL
temp_df$ret  <- NULL
temp_df$tickers  <- NULL

# check it
glimpse(temp_df)

#' 
#' 
#' ### Filtering rows of a `dataframe`
#' 
#' A fairly common `dataframe` operation in R is t
#' 
## ------------------------------------------------------------------------------------------------------------
# filter df for single stock
my_df.temp <- my_df %>%
  filter(tickers == 'COG') %>%
  glimpse()

#' 
#' We can go further and also filter data for `'CO
#' 
## ------------------------------------------------------------------------------------------------------------
# filter df for single stock and date
my_df.temp <- my_df %>%
  filter(tickers == 'COG',
         dates > as.Date('2010-01-05')) %>%
  glimpse()

#' 
#' Here we used symbol `==` to test for equality i
#' 
#' 
#' ### Sorting a `dataframe`
#' 
#' After creating or importing a `dataframe`, we c
#' 
#' As an example, consider creating a `dataframe` 
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set new df
my_df <- tibble(col1 = c(4, 1, 2), 
                col2 = c(1, 1, 3), 
                col3 = c('a','b','c'))

# print it					
print(my_df)

#' 
#' We use function  `dplyr::arrange` and the _pipe
#' 
## ------------------------------------------------------------------------------------------------------------
# sort ascending, by col1 
my_df <- my_df %>%
  arrange(col1) %>%
  print()

#' 
#' We can also sort by descending values using `de
#' 
## ------------------------------------------------------------------------------------------------------------
# sort descending, col1 and col2
my_df <- my_df %>%
  arrange(desc(col1)) %>%
  print()

#' 
#' And, for multiple columns, using extra argument
#' 
## ------------------------------------------------------------------------------------------------------------
# sort ascending, by col2 and col1
my_df <- my_df %>%
  arrange(col2, col1) %>%
  print()

#' 
#' As for base R, function `order` returns the pos
#' 
## ------------------------------------------------------------------------------------------------------------
# set index with positions of ascending order in col1
idx <- order(my_df$col1)

# print it
print(idx)

#' 
#' Therefore, when using the output of function `o
#' 
## ------------------------------------------------------------------------------------------------------------
# order my_df by col1
my_df.2 <- my_df[order(my_df$col1), ]

# print result
print(my_df.2)

#' 
#' This operation may also be performed considerin
#' 
## ------------------------------------------------------------------------------------------------------------
# sort df with col2 and col1
my_df.3 <- my_df[order(my_df$col2, my_df$col1), ]

# print result
print(my_df.3)

#' 
#' 
#' ### Combining and Aggregating `dataframes`
#' 
#' In the practice of manipulating data, often you
#' 
## ------------------------------------------------------------------------------------------------------------
# set two dfs with same colnames
my_df_1 <- tibble(col1 = 1:5, 
                  col2 = rep('a', 5))
my_df_2 <- tibble(col1 = 6:10, 
                  col2 = rep('b', 5))

# bind them by rows
my_df <- bind_rows(my_df_1, my_df_2)

# print result
print(my_df)

#' 
#' Notice that, in the previous example, the names
#' 
#' Another interesting aspect of `dplyr::bind_rows
#' 
## ---- eval=TRUE, tidy=FALSE----------------------------------------------------------------------------------
# set two df with different colnames
my_df_1 <- tibble(col1 = 1:5, 
                  col2 = rep('a', 5))
my_df_2 <- tibble(col1 = 6:10, 
                  col3 = rep('b', 5))

# bind them by rows (NA values for missing cols)
my_df <- bind_rows(my_df_1, 
                   my_df_2)

# print result
print(my_df)

#' 
#' For the case of column bind with function `dply
#' 
## ------------------------------------------------------------------------------------------------------------
# set two dfs
my_df_1 <- tibble(col1 = 1:5, 
                  col2 = rep('a', 5))
my_df_2 <- tibble(col3 = 6:10, 
                  col4 = rep('b', 5))

# column bind dfs
my_df <- cbind(my_df_1, my_df_2)

# print result
print(my_df)

#' 
#' Sometimes, aggregating different tables won't b
#' 
#' For that, you can use functions `dplyr::join*` 
#' 
## ------------------------------------------------------------------------------------------------------------
# set df
my_df_1 <- tibble(date = as.Date('2016-01-01')+0:10,
                  x = 1:11)

my_df_2 <- tibble(date = as.Date('2016-01-05')+0:10,
                  y = seq(20,30, length.out = 11))


#' 
#' Please do notice that both dataframes share a c
#' 
## ------------------------------------------------------------------------------------------------------------
# aggregate tables
my_df <- inner_join(my_df_1, 
                    my_df_2)

glimpse(my_df)

#' 
#' Now with `dplyr::full_join`:
#' 
## ------------------------------------------------------------------------------------------------------------
# aggregate tables
my_df <- full_join(my_df_1, 
                   my_df_2)

glimpse(my_df)

#' 
#' Notice the difference in the number of rows fro
#' 
#' If we had `dataframes` with different column na
#' 
## ------------------------------------------------------------------------------------------------------------
# set df
my_df_3 <- tibble(ref_date = as.Date('2016-01-01')+0:10,
                  x = 1:11)

my_df_4 <- tibble(my_date = as.Date('2016-01-05')+0:10,
                  y = seq(20,30, length.out = 11))

# join by my_df.3$ref.date and my_df.4$my.date
my_df <- inner_join(my_df_3, my_df_4,
                    by = c('ref_date' = 'my_date'))

glimpse(my_df)

#' 
#' Whenever you need to combine tables that share 
#' 
#' 
#' ### Extensions of the `dataframe` Class
#' 
#' As mentioned in the previous chapter, one benef
#' 
#' For example, it is common in economic and finan
#' 
#' See the following example, where we represent t
#' 
## ---- tidy=FALSE, message=FALSE------------------------------------------------------------------------------
# load pkg
library(xts)

# set ticker symbols as a vector
tickers <- c('AAP', 'COG', 'BLK', 'CAM')

# set a date vector
dates <- as.Date(c("2010-01-04", "2010-01-05", "2010-01-06", 
                   "2010-01-07", "2010-01-08"))

# set prices as  matrix					  
price_matrix <- matrix(c(40.38,  40.13,  40.49,  40.48,  40.63,
                         46.23,  46.16,  45.97,  45.56,  45.45,
                         238.58, 239.61, 234.66, 237.25, 238.91,
                         43.43,  43.95,  44.25,  44.5,   44.86),
                       nrow = length(dates))

# set xts object
my_xts <- xts(price_matrix, order.by = dates)

# set colnames
colnames(my_xts) <- tickers

# print it
print(my_xts)

# show its class
class(my_xts)

#' 
#' In creating the `xts` object, notice how the ti
#' 
#' The previous code can give the impression that 
#' 
## ------------------------------------------------------------------------------------------------------------
# set number of time periods
N <- 500

# create matrix with data
my_mat <- matrix(c(seq(1, N), seq(N, 1)), nrow=N)

# set xts object
my_xts <- xts(my_mat, order.by = as.Date('2016-01-01')+1:N)

# apply mean function for each weel
my_xts_weekly_mean <- apply.weekly(my_xts, mean)

# print result
print(head(my_xts_weekly_mean))

#' 
#' In finance and economics, these time aggregatio
#' 
#' Package `xts` is not alone as an alternative to
#' 
#' 
#' ### Other Useful Functions for Handling `datafr
#' 
#' **head** Returns the first `n` rows of a `dataf
#' 
## ------------------------------------------------------------------------------------------------------------
# set df
my_df <- tibble(col1 = 1:5000, 
                col2 = rep('a', 5000))

# print its first 5 rows
print(head(my_df, 5))

#' 
#' **tail** - Returns the last `n` rows of a `data
#' 
## ------------------------------------------------------------------------------------------------------------
# print its last 5 rows
print(tail(my_df, 5))

#' 
#' **complete.cases** - Returns a logical vector w
#' 
## ------------------------------------------------------------------------------------------------------------
# create df
my_df <- tibble(x = c(1:5, NA, 10),
                y = c(5:10, NA))

# show df
print(my_df)

# print logical test of complete.cases
print(complete.cases(my_df))

# print all rows where there is at least one NA
print(which(!complete.cases(my_df)))

#' 
#' **na.omit** - Returns a `dataframe` without the
#' 
## ------------------------------------------------------------------------------------------------------------
print(na.omit(my_df))

#' 
#' **unique** - Returns a `dataframe` where all du
#' 
## ------------------------------------------------------------------------------------------------------------
# set df with repeating rows
my_df <- data.frame(col1 = c(1, 1, 2, 3, 3, 4, 5), 
                    col2 = c('A', 'A', 'A', 'C', 'C', 'B', 'D'))

# print it					
print(my_df)

# print unique df
print(unique(my_df))

#' 
#' 
#' ## `Lists`
#' 
#' A `list` is a flexible container that can hold 
#' 
#' 
#' ### Creating `lists`
#' 
#' A `list` can be created with the `base::list` c
#' 
## ------------------------------------------------------------------------------------------------------------
# create list
my_l <- list(c(1, 2, 3),
             c('a', 'b'),
             factor('A', 'B', 'C'),
             data.frame(col1 = 1:5))

# use base::print
print(my_l)

# use dplyr::glimpse
glimpse(my_l)

#' 
#' Notice how we gather four objects: a numeric ve
#' 
#' Following other objects, the elements of a `lis
#' 
## ------------------------------------------------------------------------------------------------------------
# set named list
my_named_l <- list(tickers = 'CMPY',
                   markets = 'NYSE',
                   df_prices = data.frame(P = c(1,1.5,2,2.3),
                                          ref_date = Sys.Date()+0:3))

# check content
glimpse(my_named_l)

#' 
#' The information is self-contained in a single o
#' 
#' 
#' ### Accessing the Elements of a `list`
#' 
#' As mentioned, the individual elements of a `lis
#' 
## ------------------------------------------------------------------------------------------------------------
# accessing elements from list
print(my_named_l[[2]])
print(my_named_l[[3]])

#' 
#' You can also access the elements of a `list` wi
#' 
## ------------------------------------------------------------------------------------------------------------
# set list
my_l <- list('a',
             c(1, 2, 3),
             factor('a', 'b'))

# check classes
class(my_l[[2]])
class(my_l[2])

#' 
#' If we try to add an element to `my_l[2]`, we wi
#' 
## ----error=TRUE----------------------------------------------------------------------------------------------
# adding an element to a list (WRONG)
my_l[2] + 1

#' 
#' An error is returned because a `list` object ca
#' 
## ------------------------------------------------------------------------------------------------------------
# set new list with the first and second element of my_l
my_new_l <- my_l[c(1,2)]

# print result
print(my_new_l)

#' 
#' With the named lists, we can access its element
#' 
#' Next, we provide several examples of how to acc
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## # different ways to access a list
## my_named_l$tickers
## my_named_l$markets
## my_named_l[['tickers']]
## my_named_l[['markets']]

#' 
#' Another useful trick for working with lists is 
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
my_l <- list(slot1 = c(num1 = 1, 
                       num2 = 2, 
                       num3 = 3), 
             slot2 = c('a', 'b'))

# access the second value of the first element of my_l
print(my_l[[1]][2])

# access the first value of the second element of my_l
print(my_l[[2]][1])

# access the value 'num3' in 'slot1'
print(my_l[['slot1']]['num3'])

#' 
#' This operation is very useful when interested i
#' 
#' 
#' ### Adding and Removing Elements from a `list`
#' 
#' To add or replace elements in a `list`, just se
#' 
## ------------------------------------------------------------------------------------------------------------
# set list
my_l <- list('a', 1, 3)
glimpse(my_l)

# add new elements to list
my_l[[4]] <- c(1:5)
my_l[[2]] <- c('b')

# print result
glimpse(my_l)

#' 
#' This operation is also possible with the use of
#' 
## ------------------------------------------------------------------------------------------------------------
# set list
my_l <- list(elem1 = 'a', 
             name1=5)

# set new element
my_l$name2 <- 10

# check it
glimpse(my_l)

#' 
#' To remove elements from a `list`, set the eleme
#' 
## ------------------------------------------------------------------------------------------------------------
# set list
my_l <- list(text = 'b', num1 = 2, num2 = 4)
glimpse(my_l)

# remove elements
my_l[[3]] <- NULL
glimpse(my_l)

# remove elements
my_l$num1 <- NULL
glimpse(my_l)

#' 
#' Another way of removing elements from a `list` 
#' 
## ------------------------------------------------------------------------------------------------------------
# set list
my_l <- list(a = 1, 
             b = 'text')

# remove second element
glimpse(my_l[[-2]])

#' 
#' As with atomic vectors, removing elements of a 
#' 
## ------------------------------------------------------------------------------------------------------------
# set list
my_l <- list(1, 2, 3, 4)

# remove elements by condition
my_l[my_l > 2] <- NULL
glimpse(my_l)

#' 
#' However, note this operation only works because
#' 
#' 
#' ### Processing the Elements of a `list`
#' 
#' A very important point about working with `list
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set list with different numerical vectors.
my_l_num <- list(c(1,2,3), 
                 seq(1:50), 
                 seq(-5,5, by=0.5))

#' 
#' Let's assume we need to calculate the average o
#' 
## ------------------------------------------------------------------------------------------------------------
# calculate means
mean_1 <- mean(my_l_num[[1]])
mean_2 <- mean(my_l_num[[2]])
mean_3 <- mean(my_l_num[[3]])

# print result
print(c(mean_1, mean_2, mean_3))

#' 
#' However, the code looks bad and it took three l
#' 
## ------------------------------------------------------------------------------------------------------------
# using sapply
my_mean <- sapply(my_l_num, mean)

# print result
print(my_mean)

#' 
#' As expected, the result is identical to the pre
#' 
#' Intelligently, function `sapply` works the same
#' 
#' Using generic procedures is one premise of good
#' 
#' 
#' ### Other Useful Functions
#' 
#' **unlist** - Returns the elements of a `list` i
#' 
## ------------------------------------------------------------------------------------------------------------
my_named_l <- list(ticker = 'XXXX4',
                   price = c(1,1.5,2,3),
                   market = 'Be')
my_unlisted <- unlist(my_named_l)
print(my_unlisted)
class(my_unlisted)

#' 
#' **as.list** - Converts an object to the `list` 
#' 
## ------------------------------------------------------------------------------------------------------------
my_x <- 10:13
my_x_as_list <- as.list(my_x)
print(my_x_as_list)

#' 
#' **names** - Returns or defines the names of the
#' 
## ------------------------------------------------------------------------------------------------------------
my_l <- list(value1 = 1, value2 = 2, value3 = 3)
print(names(my_l))
my_l <- list(1,2,3)
names(my_l) <- c('num1', 'num2', 'num3')
print(my_l)

#' 
#' 
#' ## `Matrices`
#' 
#' A matrix is a two-dimensional representation of
#' 
#' In R, matrices are objects with two dimensions,
#' 
#' A simple example of using matrices in finance i
#' 

#' 
#' The above matrix could be created in R with the
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set raw data with prices 
raw_data <- c(40.38,  40.14,  40.49,  40.48,  40.64,
              46.23,  46.17,  45.97,  45.56,  45.46,
              238.58, 239.61, 234.67, 237.25, 238.92,
              43.43,  43.96,  44.26,  44.5,   44.86)

# create matrix          
my_mat <- matrix(raw_data, nrow = 5, ncol = 4)
colnames(my_mat) <- c('AAP', 'COG', 'BLK', 'CAM')
rownames(my_mat) <- c("2010-01-04", "2010-01-05", "2010-01-06", 
                      "2010-01-07", "2010-01-08")

# print result
print(my_mat)

#' 
#' We set the number of rows and columns explicitl
#' 
## ------------------------------------------------------------------------------------------------------------
# print the names of columns 
print(colnames(my_mat))

# print the names of rows
print(rownames(my_mat))

#' 
#' After matrix `my_mat` is created, we have at ou
#' 

#' 

#' 
#' In this formula, `r if (my.engine!='epub3') {'$
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector with shares purchased
my_stocks <- as.matrix(c(200, 300, 100, 50), nrow = 4)

# get value of portfolio with matrix multiplication
my_port <- my_mat %*% my_stocks

# print result
print(my_port)

#' 
#' In this last example, we use symbol `%*%`, whic
#' 
#' A `matrix` object is also flexible with its con
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# create matrix with character
my_mat_char <- matrix(rep(c('a','b','c'), 3), 
                      nrow = 3, 
                      ncol = 3)

# print it					  
print(my_mat_char)

#' 
#' Now with a `logic` type:
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# create matrix with logical
my_mat_logical <- matrix(sample(c(TRUE,FALSE), 
                                size = 3*3,
                                replace = TRUE),
                         nrow = 3, 
                         ncol = 3)

# print it					  
print(my_mat_logical)

#' 
#' This flexibility allows the user to expand the 
#' 
#' 
#' ### Selecting Elements from a `matrix`
#' 
#' Following the same notation as the atomic vecto
#' 
#' [^1]: To avoid confusion, atomic vectors in R h
#' 
## ------------------------------------------------------------------------------------------------------------
# create matrix
my_mat <- matrix(1:9, nrow = 3)

# display it
print(my_mat)

# display element in [1,2]
print(my_mat[1,2])

#' 
#' To select an entire row or column, simply leave
#' 
## ------------------------------------------------------------------------------------------------------------
# select all rows from column 2
print(my_mat[, 2])

# select all columns from row 1
print(my_mat[1, ])

#' 
#' Notice the result of indexing is an atomic vect
#' 
## ------------------------------------------------------------------------------------------------------------
# force matrix conversion and print result
print(as.matrix(my_mat[ ,2]))

# force matrix conversion for one row and print result
print(matrix(my_mat[1, ], nrow=1))

#' 
#' Pieces of the `matrix` can also be selected usi
#' 
## ------------------------------------------------------------------------------------------------------------
# select some elements and print them
print(my_mat[2:3, 1:2])

#' 
#' Finally, using logical tests to select elements
#' 
## ------------------------------------------------------------------------------------------------------------
# set matrix
my_mat <- matrix(1:9, nrow = 3)

# print logical matrix where value is higher than 5
print(my_mat >5)

# print the result
print(my_mat[my_mat >5])

#' 
#' 
#' ### Other Useful Functions
#' 
#' **as.matrix** - Transforms raw data to a `matri
#' 
## ------------------------------------------------------------------------------------------------------------
my_mat <- as.matrix(1:5)
print(my_mat)

#' 
#' **t** - Returns a transposed  `matrix`. \index{
#' 
## ------------------------------------------------------------------------------------------------------------
my_mat <- matrix(seq(10,20, 
                     length.out = 6), 
                 nrow = 3)
print(my_mat)

print(t(my_mat))

#' 
#' **rbind** - Returns the merger (bind) of matric
#' 
## ------------------------------------------------------------------------------------------------------------
my_mat_1 <- matrix(1:5, nrow = 1)
print(my_mat_1)
my_mat_2 <- matrix(10:14, nrow = 1)
print(my_mat_2)

my_rbind_mat <- rbind(my_mat_1, my_mat_2)
print(my_rbind_mat)

#' 
#' 
#' **cbind** - Returns the merger (bind) of matric
#' 
## ------------------------------------------------------------------------------------------------------------
my_mat_1 <- matrix(1:4, nrow = 2)
print(my_mat_1)
my_mat_2 <- matrix(10:13, nrow = 2)
print(my_mat_2)

my_cbind_mat <- cbind(my_mat_1, my_mat_2)
print(my_cbind_mat)

#' 
#' **rowMeans** - Returns the mean of a matrix, ro
#' 
## ------------------------------------------------------------------------------------------------------------
my_mat <- matrix(1:9, nrow=3)
print(rowMeans(my_mat))

#' 
#' **colMeans** - Returns the mean of a matrix, co
#' 
## ------------------------------------------------------------------------------------------------------------
my_mat <- matrix(1:9, nrow=3)
print(colMeans(my_mat))

#' 
#' ## Exercises 
#' 
#' 01. Use functions from the `tibble` package to 
#' 
#' 02. Using operator `$`, create a new column nam
#' 
#' 03. Use function `dplyr::filter` and the _pipel
#' 
#' 04. If not done yet, repeat exercises 1, 2 and 
#' 
#' 05. Use package `BatchGetSymbols` to download F
#' 
#' 06. Use function `afedR::afedR_get_data_file` t
#' 
#' 07. Create a list object with three dataframes 
#' 
#' 08. Create an identity matrix (value 1 diagonal