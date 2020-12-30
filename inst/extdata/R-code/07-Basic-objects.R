#' # Basic Object Classes {#basic-classes}
#' 
## ---- include = FALSE----------------------------------------------------------------------------------------
source('Scripts/preamble_chapters.R')

#' 
#' **The basic classes are the most primary elemen
#' 
#' In this chapter, we will study R's basic object
#' 
#' - Numeric (`numeric`)
#' - Text (`character`)
#' - Factors (`factor`)
#' - Logical Values (`logical`)
#' - Dates and Time (`Date` and `timedate`)
#' - Missing Data (`NA`)
#' 
#' 
#' ## `Numeric` Objects
#' 
#' The objects of type `numeric` represent quantit
#' 
#' 
#' ### Creating and Manipulating `numeric` Objects
#' 
#' It is easy to create and manipulate the `numeri
#' 
#' As you can see in the next example, where we ha
#' 
## ------------------------------------------------------------------------------------------------------------
# create numeric vectors
x <- 1:5
y <- 2:6

# print sum
print(x+y)

# print multiplication
print(x*y)

# print division
print(x/y)

# print exponentiation
print(x^y)

#' 
#' The difference between R and other programming 
#' 
## ------------------------------------------------------------------------------------------------------------
# set x with 4 elements and y with 2
x <- 1:4
y <- 2:1

# print multiplication
print(x + y)

#' 
#' The result of `x + y` is equivalent to `1:4 + c
#' 
## ----warning=TRUE, error=TRUE--------------------------------------------------------------------------------
# set x = 4 elements and y with 3
x <- c(1, 2, 3, 4)
y <- c(1, 2, 3)

# print sum (recycling rule)
print(x +y)

#' 
#' The first three elements of `x` were summed to 
#' 
#' Elements of a `numeric` vector can also be name
#' 
## ------------------------------------------------------------------------------------------------------------
# create named vector
x <- c(item1 = 10, 
       item2 = 14, 
       item3 = 9, 
       item4 = 2)

# print it
print(x)

#' 
#' Empty `numeric` vectors can also be created. So
#' 
## ------------------------------------------------------------------------------------------------------------
# create empty numeric vector of length 10
my_x <- numeric(length = 10)

# print it
print(my_x)

#' 
#' As you can see, when using `numeric(length = 10
#' 
#' 
#' ### Creating a `numeric` Sequence
#' 
#' In R, you have two ways to create a sequence of
#' 
#' However, using the operator `:` can be restrict
#' 
## ------------------------------------------------------------------------------------------------------------
# create sequence with seq
my_seq <- seq(from = -10, 
              to = 10, 
              by = 2)

# print it
print(my_seq)

#' 
#' Another interesting feature of function `seq` i
#' 
## ------------------------------------------------------------------------------------------------------------
# create sequence with defined number of elements
desired_len <- 20
my_seq <- seq(from = 0, 
              to = 10, 
              length.out = desired_len)

# print it
print(my_seq)

#' 
#' The final number of elements in object `my_seq`
#' 
#' 
#' ### Creating Vectors with Repeated Elements
#' 
#' Another way to create `numeric` vectors is by u
#' 
## ------------------------------------------------------------------------------------------------------------
# repeat vector three times
my_x <- rep(x = 1, times = 10)

# print it
print(my_x)

#' 
#' It also works with vectors. For example, let's 
#' 
## ------------------------------------------------------------------------------------------------------------
# created a vector with repeated elements
my_x <- rep(x = c(1, 2), 
            times = 3)

# print it
print(my_x)

#' 
#' 
#' ### Creating Vectors with Random Numbers
#' 
#' Some applications in finance and economics requ
#' 
#' In R, several functions create random numbers f
#' 
#' Function `rnorm` generates random numbers from 
#' 
## ------------------------------------------------------------------------------------------------------------
# generate 10 random numbers from a Normal distribution
my_rnd_vec <- rnorm(n = 10000, 
                    mean = 0, 
                    sd = 1)

# print it
glimpse(my_rnd_vec)

#' 
#' We generated ten thousand random numbers from a
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
p <- ggplot(tibble(x = my_rnd_vec), aes(x = x)) + 
  geom_histogram()

print(p)

#' 
#' Yes, it is pretty close. You can change the par
#' 
#' Function `runif` generates random values unifor
#' 
## ------------------------------------------------------------------------------------------------------------
# create a random vector with minimum and maximum
my_rnd_vec <- runif(n = 10, 
                    min = -5, 
                    max = 5)

# print it
print(my_rnd_vec)

#' 
#' Note that both functions, `rnorm` and `runif,` 
#' 
## ------------------------------------------------------------------------------------------------------------
# create sequence
my_vec <- seq(from = 0, to = 25, by=5)

# sample sequence
my_rnd_vec <- sample(my_vec)

# print it
print(my_rnd_vec)

#' 
#' Function `sample` also allows the random select
#' 
## ------------------------------------------------------------------------------------------------------------
# sample one element of my_vec
my_rnd_vec <- sample(my_vec, size = 1)

# print it
print(my_rnd_vec)

#' 
#' If we wanted two random elements from `my_rnd_v
#' 
## ------------------------------------------------------------------------------------------------------------
# sample two elements of my_vec
my_rnd_vec <- sample(my_vec, size = 2)

# print it
print(my_rnd_vec)

#' 
#' Besides, you can select values from a smaller v
#' 
## ------------------------------------------------------------------------------------------------------------
# create vector
my_vec <- c(5, 10, 15)

# sample
my_rnd_vec <- sample(x = my_vec, size = 10, replace = TRUE)
print(my_rnd_vec)

#' 
#' Another important feature of `sample` is it wor
#' 
## ------------------------------------------------------------------------------------------------------------
# example of sample with characters
print(sample(c('elem 1','elem 2','elem 3'),
             size = 1))

# example of sample with list
print(sample(list(x = c(1,1,1),
                  y = c('a', 'b')),
             size = 1))

#' 
#' At this point, it is important to acknowledge t
#' 
#' One neat trick is that we can select the starti
#' 
## ------------------------------------------------------------------------------------------------------------
# set seed with integer 10
set.seed(seed = 10)

# create and print "random" vectors
my_rnd_vec_1 <- runif(5)
print(my_rnd_vec_1)

my_rnd_vec_2 <- runif(5)
print(my_rnd_vec_2)

#' 
#' In the previous code, the value 10 in `set.seed
#' 
#' Function `set.seed` also works for `sample`:
#' 
## ------------------------------------------------------------------------------------------------------------
# fix seed
set.seed(seed = 15)

# print vectors
print(sample(1:10))
print(sample(10:20))

#' 
#' Likewise, if you execute the previous code in y
#' 
#' 
#' ### Accessing the Elements of a `numeric` Vecto
#' 
#' All elements of a numerical vector can be acces
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector
x <- c(-1, 4, -9, 2)

# get first element
first_elem_x <- x[1]

# print it
print(first_elem_x)

#' 
#' The same notation is used to extract parts of a
#' 
## ------------------------------------------------------------------------------------------------------------
# sub-vector of x
sub_x <- x[1:2]

# print it
print(sub_x)

#' 
#' To access named elements of a numeric array, si
#' 
## ------------------------------------------------------------------------------------------------------------
# set named vector
x <- c(item1 = 10, item2 = 14, item3 = -9, item4 = -2)

# access elements by name
print(x['item2'])
print(x[c('item2','item4')])

#' 
#' We can also access the elements of a numerical 
#' 
## ------------------------------------------------------------------------------------------------------------
# find all values of x higher than zero
print(x[x > 0])

#' 
#' The selection of elements from a vector, accord
#' 
#' 
#' ### Modifying and Removing Elements of a `numer
#' 
#' The modification of a vector is very simple. Ju
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector
my_x <- 1:4

# modify first element to 5
my_x[1] <- 5

# print result
print(my_x)

#' 
#' This modification can also be performed block-w
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector
my_x <- 0:5

# set the first three elements to 5
my_x[1:3] <- 5

# print result
print(my_x)

#' 
#' Using conditions to change values in a vector i
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector
my_x <- -5:5

# set any value lower than 2 to 0
my_x[my_x<2] <- 0

# print result
print(my_x)

#' 
#' The removal of elements of a vector is carried 
#' 
## ------------------------------------------------------------------------------------------------------------
# create vector
my_x <- -5:5

# remove first and second element of my_x
my_x <- my_x[-(1:2)]

# show result
print(my_x)

#' 
#' Notice how using negative index simply returns 
#' 
#' 
#' ### Creating Groups
#' 
#' A common data task is to create groups based on
#' 
#' In R, the function used to create intervals fro
#' 
## ------------------------------------------------------------------------------------------------------------
# set random vector
my_x <- rnorm(10000)

# create groups with 5 breaks
my_cut <- cut(x = my_x, breaks = 5)

# print it!
print(head(my_cut))

#' 
#' You should take note that the ranges define the
#' 
## ------------------------------------------------------------------------------------------------------------
print(table(my_cut))

#' 
#' As expected, the distribution of values is bala
#' 
#' With the `cut` function, you can also define cu
#' 
## ------------------------------------------------------------------------------------------------------------
# create random vector in tibble
my_df <- tibble(x = rnorm(10000))

# define breaks and labels manually
my_breaks <- c(min(my_x)-1, -1, 1, max(my_x)+1)
my_labels <- c('Low','Normal', 'High')

# create group from numerical vector
my_df <- my_df %>%
  mutate(cut_x = cut(x = x, 
                     breaks = my_breaks, 
                     labels = my_labels))

# glimpse it!
glimpse(my_df)

#' 
#' Notice that, in this example of creating a grou
#' 
## ------------------------------------------------------------------------------------------------------------
print(table(my_df$cut_x))

#' 
#' 
#' ### Other Useful Functions
#' 
#' **as.numeric** - Converts an object to the `num
#' 
## ------------------------------------------------------------------------------------------------------------
my_text <- c('1', '2', '3')
class(my_text)
my_x <- as.numeric(my_text)
print(my_x)
class(my_x)

#' 
#' 
#' **unique** - Returns all unique values of a num
#' 
## ------------------------------------------------------------------------------------------------------------
my_x <- c(1, 1, 2, 3, 3, 5)
print(unique(my_x))

#' 
#' **sum** - Sums all elements of a `numeric` vect
#' 
## ------------------------------------------------------------------------------------------------------------
my_x <- 1:50
my_sum <- sum(my_x)
print(my_sum)

#' 
#' **max** - Returns the maximum value of a `numer
#' 
## ------------------------------------------------------------------------------------------------------------
x <- c(10, 14, 9, 2)
max_x <- max(x)
print(max_x)

#' 
#' **min** - Returns the minimum value of a `numer
#' 
## ------------------------------------------------------------------------------------------------------------
x <- c(12, 15, 9, 2)
min_x <- min(x)
print(min_x)

#' 
#' **which.max** - Returns the position of the max
#' 
## ------------------------------------------------------------------------------------------------------------
x <- c(100, 141, 9, 2)
which_max_x <- which.max(x)
cat(paste('The position of the maximum value of x is ', 
          which_max_x))
cat(' and its value is ', x[which_max_x])

#' 
#' **which.min** - Returns the position of the min
#' 
## ------------------------------------------------------------------------------------------------------------
x <- c(10, 14, 9, 2)
which_min_x <- which.min(x)
cat(paste('The position of the minimum value of x is ',
          which_min_x, ' and its value is ', x[which_min_x]))

#' 
#' **sort** - Returns a sorted (ascending or desce
#' 
## ------------------------------------------------------------------------------------------------------------
x <- runif(5)
print(sort(x, decreasing = FALSE))
print(sort(x, decreasing = TRUE))

#' 
#' **cumsum** - Returns the cumulative sum of the 
#' 
## ------------------------------------------------------------------------------------------------------------
my_x <- 1:25
my_cumsum <- cumsum(my_x)
print(my_cumsum)

#' 
#' **prod** - Returns the product (multiplication)
#' 
## ------------------------------------------------------------------------------------------------------------
my_x <- 1:10
my_prod <- prod(my_x)
print(my_prod)

#' 
#' **cumprod** - Returns the cumulative product of
#' 
## ------------------------------------------------------------------------------------------------------------
my_x <- 1:10
my_prod <- cumprod(my_x)
print(my_prod)

#' 
#' 
#' ## `Character` Objects
#' 
#' The `character` class, or simply text class, is
#' 
#' R has several features that facilitate the crea
#' 
#' A positive aspect of `stringr` is that all func
#' 
#' 
#' ### Creating a Simple `character` Object
#' 
#' In R, every `character` object is created by en
#' 
## ------------------------------------------------------------------------------------------------------------
tickers <- c('MMM', 'FB', 'ICE')
print(tickers)

#' 
#' We can confirm the class of the created object 
#' 
## ------------------------------------------------------------------------------------------------------------
class(tickers)

#' 
#' 
#' ### Creating Structured `character` Objects
#' 
#' We can also use R to create a text vector with 
#' 
#' To create a text vector with the junction of te
#' 
## ------------------------------------------------------------------------------------------------------------
library(stringr)

# create sequence and tex
my_seq <- 1:20
my_text <- 'text'

# paste objects together (without space)
my_char <- str_c(my_text, my_seq)
print(my_char)

# paste objects together (with space)
my_char <- str_c(my_text, 
                 my_seq, 
                 sep = ' ')
print(my_char)

#' 
#' We can do the same procedure with text vectors:
#' 
## ------------------------------------------------------------------------------------------------------------
# set character value
my_x <- 'My name is'

# set character vector
my_names <- c('Marcelo', 'Ricardo', 'Tarcizio')

# paste and print
print(str_c(my_x, my_names, sep = ' '))

#' 
#' Another possibility of building structured text
#' 
## ------------------------------------------------------------------------------------------------------------
# repeat 'abc' five times
my_char <- str_dup(string = 'abc', times = 5)

# print it
print(my_char)

#' 
#' 
#' ### `character` Constants
#' 
#' R also allows direct access to all letters of t
#' 
## ------------------------------------------------------------------------------------------------------------
# print all letters in alphabet (no cap)
print(letters)

#' 
## ------------------------------------------------------------------------------------------------------------
# print all letters in alphabet (WITH CAP)
print(LETTERS)

#' 
#' Note that, in both cases, `letters` and `LETTER
#' 
#' Other constant `character` objects in R are `mo
#' 
## ------------------------------------------------------------------------------------------------------------
# print abbreviation and full names of months
print(month.abb)
print(month.name)

#' 
#' 
#' ### Selecting Pieces of a Text Object
#' 
#' A common beginner's mistake is to select charac
#' 
## ------------------------------------------------------------------------------------------------------------
# set char object
my_char <- 'ABCDE'

# print its second element (WRONG - RESULT is NA)
print(my_char[2])

#' 
#' The `NA` value indicates the second element of 
#' 
## ------------------------------------------------------------------------------------------------------------
print(my_char[1])

#' 
#' The result is simply the _ABCDE_ text, on the f
#' 
## ------------------------------------------------------------------------------------------------------------
# print third and fourth characters
my_substr <- str_sub(string = my_char,
                     start = 2,
                     end = 2)
print(my_substr)

#' 
#' These functions also work for atomic vectors. L
#' 
## ------------------------------------------------------------------------------------------------------------
# build char vec
my_char_vec <- paste0(c('ABC','VBC','ZMN'),
                      ' - other ignorable text')
print(my_char_vec)

#' 
#' Here, we want the information in the first thre
#' 
## ------------------------------------------------------------------------------------------------------------
# get ids with stringr::str_sub
ids_vec <- str_sub(my_char_vec, 1, 3)
print(ids_vec)

#' 
#' **Vector operations in character objects are ve
#' 
#' 
#' ### Finding and Replacing Characters of a Text
#' 
#' A useful operation in handling texts is to loca
#' 
#' The most common case of string search is to ver
#' 
#' The following example shows how to find the _D_
#' 
## ------------------------------------------------------------------------------------------------------------
# set character object
my_char <- 'ABCDEF-ABCDEF-ABC'

# find position of 'D' using str_locate
pos <- str_locate(my_char, fixed('D'))
print(pos)

#' 
#' Note the `str_locate` function returns only the
#' 
## ------------------------------------------------------------------------------------------------------------
# set object
my_char <- 'ABCDEF-ABCDEF-ABC'

# find position of ALL 'D' using str_locate_all
pos <- str_locate_all(my_char, fixed('D'))
print(pos)


#' 
#' To replace characters in a text, use functions 
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set char object
my_char <- 'ABCDEF-ABCDEF-ABC'

# substitute the FIRST 'ABC' for 'XXX' with str_replace
my_char <- str_replace(string = my_char,
                       pattern = 'ABC',
                       replacement = 'XXX')
print(my_char)

#' 
#' And now, we globally substitute characters.
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set char object
my_char <- 'ABCDEF-ABCDEF-ABC'

# substitute ALL 'ABC' for 'XXX' with str_replace_all
my_char <- str_replace_all(string = my_char,
                           pattern = 'ABC',
                           replacement = 'XXX')

# print result
print(my_char)

#' 
#' Again, it is worth pointing out that the operat
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set char object
my_char <- c('ABCDEF','DBCFE','ABC')

# create an example of vector
my_char_vec <- paste(sample(my_char, 5, replace = T),
                     sample(my_char, 5, replace = T),
                     sep = ' - ')

# show it
print(my_char_vec)

# substitute all occurrences of 'ABC'
my_char_vec <- str_replace_all(string = my_char_vec,
                               pattern = 'ABC',
                               replacement = 'XXX')

# print result
print(my_char_vec)

#' 
#' 
#' ### Splitting Text
#' 
#' Eventually, you will need to break a text into 
#' 
## ------------------------------------------------------------------------------------------------------------
# set char
my_char <- 'ABC;ABC;BCD'

# split it based on ';' and using stringr::str_split
splitted_char <- str_split(my_char, ';')

# print result
print(splitted_char)


#' 
#' The output of this function is an object of typ
#' 
## ------------------------------------------------------------------------------------------------------------
print(splitted_char[[1]][3])

#' 
#' For an example of a split in character vectors,
#' 
## ------------------------------------------------------------------------------------------------------------
# set char
my_char_vec <- c('ABCDEF','DBCFE','ABFC','ACD')

# split it based on 'B' and using stringr::strsplit
splitted_char <- str_split(my_char_vec, 'B')

# print result
print(splitted_char)

#' 
#' Notice how, again, an object of type `list` is 
#' 
#' 
#' ### Finding the Number of Characters in a Text
#' 
#' If we want to discover the number of characters
#' 
## ------------------------------------------------------------------------------------------------------------
# set char
my_char <- 'abcdef'

# print number of characters using stringr::str_length
print(str_length(my_char))

#' 
#' And now an example with vectors.
#' 
## ------------------------------------------------------------------------------------------------------------
#set char
my_char <- c('a', 'ab', 'abc')

# print number of characters using stringr::str_length
print(str_length(my_char))

#' 
#' 
#' ### Generating Combinations of Text
#' 
#' One useful trick in R is to use functions `oute
#' 
## ------------------------------------------------------------------------------------------------------------
# set char vecs
my_vec_1 <- c('a','b')
my_vec_2 <- c('A','B')

# combine in matrix
comb_mat <- outer(my_vec_1, my_vec_2, paste, sep='-')

# print it!
print(comb_mat)

#' 
#' The output of `outer` is a `matrix` type of obj
#' 
## ------------------------------------------------------------------------------------------------------------
print(as.character(comb_mat))

#' 
#' Another way to reach the same objective is by u
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# set vectors
my_vec_1 <- c('John ', 'Claire ', 'Adam ')
my_vec_2 <- c('is fishing.', 'is working.')

# create df with all combinations
my_df <- expand.grid(name = my_vec_1,
                     verb = my_vec_2)

# print df
print(my_df)

# paste columns together in tibble
my_df <- my_df %>%
  mutate(phrase = paste0(name, verb) )

# print result
print(my_df)

#' 
#' Here, we used the function `expand.grid` to cre
#' 
#' 
#' ### Encoding of `character` Objects
#' 
#' For R, a text string is just a sequence of _byt
#' 
#' Let's explore an example to understand the prob
#' 
## ------------------------------------------------------------------------------------------------------------
# read text file
my_char <- readLines('data/FileWithLatinChar_ISO-8859-9.txt')

# print it
print(my_char)

#' 
#' The original content of the file is a text in P
#' 
#' The easiest solution is to modify the encoding 
#' 
## ------------------------------------------------------------------------------------------------------------
# read a text file with utf-8
my_char <- readLines('data/FileWithLatinChar_UTF-8.txt')

# print it
print(my_char)

#' 
#' The Latin characters are now correct because th
#' 
#' As for the text objects available in the enviro
#' 
## ------------------------------------------------------------------------------------------------------------
# read text file
my_char <- readLines('data/FileWithLatinChar_ISO-8859-9.txt')

# change encoding
Encoding(my_char) <- 'UTF-8'

# show its encoding
print(Encoding(my_char))

#' 
#' 
#' After reading the contents of `"FileWithLatinCh
#' 
#' 
#' ### Other Useful Functions
#' 
#' **stringr::str_to_lower**/**base::tolower** - C
#' 
## ------------------------------------------------------------------------------------------------------------
print(stringr::str_to_lower('ABC'))

#' 
#' **stringr::str_to_upper**/**base::toupper** - C
#' 
## ------------------------------------------------------------------------------------------------------------
print(stringr::str_to_upper('abc'))

#' 
#' 
#' ## `Factor` Objects
#' 
#' Object class `factor` is used to represent grou
#' 
#' The `factor` class offers a special object to d
#' 
#' 
#' ### Creating `factors`
#' 
#' The creation of factors is accomplished with fu
#' 
## ------------------------------------------------------------------------------------------------------------
# create factor
my_factor <- factor(c('M', 'F', 'M',
                      'M', 'F', 'F'))

# print it
print(my_factor)

#' 
#' Notice that, in the previous example, the prese
#' 
## ------------------------------------------------------------------------------------------------------------
# create factor with 3 levels
my_factor <- factor(c('M', 'F', 'M', 
                      'M', 'F', 'F',
                      'ND'))

# print factor
print(my_factor)

#' 
#' Here, we also have the `ND` (not defined) group
#' 
#' An important point about creating factors is th
#' 
## ------------------------------------------------------------------------------------------------------------
# set factors with 1 level
my_status <- factor(c('Single', 'Single', 'Single'))

# print it
print(my_status)

#' 
#' Accidentally, the data in `my_status` only show
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
my_status <- factor(c('Single', 'Single', 'Single'),
                    levels = c('Single', 'Married'))

print(my_status)

#' 
#' 
#' ### Modifying `factors`
#' 
#' An important point about the `factor` type of o
#' 
## ----warning=TRUE--------------------------------------------------------------------------------------------
# set factor
my_factor <- factor(c('a', 'b', 'a', 'b'))

# change first element of a factor to 'c'
my_factor[1] <- 'c'

# print result
print(my_factor)

#' 
#' As we expected, the first element of `my_factor
#' 
## ------------------------------------------------------------------------------------------------------------
# set factor
my_factor <- factor(c('a', 'b', 'a', 'b'))

# change factor to character
my_char <- as.character(my_factor)

# change first element
my_char[1] <- 'c'

# mutate it back to class factor
my_factor <- factor(my_char)

# show result
print(my_factor)

#' 
#' Using these steps, we have the desired result i
#' 
#' The `tidyverse` universe also has its own packa
#' 
## ------------------------------------------------------------------------------------------------------------
library(forcats)

# set factor
my_factor <- factor(c('A', 'B', 'C', 
                      'A', 'C', 'M', 
                      'N'))

# modify factors
my_factor <- fct_recode(my_factor,
                        'D' = 'A',
                        'E' = 'B',
                        'F' = 'C')

# print result
print(my_factor)

#' 
#' Using `forcats::fct_recode` is intuitive. All w
#' 
#' 
#' ### Converting `factors` to Other Classes
#' 
#' Attention is required when converting a `factor
#' 
## ------------------------------------------------------------------------------------------------------------
# create factor
my_char <-factor(c('a', 'b', 'c'))

# convert and print
print(as.character(my_char))

#' 
#' However, when the same procedure is performed f
#' 
## ------------------------------------------------------------------------------------------------------------
# set factor
my_values <- factor(5:10)

# convert to numeric (WRONG)
print(as.numeric(my_values))

#' 
#' As you can see, all elements in `my_values` wer
#' 
## ------------------------------------------------------------------------------------------------------------
# converting factors to character and then to numeric
print(as.numeric(as.character(my_values)))

#' 
#' As we can see, now we got the result we expecte
#' 
#' 
#' ### Creating Contingency Tables
#' 
#' After creating a factor, we can find the number
#' 
## ------------------------------------------------------------------------------------------------------------
# create factor
my_factor <- factor(sample(c('Pref', 'Ord'),
                           size = 20,
                           replace = TRUE))

# print contingency table
print(table(my_factor))

#' 
#' A more advanced usage of function `table` is to
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set factors
my_factor_1 <- factor(sample(c('Pref', 'Ord'),
                             size = 20,
                             replace = TRUE))

my_factor_2 <- factor(sample(paste('Grupo', 1:3),
                             size = 20,
                             replace = TRUE))

# print contingency table with two factors
print(table(my_factor_1, 
            my_factor_2))

#' 
#' The table that we created previously demonstrat
#' 
#' 
#' ### Other Useful Functions
#' 
#' **levels** - Returns the `Levels` an object of 
#' 
## ------------------------------------------------------------------------------------------------------------
my_factor <- factor(c('A', 'A', 'B', 'C', 'B'))
print(levels(my_factor))

#' 
#' **as.factor** - Transforms an object to the cla
#' 
## ------------------------------------------------------------------------------------------------------------
my_y <- c('a','b', 'c', 'c', 'a')
my_factor <- as.factor(my_y)

print(my_factor)

#' 
#' 
#' **split** - Based on a grouping variable and an
#' 
## ------------------------------------------------------------------------------------------------------------
my_factor <- factor(c('A','B','C','C','C','B'))
my_x <- 1:length(my_factor)

my_l <- split(x = my_x, f = my_factor)

print(my_l)

#' 
#' 
#' ## `Logical` Objects
#' 
#' Logical tests are at the heart of R. In one lin
#' 
#' 
#' ### Creating `logical` Objects
#' 
#' Objects of class `logical` are created based on
#' 
## ------------------------------------------------------------------------------------------------------------
# set numerical
my_x <- 1:10

# print a logical test
print(my_x > 5)

# print position of elements from logical test
print(which(my_x > 5))

#' 
#' In the previous example, function `which` retur
#' 
#' To perform equality tests, simply use the equal
#' 
## ------------------------------------------------------------------------------------------------------------
# create char
my_char <- rep(c('abc', 'bcd'), 
               times = 5)

# print its contents
print(my_char)

# print logical test
print(my_char == 'abc')

#' 
#' For an inequality test, use symbol **`!=`**, as
#' 
## ------------------------------------------------------------------------------------------------------------
# print inequality test
print(my_char != 'abc')

#' 
#' It is also possible to test multiple logical co
#' 
## ------------------------------------------------------------------------------------------------------------
my_x <- 1:10

# print logical for values higher than 4 and lower than 7
print((my_x > 4)&(my_x < 7) )

# print the actual values
idx <- which( (my_x > 4)&(my_x < 7) )
print(my_x[idx])

#' 
#' For non-simultaneous conditions, i.e., the occu
#' 
## ------------------------------------------------------------------------------------------------------------
# location of elements higher than 7 or lower than 4
idx <- which( (my_x > 7)|(my_x < 4) )

# print elements from previous condition
print(my_x[idx])

#' 
#' Besides, you should be aware that we used paren
#' 
#' Another interesting use of logical objects is t
#' 
## ------------------------------------------------------------------------------------------------------------
library(dplyr)
# location of elements higher than 7 or lower than 4
my_contries <- c('Country 1', 'Country 2')

# set df
n_obs <- 100
df_temp <- tibble(country = str_c('Country ',
                                  sample(1:10, 
                                         size = n_obs,
                                         replace = TRUE)),
                  inflation.rate = rnorm(n_obs, sd = 0.05) ) %>%
  glimpse()

# filter rows of df with selected tickers
df_temp <- df_temp %>%
  filter(country %in% my_contries) %>%
  glimpse()


#' 
#' The resulting dataframe only has rows for `'Cou
#' 
#' 
#' ## Date and Time
#' 
#' The representation and manipulation of dates is
#' 
#' 
#' ### Creating Simple Dates
#' 
#' In R, several classes can represent dates. The 
#' 
#' The most basic class, indicating the day, month
#' 
## ------------------------------------------------------------------------------------------------------------
library(lubridate)

# set Date object (YMD)
print(ymd('2020-06-24'))

# set Date object (DMY)
print(dmy('24-06-2020'))

# set Date object (MDY)
print(mdy('06-24-2020'))

#' 
#' Note that the functions return the exact same o
#' 
#' One benefit of using the `lubridate` package is
#' 
## ------------------------------------------------------------------------------------------------------------
# set Date object 
print(ymd('2020/06/24'))

# set Date object 
print(ymd('2020&06&24'))

# set Date object
print(ymd('2020 june 24'))

# set Date object
print(dmy('24 of june 2020'))

#' 
#' This is a very useful property of `lubridate,` 
#' 
#' Now, using the `base` package, we can create a 
#' 
## ------------------------------------------------------------------------------------------------------------
# set Date from dd/mm/yyyy with the definition of format
my_date <- as.Date('24/06/2020', format = '%d/%m/%Y')

# print result
print(my_date)

#' 
#' The symbols used in _input_ `format,` such as `
#' 
#' 
#' | Symbol |          Description   |Example |
#' |:------:|:----------------------:|:------:|
#' |%d      |day of month (decimal)  |0       |
#' |%m      |month (decimal)         |12      |
#' |%b      |month (abbreviation)    |Apr     |
#' |%B      |month (complete name)   |April   |
#' |%y      |year (2 digits)         |16      |
#' |%Y      |month (4 digits)        |2020    |
#' 
#' By using the previous table, you'll be able to 
#' 
#' 
#' ### Creating a Sequence of `Dates`
#' 
#' An interesting aspect of objects `Date` is they
#' 
## ------------------------------------------------------------------------------------------------------------
# create date
my_date <- ymd('2020-06-01')

# find next day
my_date_2 <- my_date + 1

# print result
print(my_date_2)

#' 
#' This property also works with vectors, facilita
#' 
## ------------------------------------------------------------------------------------------------------------
# create a sequence of Dates
my_date_vec <- my_date + 0:15

# print it
print(my_date_vec)

#' 
#' A more customizable way for creating `Date` seq
#' 
## ------------------------------------------------------------------------------------------------------------
# set first and last Date
my_date_1 <- ymd('2017-03-07')
my_date_2 <- ymd('2017-03-20')

# set sequence
my_vec_date <- seq(from = my_date_1,
                   to = my_date_2,
                   by = '2 days')

# print result
print(my_vec_date)

#' 
#' Likewise, if we wanted a sequence of dates for 
#' 
## ------------------------------------------------------------------------------------------------------------
# set first and last Date
my_date_1 <- ymd('2017-03-07')
my_date_2 <- ymd('2017-04-20')

# set sequence
my_vec_date <- seq(from = my_date_1,
                   to = my_date_2,
                   by = '2 weeks')

# print result
print(my_vec_date)

#' 
#' Another way to use function `seq` is by setting
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set dates
my_date_1 <- as.Date('2020-06-27')
my_date_2 <- as.Date('2020-07-27')

# set sequence with 10 elements
my_vec_date <- seq(from = my_date_1,
                   to = my_date_2,
                   length.out = 10)

# print result
print(my_vec_date)

#' 
#' Once again, the interval between the dates is a
#' 
#' 
#' ### Operations with `Dates`
#' 
#' We can calculate difference of days between two
#' 
## ------------------------------------------------------------------------------------------------------------
# set dates
my_date_1 <- ymd('2015-06-24')
my_date_2 <- ymd('2020-06-24')

# calculate difference
diff_date <- my_date_2 - my_date_1

# print result
print(diff_date)

#' 
#' The output of the subtraction operation is an o
#' 
## ------------------------------------------------------------------------------------------------------------
# print difference of days as numerical value
print(diff_date[[1]])

#' 
#' Going further, we can also use mathematical ope
#' 
## ------------------------------------------------------------------------------------------------------------
# set date and vector
my_date_1 <- ymd('2020-06-20')
my_date_vec <- ymd('2020-06-20') + seq(-5,5)

# test which elements of my_date_vec are older than my_date_1
my.test <- (my_date_vec > my_date_1)

# print result
print(my.test)

#' 
#' The previous operation is useful when selecting
#' 
## ------------------------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)

# set first and last dates
first_date <- ymd('2020-06-01')
last_date <- ymd('2020-06-15')

# create dataframe and glimpse it
my_temp_df <- tibble(date.vec = ymd('2020-05-25') + seq(0,30),
                     prices=seq(1,10,
                                length.out = length(date.vec)))

# find dates that are between the first and last date
my_idx <- (my_temp_df$date.vec >= first_date) &
  (my_temp_df$date.vec <= last_date)

# use index to filter dataframe
my_temp_df_filtered <- my_temp_df %>%
  filter(my_idx) %>%
  glimpse()

#' 
#' In the previous code, the object `my_temp_df_fi
#' 
#' 
#' ### Dealing with Time
#' 
#' Using the `Date` class is sufficient when deali
#' 
#' In the `base` package, one class used for this 
#' 
#' In R, the time/date format also follows the [IS
#' 
## ------------------------------------------------------------------------------------------------------------
# creating a POSIXct object
my_timedate <- as.POSIXct('2020-01-01 16:00:00')

# print result
print(my_timedate)

#' 
#' The `lubridate` package also offers intelligent
#' 
## ------------------------------------------------------------------------------------------------------------
library(lubridate)

# creating a POSIXlt object
my_timedate <- ymd_hms('2020-01-01 16:00:00')

# print it
print(my_timedate)

#' 
#' You should note that this class automatically a
#' 
## ------------------------------------------------------------------------------------------------------------
# creating a POSIXlt object with custom timezone
my_timedate_tz <- ymd_hms('2020-01-01 16:00:00',
                          tz = 'GMT')

# print it
print(my_timedate_tz)

#' 
#' An important note in the case of `POSIXlt` and 
#' 
## ------------------------------------------------------------------------------------------------------------
# Adding values (seconds) to a POSIXlt object and printing it
print(my_timedate_tz + 30)

#' 
#' 
#' ### Customizing the Format of Dates and Times
#' 
#' The ISO format for representing dates and `date
#' 
#' In the same way as objects of class `Date`, the
#' 
#' 
#' | Symbol |           Description    | Example |
#' |:------:|:------------------------:|:-------:|
#' | %H     | Hour (decimal, 24 hours) | 23      |
#' | %I     | Hour (decimal, 12 hours) | 11      |
#' | %M     | Minutes (decimal, 0-59)  | 12      |
#' | %p     | AM/PM indicator          | AM      |
#' | %S     | Seconds (decimal, 0-59)  | 50      |
#' 
#' To format a date, use the `format` function. Us
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# create vector of dates
my_dates <- seq(from = as.Date('2020-01-01'),
                to = as.Date('2020-01-15'),
                by = '1 day')

# change format
my_dates_US_format <- format(my_dates, '%m/%d/%Y')

# print result
print(my_dates_US_format)

#' 
#' The same procedure can be used for `POSIXlt` ob
#' 
## ------------------------------------------------------------------------------------------------------------
# create vector of date-time
my_datetime <- as.POSIXlt('2020-02-01 12:00:00') + seq(0,560,60)

# change to US format
my_dates_US_format <- format(my_datetime, '%m/%d/%Y %H:%M:%S')

# print result
print(my_dates_US_format)

#' 
#' Likewise, we can customize our dates for very s
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set custom format
my_dates_my_format <- format(my_dates,
                             'Year=%Y | Month=%m | Day=%d')

# print result
print(my_dates_my_format)

#' 
#' 
#' ### Extracting Elements of a Date
#' 
#' We can use function `format` to extract data el
#' 
## ------------------------------------------------------------------------------------------------------------
library(lubridate)

# create vector of date-time
my_datetime <- seq(from = ymd_hms('2020-01-01 12:00:00'),
                   to = ymd_hms('2020-01-01 18:00:00'),
                   by = '1 hour')

# get hours from POSIXlt
my_hours <- format(my_datetime, '%H')

# print result
print(my_hours)

#' 
#' Likewise, we can use symbols `%M` and `%S` to e
#' 
## ------------------------------------------------------------------------------------------------------------
# create vector of date-time
n_dates <- 10
my_datetime <- seq(from = ymd_hms('2020-01-01 12:00:00'),
                   to = ymd_hms('2020-01-01 18:00:00'),
                   length.out = n_dates) + sample(1:59, 
                                                  size = n_dates)

# get minutes from POSIXlt
my_minutes <- format(my_datetime, '%H:%M:%S')

# print result
print(my_minutes)

#' 
#' Alternatively, we can use `lubridate` functions
#' 
## ------------------------------------------------------------------------------------------------------------
# get hours with lubridate
print(hour(my_datetime))

# get minutes with lubridate
print(minute(my_datetime))

#' 
#' Functions for extracting other components of a 
#' 
#' 
#' ### Find the Current Date and Time
#' 
#' R's specific functions allow the user to find t
#' 
#' If you want to find the present day, use functi
#' 
## ------------------------------------------------------------------------------------------------------------
library(lubridate)

# get today
print(Sys.Date())

# print it
print(today())

#' 
#' If you want to find the current date and time, 
#' 
## ------------------------------------------------------------------------------------------------------------
# get time!
print(Sys.time())

# get time!
print(now())

#' 
#' Going further, based on these functions, we can
#' 
## ------------------------------------------------------------------------------------------------------------
# example of log message
my_sys_info <- Sys.info()
my_str <- str_c('Log of execution\n',
                'Time of execution: ', now(), '\n',
                'User: ', my_sys_info['user'], '\n',
                'Computer: ', my_sys_info['nodename'])

# print it
cat(my_str)

#' 
#' This is the exact time when this book was compi
#' 
#' 
#' ### Other Useful Functions
#' 
#' **weekdays** - Returns the day of the week from
#' 
## ------------------------------------------------------------------------------------------------------------
# set date vector
my_dates <- seq(from = ymd('2020-01-01'),
                to = ymd('2020-01-5'),
                by = '1 day')

# find corresponding weekdays
my_weekdays <- weekdays(my_dates)

# print it
print(my_weekdays)

#' 
#' **months** - Returns the month of one or more d
#' 
## ------------------------------------------------------------------------------------------------------------
# create date vector
my_dates <- seq(from = ymd('2020-01-01'),
                to = ymd('2020-12-31'),
                by = '1 month')

# find months
my_months <- months(my_dates)

# print result
print(my_months)

#' 
#' **quarters** - Returns the location of one or m
#' 
## ------------------------------------------------------------------------------------------------------------
# get quartiles of the year
my_quarters <- quarters(my_dates)
print(my_quarters)

#' 
#' **OlsonNames** - Returns an array with the time
#' 
## ------------------------------------------------------------------------------------------------------------
# get possible timezones
possible_tz <- OlsonNames()

# print it
print(possible_tz[1:5])

#' 
#' **Sys.timezone** - Returns the current timezone
#' 
## ------------------------------------------------------------------------------------------------------------
print(Sys.timezone())

#' 
#' **cut** - Returns a factor by grouping dates an
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# set example date vector
my_dates <- seq(from = as.Date('2020-01-01'),
                to = as.Date('2020-03-01'),
                by = '5 days')

# group vector based on monthly breaks
my_month_cut <- cut(x = my_dates,
                    breaks = 'month',
                    labels = c('Jan', 'Fev', 'Mar'))

# print result
print(my_month_cut)

#' 
## ------------------------------------------------------------------------------------------------------------
# set example datetime vector
my_datetime <- as.POSIXlt('2020-01-01 12:00:00') + seq(0,250,15)

# set groups for each 30 seconds
my_cut <- cut(x = my_datetime, breaks = '30 secs')

# print result
print(my_cut)

#' 
#' 
#' ## Missing Data - `NA` (_Not available_)
#' 
#' One of the main innovations of R is the represe
#' 
#' 
#' ### Defining `NA` Values
#' 
#' To define omissions in the dataset, use symbol 
#' 
## ------------------------------------------------------------------------------------------------------------
# a vector with NA
my_x <- c(1, 2, NA, 4, 5)

# print it
print(my_x)

#' 
#' An important information that you must remember
#' 
## ------------------------------------------------------------------------------------------------------------
# a vector 
my_y <- c(2, 3, 5, 4, 1)

# example of NA interacting with other objects
print(my_x + my_y)

#' 
#' This property demands special attention if you 
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector with NA
my_x <- c(1:5, NA, 5:10)

# print cumsum (NA after sixth element)
print(cumsum(my_x))

# print cumprod (NA after sixth element)
print(cumprod(my_x))

#' 
#' Therefore, when using functions `cumsum` and `c
#' 
#' 
#' ### Finding and Replacing `NA`
#' 
#' To find `NA` values, use function `is.na`: \ind
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector with NA
my_x <- c(1:2, NA, 4:10)

# Test and find location of NA
print(is.na(my_x))
print(which(is.na(my_x)))

#' 
#' To replace it, use indexing with the output of 
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector
my_x <- c(1, NA, 3:4, NA)

# replace NA for 2
my_x[is.na(my_x)] <- 2

# print result
print(my_x)

#' 
#' Another way to remove `NA` values is to use the
#' 
## ------------------------------------------------------------------------------------------------------------
# set vector
my_char <- c(letters[1:3], NA, letters[5:8])

# print it
print(my_char)

# use na.omit to remove NA
my_char <- na.omit(my_char)

# print result
print(my_char)

#' 
#' Although the type of object has been changed du
#' 
## ------------------------------------------------------------------------------------------------------------
# trying nchar on a na.omit object
print(nchar(my_char))

#' 
#' For other objects, however, this property may n
#' 
#' 
#' ### Other Useful Functions
#' 
#' **complete.cases** - Returns a logical vector i
#' 
## ------------------------------------------------------------------------------------------------------------
# create matrix
my_mat <- matrix(1:15, nrow = 5)

# set an NA value
my_mat[2,2] <- NA

# print index with rows without NA
print(complete.cases(my_mat))

#' 
#' ## Exercises
#' 
#' 01. Let's assume that, on a certain date, you b
#' 
#' 02. Consider these numeric vectors `x` and `y`:
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(7)
x <- sample (1:3, size = 5, replace = T)
y <- sample (1:3, size = 5, replace = T)

#' 
#' What is the sum of the elements of a new vector
#'   
#' 03. Create a sequence called `seq_1`, with valu
#' 
#' 04. Define another object named `seq_2`, which 
#' 
#' 05. Calculate the sum between `seq_1` and `seq_
#' 
#' 06. If we create an object with the cumulative 
#' 
#' 07. Create a vector according to the following 
#' 
#' $$
#' x_i = \frac{(-1^{i + 1})}{2i-1}
#' $$
#' 
#' 08. **CHALLENGE** - Create a $z_i$ vector accor
#' 
#' $$
#' z_i = \frac{y_i-x_{i-1}}{y_{i-2}}
#' $$
#' 
#' 09. Create an object named `x` with 1000 random
#' 
#' 10. Execute the following code and create objec
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(10)
my_char <- paste(sample(letters, 
                        size =  500, 
                        replace = T), 
                 collapse = '')

#' 
#' How often is the letter `'x'` found in the resu
#' 
#' 11. If we split the `my_char` object from the p
#' 
#' 12. At link [https://www.gutenberg.org/ebooks/2
#' 
#' 13. Aggregate the vector of characters in `my_b
#' 
#' 14. **CHALLENGE** - Use function `stringr::str_
#' 
#' 15. Assuming you'll live for 100 years, what is