#' # Reporting Results {#reporting}
#' 

#' 
#' In previous chapters, we learned how to use R t
#' 
#' Quality relates to the visual attractiveness of
#' 
#' The ease of change relates to how we edit and f
#' 
#' If you have many tables and figures in your rep
#' 
#' There are two strategies for reporting research
#' 
#' 
#' ## Reporting Tables
#' 
#' Simple tables, such as descriptive statistics o
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
library(tidyverse)
library(BatchGetSymbols)
library(DistributionUtils) 

# set number of rows in table
my_tickers <- c('FB', 'GOOGL',
                'JPM', 'BRK-B')

first_date = '2015-01-01'
last_date = '2018-01-01'

df_stocks <- BatchGetSymbols(tickers = my_tickers,
                             first.date = first_date,
                             last.date = last_date,
                             bench.ticker = '^BVSP')[[2]]

# create descriptive table
my_desc_table <- df_stocks %>%
  group_by(Ticker = ticker ) %>%
  summarise('Mean Ret' = mean(ret.adjusted.prices, na.rm = TRUE),
            'StDev Ret' = sd(ret.adjusted.prices, na.rm = TRUE),
            'Max Ret' = max(ret.adjusted.prices, na.rm = TRUE),
            'Min Ret' = min(ret.adjusted.prices, na.rm = TRUE),
            Assimetry = skewness(ret.adjusted.prices, na.rm = TRUE),
            Kurtosis = kurtosis(ret.adjusted.prices, na.rm = TRUE))

print(my_desc_table)

#' 
#' In creating the `dataframe`, notice how we defi
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
library(xtable)

# set xtable object
my_xtable <- xtable(x = my_desc_table,
                    label = 'tab:DescRetStats',
                    caption = 'Descriptive Statistics for Returns',
                    digits = 4)

# check if folder exists
if (!dir.exists('tabs')) {
	dir.create('tabs')
}

# print output to latex file
my_tex_file <- 'tabs/MyTable.tex'

# save it
print(my_xtable,
      include.rownames = FALSE,
      file = my_tex_file,
      type='latex')

#' 
#' In the `xtable` function, we only use `label` a
#' 

#' 
#' As for exporting tables to _Word_ (_Microsoft_)
#' 
## ------------------------------------------------------------------------------------------------------------
# set html file for output
my_html_file <- 'tabs/MyTable.html'

# write it!
print(x = my_xtable,
      file = my_html_file,
      type = 'html',
      include.rownames = FALSE )

#' 
#' Once the file is available, we can open `r my_h
#' 

#' 
#' Going further, if you are dealing with several 
#' 
#' 
#' ## Reporting Models {#reporting-models}
#' 
#' Reporting the estimation of models requires a s
#' 
#' As an example, let's use the `texreg` package t
#' 
## ---- tidy=FALSE, message=FALSE------------------------------------------------------------------------------
library(texreg)
library(dplyr)
library(BatchGetSymbols)

# get Ibov data
my_tickers <- c('^GSPC')

first_date = '2015-01-01'
last_date = '2019-01-01'

df_sp500 <- BatchGetSymbols(tickers = my_tickers,
                             first.date = first_date,
                             last.date = last_date)[[2]]

# set sp500 ret column
idx <- match(df_stocks$ref.date, df_sp500$ref.date)
df_stocks$ret_mkt <- df_sp500$ret.adjusted.prices[idx]

# estimate betas
beta_tab <- df_stocks %>%
  group_by(ticker) %>%
  do(beta_model = lm(data=., ret.adjusted.prices ~ ret_mkt))

# report result
est_table <- screenreg(l = beta_tab$beta_model,
                       custom.model.names = beta_tab$ticker,
                       custom.coef.names = c('Alpha', 'Beta'),
                       digits = 2)

# print it
print(est_table)

#' 
#' In the previous code, `beta_model` column of `b
#' 
#' The `texreg` package also offers several other 
#' 
#' The following chunk is an example of using `tex
#' 
## ---- tidy=FALSE, message=FALSE------------------------------------------------------------------------------
# report result
est_table <- texreg(l = beta_tab$beta_model,
                    file = 'tabs/Example_texreg.tex',
                    custom.model.names = beta_tab$ticker,
                    custom.coef.names = c('Alpha', 'Beta'),
                    digits = 2)

#' 
#' The result in a LaTex file compiled for pdf wil
#' 

#' 
#' 
#' ## Creating Reports with _RMarkdown_
#' 
#' _RMarkdown_ innovates the process of writing re
#' 
#' _Rmarkdown_'s text structure is based on [**mar
#' 
#' Let's explore how `Rmarkdown` works with a prac
#' 
#' Going forward, we will edit the report title as
#' 

#' 
#' The next choices relate to the output when comp
#' 
#' After pressing _OK_, a file will appear in the 
#' 

#' 
#' The document header is identified using the `--
#' 
#' The next piece of text is identified in Figure 
#' 
#' The `include = FALSE` item is a specific option
#' 

#' 
#' For the first chunk of code, the `knitr::opts_c
#' 
#' An important point here is that, at the time of
#' 
#' Moving on, the `## R Markdown` text in Figure \
#' 

#' 
#' The second part of \@ref(fig:rmarkdown-text) is
#' 
#' An important point here is the use of code (or 
#' 
#' > "The estimated value of beta is X_1, with a T
#'     
#' where _X*_ are numerical results from the code 
#' 

#' 
#' R will evaluate the value of objects `value_bet
#' 

#' 
#' Note here the great advantage of _RMarkdown_, t
#' 
#' The last part in \@ref(fig:rmarkdown-text) disp
#' 
#' Figure \@ref(fig:rmarkdown-figures) shows the l
#' 

#' 
#' Now that we understand the components that make
#' 

#' 
#' It's important to know that the table export pr
#' 
#' This section showed a small portion of the univ
#' 
#' 
#' ## Exercises
#' 
#' 01. Consider the Grunfeld data available at lin
#' 
#' 02. Using `BatchGetSymbols::GetSP500Stocks` fun
#' 
#' 03. Create a new report in _Rmarkdown_ covering
#' 
#' 04. [**CHALLENGE**] - Download SP500 components