#' # Financial Econometrics with R {#models}
#' 

#' 
#' The modeling tools from econometrics and statis
#' 
#' The variety of models used in financial econome
#' 
#' - Linear models (OLS)
#' - Generalized linear models (GLS)
#' - Panel data models
#' - Arima models (Integrated Autoregressive Movin
#' - Garch models (Generalized Autoregressive Cond
#' - Markov Regime switching models
#' 
#' We will not present a full description of the u
#' 
#' 
#' ## Linear Models (OLS) {#linear-models}
#' 
#' A linear  model is, without a doubt, one of the
#' 
#' In finance, the most direct and popular use of 
#' 
#' A linear model with _N_ explanatory variables c
#' 

#' 

#' 
#' The left side of the equation, (`r if (my.engin
#' 
#' 
#' ### Simulating a Linear Model
#' 
#' Consider the following equation:
#' 

#' 

#' 
#' We can use R to simulate _1.000_ observations f
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(50)

# number of obs
n_T <- 1000 

# set x as Normal (0, 1)
x <- rnorm(n_T)

# set coefficients
my_alpha <- 0.5
my_beta <- 2

# build y
y <- my_alpha + my_beta*x + rnorm(n_T)

#' 
#' Using `ggplot,` we can create a scatter plot to
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width--------------------------------
library(tidyverse)

# set temp df
temp_df <- tibble(x = x, 
                  y = y)

# plot it
p <- ggplot(temp_df, aes(x = x, y = y)) + 
  geom_point(size=0.5) + 
  labs(title = 'Example of Correlated Data') + 
  theme_bw()

print(p)

#' 
#' Clearly, there is a positive linear correlation
#' 
#' 
#' ### Estimating a Linear Model {#estimating-ols}
#' 
#' In R, the main function for estimating a linear
#' 
## ------------------------------------------------------------------------------------------------------------
# set df
lm_df <- tibble(x, y)

# estimate linear model
my_lm <- lm(data = lm_df, formula = y ~ x)
print(my_lm)

#' 
#' The `formula` argument defines the shape of the
#' 
#' Argument `formula` allows other custom options,
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(15)

# set simulated dataset
N <- 100
df <- tibble(x = runif(N),
                 y = runif(N),
                 z = runif(N),
                 group = sample(LETTERS[1:3],
                                N,
                                replace = TRUE ))

#' 
## ------------------------------------------------------------------------------------------------------------
# Vanilla formula
#
# example: y ~ x + z
# model: y(t) = alpha + beta(1)*x(t) + beta(2)*z(t) + error(t)
my_formula <- y ~ x + z
print(lm(data = df, 
         formula = my_formula))

#' 
## ------------------------------------------------------------------------------------------------------------
# vannila formula with dummies
#
# example: y ~ group + x + z
# model: y(t) = alpha + beta(1)*D_1(t)+beta(2)*D_2(t) + 
#               beta(3)*x(t) + beta(4)*z(t) + error(t)
# D_i(t) - dummy for group i
my_formula <- y ~ group + x + z
print(lm(data = df, 
         formula = my_formula))

#' 
## ------------------------------------------------------------------------------------------------------------
# Without intercept
#
# example: y ~ -1 + x + z
# model: y(t) = beta(1)*x(t) + beta(2)*z(t) + error(t)
my_formula <- y ~ -1 + x + z
print(lm(data = df, 
         formula = my_formula))

#' 
## ------------------------------------------------------------------------------------------------------------
# Using combinations of variables
# example: y ~ x*z
# model: y(t) = alpha + beta(1)*x(t) + beta(2)*z(t) + 
#               beta(3)*x(t)*z(t) + error(t)
my_formula <- y ~ x*z
print(lm(data = df, 
         formula = my_formula))

#' 
## ------------------------------------------------------------------------------------------------------------
# Interacting variables
# example: y ~ x:group + z
# model: y(t) = alpha + beta(1)*z(t) + beta(2)*x(t)*D_1(t) + 
#               beta(3)*x(t)*D_2(t) + beta(4)*x(t)*D_3(t) + 
#               error(t)
# D_i(t) - dummy for group i
my_formula <- y ~ x:group + z
print(lm(data = df, 
         formula = my_formula))

#' 
#' The different options in the `formula` input al
#' 
#' Moreover, when it comes to the output of `lm,` 
#' 
## ------------------------------------------------------------------------------------------------------------
# print names in model
print(names(my_lm))

#' 
#' As you can see, there is a slot called `coeffic
#' 
## ------------------------------------------------------------------------------------------------------------
print(my_lm$coefficients)

#' 
#' The result is a simple atomic vector that incre
#' 
#' In our example of using `lm` with simulated dat
#' 
#' Experienced researchers have probably noted tha
#' 
## ------------------------------------------------------------------------------------------------------------
print(summary(my_lm))

#' 

#' 
#' The estimated coefficients have high _T_ values
#' 
#' Additional information is available in the resu
#' 
## ------------------------------------------------------------------------------------------------------------
my_summary <- summary(my_lm)
print(names(my_summary))

#' 
#' Each element contains information that can be r
#' 
#' Now, let's move to an example with real data. F
#' 

#' 

#' 
#' First, let's load the SP500 dataset. \index{cal
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# load stock data
my_f <- afedR::afedR_get_data_file('SP500-Stocks-WithRet.rds')
my_df <- read_rds(my_f)

# select rnd asset and filter data 
set.seed(10)

my_asset <- sample(my_df$ticker,1)
my_df_asset <- my_df[my_df$ticker == my_asset, ]

# load SP500 data
df_sp500 <- read.csv(file = 'data/SP500.csv', 
                     colClasses = c('Date','numeric'))

# calculate return
calc_ret <- function(P) {
  N <- length(P)
  ret <- c(NA, P[2:N]/P[1:(N-1)] -1)
}

df_sp500$ret <- calc_ret(df_sp500$price)

# print number of rows in datasets
print(nrow(my_df_asset))
print(nrow(df_sp500))

#' 
#' You can see the number of rows of the dataset f
#' 
## ------------------------------------------------------------------------------------------------------------
# find location of dates in df_sp500
idx <- match(my_df_asset$ref.date, df_sp500$ref.date)

# create column in my_df with sp500 returns
my_df_asset$ret_sp500 <- df_sp500$ret[idx]

#' 
#' As a start, let's create a scatter plot with th
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width--------------------------------
library(ggplot2)

p <- ggplot(data = my_df_asset, 
            aes(x=ret_sp500, y=ret)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = 'lm') + 
  labs(x = 'SP500 Returns', 
       y = paste0(my_asset, ' Returns'),
       title = paste0(my_asset, ' and ', ' the SP500' ),
       caption = 'Data from Yahoo Finance') + 
  theme_bw()

print(p)

#' 
#' The figure shows a clear linear tendency;  the 
#' 
## ------------------------------------------------------------------------------------------------------------
# estimate beta model
my_beta_model <- lm(data = my_df_asset, 
                    formula = ret ~ ret_sp500)

# print it
print(summary(my_beta_model))

#' 

#' 
#' The output shows that stock `r my_asset` has a 
#' 
#' 
#' ### Statistical Inference in Linear Models {#te
#' 
#' After estimating a model with function `lm,` th
#' 
## ------------------------------------------------------------------------------------------------------------
n_T <- 100
df <- data.frame(y = runif(n_T),
                 x_1 = runif(n_T),
                 x_2 = runif(n_T))

my_lm <- lm(data = df, 
            formula = y ~ x_1 + x_2)

print(summary(my_lm))

#' 
#' In this example, the F statistic is `r summary(
#' 
#' Another type of test automatically executed by 
#' 
#' In the practice of research, it is likely that 
#' 
#' As a simple example, let's test a linear hypoth
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(10)

# number of time periods
n_T <- 1000

# set parameters
my_intercept <- 0.5
my_beta <- 1.5

# simulate
x <- rnorm(n_T)
y <- my_intercept + my_beta*x + rnorm(n_T)

# set df
df <- tibble(y, x)

# estimate model
my_lm <- lm(data = df, 
            formula = y ~ x )

#' 
#' After the estimation of the model, we use the f
#' 

#' 

#' 
#' With this matrix operation, we test the joint h
#' 
## ------------------------------------------------------------------------------------------------------------
library(car)

# set test matrix
test_matrix <- matrix(c(my_intercept,  # alpha test value
                        my_beta))  # beta test value

# hypothesis matrix 
hyp_mat <- matrix(c(1,0,
                    0,1),nrow = 2)

# do test
my_waldtest <- linearHypothesis(my_lm, 
                                hypothesis.matrix = hyp_mat, 
                                rhs = test_matrix)

# print result
print(my_waldtest)

#' 
#' As we can see, the test fails to reject the nul
#' 
#' Another family of tests commonly applied to lin
#' 
#' In R, we can use the package `lmtest` [@lmtest]
#' 
## ---- results='hold'-----------------------------------------------------------------------------------------
library(lmtest)

# Breush Pagan test 1 - Serial correlation
# Null Hypothesis: No serial correlation in residual
print(bgtest(my_lm, order = 5))

# Breush Pagan test 2 - Homocesdasticity of residuals
# Null Hypothesis: homocesdasticity 
#                  (constant variance of residuals)
print(ncvTest(my_lm))

# Durbin Watson test - Serial correlation
# Null Hypothesis: No serial correlation in residual
print(dwtest(my_lm))

# Shapiro test  - Normality
# Null Hypothesis: Data is normally distributed
print(shapiro.test(my_lm$residuals))

#' 
#' As expected, the model with artificial data pas
#' 
#' Another interesting approach for validating lin
#' 
## ------------------------------------------------------------------------------------------------------------
library(gvlma)

# global validation of model
gvmodel <- gvlma(my_lm) 

# print result
summary(gvmodel)

#' 
#' The output of `gvlma` shows several tests perfo
#' 
#' 
#' ## Generalized Linear Models (GLM)
#' 
#' The generalized linear model (GLM) is a flexibl
#' 
#' We can write a general univariate GLM specifica
#' 

#' 

#' 
#' The main difference of a GLM model and a OLS mo
#' 

#' 

#' 
#' Did you notice that function _g()_ ensures any 
#' 
#' 
#' ### Simulating a GLM Model
#' 
#' As an example, let's simulate the following GLM
#' 

#' 

#' 
#' In R, we use the following code to build the re
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(15)

# set number of obs
n_T <- 500

# set x
x = rnorm(n_T)

my_alpha <- 2
my_beta <- 5

# set probabilities
z = my_alpha + my_beta*x
p = exp(z)/(1+exp(z))

# set response variable
y = rbinom(n = n_T,
           size = 1, 
           prob = p)

#' 
#' Function `rbinom` creates a vector of 1s and 0s
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width--------------------------------
summary(y)

#' 
#' Object `y` contains zeros and ones, as expected
#' 
#' 
#' ### Estimating a GLM Model
#' 
#' In R, the estimation of GLM models is accomplis
#' 
#' Let's use the previously simulated data to esti
#' 
## ------------------------------------------------------------------------------------------------------------
# estimate GLM
df <- tibble(x, y)
my_family <- binomial(link = "logit")
my_glm <- glm(data = df, 
              formula = y ~ x , 
              family = my_family)

# print it with summary
print(summary(my_glm))

#' 
#' The estimated coefficients are close to what we
#' 
#' Function `glm` offers many options for setting 
#' 

#' 
#' The first step in using a GLM model is to ident
#' 
#' As an example, with real data, we'll use a cred
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# read default data
my_f <- afedR::afedR_get_data_file('UCI_Credit_Card.csv')
df_default <- read_csv(my_f, 
                       col_types = cols())

glimpse(df_default)

#' 
#' This is a comprehensive dataset with several pi
#' 
## ------------------------------------------------------------------------------------------------------------
library(tidyverse)

# read credit card data 
# source: 
# www.kaggle.com/uciml/default-of-credit-card-clients-dataset
# COLUMNS: GENDER: (1 = male; 2 = female). 
#          EDUCATION: 1 = graduate school; 
#                     2 = university; 
#                     3 = high school; 
#                     4 = others. 
#          MARRIAGE: 1 = married; 
#                    2 = single; 
#                    3 = others 
df_default <- df_default %>% 
  mutate(default = (default.payment.next.month == 1), 
         D_male_gender = (SEX == 1),
         age = AGE,
         educ = dplyr::recode(as.character(EDUCATION),
                              '1' = 'Grad',
                              '2' = 'University', 
                              '3' = 'High School', 
                              '4' = 'Others',
                              '5' = 'Unknown',
                              '6' = 'Unknown'),
         D_marriage = (MARRIAGE == 1)) %>%
  select(default, D_male_gender, age, educ, D_marriage) 

glimpse(df_default)


#' 
#' Much better! Now we only have the columns of in
#' 
## ------------------------------------------------------------------------------------------------------------
# estimate glm model
glm_credit <- glm(data=df_default, 
                   formula = default ~ D_male_gender +  age + 
                                       educ + D_marriage,
                   family = binomial(link = "logit"))

# show output
summary(glm_credit)

#' 
#' We find that the only coefficients with statist
#' 
#' 
#' ## Panel Data Models 
#' 
#' Panel data models are advised when the modeled 
#' 
#' The main motivation to use panel data models is
#' 
#' We can represent the simplest case of a panel d
#' 

#' 

#' 
#' Notice we now use index _i_ in the dependent an
#' 
#' 
#' ### Simulating Panel Data Models
#' 
#' Let's simulate a balanced panel data with fixed
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(25)

# number of obs for each case
n_T <- 5

# set number of groups
N <- 12

# set possible cases
possible_cases <- LETTERS[1:N]

# set parameters
my_alphas <- seq(-10, 10,
                 length.out = N)
my_beta <- 1.5

# set indep var (x) and dates
indep_var <- sapply(rep(n_T,N), rnorm)
my_dates <- Sys.Date() + 1:n_T

# create response matrix (y)
response_matrix <- matrix(rep(my_alphas, 
                              n_T), 
                          nrow = n_T, 
                          byrow = TRUE) + 
  indep_var*my_beta + sapply(rep(n_T,N),rnorm, sd = 0.25) 

# set df
sim_df <- tibble(firm = as.character(sapply(possible_cases, 
                                            rep, 
                                            times=n_T )),
                 dates = rep(my_dates, times=N),
                 y = as.numeric(response_matrix),
                 x = as.numeric(indep_var), 
                 stringsAsFactors = FALSE)

# print result
glimpse(sim_df)

#' 
#' The result is a `dataframe` object with `r nrow
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width--------------------------------
library(ggplot2)

p <- ggplot(sim_df, aes(x = x, 
                        y = y)) + 
  geom_point() + geom_line() + 
  facet_wrap(~ firm) + 
  labs(title = 'Simulated Panel Data') + 
  theme_bw()

print(p)

#' 
#' The figure shows the strong linear relationship
#' 
#' 
#' ### Estimating Panel Data Models
#' 
#' With the artificial data simulated in the previ
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
library(plm)

# estimate panel data model with fixed effects
my_pdm <- plm(data = sim_df, 
              formula = y ~ x, 
              model = 'within',
              index = c('firm','dates'))

# print coeficient
print(coef(my_pdm))

#' 
#' As expected, the slope parameter was correctly 
#' 
## ------------------------------------------------------------------------------------------------------------
print(fixef(my_pdm))

#' 
#' Again, the simulated intercept values are close
#' 
#' As an example with real data, let's use the dat
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
library(plm)

# data from Grunfeld
data("Grunfeld")

# print it
glimpse(Grunfeld)

#' 
#' The `Grunfeld` dataset contains company informa
#' 
#' A note here is important; given its high number
#' 
#' First, let's explore the raw data by estimating
#' 
## ------------------------------------------------------------------------------------------------------------
est_lm <- function(df) {
  # Estimates a linear model from Grunfeld data
  #
  # Args:
  #   df - dataframe from Grunfeld
  #
  # Returns:
  #   lm object
  
  my_model <- lm(data = df, 
                 formula = inv ~  value + capital)
  
  return(my_model)
}

# estimate model for each firm
my_l <- by(Grunfeld, 
           INDICES = Grunfeld$firm, 
           FUN = est_lm)

# print result
my_coefs <- sapply(my_l, coef)
print(my_coefs)

#' 

#' 
#' The results show a great discrepancy between th
#' 
## ------------------------------------------------------------------------------------------------------------
# test if all coef are the same across firms
my_pooltest <- pooltest(inv ~ value + capital, 
                        data = Grunfeld, 
                        model = "pooling")

# print result
print(my_pooltest)

#' 
#' The high F test and small p-value suggest the r
#' 
#' Before estimating the model, we need to underst
#' 
#' We can test the model specification using the `
#' 
## ------------------------------------------------------------------------------------------------------------
# set options for Hausman test
my_formula <- inv ~ value + capital
my_index <- c('firm','year')

# do Hausman test
my_hausman_test <- phtest(x = my_formula, 
                          data = Grunfeld,
                          model = c('within', 'random'),
                          index = my_index)

# print result
print(my_hausman_test)

#' 
#' The p-value of `r format(my_hausman_test$p.valu
#' 
#' After identifying the model, let's estimate it 
#' 
## ------------------------------------------------------------------------------------------------------------
# set panel data model with random effects
my_model <- 'random'
my_formula <- inv ~ value + capital
my_index <- c('firm','year')

# estimate it
my_pdm_random <- plm(data = Grunfeld, 
                     formula = my_formula, 
                     model = my_model,
                     index = my_index)

# print result
print(summary(my_pdm_random))

#' 

#' 
#' As expected, the coefficients are significant a
#' 
#' For one last example of using R in panel models
#' 
#' The `systemfit` package offers a function with 
#' 
## ------------------------------------------------------------------------------------------------------------
library(systemfit)

# set pdataframe
p_Grunfeld <- pdata.frame(Grunfeld, c( "firm", "year" ))

# estimate sur
my_SUR <- systemfit(formula = inv ~ value + capital,
                    method =  "SUR",
                    data = p_Grunfeld)
print(my_SUR)

#' 
#' The output object `my_SUR` contains the estimat
#' 
#' 
#' ## Arima Models
#' 
#' Arima is a special type of model that uses the 
#' 
#' A simple example of an Arima model is defined b
#' 

#' 

#' 
#' In this example, we have an ARIMA(AR = 1, D = 0
#' 
#' 
#' ### Simulating Arima Models
#' 
#' First, let's simulate an Arima model using func
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
set.seed(1)

# set number of observations
my_T <- 5000

# set model's parameters
my_model <- list(ar = 0.5, 
                 ma = -0.1)
my_sd <- 1

# simulate model
my_ts <- arima.sim(n = my_T, 
                   model = my_model , 
                   sd = my_sd)

#' 
#' We can look at the result of the simulation by 
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
library(ggplot2)

# set df
temp_df <- data.frame(y = unclass(my_ts), 
                      date = Sys.Date() + 1:my_T)

p <- ggplot(temp_df, aes(x = date, y = y)) + 
  geom_line(size=0.25) + 
  labs(title = 'Simulated ARIMA Model') + 
  theme_bw()

print(p)

#' 
#' The graph shows a time series with an average c
#' 
#' 
#' ### Estimating Arima Models {#arima-estimating}
#' 
#' To estimate an Arima model, we use function `ar
#' 
## ------------------------------------------------------------------------------------------------------------
# estimate arima model
my_arima <- arima(my_ts, order = c(1,0,1))

# print result
print(coef(my_arima))

#' 
#' As expected, the estimated parameters are close
#' 
## ------------------------------------------------------------------------------------------------------------
attributes(summary(my_arima))

#' 
#' We have the adjustment criteria in `aic`, resid
#' 
#' The identification of the Arima model,  definin
#' 
#' In the next example, we use the function `auto.
#' 
## ------------------------------------------------------------------------------------------------------------
library(BatchGetSymbols)

df_SP500 <- BatchGetSymbols(tickers = '^GSPC', 
                            first.date = '2015-01-01', 
                            last.date = '2019-01-01')$df.tickers

#' 
#' Before estimating the model, we need to check t
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
library(tseries)
print(adf.test(na.omit(df_SP500$ret.adjusted.prices)))

#' 
#' The result of the test shows a small p-value th
#' 
## ---- message=FALSE------------------------------------------------------------------------------------------
print(adf.test(df_SP500$price.close))

#' 
#' This time, we easily fail to reject the null hy
#' 
#' One issue in working with Arima models is with 
#' 
## ------------------------------------------------------------------------------------------------------------
library(forecast)

# estimate arima model with automatic identification
my_autoarima <- auto.arima(x = df_SP500$ret.closing.prices)

# print result
print(my_autoarima)

#' 
#' The result tells us the best model for the retu
#' 
#' 
#' ### Forecasting Arima Models
#' 
#' We can obtain the forecasts of an Arima model w
#' 
## ------------------------------------------------------------------------------------------------------------
# forecast model
print(forecast(my_autoarima, h = 5))

#' 
#' 
#' ## GARCH Models
#' 
#' GARCH (Generalized Autoregressive Conditional H
#' 
#' A GARCH model is modular. In its simplest forma
#' 

#' 

#' 
#' The `r if (my.engine!='epub3') {'$y_t$'} else {
#' 
#' 
#' ### Simulating Garch Models
#' 
#' In CRAN, we can find two main packages related 
#' 
#' In `fGarch,` we simulate a model using function
#' 
## ---- tidy=FALSE, message=FALSE------------------------------------------------------------------------------
library(fGarch)

# set list with model spec
my_model = list(omega=0.001, 
                alpha=0.15, 
                beta=0.8, 
                mu=0.02, 
                ar = 0.1)

# set garch spec				
spec = garchSpec(model = my_model)

# print it
print(spec)

#' 
#' The previous code defines a Garch model equival
#' 

#' 

#' 
#' To simulate _1000_ observations of this model, 
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(20)
# simulate garch model
sim_garch = garchSim(spec, n = 1000)

#' 
#' We can visualize the artificial time series gen
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width--------------------------------
# set df for ggplot
temp_df <- tibble(sim.ret = sim_garch$garch, 
                  idx=seq_along(sim_garch$garch))

p <- ggplot(temp_df, aes(x = idx, 
                         y = sim.ret)) + 
  geom_line() + 
  labs(title = 'Simulated time series of garch model',
       y = 'Value of Time Series',
       x = '') + 
  theme_bw()

print(p)

#' 
#' The behavior of the simulated series is similar
#' 
#' 
#' ### Estimating Garch Models {#estimating-garch}
#' 
#' The estimation of the parameters from a GARCH m
#' 
#' In the following example, we estimate a Garch m
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
# estimate garch model
my_form <- formula('sim.ret ~ arma(1,0) + garch(1,1)')

my_garchfit <- garchFit(
  data = sim_garch, 
  formula = my_form,
  trace = FALSE)

#' 
#' To learn more about the estimated model, we can
#' 
## ------------------------------------------------------------------------------------------------------------
print(my_garchfit) 

#' 
#' The resulting parameters from the estimation ar
#' 
#' Now, we will conduct another example using real
#' 
## ------------------------------------------------------------------------------------------------------------
library(MTS)

# test for Arch effects
archTest(rt = na.omit(df_SP500$ret.adjusted.prices))

#' 
#' The evidence is strong for Arch effects in the 
#' 
## ------------------------------------------------------------------------------------------------------------
# set object for estimation
df_est <- as.timeSeries(na.omit(df_SP500))

# estimate garch model for SP500
my_garchfit_SP500 <- garchFit(
  data = df_est, 
  formula = ret.adjusted.prices ~ arma(1,0) + 
                                 garch(1,1),
  trace = FALSE)

# print model
print(my_garchfit_SP500)

#' 
#' As expected, all Garch coefficients are signifi
#' 
#' 
#' ### Forecasting Garch Models
#' 
#' Forecasting Garch models involves two elements:
#' 
#' In package `fGarch`, both forecasts are calcula
#' 
## ------------------------------------------------------------------------------------------------------------
# static forecast for garch
my_garch_forecast <- predict(my_garchfit_SP500, n.ahead = 3)

# print df
print(my_garch_forecast)

#' 
#' The first column of the previous result is the 
#' 
#' 
#' ## Regime Switching Models 
#' 
#' Markov regime-switching models are a specificat
#' 
#' If we want to motivate the model, we need to co
#' 

#' 

#' 
#' where `r if (my.engine!='epub3') {'$S_t=1..k$'}
#' 
#' Now, let's assume the previous model has two st
#' 

#' 

#' 
#' where:
#' 

#' 

#' 
#' This representation implies two processes for t
#' 
#' We will now look at a financial example where t
#' 
#' The different volatilities represent higher unc
#' 
#' The changes in the states in the model can be s
#' 
#' Markov switching is a special type of model for
#' 

#' 

#' 
#' In the previous matrix, row _i_, column _j_ con
#' 
#' 
#' ### Simulating Regime Switching Models
#' 
#' In R, two packages are available for handling u
#' 
## ---- eval=FALSE---------------------------------------------------------------------------------------------
## install.packages("fMarkovSwitching",
##                  repos="http://R-Forge.R-project.org")

#' 
## ---- include = FALSE----------------------------------------------------------------------------------------

if (!require('fMarkovSwitching')){
  install.packages("fMarkovSwitching", 
                   repos="http://R-Forge.R-project.org")
}


#' 
#' Once it is installed, let's look at its functio
#' 
## ------------------------------------------------------------------------------------------------------------
library(fMarkovSwitching)

print(ls('package:fMarkovSwitching'))

#' 
#' The package includes functions for simulating, 
#' 

#' 

#' 
#' The transition matrix will be given by:
#' 

#' 

#' 
#' This model has two states with different volati
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(10)
library(fMarkovSwitching)

# number of obs
n_T <- 500 

# distribution of residuals
distrib <- "Normal"	

# number of states
k <- 2 	

# set transition matrix
P <- matrix(c(.9 ,.2,
              .1 ,.8), 
            nrow = 2, 
            byrow = T)

# set switching flag		   
S <- c(0,1)

# set parameters of model (see manual for details)
nS_param <- matrix(0)    
S_param <- matrix(0,sum(S),k)
S_param[,1] <-  .5         
S_param[,2] <- -.5

# set variance of model
sigma <- matrix(0, 1, k)
sigma[1,1] <- sqrt(0.25)  # state 1
sigma[1,2] <- 1           # state 2

# build list
Coeff <- list(P = P               ,
              S = S               ,
              nS_param = nS_param ,
              S_param = S_param   ,
              sigma = sigma       )

# simulate model
my_ms_simul <- MS_Regress_Simul(nr = n_T,
                                Coeff = Coeff, 
                                k = k, 
                                distrib = distrib)

#' 
#' In the simulation function, argument `nS_param`
#' 
#' Once the model is simulated and available, let'
#' 
## ---- fig.height=my.fig.height, fig.width=my.fig.width-------------------------------------------------------
library(ggplot2)
df_to_plot <- tibble(y = my_ms_simul@dep, 
                         x = Sys.Date()+1:my_ms_simul@nr,
                         states = my_ms_simul@trueStates[, 1])

p <- ggplot(data = df_to_plot, 
            aes(y = y, x = seq_along(y))) + 
  geom_line() + 
  labs(title = 'Simulated markov switching process',
       x = '', 
       y = 'Value') + 
  theme_bw()

print(p)

#' 
#' We can also look at the simulated states:
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width--------------------------------
library(ggplot2)
df_to_plot <- tibble(y = my_ms_simul@dep, 
                         x = Sys.Date()+1:my_ms_simul@nr,
                         states = my_ms_simul@trueStates[,1])

p <- ggplot(data = df_to_plot, 
            aes(y = states, x = x)) + 
  geom_line() + 
  labs(y = 'Probability of state 1') + 
  theme_bw()

print(p)

#' 
#' As expected, the model is switching from one st
#' 
#' 
#' ### Estimating Regime Switching Models
#' 
#' We can estimate a univariate Markov switching m
#' 
## ---- message=FALSE, results='hide', cache=TRUE--------------------------------------------------------------
# set dep and indep 
dep <- my_ms_simul@dep
indep <- my_ms_simul@indep

# set switching parameters and distribution
S <- c(0,1)	
k <- 2		
distIn <- "Normal" 

# estimate the model
my_ms_model <- MS_Regress_Fit(dep, indep, S, k)

#' 
#' Argument `dep` and `indep` sets the variables i
#' 
## ------------------------------------------------------------------------------------------------------------
# print estimation output
print(my_ms_model)

#' 
#' The estimated coefficients are close to the one
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=6, fig.width=7-------------------------------------------------------
plot(my_ms_model)	# plotting output

#' 
#' As an example with real data, let's estimate th
#' 
## ---- message=FALSE,results='hide', cache=TRUE---------------------------------------------------------------
library(BatchGetSymbols)

df_SP500 <- BatchGetSymbols(tickers = '^GSPC', 
                            first.date = '2010-01-01', 
                            last.date = '2019-01-01')$df.tickers


# set input objects to MS_Regress_Fit
ret <- na.omit(df_SP500$ret.closing.prices)
dep <- matrix(ret, nrow = length(ret))
indep <- matrix(rep(1, length(dep)),nrow = length(dep))

S <- c(1)	# where to switch (in this case in the only indep)
k <- 2		# number of states
distIn <- "Normal" #distribution assumption

# estimating the model
my_SP500_MS_model <- MS_Regress_Fit(dep, indep, S, k)	

#' 
#' And now, we check the result.
#' 
## ------------------------------------------------------------------------------------------------------------
# printing output
print(my_SP500_MS_model)	

#' 
#' 

#' 
#' The model identified two volatility regimes fro
#' 
#' A common figure in the analysis of Markov switc
#' 
#' 
## ---- fig.height=my.fig.height, fig.width=my.fig.width-------------------------------------------------------
# get variables for plot
smooth.prob <- as.numeric(my_SP500_MS_model @smoothProb[ , 1])
price <- df_SP500$price.close[2:nrow(df_SP500)]
ref_dates <- df_SP500$ref.date[2:nrow(df_SP500)]

# build long df to plot
df.to.plot <- tibble(type = c(rep('Probabilities Bull Market', 
                                  length(smooth.prob)),
                              rep('SP500', 
                                  length(smooth.prob))),
                     ref.date = rep(ref_dates ,2),
                     value = c(smooth.prob,
                               price) )

# plot with ggplot
p <- ggplot(df.to.plot,
            aes(y=value, x =ref.date)) +
  geom_line(size = 0.5) + 
  facet_wrap(~type, nrow = 2, scales = 'free_y') + 
  labs(x = '',
       y = 'Value',
       title = 'SP500 and its bull market states',
       subtitle = 'Prob. from a markov regime switching model',
       caption = 'Data from Yahoo Finance')

# plot it!
print(p)

#' 
#' The figure shows how the price increases in sta
#' 
#' 
#' ### Forecasting Regime Switching Models
#' 
#' Package `MS_Regress` provides function `MS_Regr
#' 
## ------------------------------------------------------------------------------------------------------------
# make static forecast of regime switching model
newIndep <- 1

my_for <- MS_Regress_For(my_SP500_MS_model , newIndep)

# print output
print(my_for)

#' 

#' 
#' The model predicts, the day after the last date
#' 
#' 
#' ## Dealing with Several Models
#' 
#' In the practice of research, we will likely est
#' 
#' In chapter \@ref(programming), we learned we co
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed(10)

# set number of stocks
n_stocks <- 4

# load data from .rds
my_f <- afedR::afedR_get_data_file('SP500-Stocks-WithRet.rds')
df_stocks <- read_rds(my_f) 
  
# select tickers
my_tickers <- sample(unique(df_stocks$ticker), n_stocks)

# set my_df
df_temp <- df_stocks %>% 
  dplyr::filter(ticker %in% my_tickers)

# renew factors in ticker
df_temp$ticker <- as.factor(as.character(df_temp$ticker))

#' 
#' Now, what we want to do with this data is separ
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
my_l <- tapply(X = df_temp$ret, 
               INDEX = df_temp$ticker, 
               FUN = arima, 
               order = c(1, 0, 0))

#' 
#' Each model is available in `my_l`. To retrieve 
#' 
## ------------------------------------------------------------------------------------------------------------
# print all coefficients
print(sapply(X = my_l, 
             FUN = coef))

#' 
#' A limitation is, by using `tapply`, we are rest
#' 
#' For an example, we are going to estimate severa
#' 
## ------------------------------------------------------------------------------------------------------------
# load SP500 data
df_sp500 <- read.csv(file = 'data/SP500.csv', 
                     colClasses = c('Date','numeric'))

# calculate return
df_sp500$ret <- calc.ret(df_sp500$price.close)


# find location of dates in df_sp500
idx <- match(df_stocks$ref.date, 
             df_sp500$ref.date)

# create column in my_df with sp500 returns
df_stocks$ret.sp500 <- df_sp500$ret[idx]

#' 
#' The next step is to create a function that will
#' 
## ------------------------------------------------------------------------------------------------------------
estimate_beta <- function(df) {
  # Function to estimate beta from dataframe of stocks returns
  #
  # Args:
  #   df - Dataframe with columns ret and ret.sp500
  #
  # Returns:
  #   The value of beta
  
  my_model <- lm(data = df, 
                 formula = ret ~ ret.sp500)
  
  return(coef(my_model)[2])
}

#' 
#' Now, we can use the previous function with `by.
#' 
## ------------------------------------------------------------------------------------------------------------
# calculate beta for each stock
my_betas <- by(data = df_stocks, 
               INDICES = df_stocks$ticker, 
               FUN = estimate_beta)

glimpse(as.numeric(my_betas))

#' 
#' The values of the different `betas` are availab
#' 
## ---- eval=TRUE, tidy=FALSE, fig.height=my.fig.height, fig.width=my.fig.width--------------------------------
library(ggplot2)

df_to_plot <- tibble(betas = as.numeric(my_betas)) 

p <- ggplot(df_to_plot, aes(x = my_betas)) +
  geom_histogram(bins = 40) + 
  labs(x = 'Betas',
       y = 'Frequency',
       title = 'Histogram of Betas for SP500 stocks',
       subtitle = paste0('Market models estimated with data from ',
                         min(df_stocks$ref.date), ' to ', 
                         max(df_stocks$ref.date), '\n',
                         length(unique(df_stocks$ticker)), 
                         ' stocks included'),
       caption = 'Data from Yahoo Finance') + 
  theme_bw()
  

print(p)

#' 
#' For the SP500 data, we find no negative value o
#' 
#' Another way of storing and managing several mod
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
library(dplyr)

my_tab <- df_stocks %>%
  group_by(ticker) %>%
  do(my_model = arima(x = .$ret, order = c(1,0,0)))

glimpse(my_tab)

#' 
#' We have a list-column, called `my_model`, stori
#' 
## ---- tidy=FALSE---------------------------------------------------------------------------------------------
my_model_tab <- df_stocks %>%
  group_by(ticker) %>%
  do(my_model = arima(x = .$ret, order = c(1,0,0))) %>%
  mutate(alpha = coef(my_model)[2],
         ar1 = coef(my_model)[1])

glimpse(my_model_tab)

#' 
#' Another trick in handling models with `dplyr` i
#' 
## ---- message=FALSE, tidy=FALSE------------------------------------------------------------------------------
library(broom)

# get coefs with tidy
my_coef_tab <- my_model_tab %>% 
  tidy(my_model)

# print result
print(head(my_coef_tab))

#' 
#' Notice how function `tidy` included the estimat
#' 
## ------------------------------------------------------------------------------------------------------------
# get info on models
my_info_models <- my_model_tab %>% 
  glance(my_model)

print(head(my_info_models))

#' 
#' It includes information about coefficients and 
#' 
#' 
#' ## Exercises
#' 
#' 01. Simulate the following linear process in R:
#' 
## ------------------------------------------------------------------------------------------------------------
set.seed (5)

# number of obs
nT <- 100

# set x as Normal (0, 1)
x <- rnorm (nT)

# set coefficients
my_alpha <- 1.5
my_beta <- 0.5

# build y
y <- my_alpha + my_beta * x + rnorm (nT, sd = 5)

#' 
#' Using the simulated data, `x` and `y` estimate 
#' 
#' 02. Using package `car` and the data we previou
#' 
#' 03. Use package `gvlma` to test the OLS assumpt
#' 
#' 04. **CHALLENGE** - Using your programming skil
#' 
#' 05. From package `BatchGetSymbols,` use functio
#' 
#' 06. For the same stock data from the previous e
#' 
#' 07. Using the tidyverse functions `dplyr::group
#' 
#' 08. Using the same "pipeline" code as the previ
#' 
#' 09. For the same SP500 database, set `set.seed(