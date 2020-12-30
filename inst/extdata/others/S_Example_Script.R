if(!require(ggplot2)) install.packages('ggplot2')
if(!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
if(!require(dplyr)) install.packages('dplyr')

# set tickers
my_tickers <- c('FB', 'GM')

# get data
l_out <- BatchGetSymbols(tickers = my_tickers,
                             first.date = '2015-01-01',
                             last.date = '2019-01-01')

df_prices <- l_out$df.tickers


# make plot
p <- ggplot(data = df_prices, aes(x = ref.date,
                                  y = price.adjusted))+
  geom_line() + facet_wrap(~ticker) +
  theme_bw()

x11() ; print(p)

# make table
tab <- df_prices %>%
  group_by(ticker) %>%
  summarise(total_return = last(price.adjusted)/first(price.adjusted) - 1)

print(tab)
