# https://systematicinvestor.wordpress.com/2012/05/01/volatility-position-sizing-to-improve-risk-adjusted-performance/
# Technical indicators: Average True Range (ATR)
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
rm(list=ls())

con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')   
tickers = spl('SPY')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)            
bt.prep(data, align='keep.all', dates='1970::') 

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   
nperiods = nrow(prices)

models = list()

#*****************************************************************
# Buy & Hold
#****************************************************************** 
data$weight[] = 0
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)
# Next, let’s modify Buy & Hold strategy to vary it’s allocation according to the 
# Average True Range (ATR).
#*****************************************************************
# Volatility Position Sizing - ATR
#****************************************************************** 
atr = bt.apply(data, function(x) ATR(HLC(x),20)[,'atr'])
atr

# position size in units = ((porfolio size * % of capital to risk)/(ATR*2)) 
data$weight[] = NA
capital = 100000

# risk 2% of capital
data$weight[] = (capital * 2/100) / (2 * atr)

# make sure you are not committing more than 100%
max.allocation = capital / prices
data$weight[] = iif(data$weight > max.allocation, max.allocation,data$weight)

models$buy.hold.2atr = bt.run(data, type='share', capital=capital)                  

#*****************************************************************
# Create Report
#******************************************************************     
models = rev(models)

plotbt.custom.report.part1(models)

plotbt.custom.report.part2(models)



















