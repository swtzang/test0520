#================================================================================================
# https://systematicinvestor.wordpress.com/2011/12/13/backtesting-minimum-variance-portfolios/
#
# Load Systematic Investor Toolbox (SIT)
# setInternet2(TRUE)
rm(list=ls())
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
library(pacman)
p_load(quantmod, quadprog, lpSolve)
#1. S&P500 (SPY)
#2. Nasdaq 100 (QQQ)
#3. Emerging Markets (EEM)
#4. Russell 2000 (IWM)
#5. MSCI EAFE (EFA)
#6. Long-term Treasury Bonds (TLT)
#7. Real Estate (IYR)
#8. Gold (GLD)

tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')


data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

data.weekly <- new.env()
for(i in tickers) data.weekly[[i]] = to.weekly(data[[i]], indexAt='endof')

bt.prep(data, align='remove.na', dates='1990::2011')
bt.prep(data.weekly, align='remove.na', dates='1990::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   
n = ncol(prices)

# find week ends
head(prices)
week.ends = endpoints(prices, 'weeks')
week.ends = week.ends[week.ends > 0]     

# Equal Weight 1/N Benchmark
data$weight[] = NA
data$weight[week.ends,] = ntop(prices[week.ends,], n)       

capital = 100000
data$weight[] = (capital / prices) * data$weight
equal.weight = bt.run(data, type='share')
head(equal.weight$ret)


#*****************************************************************
# Create Constraints
#*****************************************************************
constraints = new.constraints(n, lb = -Inf, ub = +Inf)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        


ret = prices / mlag(prices) - 1
weight = coredata(prices)
weight[] = NA
#
data$weight[64:68,]
#
for( i in week.ends[week.ends >= (63 + 1)] ) {
  # one quarter is 63 days
  hist = ret[ (i- 63 +1):i, ]
  
  # create historical input assumptions
  ia = create.historical.ia(hist, 252)
  s0 = apply(coredata(hist),2,sd)     
  ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
  
  weight[i,] = min.risk.portfolio(ia, constraints)
}

head(weight, 70)

# Minimum Variance
data$weight[] = weight      
capital = 100000
data$weight[] = (capital / prices) * data$weight
min.var.daily = bt.run(data, type='share', capital=capital)

# Next let’s create Minimum Variance portfolios using weekly data:
#*****************************************************************
# Code Strategies: Weekly
#******************************************************************     
retw = data.weekly$prices / mlag(data.weekly$prices) - 1
weightw = coredata(prices)
weightw[] = NA

for( i in week.ends[week.ends >= (63 + 1)] ) {   
  # map
  j = which(index(ret[i,]) == index(retw))
  
  # one quarter = 13 weeks
  hist = retw[ (j- 13 +1):j, ]
  
  # create historical input assumptions
  ia = create.historical.ia(hist, 52)
  s0 = apply(coredata(hist),2,sd)     
  ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
  
  weightw[i,] = min.risk.portfolio(ia, constraints)
}   

data$weight[] = weightw     
capital = 100000
data$weight[] = (capital / prices) * data$weight
min.var.weekly = bt.run(data, type='share', capital=capital)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt.custom.report.part1(min.var.weekly, min.var.daily, equal.weight)

# plot Daily and Weekly transition maps
layout(1:2)
plotbt.transition.map(min.var.daily$weight)
legend('topright', legend = 'min.var.daily', bty = 'n')
plotbt.transition.map(min.var.weekly$weight)
legend('topright', legend = 'min.var.weekly', bty = 'n')
#
# I find it very interesting that the Minimum Variance portfolios constructed 
# using daily returns to create input assumptions are way different from the 
# Minimum Variance portfolios constructed using weekly returns to create 
# input assumptions. One possible explanation for this discrepancy was examined 
# by Pat Burns in the The volatility mystery continues post.
# https://www.portfolioprobe.com/2011/12/05/the-volatility-mystery-continues/
# https://www.portfolioprobe.com/2011/11/08/the-mystery-of-volatility-estimates-from-daily-versus-monthly-returns/

#-------------------------------------------------------------------------------
# make covariance matrix estimate more stable, use the Ledoit-Wolf 
# covariance shrinkage estimator from tawny package
ia$cov = tawny::cov.shrink(hist)
# or
ia$cov = cor(coredata(hist), use='complete.obs',method='spearman') * (s0 %*% t(s0))
# or
ia$cov = cor(coredata(hist), use='complete.obs',method='kendall') * (s0 %*% t(s0))





