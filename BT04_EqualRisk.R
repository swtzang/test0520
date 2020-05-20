# https://systematicinvestor.wordpress.com/2012/03/19/backtesting-asset-allocation-portfolios/
# https://www.portfolioprobe.com/2011/04/13/unproxying-weight-constraints/
# how Equal Risk Contribution portfolio can be formulated and solved using a non-linear solver.
# backtest Equal Risk Contribution portfolio and 
# other Asset Allocation portfolios based on various risk measures

#--------------------------------------------------------------------------
# Equal Risk Contribution portfolio
#--------------------------------------------------------------------------
install.packages("Rdonlp2", repos="http://R-Forge.R-project.org")
library(Rdonlp2)
#
ia = aa.test.create.ia()
n = ia$n        

# 0 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

# find Equal Risk Contribution portfolio 
w = find.erc.portfolio(ia, constraints) 

# compute Risk Contributions    
risk.contributions = portfolio.risk.contribution(w, ia)

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
library(pacman)
p_load(quantmod, quadprog, corpcor, lpSolve)
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                            
bt.prep(data, align='remove.na', dates='1990::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   
n = ncol(prices)

# find week ends
period.ends = endpoints(prices, 'weeks')
period.ends = period.ends[period.ends > 0]

#*****************************************************************
# Create Constraints
#*****************************************************************
constraints = new.constraints(n, lb = 0, ub = 1)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)        

#*****************************************************************
# Create Portfolios
#*****************************************************************          
ret = prices / mlag(prices) - 1
start.i = which(period.ends >= (63 + 1))[1]

weight = NA * prices[period.ends,]
weights = list()
# Equal Weight 1/N Benchmark
weights$equal.weight = weight
weights$equal.weight[] = ntop(prices[period.ends,], n)  
weights$equal.weight[1:start.i, ] = NA

weights$min.var = weight
weights$min.maxloss = weight
weights$min.mad = weight
weights$min.cvar = weight
weights$min.cdar = weight
weights$min.cor.insteadof.cov = weight
weights$min.mad.downside = weight
weights$min.risk.downside = weight

# following optimizations use a non-linear solver
weights$erc = weight        
weights$min.avgcor = weight     

risk.contributions = list() 
risk.contributions$erc = weight     

# construct portfolios
# j = start.i = 14

for( j in start.i:len(period.ends) ) {
  i = period.ends[j]
  
  # one quarter = 63 days
  hist = ret[ (i- 63 +1):i, ]
  
  # create historical input assumptions
  ia = create.historical.ia(hist, 252)
  s0 = apply(coredata(hist),2,sd)     
  ia$correlation = cor(coredata(hist), use='complete.obs',method='pearson')
  ia$cov = ia$correlation * (s0 %*% t(s0))
  
  # construct portfolios based on various risk measures
  weights$min.var[j,] = min.risk.portfolio(ia, constraints)
  weights$min.maxloss[j,] = min.maxloss.portfolio(ia, constraints)
  weights$min.mad[j,] = min.mad.portfolio(ia, constraints)
  weights$min.cvar[j,] = min.cvar.portfolio(ia, constraints)
  weights$min.cdar[j,] = min.cdar.portfolio(ia, constraints)
  weights$min.cor.insteadof.cov[j,] = min.cor.insteadof.cov.portfolio(ia, constraints)
  weights$min.mad.downside[j,] = min.mad.downside.portfolio(ia, constraints)
  weights$min.risk.downside[j,] = min.risk.downside.portfolio(ia, constraints)
  
  # following optimizations use a non-linear solver       
  constraints$x0 = weights$erc[(j-1),]
  weights$erc[j,] = find.erc.portfolio(ia, constraints)       
  
  constraints$x0 = weights$min.avgcor[(j-1),]
  weights$min.avgcor[j,] = min.avgcor.portfolio(ia, constraints)                      
  
  risk.contributions$erc[j,] = portfolio.risk.contribution(weights$erc[j,], ia)
}

#*****************************************************************
# Create strategies
#******************************************************************         
models = list()
for(i in names(weights)) {
  data$weight[] = NA
  data$weight[period.ends,] = weights[[i]]    
  models[[i]] = bt.run.share(data, clean.signal = F)
}

#*****************************************************************
# Create Report
#****************************************************************** 
models = rev(models)

# Plot perfromance
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)

# Plot Strategy Statistics  Side by Side
plotbt.strategy.sidebyside(models)

# Plot transition maps
layout(1)
#
pdf(file = 'report.pdf', width=8.5, height=20)
#png(filename = 'plot_12.png', width = 1200, height = 1800, units = 'px', pointsize = 12, bg = 'white')	
layout(1:len(models))
for(m in names(models)) {
  plotbt.transition.map(models[[m]]$weight, name=m)
  legend('topright', legend = m, bty = 'n')
}
dev.off()
# Plot risk contributions
layout(1:len(risk.contributions))
for(m in names(risk.contributions)) {
  plotbt.transition.map(risk.contributions[[m]], name=paste('Risk Contributions',m))
  legend('topright', legend = m, bty = 'n')
}

# Compute portfolio concentration and turnover stats based on the
# property of equally-weighted risk contributions portfolios by S. Maillard, 
# T. Roncalli and J. Teiletche (2008), page 22
# http://www.thierry-roncalli.com/download/erc.pdf
out = compute.stats( rev(weights),
                     list(Gini=function(w) mean(portfolio.concentration.gini.coefficient(w), na.rm=T),
                          Herfindahl=function(w) mean(portfolio.concentration.herfindahl.index(w), na.rm=T),
                          Turnover=function(w) 52 * mean(portfolio.turnover(w), na.rm=T) ))

out[] = plota.format(100 * out, 1, '', '%')
plot.table(t(out))

#
mean(portfolio.concentration.gini.coefficient(weights), na.rm=T)

lapply(weights, function(w) mean(portfolio.concentration.gini.coefficient(w), na.rm=T))









