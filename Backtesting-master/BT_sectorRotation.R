#================================================================================
# Minimum variance section rotation
# https://quantivity.wordpress.com/2011/04/20/minimum-variance-sector-rotation/
#================================================================================
library(pacman)
p_load(xts, tseries)
install.packages('https://CRAN.R-project.org/package=tawny.types')
install.packages("tawny")
library(tawny)

# MODEL PARAMETERS
s <- "2003-01-01"
spyS <- "2004-01-01"
e <- "2011-01-01"
q <- "AdjClose"
usEquities <- c("XLB", "XLE", "XLF", "XLK", "XLI", "XLP", "XLU", "XLV", "XLY")
usEquityNames <- c("materials", "energy", "financials", "tech", "industrial", "staples", "utilities", "healthcare", "discretionary")
colors <- c('black', 'red', 'blue', 'green', 'orange', 'purple', 'yellow', 'brown', 'pink');
usClose <- as.xts(data.frame(lapply(usEquities, get.hist.quote, start=s, end=e, quote=q)))
usRets <- xts(data.frame(lapply(log(usClose), diff)), order.by=index(usClose))[2:nrow(usClose)]
colnames(usRets) <- usEquities
spy <- get.hist.quote("SPY", start=spyS, end=e, quote=q)

# ANNUALIZE TRADE RETURNS AND CALCULATE MVP WEIGHTS
annualNames <- array(c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010"))
annualReturns <- do.call(rbind, sapply(annualNames, function (yr) { usRets[yr] }))
annualWeights <- t(sapply(c(1:length(annualNames)), function(i) { minvar(annualReturns[annualNames[i]]) } ))
colnames(annualWeights) <- usEquities
rownames(annualWeights) <- annualNames
annualTradeRets <- matrix(vapply(c(1:(nrow(annualNames)-1)), function (i) { r <- cumsum(annualReturns[annualNames[i+1]] %*% annualWeights[i,]); r[length(r)] }, -100))
dailyPnL <- do.call(rbind, sapply(c(1:(nrow(annualWeights)-1)), function (i) { matrix(annualReturns[annualNames[i+1]] %*% annualWeights[i,]) }))

# PLOT LONGITUDINAL EVOLUTION OF PCA COMPONENT VARIANCE
pcaStds <- do.call(cbind, lapply(annualNames, function(yr) { sdev <- princomp(covmat=cov.shrink(annualReturns[yr]))$sdev; sdev^2/sum(sdev^2) }))
colnames(pcaStds) <- annualNames
pcaStdMeans <- matrix(rowMeans(pcaStds))
demeanedPcaStds <- sweep(pcaStds, 1, rowMeans(pcaStds), "-")
plot(pcaStds[1,], ylim=range(pcaStds), type='l', xaxt="n", xlab="Year", ylab="Proportion of Variance", main="Longitudinal PCA Variance Decomposition by Component")
lapply(c(2:5), function (i) { lines(pcaStds[i,], type='l', col=colors[i])})
axis(1, 1:nrow(annualNames), annualNames)
legend(.45,legend=rownames(pcaStds)[1:5], fill=c(colors[1:5]), cex=0.5)

# PLOT SECTOR RETURNS
par(mfrow=c(3,3))
sapply(c(1:(ncol(usRets))), function (i) { plot(cumsum(usRets[,i]), type='l', xlab="", ylab="Return", main=format(usEquityNames[i])) })

# PLOT LONGITUDINAL ANNUAL WEIGHTS
plot(annualWeights[,1], ylim=range(annualWeights), type='o', ylab="Weight", xlab="Year", xaxt="n", main="Annualized Minimum Variance Sector Weights", col=colors[1])
axis(1, 1:nrow(annualWeights), rownames(annualWeights))
for (i in c(2:ncol(annualWeights))) {
  lines(annualWeights[,i], col=colors[i], type='o')
}
legend(-.4,legend=usEquityNames, fill=c(colors), cex=0.5)

# PLOT CUMULATIVE DAILY RETURNS
par(mfrow=c(3,3))
sapply(c(1:(nrow(annualWeights)-1)), function (i) { plot(cumsum(annualReturns[annualNames[i+1]] %*% annualWeights[i,]), type='l', xlab="Trading Day", ylab="Return", main=format(annualNames[i+1])) })

# PLOT DAILY PNL
cumDailyPnL <- cumsum(dailyPnL)
cumSpy <- cumsum(diff(log(coredata(spy))))
maxRange <- max(range(cumDailyPnL), range(cumSpy))
minRange <- min(range(cumDailyPnL), range(cumSpy))
plot(cumDailyPnL, type='l', xlab="Trading Day", ylab="Return", main="Annualized Minimum Variance Sector Strategy P&L", ylim=c(minRange, maxRange))
lines(cumSpy, type='l', col='red')
legend(.6,legend=c("MVP","SPY"), fill=c("black", "red"), cex=0.5)
axis(1,index(usClose))

# PRINT STRATEGY SUMMARY STATISTICS
plSummary(dailyPnL)

# FUNCTION TO GENERAT WEIGHTS FOR MVP FROM A RETURN SERIES
minvar <- function(rets) {
  N <- ncol(rets)
  zeros <- array(0, dim = c(N,1))
  aMat <- t(array(1, dim = c(1,N)))
  res <- solve.QP(cov.shrink(rets), zeros, aMat, bvec=1, meq = 1)
  return (res$solution)
}

# FUNCTION TO PRETTY PRINT STRATEGY STATISTICS
plSummary <-function(dailyPnL)
{
  cumDailyPnL <- cumprod(1 + dailyPnL) - 1
  cat("Max drawdown:", (maxdrawdown(dailyPnL)$maxdrawdown * 100), "%\n")
  cat("Std dev:", sd(dailyPnL), "\n")
  cat("Sharpe:", sharpe(cumDailyPnL), "\n")
  win <- mean(ifelse(dailyPnL > 0, 1, 0))
  cat("Wins:", (win*100), "%\n")
  cat("Losses:", ((1-win)*100), "%\n")
  cat("Average Win:",(mean(ifelse(dailyPnL > 0, dailyPnL, 0)) * 100), "%\n")
  cat("Average Loss:",(mean(ifelse(dailyPnL < 0, dailyPnL, 0)) * 100), "%\n")
}

















