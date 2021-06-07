library('strucchange')

test.df = read.csv("/Users/sirikscherer/Algemeen/School/Jaar 3/Afstudeerproject/Programmeren/Data/TS.csv", header = TRUE, stringsAsFactors = FALSE, sep=';')
test.df$ID <- seq.int(nrow(test.df))
Rollingseries <- ts(test.df$Rolling.R, frequency=365, start=c(2020, 55), end=c(2021, 125))

bp.rollingR <- breakpoints(Rolling.R ~ 1 , data=test.df)
breakpoints(bp.rollingR)

# plot 1
plot(bp.rollingR)

# plot 2
plot(test.df$ID, test.df$Rolling.R, type='l', xlab = 'time (days since 25 February 2020)', 
     ylab='Infection rate', main='Breakpoints')
ci.rollingR <- confint(bp.rollingR)
abline(v=ci.rollingR$confint, col=c("blue", "blue", "blue", "red", "red", "red", "blue", 'blue', 'blue'), 
       lty=c(1,1,1, 2, 2, 2, 1, 1, 1), lwd=c(1, 1, 1, 3, 3, 3, 1, 1, 1))

# plot 3
plot(test.df$ID, test.df$Rolling.R, type='l', xlab = 'time (days since 25 February 2020)', 
     ylab='Infection rate', main='Segment model fits')
fm1 <- lm(test.df$Rolling.R ~ 1 + breakfactor(bp.rollingR, breaks = 3))
lines(fitted(fm1), col = 2)
newx = breakfactor(bp.rollingR, breaks = 3)
conf_interval <- predict(fm1, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
lines(conf_interval[,2], col="blue", lty=2)
lines(conf_interval[,3], col="blue", lty=2)

conf_interval

summary(fm1)
