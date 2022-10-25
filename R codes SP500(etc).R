#import libraries
library(quantmod)
library(moments)

#download sp500 daily open high low close (ohlc) price for last 252 days
getSymbols(Symbols = "^GSPC",from = Sys.Date() - 252, to = Sys.Date(), auto_assign = FALSE)

#create variable for adjusted close price
sp500_close <- GSPC$GSPC.Adjusted

#create variable for daily returns
sp500_return <- ROC(sp500_close)
#remove NA
sp500_return <- na.omit(sp500_return)

#plot histogram 
hist(sp500_return,main="histogram of sp500 return series",col = 'red',nclass = 20)

#calculate mean
mean_ret <- mean(sp500_return)

#calculate variance
var_ret <- var(sp500_return)

#calculate standard deviation
sd_ret <- sd(sp500_return)
# or
sd_ret < sqrt(var_ret)

#calculate skewness
skew_ret <- skewness(sp500_return)

#calculate kurtosis
kurt_ret <- kurtosis(sp500_return)

ret_as_numeric <- coredata(sp500_return)

#sort returns from least to greatest
sorted_returns <- sort(ret_as_numeric, decreasing = FALSE)
sorted_returns <- as.data.frame(sorted_returns)

# if we want to convert xts object to data.frame
ret_as_dataframe <- data.frame(date=index(sp500_return), coredata(sp500_return))

#calculate qth quantile for returns (i.e. q = 0.05
q_05 <- quantile(sp500_return,0.05)

#Alptug'un sorusu

f <- ecdf(ret_as_dataframe$GSPC.Adjusted)
plot(f)
f(-0.02)

# P(-%2 < X < %1)
diff(f(c(-0.02,0.01)))

#roll a dice example 
obs <- 0
obs_mean <- 0
for (x in c(1:10000)) {
  obs <- rbind(obs,sample(1:6, size = 1, replace = TRUE))
  obs_mean <- rbind(obs_mean,mean(obs))
}
plot(obs_mean)
last(obs_mean)

#toss a coin example 
obs <- 0
obs_mean <- 0
for (x in c(1:10000)) {
  obs <- rbind(obs,sample(0:1, size = 1, replace = TRUE))
  obs_mean <- rbind(obs_mean,mean(obs))
}
plot(obs_mean)
last(obs_mean)



# binomial distribution example for empirical VaR (Value-at-risk)

days <- c(1:nrow(sp500_return))

p_sum <- 0
for (x in days) {
  prob <- dbinom(x, nrow(sp500_return), 0.05)
  p_sum <- rbind(p_sum, prob)
}
p_sum<- cumsum(p_sum)
plot(p_sum)
# convert to data.frame 
p_sum<-data.frame(p_sum)



n=nrow(sp500_return)
p=0.05
# P(X = 4):
dbinom(4,n,p)

# P(X>=10) = 1-P(X<=9):
1-pbinom(9,n,p)

# fitting a normal distribution to a data 
hist(sp500_return$GSPC.Adjusted,nclass = 15)
mu <- mean(sp500_return$GSPC.Adjusted)
st <- sd(sp500_return$GSPC.Adjusted)
x <- seq(mu-4*st,mu+4*st,by = .1)
curve(dnorm(x,mean(sp500_return$GSPC.Adjusted),sd(sp500_return$GSPC.Adjusted)),add = TRUE, col = 'red')

#The QQ plot compares visually the observed data with what we would expect 
#to observe had the data been normally-distributed.

qqnorm(sp500_return)
qqline(sp500_return)

#VaR under normal dist. 
qnorm(0.05,mu,st)

#normalized returns
zscore <- (sp500_return-mean(sp500_return)) / sd(sp500_return)


hist(zscore)




#Download microsoft and apple historical prices
getSymbols(Symbols = "MSFT",from = Sys.Date() - 252, to = Sys.Date(), auto_assign = FALSE)
getSymbols(Symbols = "AAPL",from = Sys.Date() - 252, to = Sys.Date(), auto_assign = FALSE)



Rm <- ROC(MSFT$MSFT.Close)
Rm <- na.omit(Rm)
Ra <- ROC(AAPL$AAPL.Close)
Ra <- na.omit(Ra)

#number of row
N <- nrow(Rm)
N<- nrow(Ra)

# Empirical joint probability that Ra < -0.02 *and* Rm < -0.02
sum(Rm < -0.02 & Ra < -0.02)/N

# Empirical joint probability that Ra < -0.02 and -0.02 < Rm < 0.02
sum(Ra < -0.02 & Rm > -0.02 & Rm<0.02 )/N

# Empirical joint probability that Ra < -0.02 and  Rm > 0.02
sum(Ra < -0.02 & Ra>0.02 )/N

p <- matrix(c(sum(Ra < -0.02 & Rm < -0.02)/N,
            sum(Ra < -0.02 & Rm > -0.02 & Rm<0.02 )/N,
            sum(Ra < -0.02 & Rm>0.02 )/N,
            sum(Ra > -0.02 &Ra < 0.02 & Rm < -0.02)/N,
            sum(Ra > -0.02 &Ra < 0.02 & Rm > -0.02 & Rm<0.02)/N,
            sum(Ra > -0.02 &Ra < 0.02 & Rm>0.02)/N,  
            sum(Ra > 0.02 & Rm < -0.02)/N,
            sum(Ra > 0.02 & Rm > -0.02 & Rm<0.02)/N,
            sum(Ra > 0.02 & Rm>0.02)/N),ncol = 3,byrow = TRUE)

#Compute Marginal probabilities
px <- apply(p,1,sum)
py <- apply(p,2,sum)


p_quantity <- matrix(c(sum(Ra < -0.02 & Rm < -0.02),
              sum(Ra < -0.02 & Rm > -0.02 & Rm<0.02 ),
              sum(Ra < -0.02 & Rm>0.02 ),
              sum(Ra > -0.02 &Ra < 0.02 & Rm < -0.02),
              sum(Ra > -0.02 &Ra < 0.02 & Rm > -0.02 & Rm<0.02),
              sum(Ra > -0.02 &Ra < 0.02 & Rm>0.02),  
              sum(Ra > 0.02 & Rm < -0.02),
              sum(Ra > 0.02 & Rm > -0.02 & Rm<0.02),
              sum(Ra > 0.02 & Rm>0.02)),ncol = 3,byrow = TRUE)

#Computing conditional probabilities
p_a1_m1 <- p[1,1]/py[1]  ## computes conditional probability P(Apple<-2%|Microsoft<-2%)
p_a1_m2 <- p[1,2]/py[2]  ## computes conditional probability P(Apple<-2%|-2%<Microsoft<2%)

p_m2_a3 <- p[3,2]/px[3]  ## computes conditional probability P(Microsoft<-2%|-2%<Apple<2%)


#Monte Carlo Simulation example for CLT

n_long = 1000
M = 1000; # number of samples
mu = 0
sigma = 1
# generate the matrix:
Y_long = replicate(M,rnorm(n_long,mu,sigma))
# Compute the sample means of all samples together
xbar_long = colMeans(Y_long)
hist(xbar_long)

#CLT example for uniform distribution
random_num <- replicate(M,sample.int(100, 10, replace = TRUE))
rand_mean = colMeans(random_num)
hist(rand_mean)
rand_mean_normalized <- (rand_mean-mean(rand_mean))/sd(rand_mean)
xbar <- rand_mean_normalized

n <- length(xbar)
#Confidence interval 
q1 = qnorm(0.025,mean(rand_mean),sd(rand_mean))
q2 = qnorm(0.975,mean(rand_mean),sd(rand_mean))

library('tseries')
#test for nomrality if p>0.05 We would not be able to 
#reject the null hypothesis that the data is 
#normally distributed 
#In other words, we can assume the normality.

#jarque bera test is mostly performed to test for normality
#on residuals of regression analysis
jarque.bera.test(Ra)

#Shapiro-Wilk's test for normality (perform better for small
#sample sizes)
shapiro.test(coredata(Ra))

#Kolmogorov-Smirnov test for normality (can be used for large
#sample sizes)
ks.test(coredata(Ra), 'pnorm')

#augmented Dickey-Fuller (unit root test to check stationarity
#in time series.
#H0:Null hypotesis: unit root is present or data is nonstationary 
#H1: Alternative hypotesis: we can reject H0. In other words data 
#is stationary. If p-value < 0.05: data is stationary) 
adf.test(coredata(Ra))
