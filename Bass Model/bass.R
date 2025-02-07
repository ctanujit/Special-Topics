
### Toy Example ###

# For example, set market size, m=100,000; 
# coefficient of innovation, p=0.01; and 
# coefficient of imitation, q=0.20. Adoption curve is...

f = function(p,q,t) {
  res = (exp((p+q)*t)*p*(p+q)^2)/(p*exp((p+q)*t)+q)^2 # Eqn. 5
}
t = seq(1,20)
m = 100000
p = 0.01
q = 0.20
plot(t,m*f(p,q,t),type="l",col="blue",lwd=3,xlab="Time (years)"
     ,ylab="Adoptions")
grid(lwd=2)

### Symbolic Math in R ###

# Bass Model (Define the CDF (Eqn.4) and PDF (Eqn.5))
FF = expression(p*(exp((p+q)*t)-1)/(p*exp((p+q)*t)+q))
print(FF)

#Take derivative
ff = D(FF,"t")
print(ff)

# Set Up the function
ff = function(p,q,t) {
  res = D(FF,"t")
}
m=100000; p=0.01; q=0.20; t=seq(1,20)
plot(t,m*eval(ff(p,q,t)),type="l",col="red",lwd=3)
grid(lwd=2)

### iPhone Sales Forecasting ###

# As an example, let's look at the trend for iPhone sales 
# We store the quarterly sales in a file 
# We then undertake the Bass model analysis. 
# We get the data from: http://www.statista.com/statistics/263401/global-apple-iphone-sales-since-3rd-quarter-2007/
# The R code for this computation is as follows:

data <- apple_iphone_unit_sales
View(data)
isales = data[,2]
# Compute betas (bs) using Eqn.7 (linear regression model)
cum_isales = cumsum(isales)
cum_isales2 = cum_isales^2
res = lm(unlist(isales) ~ unlist(cum_isales)+
           unlist(cum_isales2))
print(summary(res))
b = res$coefficients
print(b)

# Fit the Model (Refer to Eqn.8 for computing m, if not given)

m1 = (-b[2]+sqrt(b[2]^2-4*b[1]*b[3]))/(2*b[3])
m2 = (-b[2]-sqrt(b[2]^2-4*b[1]*b[3]))/(2*b[3])
print(c(m1,m2))
m = max(m1,m2)
print(m)

# Finding p and q

p = b[1]/m
q = -m*b[3]
print(c("p,q=",p,q))

# Plot the Fitted Model 
nqtrs = 100
t=seq(0,nqtrs)

# Define the CDF (Eqn.4) and PDF (Eqn.5)
FF = expression(p*(exp((p+q)*t)-1)/(p*exp((p+q)*t)+q))
ff = D(FF,"t")
fn_f = eval(ff)*m

plot(t,fn_f,type="l",ylab="Qtrly Units (MM)",main="Apple Inc Sales")
n = length(unlist(isales))
lines(1:n,ts(isales),col="red",lwd=2,lty=2)

# Data Frame of Bass Model Curve
df = as.data.frame(cbind(t,fn_f))
df

# Sales Peak (in Quarters) for Apple
tstar = 1/(p+q)*log(q/p)
print(tstar)

### Trading-off p and q:

t = seq(0,5,0.1)
p = 0.1; q=0.22
plot(t,f(p,q,t),type="l",col="blue",lwd=2)
p = 0.1; q=0.20
lines(t,f(p,q,t),type="l",col="red",lwd=2)

