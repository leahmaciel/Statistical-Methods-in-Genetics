#Leah Maciel- code adapted from Canvas example

#Question 2
#2a lower and upper CI bounds calculations
phat = 41/100;
n=100;
ConfidenceLevel = 0.99;
zscore = qnorm((1+ConfidenceLevel)/2);
SE = sqrt(phat*(1-phat)/n); #standard error

phat - zscore*SE; #Lower Bound of the C.I.
#[1] 0.2833121

phat + zscore*SE; #Upper Bound of the C.I
#[1] 0.5366879

#2b
phat = 41/100; #sample proportion
p0 = 0.5; #The null value of proportion.
n=100;
##Z-Test (based on normal approximation)
Z = (phat - p0)/sqrt(p0*(1-p0)/n);
Z; 
pvalue = 2*(1-pnorm(abs(Z))); #Two sided test H_a: p != p0.
pvalue;
#[1] 0.07186064


#Question 3
N <- 50 
K <- 10  
n <- 5  
X <- 3  

# Calculate p-value using phyper (distribution function for hypergeometric)

p_value <- phyper(X - 1, m = K, n = N - K, k = n, lower.tail = FALSE) #lower.tail false gives P(X > x)
p_value  # 0.0482603



#Question 4
#4a
Z <- 1
p_value <- 2 * (1 - pnorm(abs(Z)))
p_value #[1] 0.3173105

#4b
p_value_chisq <- pchisq(1, 1, lower.tail = FALSE) #LRT = 1, degrees of freedom = 1
p_value_chisq #[1] 0.3173105

#4C i
alpha = 0.05;
mu0 = 0.1;
mu1 = 0; 
sigma = 1


#Function for calculating the power of two-sided Z test for proportion
power.Z = function(alpha, mu0, mu1, sigma, n){
  zscore = qnorm(alpha/2, lower.tail=F); 
  a = 1;
  b = (mu1-mu0)/(sigma/sqrt(n));
  power = 1 - pnorm(zscore/a - b) + pnorm(-zscore/a - b); 
  return(power); 
}

#calculate power at given sample size n; 
power.Z(alpha=alpha, mu0=mu0, mu1=mu1,sigma=sigma, n=30)
#[1] 0.08501566

#4c ii
#find power values for various n values. Search for the smallest n s.t. power is at least 0.99
N = 30:10000;
powers = unlist(lapply(N, power.Z, alpha=alpha, mu0=mu0, mu1=mu1,sigma=sigma));
indices = which(powers >= 0.99); #indices of n values that make power >= 0.99 
N[indices[1]]; #the smallest such n value. 
#[1] 1838