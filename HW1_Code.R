# Leah Maciel
#adapted from sample code from canvas

#Problem 4
n=1000; p=0.25;

#Part A
#E(X)
mean= n*p;  #250
mean   #250

#Var(X)
var= n*p*(1-p);  
var #187.5

#P(X=E(X))
dbinom(250, n, p) #=0.02912411

#Probability mass function:
fx = dbinom(0:1000, size=n, prob=p);
plot(0:1000, fx, type="p");

#Part B
### Probability distribution: The true distribution model
p_range_real = sum(dbinom(250:350, size=n, prob=p));
p_range_real  #= 0.5121376


### Empirical distribution: The data distribution, often used to simulate the true distribution model
sampleSize = 10000;
sim_values = rbinom(n=sampleSize, size=n, prob=p);

empirical_p_range = sum(sim_values >= 250 & sim_values <= 350) / sampleSize
empirical_p_range #=0.5109

#Part C
#Normal approximation based on true mean and standard deviation
sd = sqrt(var);

# Calculate probabilities for 250 and 350 using the normal approximation
prob_250 = pnorm(250, mean, sd)
prob_350 = pnorm(350, mean, sd)

normal_p_range = prob_350 - prob_250
normal_p_range  # = 0.5

