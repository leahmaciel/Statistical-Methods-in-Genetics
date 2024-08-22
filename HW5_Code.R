#Leah Maciel BCB 4004 HW 5
#Code adapted from Canvas example

#5a
ObsNum = matrix(c(10, 12, 25, 53), ncol=2); #observed numbers from problem

#Haplotype freqs
pA1B1 = 10/sum(ObsNum); 

#Allele freqs
pA1 = sum(ObsNum[, 1])/sum(ObsNum);  
pB1 = sum(ObsNum[1, ])/sum(ObsNum);  

#LD measures 
D = pA1B1 - pA1*pB1;
D; #[1] 0.023

#since D is positive, we use the positive bound for D_extreme
D_extreme = min(pA1*pB2, pA2*pB1);
D_extreme;

D_prime = D/D_extreme;
D_prime; #[1] 0.1608392



#5b
ExpFreq = matrix(c(pA1*pB1, pA1*pB2, pA2*pB1, pA2*pB2), ncol=2); #expected freq
ExpFreq;
ExpNum = ExpFreq*sum(ObsNum); #expected numbers
ExpNum;

Stat = sum((ObsNum-ExpNum)^2/ExpNum);
Stat;
pvalue = 1-pchisq(Stat, df=1); #hypothesis test using Chi-Squared test
pvalue; #0.2443963 > 0.05 so we don't reject H0
