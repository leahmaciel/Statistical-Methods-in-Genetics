#Leah Maciel
N <- 50 
K <- 10  
n <- 5  
X <- 3  

# Calculate p-value using phyper (distribution function for hypergeometric)

p_value <- phyper(X - 1, m = K, n = N - K, k = n, lower.tail = FALSE) #lower.tail false gives P(X > x)
p_value  # 0.0482603


