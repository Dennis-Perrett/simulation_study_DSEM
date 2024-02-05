#intercept variance
alpha_2 = seq(0.50,0.75,0.005)
beta_2 = 0.1*+alpha_2 +0.1


int <- function(x)
qinvgamma(x,shape=alpha_2[34],rate=beta_2[34])
print(hdi(int))
print(alpha_2[34])
print(beta_2[34])


#TVC variance
alpha = seq(0.125,0.25,0.005)
beta = 0.1*+alpha +0.1


int <- function(x)
qinvgamma(x,shape=alpha[21],rate=beta[21])
print(hdi(int))
print(alpha[21])
print(beta[21])

#log residual variance
