## specify parameters, initial conditions, and output matrix
num.years <- 50 
num.patch <- 10
freq.1.mat <- matrix(nrow = num.years, ncol = num.patch)

J <- 100 # number of individuals PER PATCH
init.1 <- 0.5*J 
COM <- matrix(nrow=J, ncol=num.patch)
COM[1:init.1,] <- 1; COM[(init.1+1):J,] <- 2 

year <- 2 

m <- 0
fit.ratio.avg <- vector(length=num.patch)
fit.ratio.avg[] <- 1
freq.dep <- vector(length=num.patch)
freq.dep[] <- 0

## record data (frequency of species 1) for year 1
freq.1.mat[1,] <- init.1/J 

## run simulation
for (i in 1:(J*num.patch*(num.years-1))) {
  
  ## choose a patch where a death even will occur
  patch <- sample(1:num.patch,1)
  
  ## calculate Pr.1 if dispersal occurs
  if (runif(1) < m) {
    Pr.1 <- sum(COM==1)/(J*num.patch)
  } else { 

  ## calculate Pr.1 if local reproduction (not dispersal)
    freq.1 <- sum(COM[,patch]==1)/J; freq.2 <- 1 - freq.1
    fit.ratio <- exp(freq.dep[patch]*(freq.1-0.5) + log(fit.ratio.avg[patch]))
    Pr.1 <-  fit.ratio*freq.1/(fit.ratio*freq.1 + freq.2)
  }
  
  COM[ceiling(J*runif(1)),patch] <- sample(c(1,2), 1, prob=c(Pr.1,1-Pr.1)) 

  ## record data  
  if (i %% (J*num.patch) == 0) {
    freq.1.mat[year,] <- colSums(COM==1)/J
    year <- year + 1 
  }
} 

## graph the results
plot(1:num.years, freq.1.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 1", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.1.mat[,i], type="l", lty=2, ylim=c(0,1))
}