data_generate <- function(n, p, p_signal, coef_1, data_cov, prevalence) {
  
  library(tidyverse); library(MASS); library(splitstackshape)
  
  if (data_cov == "indep"){
    Sigma <- diag(p)
  } else if (data_cov == "toep") {
    Sigma <- toeplitz (0.9^(1:p))
  } else if (data_cov == "exchange_09") {
    Sigma <- matrix(0.9, ncol = p, nrow = p)
    diag(Sigma) <- 1
  } else if (data_cov == "exchange_07") {
    Sigma <- matrix(0.7, ncol = p, nrow = p)
    diag(Sigma) <- 1
  } else {
    Sigma <- matrix(0.5, ncol = p, nrow = p)
    diag(Sigma) <- 1
  }
  
  set.seed(5225)
  p_signal_index <- sample(1:p,p_signal,replace = F)
  if (coef_1 == "1"){
    p_signal_beta <- runif(p_signal,0.5,1.5) %>% round(1)
  } else if (coef_1 == "2") {
    p_signal_beta <- runif(p_signal,-3,3) %>% round(1)
  } 
  
  set.seed(NULL)
  data <- mvrnorm(n = n*2, rep(0,p), Sigma, empirical = F)
  linear <- data[,p_signal_index] %*% p_signal_beta
  logistic <- exp(linear)/(1+exp(linear))
  
  y <- rbinom(n = n*2, size = 1, prob = logistic)  
  data_1 <- as.data.frame(cbind(1:n,data,y))
  names(data_1) <- c("index",1:p,"y")
  
  size_sample <- (sample(c(0,1),n,prob=c(1-prevalence,prevalence),replace = T) %>% table())
  out <- stratified(data_1, c("y"),c("0"=as.vector(size_sample[1]),"1"=as.vector(size_sample[2])))
  data_1 <- as.data.frame(out)
  
  return(list(data = data_1, p_signal_index = p_signal_index))
  
}
