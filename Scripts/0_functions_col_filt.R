#########################################
## COLLABORATIVE FILTERING
#########################################


# optim() takes in the initial values as a vector, but we need to work on them as matrices
vec2matrix <- function(initial_values, Y, MISSING, nq, ns, nf){
  X_length <- nq * nf
  
  X <- matrix(initial_values[1:X_length], nrow = nq)
  THETA <- matrix(initial_values[(X_length + 1):length(initial_values)], nrow = ns)
  DELTA <- THETA %*% t(X) - Y; DELTA[MISSING] <- 0; DELTA <- as.matrix(DELTA)
  
  return(list(X, THETA, DELTA))
}



# cost function
cost <- function(initial_values, Y, MISSING, nq, ns, nf, lambda){
  matrices <- vec2matrix(initial_values, Y, MISSING, nq, ns, nf)
  
  X <- matrices[[1]]
  THETA <- matrices[[2]]
  DELTA <- matrices[[3]]
  
  
  J <- 0.5 * (sum(DELTA^2, na.rm = T) + lambda * (sum(X^2) + sum(THETA^2)))
  
}


# gradient of the cost function
gradient <- function(initial_values, Y, MISSING, nq, ns, nf, lambda, alpha){
  matrices <- vec2matrix(initial_values, Y, MISSING, nq, ns, nf)
  
  X <- matrices[[1]]
  THETA <- matrices[[2]]
  DELTA <- matrices[[3]]
  
  X_grad <- (t(DELTA) %*% THETA) + lambda * X
  THETA_grad <- (DELTA %*% X) + lambda * THETA
  
  return(c(X_grad, THETA_grad))
  
}



## for 5-fold CV,  
col_filt_cv5f_splitter <- function(DF){
  Y <- as.matrix(DF - 2.5)
  MISSING <- is.na(Y)
  
  val_set <- sample(which(MISSING == F))
  
  train <- list(Y,Y,Y,Y,Y)
  test <- list(Y,Y,Y,Y,Y)
  
  for(i in 1:5){
    this_val_set <- val_set[1:length(val_set) %% 5 == i]
    
    train[[i]][-this_val_set] <- NA
    test[[i]][this_val_set] <- NA
  }
  
  
  
  return(list(train, test))
  
}



## tuning
col_filt_cv5f <- function(DF, max_latent_features = 7, n_cores = 8) {
  require(foreach)
  require(doParallel)
  require(data.table)
  
  nq <- ncol(DF)
  ns <- nrow(DF)
  
  Y <- col_filt_cv5f_splitter(DF)
  
  registerDoParallel(cores = n_cores)
  
  J_counter <-
    foreach(nf = 1:max_latent_features, .combine = rbind, .multicombine = T, .packages = "data.table") %:%
    foreach(fold = 1:5, .combine = rbind) %dopar% {
      
      results <- optim(par = runif(nq*nf + ns*nf, -1, 1),
                       fn = cost,
                       gr = gradient,
                       Y = Y[["train"]][[fold]], 
                       MISSING = is.na(Y[["train"]][[fold]]), 
                       nq = nq, 
                       ns = ns, 
                       nf = nf, 
                       lambda = 1, 
                       alpha = 0.001,
                       method = "L-BFGS-B",
                       control = list(trace = 1,
                                      maxit = 1000))
      
      test_error <- cost(results$par, 
                         Y[["test"]][[fold]], 
                         MISSING = is.na(Y[["test"]][[fold]]),
                         nq = nq, 
                         ns = ns, 
                         nf = nf, 
                         lambda = 1)
      
      output <- data.table(latent_features = nf, fold = fold, training_cost = results$value, test_cost = test_error)
      
    }
  
  registerDoSEQ()
  
  
  return(J_counter)
}

##training
col_filt_estimate <- function(DF, nf, key) {
  Y <- as.matrix(DF - 2.5)
  MISSING <- is.na(Y)
  
  nq <- ncol(DF)
  ns <- nrow(DF)
  
  Y <- col_filt_cv5f_splitter(DF)
  
  results <- optim(par = runif(nq*nf + ns*nf, -1, 1),
                   fn = cost,
                   gr = gradient,
                   Y = Y, 
                   MISSING = MISSING, 
                   nq = nq, 
                   ns = ns, 
                   nf = 4, 
                   lambda = 1, 
                   alpha = 0.001,
                   method = "L-BFGS-B",
                   control = list(trace = 1,
                                  maxit = 1000)
  )
  
  
  output <- vec2matrix(results$par, Y, MISSING, nq, ns, nf = 4)
  
  X <- output[[1]]
  THETA <- output[[2]]
  
  GUESS <- round(THETA %*% t(X) + 2.5)
  GUESS <- as.data.table(GUESS)
  setnames(GUESS, colnames(DF))
  GUESS[, key := key]
  
  return(GUESS)
}

