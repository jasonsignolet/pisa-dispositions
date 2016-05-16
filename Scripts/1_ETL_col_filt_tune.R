
Y <- as.matrix(stu[, dispositions, with = F] - 2.5)
MISSING <- is.na(Y)


# initialise feature matrices

ns <- nrow(Y)
nq <- ncol(Y)



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
col_filt_cv <- function(Y, MISSING){
  
  val_set <- matrix(c(sample(which(MISSING == F))), ncol = 5)
  colnames(val_set) <- paste("Set", 1:5, sep = ".")
  
  Y_train <- list(Y,Y,Y,Y,Y)
  Y_test <- list(Y,Y,Y,Y,Y)
  
  for(i in 1:5){
    Y_train[[i]][val_set[,i]] <- NA
    Y_test[[i]][-val_set[,i]] <- NA
  }
  
  
  
  return(list(Y_train, Y_test))
  
}


###########################################


Y_5foldCV <- col_filt_cv(Y, MISSING)

registerDoParallel(cores = 8)

J_counter <-
  foreach(nf = 1:7, .combine = rbind) %:%
  foreach(fold = 1:5, .combine = rbind, .multicombine = T, .packages = "data.table") %dopar%{
    results <- optim(par = runif(nq*nf + ns*nf, -1, 1),
                     fn = cost,
                     gr = gradient,
                     Y = Y_5foldCV[[1]][[fold]], 
                     MISSING = is.na(Y_5foldCV[[1]][[fold]]), 
                     nq = nq, ns = ns, nf = nf, lambda = 1, #alpha = 0.001,
                     method = "L-BFGS-B",
                     control = list(trace = 1,
                                    maxit = 1000))
    test_error <- cost(results$par, Y_5foldCV[[2]][[fold]], MISSING = is.na(Y_5foldCV[[2]][[fold]]),
                       nq, ns, nf, lambda)
    
    output <- data.table(nf = nf, fold = fold, Jtrain = results$value, Jtest = test_error)
    
  }
registerDoSEQ()

J_summary <- 
J_counter[, .(
  mean(Jtrain / (sum(!MISSING) * 0.8)), 
  sd(Jtrain / (sum(!MISSING) * 0.8)) / sqrt(5),
  mean(Jtest / (sum(!MISSING) * 0.2)),
  sd(Jtest / (sum(!MISSING) * 0.2)) / sqrt(5)
  ), by = nf]


ggplot(J_summary, aes(nf)) + 
  geom_line(aes(y = V1), colour = "red") + 
  geom_line(aes(y = V3), colour = "blue") +
  geom_errorbar(aes(ymin = V1 - V2 /2, ymax = V1 + V2 /2)) +
  geom_errorbar(aes(ymin = V3 - V4 /2, ymax = V3 + V4 /2)) 
  



