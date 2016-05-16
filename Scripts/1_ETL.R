#########################################
## ETL Australian data for PISA 2012
#########################################

library(data.table)
library(plyr)


#########################################
## Columns to import
#########################################

identifiers <- c("STRATUM", "SCHOOLID", "StIDStd",
                 "ST03Q01", "ST03Q02" # birth month, birth year
                 )

demographics <- c("ST04Q01", "ESCS", "ausSTATE", "ausGEOLOC_3", "ausINDIG")

int_motivation <- c("ST29Q01", "ST29Q03", "ST29Q04", "ST29Q06") # intrinsic motivation INTMAT

ext_motivation <- c("ST29Q02", "ST29Q05", "ST29Q07", "ST29Q08") # extrinsic motivation INSTMOT

self_concept <- c("ST42Q02", "ST42Q04", "ST42Q06", "ST42Q07", "ST42Q09") # SCMAT

self_efficacy <- c("ST37Q01", "ST37Q02", "ST37Q03", "ST37Q04", "ST37Q05", "ST37Q06", "ST37Q07", "ST37Q08") # MATHEFF

control_in_school <- c("ST91Q01", "ST91Q02", "ST91Q03", "ST91Q04", "ST91Q05", "ST91Q06")

control_in_maths <- c("ST43Q01", "ST43Q02", "ST43Q03", "ST43Q04", "ST43Q05", "ST43Q06")

attr_failure <- c("ST44Q01", "ST44Q03", "ST44Q04", "ST44Q05", "ST44Q07", "ST44Q08") # FAILMAT

maths_anxiety <- c("ST42Q01", "ST42Q03", "ST42Q05", "ST42Q08", "ST42Q10") # ANXMAT

subj_norms <- c("ST35Q01", "ST35Q02", "ST35Q03", "ST35Q04", "ST35Q05", "ST35Q06") # SUBNORM

dispositions <- c(int_motivation, ext_motivation, self_concept,
                  self_efficacy, control_in_school, control_in_maths, attr_failure,
                  maths_anxiety, subj_norms)

maths_literacy <- paste0("PV", 1:5, "MATH")


#########################################
## Load data
#########################################

d <- fread("Data/Raw/PISA2012_StdQ_AUS.dat", 
           select = c(identifiers, demographics, dispositions, maths_literacy)
)


#########################################
## Keep only rows where students saw all parts of the test
#########################################

all_seated_test <-
  all_seated(d[, dispositions, with = F])

d <- d[all_seated_test] # reduces rows from 14481 to 4752

#########################################
## Set missing values to NA
#########################################

stuq_dict <- fread("Data/Raw/StdQ_dictionary.dat")

stu <- missingPISA(d, stuq_dict)



#########################################
## Include cols for plausible value levels
#########################################

pv_cutoffs <- c(0, 358, 420, 482, 545, 607, 669, 1000) # from 2014024_tables.pdf

stu[, paste0("PV", 1:5, "MATH_LEVEL") :=
      llply(list(PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH), 
            cut, breaks = pv_cutoffs, labels = F, include.lowest = T)]


#########################################
## Rename unclear cols (not disposition cols)
#########################################


setnames(stu, c("ST03Q01", "ST03Q02"), c("BIRTHMONTH", "BIRTHYEAR"))



#########################################
## Rename state
#########################################


states <- c("ACT", "VIC", "NSW", "QLD", "SA", "WA", "TAS", "NT")

for(i in 1:8){
  stu[ausSTATE == i, STATE := states[i]]
}




#########################################
## Rename geolocation
#########################################

geolocs <- c("Urban", "Provincial", "Rural")

for(i in 1:3){
  stu[ausGEOLOC_3 == i, GEOLOC := geolocs[i]]
}




#########################################
## Rename gender
#########################################

mfs <- c("Female", "Male")

for(i in 1:2){
  stu[ST04Q01 == i, GENDER := mfs[i]]
}


#########################################
## Rename indig
#########################################

indig <- c("Indigenous", "Non-indigenous")
stu$INDIG <- "Indigenous"
stu[ausINDIG == 0, INDIG := "Non-indigenous"]


#########################################
## Rename ESCS (4 is the top quartile)
#########################################

stu[is.na(ESCS) | ESCS == 9999.0, 
    ESCS := mean(stu[ESCS != 9999.0, ESCS], na.rm = T)] # assume missing values cluster around mean

escs_quantile <- quantile(stu$ESCS)
stu[, ESCS_Q := 0]

for(i in 1:4) {
  stu[ESCS >= escs_quantile[i], ESCS_Q := i]
}


#########################################
## Clear unused cols
#########################################

stu[, `:=`(ausSTATE = NULL,
           ausGEOLOC_3 = NULL,
           ST04Q01 = NULL,
           ausINDIG = NULL)]


#########################################
## Impute missing disposition data using collaborative filtering
#########################################

## tune number of latent features
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
    mean(Jtrain / (sum(!MISSING) * 0.8)), # Jtrain is the sum total cost; we want average cost (mse)
    sd(Jtrain / (sum(!MISSING) * 0.8)) / sqrt(5),
    mean(Jtest / (sum(!MISSING) * 0.2)),
    sd(Jtest / (sum(!MISSING) * 0.2)) / sqrt(5)
  ), by = nf]


p1 <-  ggplot(J_summary, aes(nf)) + 
  geom_line(aes(y = V1), colour = "red") + 
  geom_line(aes(y = V3), colour = "blue") +
  geom_errorbar(aes(ymin = V1 - V2 /2, ymax = V1 + V2 /2)) +
  geom_errorbar(aes(ymin = V3 - V4 /2, ymax = V3 + V4 /2)) 


#### Estimate the missing values

nf <- 6

results <- optim(par = runif(nq*nf + ns*nf, -1, 1),
                 fn = cost,
                 gr = gradient,
                 Y = Y, 
                 MISSING = MISSING, 
                 nq = nq, 
                 ns = ns, 
                 nf = nf, 
                 lambda = 1, 
                 method = "L-BFGS-B",
                 control = list(trace = 1,
                                maxit = 1000)
)


output <- vec2matrix(results$par, Y, MISSING, nq, ns, nf)

X <- output[[1]]
THETA <- output[[2]]

GUESS <- round(THETA %*% t(X))

Y[MISSING] <- GUESS[MISSING]


Y = Y + 2.5


#########################################
## Write table to output
#########################################


imputed_table <- cbind(stu[, !names(stu) %in% dispositions, with = F], Y)


write.table(imputed_table, "Data/Clean/imputed_aus_data.csv", col.names = T, row.names = F, sep = ",")

pdf(file = "Outputs/Plots/tune_nf_to_impute_missing_values.pdf", width = 8, height = 8)
print(p1)
dev.off()

