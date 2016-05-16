library(data.table)
library(mlr)
library(xgboost)
library(ggplot2)

d <- fread("Data/Clean/imputed_aus_data.csv")

#########################################
## Columns
#########################################

identifiers <- c("STRATUM", "SCHOOLID", "StIDStd",
                 "BIRTHMONTH", "BIRTHYEAR" # birth month, birth year
)

demographics <- c("GENDER", "ESCS", "ESCS_Q","STATE", "GEOLOC", "INDIG")

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

maths_literacy_level <- paste0("PV", 1:5, "MATH_LEVEL")

#########################################
## Make demographics columns into factors
#########################################

for (col in c("GENDER", "STATE", "GEOLOC", "INDIG"))
  set(d, j=col, value=as.factor(d[[col]]))


#########################################
## Tuning hyperparameters
#########################################

task <- makeRegrTask(
  data = d[, c(demographics, dispositions, maths_literacy[1]), with = F],
  target = paste0("PV", 1, "MATH")
)

lrn <- makeLearner(
  "regr.xgboost",
  par.vals = list(
    nrounds = 2000,
    print.every.n = 200,
    #maximize = FALSE,
    #early.stop.round = 10,
    subsample = 0.5
  )
)

ps <- makeParamSet(
  makeDiscreteParam("eta", values = 10^(-3:-1)),
  makeDiscreteParam("max_depth", values = c(1, 10))
)

ctrl <- makeTuneControlGrid()

cv5f <- makeResampleDesc("CV", iters = 5)

results <- tuneParams(
  lrn,
  task = task,
  resampling = cv5f,
  par.set = ps,
  control = ctrl
)

opt <- as.data.table(results$opt.path)

g <- ggplot(opt, aes(
  colour = max_depth, 
  y = mse.test.mean, 
  x = as.numeric(as.character(eta)), 
  #colour = reorder(eta, as.numeric(as.character(eta)))
))

g + geom_line() + coord_cartesian(ylim = c(0, 5000))







