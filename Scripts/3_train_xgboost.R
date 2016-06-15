
library(data.table)
library(mlr)
library(xgboost)
library(ggplot2)
library(foreach)
library(doParallel)

d <- fread("Data/Clean/imputed_aus_data.csv")

error_table <- data.table(percentile = c("pc50", "pc90", "pc99"),
                          Trees = c(0,0,0),
                          Stumps = c(0,0,0),
                          RF = c(0,0,0))

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
## Restrict imputed item responses to within 1:4
#########################################

for(col in dispositions) {
  set(d, i = which(d[[col]] < 1), j = col, value = 1)
  set(d, i = which(d[[col]] > 4), j = col, value = 4)
}


#########################################
## Train xgboost tree model
#########################################

current_name <- "_tree"

set.seed(20160522)
test_set <- sort(sample(nrow(d), 400))
train_set <- seq(1,nrow(d))[-test_set]

task <- makeRegrTask(
  id = "pisa",
  data = as.data.frame(d[, c(demographics, dispositions, maths_literacy[1]), with = F]),
  target = paste0("PV", 1, "MATH")
)

lrn_tree <- makeLearner(
  "regr.xgboost",
  par.vals = list(
    nrounds = 8000,
    print.every.n = 800,
    maximize = FALSE,
    early.stop.round = 10,
    subsample = 0.5,
    eta = 0.005,
    max_depth = 4
  )
)

fit_tree <- train(lrn_tree, task = task, subset = train_set)

pred_tree <- as.data.table(predict(fit_tree, task = task, subset = test_set))

### Tree outputs

trplot_tree <- ggplot(pred_tree, aes(truth, response)) + 
  geom_point() + 
  geom_abline(colour = "red", linetype = "dashed")

error_table[, Trees := pred_tree[, quantile(abs(truth-response), c(.5, .9, .99))] ]


imp_tree <- xgb.importance(feature_names = fit_tree$features, model = fit_tree$learner.model)
write.table(imp_tree, paste0("Outputs/Tables/feature_importance", current_name, ".csv"), row.names = F, col.names = T, sep = ",")


impplot_tree <- xgb.plot.importance(imp_tree)
#xgb.plot.multi.trees(fit_tree$learner.model, feature_names = fit_tree$features)
pd_tree <- generatePartialPredictionData(fit_tree, task, imp_tree$Feature)
write.table(pd_tree, paste0("Outputs/Tables/partial_dependency", current_name, ".csv"), row.names = F, col.names = T, sep = ",")

pdplot_tree <- plotPartialPrediction(pd_tree)

pdf(file = paste0("Outputs/Plots/trplot", current_name, ".pdf"),
    width = 8, height = 8)
print(trplot_tree)
dev.off()

pdf(paste0("Outputs/Plots/impplot", current_name, ".pdf"),
    width = 8, height = 8)
impplot_tree
dev.off()

pdf(paste0("Outputs/Plots/pdplot", current_name, ".pdf"),
    width = 12, height = 8)
pdplot_tree
dev.off()


#########################################
## Train xgboost stump model
#########################################

current_name <- "_stump"

set.seed(20160522)
test_set <- sort(sample(nrow(d), 400))
train_set <- seq(1,nrow(d))[-test_set]

task <- makeRegrTask(
  id = "pisa",
  data = as.data.frame(d[, c(demographics, dispositions, maths_literacy[1]), with = F]),
  target = paste0("PV", 1, "MATH")
)

lrn_stump <- makeLearner(
  "regr.xgboost",
  par.vals = list(
    nrounds = 8000,
    print.every.n = 800,
    maximize = FALSE,
    early.stop.round = 10,
    subsample = 0.5,
    eta = 0.02,
    max_depth = 1
  )
)

fit_stump <- train(lrn_stump, task = task, subset = train_set)

pred_stump <- as.data.table(predict(fit_stump, task = task, subset = test_set))

### Stump outputs

trplot_stump <- ggplot(pred_stump, aes(truth, response)) + 
  geom_point() + 
  geom_abline(colour = "red", linetype = "dashed")

error_table[, Stumps := pred_stump[, quantile(abs(truth-response), c(.5, .9, .99))] ]


imp_stump <- xgb.importance(feature_names = fit_stump$features, model = fit_stump$learner.model)
write.table(imp_stump, paste0("Outputs/Tables/feature_importance", current_name, ".csv"), row.names = F, col.names = T, sep = ",")


impplot_stump <- xgb.plot.importance(imp_stump)
#xgb.plot.multi.trees(fit_stump$learner.model, feature_names = fit_stump$features)
pd_stump <- generatePartialPredictionData(fit_stump, task, imp_stump$Feature)
write.table(pd_stump, paste0("Outputs/Tables/partial_dependency", current_name, ".csv"), row.names = F, col.names = T, sep = ",")

pdplot_stump <- plotPartialPrediction(pd_stump)

pdf(file = paste0("Outputs/Plots/trplot", current_name, ".pdf"),
    width = 8, height = 8)
print(trplot_stump)
dev.off()

pdf(paste0("Outputs/Plots/impplot", current_name, ".pdf"),
    width = 8, height = 8)
impplot_stump
dev.off()

pdf(paste0("Outputs/Plots/pdplot", current_name, ".pdf"),
    width = 12, height = 8)
pdplot_stump
dev.off()


#########################################
## Train xgboost RF model
#########################################

current_name <- "_rf"

set.seed(20160522)
test_set <- sort(sample(nrow(d), 400))
train_set <- seq(1,nrow(d))[-test_set]

task <- makeRegrTask(
  id = "pisa",
  data = as.data.frame(d[, c(demographics, dispositions, maths_literacy[1]), with = F]),
  target = paste0("PV", 1, "MATH")
)

lrn_rf <- makeLearner(
  "regr.xgboost",
  par.vals = list(
    nrounds = 1,
    print.every.n = 800,
    maximize = FALSE,
    subsample = 0.5,
    max_depth = 20,
    num_parallel_tree = 2000,
    colsample_bytree = 0.15
  )
)

fit_rf <- train(lrn_rf, task = task, subset = train_set)

pred_rf <- as.data.table(predict(fit_rf, task = task, subset = test_set))

### RF outputs

trplot_rf <- ggplot(pred_rf, aes(truth, response)) + 
  geom_point() + 
  geom_abline(colour = "red", linetype = "dashed")

error_table[, RF := pred_rf[, quantile(abs(truth-response), c(.5, .9, .99))] ]


imp_rf <- xgb.importance(feature_names = fit_rf$features, model = fit_rf$learner.model)
write.table(imp_rf, paste0("Outputs/Tables/feature_importance", current_name, ".csv"), row.names = F, col.names = T, sep = ",")


impplot_rf <- xgb.plot.importance(imp_rf)
#xgb.plot.multi.trees(fit_rf$learner.model, feature_names = fit_rf$features)
pd_rf <- generatePartialPredictionData(fit_rf, task, imp_rf$Feature)
write.table(pd_rf, paste0("Outputs/Tables/partial_dependency", current_name, ".csv"), row.names = F, col.names = T, sep = ",")

pdplot_rf <- plotPartialPrediction(pd_rf)

pdf(file = paste0("Outputs/Plots/trplot", current_name, ".pdf"),
    width = 8, height = 8)
print(trplot_rf)
dev.off()

pdf(paste0("Outputs/Plots/impplot", current_name, ".pdf"),
    width = 8, height = 8)
impplot_rf
dev.off()

pdf(paste0("Outputs/Plots/pdplot", current_name, ".pdf"),
    width = 12, height = 8)
pdplot_rf
dev.off()

####


write.table(error_table, "Outputs/Tables/error_table.csv", row.names = F, col.names = T, sep = ",")
