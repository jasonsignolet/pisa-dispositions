library(data.table)
library(mlr)
library(gbm)
library(ggplot2)
library(parallelMap)
library(dismo)
library(magrittr)

d <- fread("Data/Clean/imputed_aus_data.csv")

#########################################
## Columns
#########################################

identifiers <- c("STRATUM", "SCHOOLID", "StIDStd", "BIRTHMONTH", "BIRTHYEAR")
demographics <- c("GENDER", "ESCS", "STATE", "GEOLOC", "INDIG")

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
## Train gbm tree model
#########################################

current_name <- "_gbm"

set.seed(20160522)
test_set <- sort(sample(nrow(d), 400))
train_set <- seq(1,nrow(d))[-test_set]

task <- makeRegrTask(
  id = "pisa",
  data = as.data.frame(d[, c(demographics, dispositions, maths_literacy[1]), with = F]),
  target = paste0("PV", 1, "MATH")
)

lrn_gbm <- makeLearner(
  "regr.gbm",
  par.vals = list(
    n.trees = 3000,
    bag.fraction = 0.5,
    shrinkage = 0.01,
    interaction.depth = 4
  )
)

fit_gbm <- train(lrn_gbm, task = task, subset = train_set)

pred_gbm <- as.data.table(predict(fit_gbm, task = task, subset = test_set))

### Tree outputs

trplot_gbm <- ggplot(pred_gbm, aes(truth, response)) + 
  geom_point() + 
  geom_abline(colour = "red", linetype = "dashed")

error_table[, GBM := pred_gbm[, quantile(abs(truth-response), c(.5, .9, .99))] ]


imp_gbm <- summary(fit_gbm$learner.model)
write.table(imp_gbm, paste0("Outputs/Tables/feature_importance", current_name, ".csv"), row.names = F, col.names = T, sep = ",")


impplot_gbm <- 
  ggplot(imp_gbm, aes(reorder(var, rel.inf), rel.inf)) +
  geom_bar(stat = "identity") +
  coord_flip()
  
#xgb.plot.multi.trees(fit_gbm$learner.model, feature_names = fit_gbm$features)
pd_gbm <- generatePartialPredictionData(fit_gbm, task, imp_gbm$var)
write.table(pd_gbm$data, paste0("Outputs/Tables/partial_dependency", current_name, ".csv"), row.names = F, col.names = T, sep = ",")

pdplot_gbm <- plotPartialPrediction(pd_gbm)

pdf(file = paste0("Outputs/Plots/trplot", current_name, ".pdf"),
    width = 8, height = 8)
print(trplot_gbm)
dev.off()

pdf(paste0("Outputs/Plots/impplot", current_name, ".pdf"),
    width = 8, height = 8)
impplot_gbm
dev.off()

pdf(paste0("Outputs/Plots/pdplot", current_name, ".pdf"),
    width = 12, height = 8)
pdplot_gbm
dev.off()

######################################

for(col in c("INDIG", "GENDER")) {
  if(is.factor(d[[col]])) {
    set(d, j = col, value = as.numeric(d[[col]]))
  }
}


model_gbm <-
  gbm.step(gbm.y = "PV1MATH", 
      gbm.x = c(demographics, dispositions),
      data = as.data.frame(d),
      family = "gaussian",
      n.folds = 5,
      n.trees = 2000,
      max.trees = 3000,
      learning.rate = 0.01,
      tree.complexity = 4,
      bag.fraction = 0.5,
      verbose = T
  )

gbm.plot(model_gbm, n.plots = 12)
int_gbm <- gbm.interactions(model_gbm)
# write.table(int_gbm$rank.list, "Outputs/Tables/interaction_ranks.csv", row.names = F, col.names = T, sep = ",")
# write.table(int_gbm$interactions, "Outputs/Tables/interaction_full_table.csv", row.names = F, col.names = T, sep = ",")
int_gbm$rank.list %>% head
gbm.perspec(model_gbm, 23, 2, z.range = c(425, 580), y.range = c(-3.81, 2.44), x.range = c(1,4))
gbm.perspec(model_gbm, 3, 5, z.range = c(350, 560), y.range = c(1,2), x.range = c(1,2))
gbm.perspec(model_gbm, 4, 5, z.range = c(425, 560), y.range = c(1,2), x.range = c(1,3))
gbm.perspec(model_gbm, 23, 20, z.range = c(425, 560), y.range = c(1,4), x.range = c(1,4))
gbm.perspec(model_gbm, 7, 3, z.range = c(425, 560), y.range = c(1,8), x.range = c(1,4))
gbm.perspec(model_gbm, 2, 1, z.range = c(425, 580), x.range = c(-3.81, 2.44), y.range = c(0,1))
gbm.perspec(model_gbm, 2, 3, z.range = c(425, 580), x.range = c(-3.81, 2.44), y.range = c(1,2))
