library(data.table)
library(dismo)
library(gbm)
library(mlr)
library(parallelMap)

identifiers <- c("STRATUM", "SCHOOLID", "StIDStd", "BIRTHMONTH", "BIRTHYEAR")

demographics_raw <- c("ST04Q01", "ESCS", "ausSTATE", "ausGEOLOC_3", "ausINDIG")
demographics <- c("GENDER", "ESCS", "STATE", "GEOLOC", "INDIG")
disposition_indices <- c("MATHEFF", "SCMAT", "ANXMAT", "INTMAT", "INSTMOT", "FAILMAT", "SUBNORM")

stu <- fread("Data/Raw/PISA2012_StdQ_AUS.dat", select = c(identifiers, demographics_raw, disposition_indices, "PV1MATH"))

for(col in names(stu)) {
  stu <- stu[eval(parse(text = col)) != 9999.0]
}

## Rename state
states <- c("ACT", "VIC", "NSW", "QLD", "SA", "WA", "TAS", "NT")

for(i in 1:8){
  stu[ausSTATE == i, STATE := states[i]]
}

stu[, ausSTATE := NULL]

## Rename geolocation
geolocs <- c("Urban", "Provincial", "Rural")

for(i in 1:3){
  stu[ausGEOLOC_3 == i, GEOLOC := geolocs[i]]
}

stu[, ausGEOLOC_3 := NULL]

## Rename gender
mfs <- c("Female", "Male")

for(i in 1:2){
  stu[ST04Q01 == i, GENDER := mfs[i]]
}

stu[, ST04Q01 := NULL]

## Rename indig
indig <- c("Indigenous", "Non-indigenous")
stu$INDIG <- "Indigenous"
stu[ausINDIG == 0, INDIG := "Non-indigenous"]
stu[, ausINDIG := NULL]

## Make factors
for (col in c("GENDER", "STATE", "GEOLOC", "INDIG"))
  set(stu, j=col, value=as.factor(stu[[col]]))


### training set

students_in_testset1 <- stu[StIDStd %in% d[test_set, StIDStd], StIDStd]
test_set_2 <- which(stu$StIDStd %in% students_in_testset1)
train_set_2 <- seq(1,nrow(stu))[-test_set_2]

#########################################
## Set controllers for hyperparameter tuning
#########################################

task <- makeRegrTask(
  data = stu[train_set, c(demographics, disposition_indices, "PV1MATH"), with = F],
  target = "PV1MATH"
)

lrn <- makeLearner(
  "regr.gbm",
  par.vals = list(
    n.trees = 1500,
    #maximize = FALSE,
    #early.stop.round = 10,
    bag.fraction = 0.5
  )
)

ps <- makeParamSet(
  makeDiscreteParam("shrinkage", values = c(0.005, 0.01, 0.02)),
  makeDiscreteParam("interaction.depth", values = c(1, 2, 5, 10, 20))
)

ctrl <- makeTuneControlGrid()

cv5f <- makeResampleDesc("CV", iters = 5)


#########################################
## Perform tuning
#########################################

parallelStartSocket(8)

results <- tuneParams(
  lrn,
  task = task,
  resampling = cv5f,
  par.set = ps,
  control = ctrl,
  measures = mae
)

parallelStop()

#########################################
## Look at tuning results
#########################################


opt <- as.data.table(results$opt.path)

g <- ggplot(opt, aes(
  x = reorder(interaction.depth, as.numeric(as.character(interaction.depth))), 
  y = mae.test.mean, 
  group = as.numeric(as.character(shrinkage)), 
  colour = reorder(shrinkage, as.numeric(as.character(shrinkage)))
))

g + geom_line() + geom_point()

#+ coord_cartesian(ylim = c(0, 5000))

## Choose 
## eta = 0.005 (could go smaller)
## max_depth = 4 
## max nrounds = 2000
## subsample = 0.5






