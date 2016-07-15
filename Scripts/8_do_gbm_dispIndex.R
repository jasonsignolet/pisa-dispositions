model_gbm_dispIndex <-
  gbm.step(gbm.y = "PV1MATH", 
           gbm.x = c(demographics, disposition_indices),
           data = as.data.frame(stu[train_set_2, ]),
           family = "gaussian",
           n.folds = 5,
           n.trees = 1500,
           max.trees = 1500,
           learning.rate = 0.005,
           tree.complexity = 10,
           bag.fraction = 0.5,
           verbose = T
  )

int_gbm_dispIndex_pred <- data.table(
  truth = stu[test_set_2, PV1MATH],
  response = predict(model_gbm_dispIndex, as.data.frame(stu[test_set_2, ]), n.trees = 1500)
)

ggplot(int_gbm_dispIndex_pred, aes(truth, response)) + 
  geom_point() + 
  geom_abline(slope = 1, linetype = "dashed") +
  coord_cartesian(xlim = c(200, 800), ylim = c(200, 800))


gbm.plot(model_gbm_dispIndex, n.plots = 12)
int_gbm_dispIndex <- gbm.interactions(model_gbm_dispIndex)
# write.table(int_gbm$rank.list, "Outputs/Tables/interaction_ranks.csv", row.names = F, col.names = T, sep = ",")
# write.table(int_gbm$interactions, "Outputs/Tables/interaction_full_table.csv", row.names = F, col.names = T, sep = ",")
int_gbm_dispIndex$rank.list

dr <- c(-3.75, 3.907)

# ANXMAT 8 vs MATHEFF 6
gbm.perspec(model_gbm_dispIndex, 8, 6, z.range = c(350, 600), x.range = dr, y.range = dr)

# ANXMAT 8 vs ESCS 2
gbm.perspec(model_gbm_dispIndex, 8, 2, z.range = c(400, 580), x.range = dr, y.range = dr)

# # ANXMAT vs GENDER
gbm.perspec(model_gbm_dispIndex, 8, 1, z.range = c(425, 580), x.range = dr, y.range = c(0, 1))

# ANXMAT 8 vs SCMAT 7
gbm.perspec(model_gbm_dispIndex, 8, 7, z.range = c(400, 580), x.range = dr, y.range = dr)

# MATHEFF 6 vs ESCS 2
gbm.perspec(model_gbm_dispIndex, 2, 6, z.range = c(300, 580), x.range = dr, y.range = dr)

# ANXMAT 8 vs INTMAT 9
gbm.perspec(model_gbm_dispIndex, 9, 8, z.range = c(400, 580), x.range = dr, y.range = dr)

# SUBNORM 12 vs MATHEFF 6
gbm.perspec(model_gbm_dispIndex, 12, 6, z.range = c(350, 580), x.range = dr, y.range = dr)

# FAILMAT 11 vs MATHEFF 6
gbm.perspec(model_gbm_dispIndex, 11, 6, z.range = c(350, 580), x.range = dr, y.range = dr)


### PD plots in ggplot

library(foreach)

model_gbm_dispIndex$contributions

model_gbm_dispIndex_pd <-
  foreach(k = 1:12) %do% {
    as.data.table(gbm::plot.gbm(model_gbm_dispIndex, k, return.grid = TRUE))
  }

model_gbm_dispIndex_pd_merge <- Reduce(function(x, y) merge(x, y, by = "y", all = T), model_gbm_dispIndex_pd)

for(col in names(model_gbm_dispIndex_pd_merge)) {
  if(is.factor(model_gbm_dispIndex_pd_merge[[col]])) {
    set(model_gbm_dispIndex_pd_merge, j = col, value = as.numeric(model_gbm_dispIndex_pd_merge[[col]]))
  }
}

model_gbm_dispIndex_plot <- melt(model_gbm_dispIndex_pd_merge, id.vars = "y")

model_gbm_dispIndex_plot[, variable := factor(variable, levels = model_gbm_dispIndex$contributions$var %>% as.character)]

ggplot(model_gbm_dispIndex_plot, aes(value, y)) + geom_point() + facet_wrap(~variable, scales = "free_x")


