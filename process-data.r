library(data.table)


# Internal cross-validation -----------------------------------------------

benchmarks <- readRDS("data/benchmark_outputs.rds")

benchmarks[any_false_links == 1 & y_pred == 1, status := "True Positive"]
benchmarks[any_false_links == 1 & y_pred == 0, status := "False Negative"]
benchmarks[any_false_links == 0 & y_pred == 1, status := "False Positive"]
benchmarks[any_false_links == 0 & y_pred == 0, status := "True Negative"]


benchmarks_eval <- benchmarks[, .(F_star = sum(status == "True Positive") / sum(status != "True Negative"),
                                  precision = sum(status == "True Positive") / sum(status %in% c("True Positive" ,
                                                                                                 "False Positive")),
                                  recall = sum(status == "True Positive") / sum(status %in% c("True Positive" ,
                                                                                              "False Negative")),
                                  accuracy = sum(status %in% c("True Positive" ,
                                                               "True Negative")) / .N),
                              by = .(predictor_set, set_id)]

benchmarks_eval <- melt(benchmarks_eval,
                        id.vars = c("predictor_set",
                                    "set_id"),
                        variable.name = "measure",
                        variable.factor = FALSE)

internal_cv_summaries <- benchmarks_eval[, .(mean_value = mean(value),
                                            sd_value = sd(value)),
                                        by = .(predictor_set, measure)]

predictor_set_labels <- c("1: No weighted graph measures",
                          "2: Weighted diameter -\nmatch probability",
                          "3: Weighted diameters -\nall attributes")

measure_levels <- c("F_star",
                    "precision",
                    "recall",
                    "accuracy")

measure_labels <- c("F*",
                    "precision",
                    "recall",
                    "accuracy")

internal_cv_summaries[, ':=' (predictor_set = factor(predictor_set,
                                                    levels = as.character(1:3),
                                                    labels = predictor_set_labels),
                             measure = factor(measure,
                                              levels = measure_levels,
                                              labels = measure_labels))]


imbalance_ratio <- benchmarks[, .(r = sum(status %in% c("True Positive", "False Negative")) / .N),
                              by = .(set_id)][, .(r = mean(r))][, r]

naive_class_perf <- data.table(measure = c("F_star",
                                           "precision",
                                           "recall",
                                           "accuracy"),
                               predictor_set = rep(NA, 4),
                               naive_classifier_prob = c(imbalance_ratio / (1+ imbalance_ratio),
                                                         imbalance_ratio,
                                                         0.5,
                                                         0.5))
naive_class_perf[, measure := factor(measure,
                                     levels = measure_levels,
                                     labels = measure_labels)]



# External cross-validation -----------------------------------------------

pred_outputs <- readRDS("data/prediction_outputs.rds")
pred_outputs <- pred_outputs[, .(predictor_set, max_duplicates, corruption_multiplier, status, set_id)]

pred_evaluation <- pred_outputs[, .(F_star = sum(status == "True Positive") / sum(status != "True Negative"),
                                    precision = sum(status == "True Positive") / sum(status %in% c("True Positive" ,
                                                                                                   "False Positive")),
                                    recall = sum(status == "True Positive") / sum(status %in% c("True Positive" ,
                                                                                                "False Negative")),
                                    accuracy = sum(status %in% c("True Positive" ,
                                                                 "True Negative")) / .N),
                                by = .(predictor_set, max_duplicates, corruption_multiplier, set_id)]

pred_evaluation <- melt(pred_evaluation,
                        id.vars = c("predictor_set",
                                    "max_duplicates",
                                    "corruption_multiplier",
                                    "set_id"),
                        variable.name = "measure",
                        variable.factor = FALSE)

external_cv_summaries <- pred_evaluation[, .(mean_value = mean(value),
                                            sd_value = sd(value)),
                                        by = .(predictor_set, max_duplicates, corruption_multiplier, measure)]

external_cv_summaries[, ':=' (corruption_multiplier = factor(corruption_multiplier,
                                                            levels = c(0.5, 1, 2, 5)),
                             predictor_set = factor(predictor_set,
                                                    levels = as.character(1:3),
                                                    labels = predictor_set_labels),
                             max_duplicates = factor(max_duplicates,
                                                     levels = c(49, 99),
                                                     labels = paste(c(50, 100), "records max\nper entity")),
                             measure = factor(measure,
                                              levels = measure_levels,
                                              labels = measure_labels))]


pred_imbalance_ratio <- pred_outputs[, .(r = sum(status %in% c("True Positive", "False Negative")) / .N),
                                     by = .(set_id,
                                            max_duplicates,
                                            corruption_multiplier)][, .(r = mean(r)),
                                                                    by = .(max_duplicates,
                                                                           corruption_multiplier)]

pred_imbalance_ratio[, i := 1L]
pred_naive_class_perf <- data.table(i = rep(1L, 4),
                                    measure = c("F_star",
                                                "precision",
                                                "recall",
                                                "accuracy"),
                                    predictor_set = rep(NA, 4))

pred_imbalance_ratio <- merge(pred_imbalance_ratio,
                              pred_naive_class_perf,
                              by = "i",
                              allow.cartesian = TRUE,
                              all = TRUE)



pred_imbalance_ratio[measure %in% c("recall", "accuracy"), naive_classifier_prob := 0.5]
pred_imbalance_ratio[measure == "precision", naive_classifier_prob := r]
pred_imbalance_ratio[measure == "F_star", naive_classifier_prob := r / (1 + r)]

pred_imbalance_ratio[, c("r", "i") := NULL]

pred_imbalance_ratio[, ':=' (corruption_multiplier = factor(corruption_multiplier,
                                                            levels = c(0.5, 1, 2, 5)),
                             max_duplicates = factor(max_duplicates,
                                                     levels = c(49, 99),
                                                     labels = paste(c(50, 100), "records max\nper entity")),
                             measure = factor(measure,
                                              levels = measure_levels,
                                              labels = measure_labels))]



# Classifier feature importance -------------------------------------------


prediction_models <- readRDS("data/prediction_models.rds")

models_rel_imp <- rbindlist(lapply(1:3, function(x, data) {
  dt <- xgboost::xgb.importance(model = data[[x]])
  dt[, predictor_set := x]
  return(dt)
}, data = prediction_models))

models_rel_imp[, predictor_set := as.character(predictor_set)]

gm_levels <- c('diameter',
               'diameter_weight_total',
               'diameter_weight_given_name',
               'diameter_weight_family_name',
               'diameter_weight_dob_d',
               'diameter_weight_dob_m',
               'diameter_weight_dob_y',
               'diameter_weight_gender',
               'global_clustering_coefficient',
               'averaged_local_clustering_coefficient',
               'assortativity_degree',
               'density')

gm_labels <- gsub("_", " ",
                  c('diameter',
                    'diameter_weight_total',
                    'diameter_weight_given_name',
                    'diameter_weight_family_name',
                    'diameter_weight_DoB_(day)',
                    'diameter_weight_DoB_(month)',
                    'diameter_weight_DoB_(year)',
                    'diameter_weight_gender',
                    'global_clustering_coefficient',
                    'averaged_local_clustering_coefficient',
                    'assortativity_degree',
                    'density'))


setnames(models_rel_imp, tolower(colnames(models_rel_imp)))
models_rel_imp[, ':=' (feature = factor(feature,
                                        levels = gm_levels,
                                        labels = gm_labels),
                       predictor_set = factor(predictor_set,
                                              levels = as.character(1:3),
                                              labels = predictor_set_labels))]


colour_scale_values <- c("#1b9e77",
                         "#d95f02",
                         "#7570b3")

names(colour_scale_values) <- predictor_set_labels


save(internal_cv_summaries, naive_class_perf,
     external_cv_summaries, pred_imbalance_ratio,
     models_rel_imp, colour_scale_values,
     file = "plot_data.rda")
