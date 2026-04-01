#' logistic—function to perform logistic analysis and return the results to the user.

#' @param data data can be a CSV file or within an R package, such as MASS::Pima.te
#' @param colnum the column number with the logistic data
#' @param numresamples the number of resamples
#' @param positive_rate rate of 1 to 0 in the data set (default = 0.5)
#' @param save_all_trained_models "Y" or "N". Places all the trained models in the Environment
#' @param save_all_plots Options to save all plots
#' @param how_to_handle_strings 0: No strings, 1: Factor values
#' @param set_seed Asks the user to set a seed to create reproducible results
#' @param do_you_have_new_data "Y" or "N". If "Y", then you will be asked for the new data
#' @param remove_VIF_greater_than Removes features with VIGF value above the given amount (default = 5.00)
#' @param remove_data_correlations_greater_than Enter a number to remove correlations in the initial data set (such as 0.99)
#' @param remove_ensemble_correlations_greater_than Enter a number to remove correlations in the ensemble data set (such as 0.99)
#' @param stratified_column_number 0 if no stratified random sampling, or column number for stratified random sampling
#' @param use_parallel "Y" or "N" for parallel processing
#' @param train_amount set the amount for the training data
#' @param test_amount set the amount for the testing data
#' @param validation_amount Set the amount for the validation data

#' @return a real number
#' @export Logistic Automatically builds 24 logistic models (15 individual models and nine ensembles of models)

#' @importFrom C50 C5.0
#' @importFrom car vif
#' @importFrom caret dummyVars
#' @importFrom corrplot corrplot
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr across count mutate relocate select %>%
#' @importFrom e1071 naiveBayes svm
#' @importFrom gam gam
#' @importFrom gbm gbm
#' @importFrom ggplot2 geom_boxplot geom_histogram ggplot facet_wrap labs theme_bw labs aes
#' @importFrom ggplotify as.ggplot
#' @importFrom glmnet glmnet
#' @importFrom graphics hist panel.smooth par rect
#' @importFrom gridExtra grid.arrange
#' @importFrom gt gt
#' @importFrom htmltools h2
#' @importFrom htmlwidgets prependContent
#' @importFrom ipred bagging
#' @importFrom MachineShop fit
#' @importFrom magrittr %>%
#' @importFrom mda mda fda
#' @importFrom nnet nnet
#' @importFrom olsrr ols_plot_cooksd_bar ols_prep_cdplot_data ols_prep_cdplot_outliers
#' @importFrom parallel makeCluster
#' @importFrom pls pcr
#' @importFrom pROC roc ggroc
#' @importFrom purrr keep
#' @importFrom randomForest randomForest
#' @importFrom reactable reactable
#' @importFrom readr cols
#' @importFrom rpart rpart
#' @importFrom scales percent
#' @importFrom stats binomial cor lm model.matrix predict rbinom reorder sd
#' @importFrom tidyr gather pivot_longer
#' @importFrom utils head read.csv str tail
#' @importFrom vip vi
#' @importFrom xgboost xgb.DMatrix xgb.train


### The LogisticEnsembles function ####
Logistic <- function(data, colnum, numresamples, positive_rate, remove_VIF_greater_than, remove_data_correlations_greater_than, remove_ensemble_correlations_greater_than,
                     save_all_trained_models = c("Y", "N"), save_all_plots = c("Y", "N"), set_seed = c("Y", "N"), how_to_handle_strings = c(0("none"), 1("factor levels"), 2("One-hot encoding"), 3("One-hot encoding with jitter")),
                     do_you_have_new_data = c("Y", "N"), stratified_column_number, use_parallel = c("Y", "N"),
                     train_amount, test_amount, validation_amount) {

#### Initialize values ####
use_parallel <- 0
no_cores <- 0

if (use_parallel == "Y") {
  cl <- parallel::makeCluster(no_cores, type = "FORK")
  doParallel::registerDoParallel(cl)
}

old_data <- data

colnames(data)[colnum] <- "y"

df <- data %>% dplyr::relocate(y, .after = dplyr::last_col()) # Moves the target column to the last column on the right

#### Set seed ####

if(set_seed == "N"){
  df <- df[sample(1:nrow(df)), ] # randomizes the rows
}

if(set_seed == "Y"){
  seed = as.integer(readline("Which integer would you like to use for the seed? "))
}

#### Set up stratified random column (if the user selects to use it) ####
if(stratified_column_number >0) {
  levels <- levels(as.factor((df[, stratified_column_number]))) # gets the levels for stratified data
}

#### Getting new data (if the user has new data) ####

if (do_you_have_new_data == "Y") {
  newdata <- readline("What is the URL for the new data? ")
  newdata <- read.csv(newdata)
  # colnames(newdata)[colnum] <- "y"
  # newdata <- newdata %>% dplyr::relocate(y, .after = dplyr::last_col()) # Moves the target column to the last column on the right
}

#### How to handle strings ####

if (how_to_handle_strings == 1) {
  df <- dplyr::mutate_if(df, is.character, as.factor)
  df <- dplyr::mutate_if(df, is.factor, as.numeric)
}

if (how_to_handle_strings == 1 && do_you_have_new_data == "Y") {
  newdata <- dplyr::mutate_if(newdata, is.character, as.factor)
  newdata <- dplyr::mutate_if(newdata, is.factor, as.numeric)
}

if (how_to_handle_strings == 2) {
  dummy <- caret::dummyVars(" ~ .", data=df)
  df <- data.frame(predict(dummy, newdata=df))
}

if (how_to_handle_strings == 2 && do_you_have_new_data == "Y") {
  dummy <- caret::dummyVars(" ~ .", data=newdata)
  newdata <- data.frame(predict(dummy, newdata=newdata))
}

if (how_to_handle_strings == 3) {
  dummy <- caret::dummyVars(" ~ .", data=df)
  df <- data.frame(predict(dummy, newdata=df))
  df <- data.frame(lapply(df, jitter))
}

if (how_to_handle_strings == 3 && do_you_have_new_data == "Y") {
  dummy <- caret::dummyVars(" ~ .", data=newdata)
  newdata <- data.frame(predict(dummy, newdata=newdata))
  newdata <- data.frame(lapply(newdata, jitter))
}

if (remove_data_correlations_greater_than > 0) {
  tmp <- stats::cor(df)
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  data_01 <- df[, !apply(tmp, 2, function(x) any(abs(x) > remove_data_correlations_greater_than, na.rm = TRUE))]
  df <- data_01
}

#### VIF ####

vif <- car::vif(stats::lm(y ~ ., data = df[, 1:ncol(df)]))
for (i in 1:ncol(df)) {
  if(max(vif) > remove_VIF_greater_than){
    df <- df %>% dplyr::select(-which.max(vif))
    vif <- car::vif(stats::lm(y ~ ., data = df[, 1:ncol(df)]))
  }
}

VIF_table <- vif

VIF_results <- reactable::reactable(as.data.frame(vif),
                                    searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                    striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "VIF_results")
)

VIF_report <- htmlwidgets::prependContent(VIF_results, htmltools::h2(class = "title", "VIF results"))


#### Save all plots in the user's choices of dimensions and formats ####

tempdir1 <- tempdir()
if(save_all_plots == "Y"){
  width = as.numeric(readline("Width of the graphics: "))
  height = as.numeric(readline("Height of the graphics: "))
  units = readline("Which units? You may use in, cm, mm or px. ")
  scale = as.numeric(readline("What multiplicative scaling factor? "))
  device = readline("Which device to use? You may enter eps, jpeg, pdf, png, svg or tiff: ")
  dpi <- as.numeric(readline("Plot resolution. Applies only to raster output types (jpeg, png, tiff): "))
}

#### Head of the data frame ####

df_head <- head(df)

head_df <- reactable::reactable(head(df),
                                searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "head_df")
)

head_df <- htmlwidgets::prependContent(head_df, htmltools::h2(class = "title", "Head of the data frame"))


#### Initialize values to 0, in alphabetical order ####

elastic_train_true_positive_rate <- 0
elastic_train_true_negative_rate <- 0
elastic_train_false_positive_rate <- 0
elastic_train_false_negative_rate <- 0
elastic_train_accuracy <- 0
elastic_train_F1_score <- 0
elastic_train_fit <- 0
elastic_test_true_positive_rate <- 0
elastic_test_true_negative_rate <- 0
elastic_test_false_positive_rate <- 0
elastic_test_false_negative_rate <- 0
elastic_test_accuracy <- 0
elastic_test_F1_score <- 0
elastic_test_predictions <- 0
elastic_validation_true_positive_rate <- 0
elastic_validation_true_negative_rate <- 0
elastic_validation_false_positive_rate <- 0
elastic_validation_false_negative_rate <- 0
elastic_validation_accuracy <- 0
elastic_validation_F1_score <- 0
elastic_validation_predictions <- 0
elastic_holdout_true_positive_rate <- 0
elastic_holdout_true_negative_rate <- 0
elastic_holdout_false_positive_rate <- 0
elastic_holdout_false_negative_rate <- 0
elastic_holdout_accuracy <- 0
elastic_holdout_F1_score <- 0
elastic_holdout_false_positive_mean <- 0
elastic_duration <- 0
elastic_train_positive_predictive_value <- 0
elastic_train_negative_predictive_value <- 0
elastic_test_positive_predictive_value <- 0
elastic_test_negative_predictive_value <- 0
elastic_validation_positive_predictive_value <- 0
elastic_validation_negative_predictive_value <- 0
elastic_holdout_positive_predictive_value <- 0
elastic_holdout_negative_predictive_value <- 0
elastic_holdout_overfitting <- 0
elastic_holdout_overfitting_mean <- 0
elastic_table_total <- 0
elastic_AUC <- 0

fda_train_true_positive_rate <- 0
fda_train_true_negative_rate <- 0
fda_train_false_positive_rate <- 0
fda_train_false_negative_rate <- 0
fda_train_accuracy <- 0
fda_train_F1_score <- 0
fda_test_true_positive_rate <- 0
fda_test_true_negative_rate <- 0
fda_test_false_positive_rate <- 0
fda_test_false_negative_rate <- 0
fda_test_accuracy <- 0
fda_test_F1_score <- 0
fda_validation_true_positive_rate <- 0
fda_validation_true_negative_rate <- 0
fda_validation_false_positive_rate <- 0
fda_validation_false_negative_rate <- 0
fda_validation_accuracy <- 0
fda_validation_F1_score <- 0
fda_holdout_true_positive_rate <- 0
fda_holdout_true_negative_rate <- 0
fda_holdout_false_positive_rate <- 0
fda_holdout_false_negative_rate <- 0
fda_holdout_accuracy <- 0
fda_holdout_F1_score <- 0
fda_duration <- 0
fda_train_positive_predictive_value <- 0
fda_train_negative_predictive_value <- 0
fda_test_positive_predictive_value <- 0
fda_test_negative_predictive_value <- 0
fda_validation_positive_predictive_value <- 0
fda_validation_negative_predictive_value <- 0
fda_holdout_positive_predictive_value <- 0
fda_holdout_negative_predictive_value <- 0
fda_holdout_overfitting <- 0
fda_holdout_overfitting_mean <- 0
fda_table_total <- 0
fda_AUC <- 0

gam_train_true_positive_rate <- 0
gam_train_true_negative_rate <- 0
gam_train_false_positive_rate <- 0
gam_train_false_negative_rate <- 0
gam_train_accuracy <- 0
gam_train_F1_score <- 0
gam_test_true_positive_rate <- 0
gam_test_true_negative_rate <- 0
gam_test_false_positive_rate <- 0
gam_test_false_negative_rate <- 0
gam_test_accuracy <- 0
gam_test_F1_score <- 0
gam_validation_true_positive_rate <- 0
gam_validation_true_negative_rate <- 0
gam_validation_false_positive_rate <- 0
gam_validation_false_negative_rate <- 0
gam_validation_accuracy <- 0
gam_validation_F1_score <- 0
gam_holdout_true_positive_rate <- 0
gam_holdout_true_negative_rate <- 0
gam_holdout_false_positive_rate <- 0
gam_holdout_false_negative_rate <- 0
gam_holdout_accuracy <- 0
gam_holdout_F1_score <- 0
gam_duration <- 0
gam_train_positive_predictive_value <- 0
gam_train_negative_predictive_value <- 0
gam_test_positive_predictive_value <- 0
gam_test_negative_predictive_value <- 0
gam_validation_positive_predictive_value <- 0
gam_validation_negative_predictive_value <- 0
gam_holdout_positive_predictive_value <- 0
gam_holdout_negative_predictive_value <- 0
gam_holdout_overfitting <- 0
gam_holdout_overfitting_mean <- 0
gam_table_total <- 0
gam_AUC <- 0

gbm_train_true_positive_rate <- 0
gbm_train_true_negative_rate <- 0
gbm_train_false_positive_rate <- 0
gbm_train_false_negative_rate <- 0
gbm_train_accuracy <- 0
gbm_train_F1_score <- 0
gbm_test_true_positive_rate <- 0
gbm_test_true_negative_rate <- 0
gbm_test_false_positive_rate <- 0
gbm_test_false_negative_rate <- 0
gbm_test_accuracy <- 0
gbm_test_F1_score <- 0
gbm_validation_true_positive_rate <- 0
gbm_validation_true_negative_rate <- 0
gbm_validation_false_positive_rate <- 0
gbm_validation_false_negative_rate <- 0
gbm_validation_accuracy <- 0
gbm_validation_F1_score <- 0
gbm_holdout_true_positive_rate <- 0
gbm_holdout_true_negative_rate <- 0
gbm_holdout_false_positive_rate <- 0
gbm_holdout_false_negative_rate <- 0
gbm_holdout_accuracy <- 0
gbm_holdout_F1_score <- 0
gbm_duration <- 0
gbm_train_positive_predictive_value <- 0
gbm_train_negative_predictive_value <- 0
gbm_test_positive_predictive_value <- 0
gbm_test_negative_predictive_value <- 0
gbm_validation_positive_predictive_value <- 0
gbm_validation_negative_predictive_value <- 0
gbm_holdout_positive_predictive_value <- 0
gbm_holdout_negative_predictive_value <- 0
gbm_holdout_overfitting <- 0
gbm_holdout_overfitting_mean <- 0
gbm_table_total <- 0
gbm_AUC <- 0

glmnet_train_true_positive_rate <- 0
glmnet_train_true_negative_rate <- 0
glmnet_train_false_positive_rate <- 0
glmnet_train_false_negative_rate <- 0
glmnet_train_accuracy <- 0
glmnet_train_F1_score <- 0
glmnet_train_fit <- 0
glmnet_test_true_positive_rate <- 0
glmnet_test_true_negative_rate <- 0
glmnet_test_false_positive_rate <- 0
glmnet_test_false_negative_rate <- 0
glmnet_test_accuracy <- 0
glmnet_test_F1_score <- 0
glmnet_test_predictions <- 0
glmnet_validation_true_positive_rate <- 0
glmnet_validation_true_negative_rate <- 0
glmnet_validation_false_positive_rate <- 0
glmnet_validation_false_negative_rate <- 0
glmnet_validation_accuracy <- 0
glmnet_validation_F1_score <- 0
glmnet_validation_predictions <- 0
glmnet_holdout_true_positive_rate <- 0
glmnet_holdout_true_negative_rate <- 0
glmnet_holdout_false_positive_rate <- 0
glmnet_holdout_false_negative_rate <- 0
glmnet_holdout_accuracy <- 0
glmnet_holdout_F1_score <- 0
glmnet_holdout_false_positive_mean <- 0
glmnet_duration <- 0
glmnet_train_positive_predictive_value <- 0
glmnet_train_negative_predictive_value <- 0
glmnet_test_positive_predictive_value <- 0
glmnet_test_negative_predictive_value <- 0
glmnet_validation_positive_predictive_value <- 0
glmnet_validation_negative_predictive_value <- 0
glmnet_holdout_positive_predictive_value <- 0
glmnet_holdout_negative_predictive_value <- 0
glmnet_holdout_overfitting <- 0
glmnet_holdout_overfitting_mean <- 0
glmnet_table_total <- 0
glmnet_AUC <- 0
glmnet_auc <- 0

gb_train_true_positive_rate <- 0
gb_train_true_negative_rate <- 0
gb_train_false_positive_rate <- 0
gb_train_false_negative_rate <- 0
gb_train_accuracy <- 0
gb_train_F1_score <- 0
gb_test_true_positive_rate <- 0
gb_test_true_negative_rate <- 0
gb_test_false_positive_rate <- 0
gb_test_false_negative_rate <- 0
gb_test_accuracy <- 0
gb_test_F1_score <- 0
gb_validation_true_positive_rate <- 0
gb_validation_true_negative_rate <- 0
gb_validation_false_positive_rate <- 0
gb_validation_false_negative_rate <- 0
gb_validation_accuracy <- 0
gb_validation_F1_score <- 0
gb_holdout_true_positive_rate <- 0
gb_holdout_true_negative_rate <- 0
gb_holdout_false_positive_rate <- 0
gb_holdout_false_negative_rate <- 0
gb_holdout_accuracy <- 0
gb_holdout_F1_score <- 0
gb_duration <- 0
gb_train_positive_predictive_value <- 0
gb_train_negative_predictive_value <- 0
gb_test_positive_predictive_value <- 0
gb_test_negative_predictive_value <- 0
gb_validation_positive_predictive_value <- 0
gb_validation_negative_predictive_value <- 0
gb_holdout_positive_predictive_value <- 0
gb_holdout_negative_predictive_value <- 0
gb_holdout_overfitting <- 0
gb_holdout_overfitting_mean <- 0
gb_table_total <- 0
gb_AUC <- 0

neuralnet_train_true_positive_rate <- 0
neuralnet_train_true_negative_rate <- 0
neuralnet_train_false_positive_rate <- 0
neuralnet_train_false_negative_rate <- 0
neuralnet_train_accuracy <- 0
neuralnet_train_F1_score <- 0
neuralnet_test_true_positive_rate <- 0
neuralnet_test_true_negative_rate <- 0
neuralnet_test_false_positive_rate <- 0
neuralnet_test_false_negative_rate <- 0
neuralnet_test_accuracy <- 0
neuralnet_test_F1_score <- 0
neuralnet_validation_true_positive_rate <- 0
neuralnet_validation_true_negative_rate <- 0
neuralnet_validation_false_positive_rate <- 0
neuralnet_validation_false_negative_rate <- 0
neuralnet_validation_accuracy <- 0
neuralnet_validation_F1_score <- 0
neuralnet_holdout_true_positive_rate <- 0
neuralnet_holdout_true_negative_rate <- 0
neuralnet_holdout_false_positive_rate <- 0
neuralnet_holdout_false_negative_rate <- 0
neuralnet_holdout_accuracy <- 0
neuralnet_holdout_F1_score <- 0
neuralnet_duration <- 0
neuralnet_train_positive_predictive_value <- 0
neuralnet_train_negative_predictive_value <- 0
neuralnet_test_positive_predictive_value <- 0
neuralnet_test_negative_predictive_value <- 0
neuralnet_validation_positive_predictive_value <- 0
neuralnet_validation_negative_predictive_value <- 0
neuralnet_holdout_positive_predictive_value <- 0
neuralnet_holdout_negative_predictive_value <- 0
neuralnet_holdout_overfitting <- 0
neuralnet_holdout_overfitting_mean <- 0
neuralnet_table_total <- 0
neuralnet_AUC <- 0

xgb_train_true_positive_rate <- 0
xgb_train_true_negative_rate <- 0
xgb_train_false_positive_rate <- 0
xgb_train_false_negative_rate <- 0
xgb_train_accuracy <- 0
xgb_train_F1_score <- 0
xgb_test_true_positive_rate <- 0
xgb_test_true_negative_rate <- 0
xgb_test_false_positive_rate <- 0
xgb_test_false_negative <- 0
xgb_test_false_negative_rate <- 0
xgb_test_accuracy <- 0
xgb_test_F1_score <- 0
xgb_test_false_positive <- 0
xgb_validation_true_positive_rate <- 0
xgb_validation_true_negative <- 0
xgb_validation_true_negative_rate <- 0
xgb_validation_false_positive_rate <- 0
xgb_validation_false_negative_rate <- 0
xgb_validation_accuracy <- 0
xgb_validation_F1_score <- 0
xgb_holdout_true_positive_rate <- 0
xgb_holdout_true_negative_rate <- 0
xgb_holdout_false_positive_rate <- 0
xgb_holdout_false_negative_rate <- 0
xgb_holdout_accuracy <- 0
xgb_holdout_F1_score <- 0
xgb_duration <- 0
xgb_train_positive_predictive_value <- 0
xgb_train_negative_predictive_value <- 0
xgb_test_positive_predictive_value <- 0
xgb_test_negative_predictive_value <- 0
xgb_validation_positive_predictive_value <- 0
xgb_validation_negative_predictive_value <- 0
xgb_holdout_positive_predictive_value <- 0
xgb_holdout_negative_predictive_value <- 0
xgb_train_sensitivity <- 0
xgb_test_sensitivity <- 0
xgb_validation_sensitivity <- 0
xgb_train_specificity <- 0
xgb_test_specificity <- 0
xgb_validation_specificity <- 0
xgb_train_precision <- 0
xgb_test_precision <- 0
xgb_validation_precision <- 0
xgb_holdout_sensitivity <- 0
xgb_holdout_specificity <- 0
xgb_holdout_precision <- 0
xgb_holdout_overfitting <- 0
xgb_holdout_accuracy_sd <- 0
xgb.params <- 0
xgb_duration_sd <- 0
XGBModel <- 0
xgb_table_total <- 0
xgb_AUC <- 0

ensemble_C50_train_true_positive_rate <- 0
ensemble_C50_train_true_negative_rate <- 0
ensemble_C50_train_false_positive_rate <- 0
ensemble_C50_train_false_negative_rate <- 0
ensemble_C50_train_accuracy <- 0
ensemble_C50_train_F1_score <- 0
ensemble_C50_train_predictions <- 0
ensemble_C50_train_table <- 0
ensemble_C50_test_true_positive_rate <- 0
ensemble_C50_test_true_negative_rate <- 0
ensemble_C50_test_false_positive_rate <- 0
ensemble_C50_test_false_negative_rate <- 0
ensemble_C50_test_accuracy <- 0
ensemble_C50_test_F1_score <- 0
ensemble_C50_validation_true_positive_rate <- 0
ensemble_C50_validation_true_negative_rate <- 0
ensemble_C50_validation_false_positive_rate <- 0
ensemble_C50_validation_false_negative_rate <- 0
ensemble_C50_validation_accuracy <- 0
ensemble_C50_validation_F1_score <- 0
ensemble_C50_holdout_true_positive_rate <- 0
ensemble_C50_holdout_true_negative_rate <- 0
ensemble_C50_holdout_false_positive_rate <- 0
ensemble_C50_holdout_false_negative_rate <- 0
ensemble_C50_holdout_accuracy <- 0
ensemble_C50_holdout_F1_score <- 0
ensemble_C50_duration <- 0
ensemble_C50_train_positive_predictive_value <- 0
ensemble_C50_train_negative_predictive_value <- 0
ensemble_C50_test_positive_predictive_value <- 0
ensemble_C50_test_negative_predictive_value <- 0
ensemble_C50_validation_positive_predictive_value <- 0
ensemble_C50_validation_negative_predictive_value <- 0
ensemble_C50_holdout_positive_predictive_value <- 0
ensemble_C50_holdout_negative_predictive_value <- 0
ensemble_C50_holdout_overfitting <- 0
ensemble_C50_holdout_overfitting_mean <- 0
ensemble_C50_table_total <- 0
ensemble_C50_AUC <- 0

ensemble_elastic_train_true_positive_rate <- 0
ensemble_elastic_train_true_negative_rate <- 0
ensemble_elastic_train_false_positive_rate <- 0
ensemble_elastic_train_false_negative_rate <- 0
ensemble_elastic_train_accuracy <- 0
ensemble_elastic_train_F1_score <- 0
ensemble_elastic_train_fit <- 0
ensemble_elastic_train_table <- 0
ensemble_elastic_test_true_positive_rate <- 0
ensemble_elastic_test_true_negative_rate <- 0
ensemble_elastic_test_false_positive_rate <- 0
ensemble_elastic_test_false_negative_rate <- 0
ensemble_elastic_test_accuracy <- 0
ensemble_elastic_test_F1_score <- 0
ensemble_elastic_test_predictions <- 0
ensemble_elastic_validation_true_positive_rate <- 0
ensemble_elastic_validation_true_negative_rate <- 0
ensemble_elastic_validation_false_positive_rate <- 0
ensemble_elastic_validation_false_negative_rate <- 0
ensemble_elastic_validation_accuracy <- 0
ensemble_elastic_validation_F1_score <- 0
ensemble_elastic_validation_predictions <- 0
ensemble_elastic_holdout_true_positive_rate <- 0
ensemble_elastic_holdout_true_negative_rate <- 0
ensemble_elastic_holdout_false_positive_rate <- 0
ensemble_elastic_holdout_false_negative_rate <- 0
ensemble_elastic_holdout_accuracy <- 0
ensemble_elastic_holdout_F1_score <- 0
ensemble_elastic_holdout_false_positive_mean <- 0
ensemble_elastic_duration <- 0
ensemble_elastic_train_positive_predictive_value <- 0
ensemble_elastic_train_negative_predictive_value <- 0
ensemble_elastic_test_positive_predictive_value <- 0
ensemble_elastic_test_negative_predictive_value <- 0
ensemble_elastic_validation_positive_predictive_value <- 0
ensemble_elastic_validation_negative_predictive_value <- 0
ensemble_elastic_holdout_positive_predictive_value <- 0
ensemble_elastic_holdout_negative_predictive_value <- 0
ensemble_elastic_holdout_overfitting <- 0
ensemble_elastic_holdout_overfitting_mean <- 0
ensemble_elastic_table_total <- 0
ensemble_elastic_AUC <- 0

ensemble_glmnet_train_true_positive_rate <- 0
ensemble_glmnet_train_true_negative_rate <- 0
ensemble_glmnet_train_false_positive_rate <- 0
ensemble_glmnet_train_false_negative_rate <- 0
ensemble_glmnet_train_accuracy <- 0
ensemble_glmnet_train_F1_score <- 0
ensemble_glmnet_train_table <- 0
ensemble_glmnet_test_true_positive_rate <- 0
ensemble_glmnet_test_true_negative_rate <- 0
ensemble_glmnet_test_false_positive_rate <- 0
ensemble_glmnet_test_false_negative_rate <- 0
ensemble_glmnet_test_accuracy <- 0
ensemble_glmnet_test_F1_score <- 0
ensemble_glmnet_test_pred <- 0
ensemble_glmnet_validation_true_positive_rate <- 0
ensemble_glmnet_validation_true_negative_rate <- 0
ensemble_glmnet_validation_false_positive_rate <- 0
ensemble_glmnet_validation_false_negative_rate <- 0
ensemble_glmnet_validation_accuracy <- 0
ensemble_glmnet_validation_F1_score <- 0
ensemble_glmnet_holdout_true_positive_rate <- 0
ensemble_glmnet_holdout_true_negative_rate <- 0
ensemble_glmnet_holdout_false_positive_rate <- 0
ensemble_glmnet_holdout_false_negative_rate <- 0
ensemble_glmnet_holdout_accuracy <- 0
ensemble_glmnet_holdout_F1_score <- 0
ensemble_glmnet_duration <- 0
ensemble_glmnet_train_positive_predictive_value <- 0
ensemble_glmnet_train_negative_predictive_value <- 0
ensemble_glmnet_test_positive_predictive_value <- 0
ensemble_glmnet_test_negative_predictive_value <- 0
ensemble_glmnet_validation_positive_predictive_value <- 0
ensemble_glmnet_validation_negative_predictive_value <- 0
ensemble_glmnet_holdout_positive_predictive_value <- 0
ensemble_glmnet_holdout_negative_predictive_value <- 0
ensemble_glmnet_holdout_overfitting <- 0
ensemble_glmnet_holdout_overfitting_mean <- 0
ensemble_glmnet_table_total <- 0
ensemble_glmnet_AUC <- 0

ensemble_neuralnet_train_true_positive_rate <- 0
ensemble_neuralnet_train_true_negative_rate <- 0
ensemble_neuralnet_train_false_positive_rate <- 0
ensemble_neuralnet_train_false_negative_rate <- 0
ensemble_neuralnet_train_accuracy <- 0
ensemble_neuralnet_train_F1_score <- 0
ensemble_neuralnet_train_table <- 0
ensemble_neuralnet_test_true_positive_rate <- 0
ensemble_neuralnet_test_true_negative_rate <- 0
ensemble_neuralnet_test_false_positive_rate <- 0
ensemble_neuralnet_test_false_negative_rate <- 0
ensemble_neuralnet_test_accuracy <- 0
ensemble_neuralnet_test_F1_score <- 0
ensemble_neuralnet_validation_true_positive_rate <- 0
ensemble_neuralnet_validation_true_negative_rate <- 0
ensemble_neuralnet_validation_false_positive_rate <- 0
ensemble_neuralnet_validation_false_negative_rate <- 0
ensemble_neuralnet_validation_accuracy <- 0
ensemble_neuralnet_validation_F1_score <- 0
ensemble_neuralnet_holdout_true_positive_rate <- 0
ensemble_neuralnet_holdout_true_negative_rate <- 0
ensemble_neuralnet_holdout_false_positive_rate <- 0
ensemble_neuralnet_holdout_false_negative_rate <- 0
ensemble_neuralnet_holdout_accuracy <- 0
ensemble_neuralnet_holdout_F1_score <- 0
ensemble_neuralnet_duration <- 0
ensemble_neuralnet_train_positive_predictive_value <- 0
ensemble_neuralnet_train_negative_predictive_value <- 0
ensemble_neuralnet_test_positive_predictive_value <- 0
ensemble_neuralnet_test_negative_predictive_value <- 0
ensemble_neuralnet_validation_positive_predictive_value <- 0
ensemble_neuralnet_validation_negative_predictive_value <- 0
ensemble_neuralnet_holdout_positive_predictive_value <- 0
ensemble_neuralnet_holdout_negative_predictive_value <- 0
ensemble_neuralnet_holdout_overfitting <- 0
ensemble_neuralnet_holdout_overfitting_mean <- 0
ensemble_neuralnet_table_total <- 0
ensemble_neuralnet_AUC <- 0

ensemble_xgb_train_true_positive_rate <- 0
ensemble_xgb_train_true_negative_rate <- 0
ensemble_xgb_train_false_positive_rate <- 0
ensemble_xgb_train_false_negative_rate <- 0
ensemble_xgb_train_accuracy <- 0
ensemble_xgb_train_F1_score <- 0
ensemble_xgb_train_table <- 0
ensemble_xgb_test_true_positive_rate <- 0
ensemble_xgb_test_true_negative_rate <- 0
ensemble_xgb_test_false_positive_rate <- 0
ensemble_xgb_test_false_negative <- 0
ensemble_xgb_test_false_negative_rate <- 0
ensemble_xgb_test_accuracy <- 0
ensemble_xgb_test_F1_score <- 0
ensemble_xgb_test_false_positive <- 0
ensemble_xgb_validation_true_positive_rate <- 0
ensemble_xgb_validation_true_negative <- 0
ensemble_xgb_validation_true_negative_rate <- 0
ensemble_xgb_validation_false_positive_rate <- 0
ensemble_xgb_validation_false_negative_rate <- 0
ensemble_xgb_validation_accuracy <- 0
ensemble_xgb_validation_F1_score <- 0
ensemble_xgb_holdout_true_positive_rate <- 0
ensemble_xgb_holdout_true_negative_rate <- 0
ensemble_xgb_holdout_false_positive_rate <- 0
ensemble_xgb_holdout_false_negative_rate <- 0
ensemble_xgb_holdout_accuracy <- 0
ensemble_xgb_holdout_F1_score <- 0
ensemble_xgb_duration <- 0
ensemble_xgb_train_positive_predictive_value <- 0
ensemble_xgb_train_negative_predictive_value <- 0
ensemble_xgb_test_positive_predictive_value <- 0
ensemble_xgb_test_negative_predictive_value <- 0
ensemble_xgb_validation_positive_predictive_value <- 0
ensemble_xgb_validation_negative_predictive_value <- 0
ensemble_xgb_holdout_positive_predictive_value <- 0
ensemble_xgb_holdout_negative_predictive_value <- 0
ensemble_xgb_train_sensitivity <- 0
ensemble_xgb_test_sensitivity <- 0
ensemble_xgb_validation_sensitivity <- 0
ensemble_xgb_train_specificity <- 0
ensemble_xgb_test_specificity <- 0
ensemble_xgb_validation_specificity <- 0
ensemble_xgb_train_precision <- 0
ensemble_xgb_test_precision <- 0
ensemble_xgb_validation_precision <- 0
ensemble_xgb_holdout_sensitivity <- 0
ensemble_xgb_holdout_specificity <- 0
ensemble_xgb_holdout_precision <- 0
ensemble_xgb_holdout_overfitting <- 0
ensemble_xgb_holdout_accuracy_sd <- 0
ensemble_xgb.params <- 0
ensemble_xgb_duration_sd <- 0
ensemble_xgbModel <- 0
ensemble_xgb_table_total <- 0
ensemble_xgb_AUC <- 0

y_ensemble_validation <- 0
y_ensemble_test <- 0
y_ensemble_train <- 0
y_ensemble_validation <- 0
accuracy_plot <- 0



y <- 0

model <- 0

value <- 0

Mean <- 0

Accuracy <- 0

type <- 0

name <- 0

holdout <- 0

perc <- 0

Duration <- 0

Model <- 0

summary_results <- 0

Overfitting_Mean <- 0

head_ensemble <- 0

Accuracy_sd <- 0
Overfitting_sd <- 0
Duration_sd <- 0
remove_VIF_above <- 0
mallows_cp <- 0
Group.1 <- 0
x <- 0
train_ratio_df <- data.frame()
test_ratio_df <- data.frame()
validation_ratio_df <- data.frame()
stratified_sampling_report <- 0
section <- 0
Percentage <- 0
Variable <- 0
Area_Under_Curve <- 0


#### Barchart of the data against y ####
barchart <-df %>%
  tidyr::pivot_longer(!y) %>%
  dplyr::summarise(dplyr::across(value, sum), .by = c(y, name)) %>%
  dplyr::mutate(perc = proportions(value), .by = c(name)) %>%
  ggplot2::ggplot(ggplot2::aes(x = y, y = value)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(aes(label = value),
                     vjust = -.5) +
  ggplot2::geom_text(aes(label = scales::percent(perc),
                         vjust = 1.5),
                     col = "white") +
  ggplot2::labs(title = "Barchart of target (0 or 1) vs each feature of the numeric data") +
  ggplot2::facet_wrap(~ name, scales = "free") +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.25)))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("barchart.eps", plot = barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("barchart.jpeg", plot = barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("barchart.pdf", plot = barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("barchart.png", plot = barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("barchart.svg", plot = barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("barchart.tiff", plot = barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

barchart_percentage <- df %>%
  dplyr::mutate(dplyr::across(-y, as.numeric)) %>%
  tidyr::pivot_longer(!y) %>%
  dplyr::summarise(dplyr::across(value, sum), .by = c(y, name)) %>%
  dplyr::mutate(perc = proportions(value), .by = c(name)) %>%
  ggplot2::ggplot(ggplot2::aes(x = y, y = value)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(aes(label = scales::percent(round(perc, 3)),
                         vjust = -0.5)) +
  ggplot2::facet_wrap(~ name, scales = "free") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.25)))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("barchart_percentage.eps", plot = barchart_percentage, width = width, height = height, path = tempdir1, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("barchart_percentage.jpeg", plot = barchart_percentage, width = width, height = height, path = tempdir1,  units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("barchart_percentage.pdf", plot = barchart_percentage, width = width, height = height, path = tempdir1,  units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("barchart_percentage.png", plot = barchart_percentage, width = width, height = height, path = tempdir1,  units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("barchart_percentage.svg", plot = barchart_percentage, width = width, height = height, path = tempdir1,  units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("barchart_percentage.tiff", plot = barchart_percentage, width = width, height = height, path = tempdir1,  units = units, scale = scale, device = device, dpi = dpi)
}

#### Summary of the dataset ####

datasummary <- summary(df)

data_summary <- reactable::reactable(round(as.data.frame(do.call(cbind, lapply(df, summary))), 4),
                                     searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                     striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "data_summary")
)

data_summary <- htmlwidgets::prependContent(data_summary, htmltools::h2(class = "title", "Data summary"))


#### Correlation plot of numeric data ####
df1 <- df %>% purrr::keep(is.numeric)
M1 <- cor(df1)
title <- "Correlation plot of the numerical data"
corrplot::corrplot(M1, method = "number", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow_com/a/14754408/54964)
corrplot::corrplot(M1, method = "circle", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow_com/a/14754408/54964)


#### message correlation matrix of numeric data ####

correlationtable <- round(cor(df), 4)

correlation_table <- reactable::reactable(round(cor(df), 4),
                                          searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                          striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "correlation_table")
)

correlation_table <- htmlwidgets::prependContent(correlation_table, htmltools::h2(class = "title", "Correlation table"))


#### Boxplots of the numeric data ####
boxplots <- df1 %>%
  tidyr::gather(key = "var", value = "value") %>%
  ggplot2::ggplot(aes(x = "", y = value)) +
  ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  ggplot2::facet_wrap(~var, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Boxplots of the numeric data")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("boxplots.eps", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("boxplots.jpeg", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("boxplots.pdf", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("boxplots.png", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("boxplots.svg", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("boxplots.tiff", plot = boxplots, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
# Thanks to https://rstudio-pubs-static_s3_amazonaws_com/388596_e21196f1adf04e0ea7cd68edd9eba966_html


#### Histograms of the numeric data ####
histograms <- ggplot2::ggplot(tidyr::gather(df1, cols, value), aes(x = value)) +
  ggplot2::geom_histogram(bins = round(nrow(df1) / 10)) +
  ggplot2::facet_wrap(. ~ cols, scales = "free") +
  ggplot2::labs(title = "Histograms of each numeric column. Each bar = 10 rows of data")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("histograms.eps", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("histograms.jpeg", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("histograms.pdf", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("histograms.png", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("histograms.svg", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("histograms.tiff", plot = histograms, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Create the stratified random sampling report ####
if(stratified_column_number > 0){
  df <- df[sample(nrow(df)),]
  train <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_column_number])) %>% dplyr::sample_frac(train_amount))
  train_ratio <- table(train[, stratified_column_number])/nrow(train)
  train_ratio_df <- dplyr::bind_rows(train_ratio_df, train_ratio)
  train_ratio_mean <- colMeans(train_ratio_df)

  test <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_column_number])) %>% dplyr::sample_frac(test_amount))
  test_ratio <- table(test[, stratified_column_number])/nrow(test)
  test_ratio_df <- dplyr::bind_rows(test_ratio_df, test_ratio)
  test_ratio_mean <- colMeans(test_ratio_df)

  validation <- as.data.frame(df %>% dplyr::group_by(colnames(df[, stratified_column_number])) %>% dplyr::sample_frac(validation_amount))
  validation_ratio <- table(validation[, stratified_column_number])/nrow(validation)
  validation_ratio_df <- dplyr::bind_rows(validation_ratio_df, validation_ratio)
  validation_ratio_mean <- colMeans(validation_ratio_df)

  total_data_mean <- table(data[, stratified_column_number])/nrow(data)

  df1 <- as.data.frame(rbind(total_data_mean, train_ratio_mean, test_ratio_mean, validation_ratio_mean))
  df1$section <- c('whole data set', 'train ratios', 'test ratios', 'validation ratios')
  df1 <- df1 %>% dplyr::relocate(section)
  colnames(df1) <- c('Section', levels)

  df1 <- data.frame(lapply(df1, function(x) if(is.numeric(x)) round(x, 4) else x))

  stratified_sampling_report <- reactable::reactable(df1, searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                                     striped = TRUE, highlight = TRUE, resizable = TRUE
  )

  htmltools::div(class = "table",
                 htmltools::div(class = "title", "stratified_sampling_report")
  )

  stratified_sampling_report <- htmlwidgets::prependContent(stratified_sampling_report, htmltools::h2(class = "title", "Stratified sampling report"))

}

#### Calculate outliers here ####

model <- lm(y ~ ., data = df)
cooks_distance_plot <- olsrr::ols_plot_cooksd_bar(model) # Thanks to https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html
k <- olsrr::ols_prep_cdplot_data(model)
outliers <- old_data[olsrr::ols_prep_cdplot_outliers(k)[, 1], ]
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("cooks_distance_plot.eps", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("cooks_distance_plot.jpeg", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("cooks_distance_plot.pdf", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("cooks_distance_plot.png", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("cooks_distance_plot.svg", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("cooks_distance_plot.tiff", plot = cooks_distance_plot, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
outlier_list <- reactable::reactable(outliers,
                                     searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                     striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "outlier_list")
)

outlier_list <- htmlwidgets::prependContent(outlier_list, htmltools::h2(class = "title", "Outlier list"))


#### Break into train, test and validation sets ####

for (i in 1:numresamples) {
  message(noquote(""))
  message(paste0("Resampling number ", i, " of ", numresamples, sep = ','))
  message(noquote(""))

  if(set_seed == "N"){

    index <- sample(c(1:3), nrow(df), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))

    train <- df[index == 1, ]
    test <- df[index == 2, ]
    validation <- df[index == 3, ]

    train01 <- train
    test01 <- test
    validation01 <- validation

    y_train <- train$y
    y_test <- test$y
    y_validation <- validation$y
  }

  if(set_seed == "Y"){
    train <- df[1:round(train_amount*nrow(df)), ]
    test <- df[round(train_amount*nrow(df)) +1:round(test_amount*nrow(df)), ]
    validation <- df[(nrow(test) + nrow(train) +1) : nrow(df), ]

    train01 <- train
    test01 <- test
    validation01 <- validation

    y_train <- train$y
    y_test <- test$y
    y_validation <- validation$y
  }


  #### Model #6  Elastic Net ####
  elastic_start <- Sys.time()
  message("Working on Elastic")
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_elastic_lambda <- elastic_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    best_elastic_model <- glmnet::glmnet(x, y, alpha = 0.5, family = "binomial")
  }
  if(set_seed == "N"){
    best_elastic_model <- glmnet::glmnet(x, y, alpha = 0.5, family = "binomial")
  }
  elastic_train_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(train[, 1:ncol(train)-1]))
  elastic_train_predictions <- ifelse(elastic_train_pred > positive_rate, 1, 0)
  elastic_train_predictions <- elastic_train_predictions[!is.na(elastic_train_predictions)]
  ifelse(sd(elastic_train_predictions) == 0,
         elastic_train_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         elastic_train_table <- table(elastic_train_predictions, y_train[1:length(elastic_train_predictions)])
  )
  elastic_train_true_positive_rate[i] <- elastic_train_table[2, 2] / sum(elastic_train_table[2, 2] + elastic_train_table[1, 2])
  elastic_train_true_positive_rate_mean <- mean(elastic_train_true_positive_rate)
  elastic_train_true_negative_rate[i] <- elastic_train_table[1, 1] / sum(elastic_train_table[1, 1] + elastic_train_table[2, 1])
  elastic_train_true_negative_rate_mean <- mean(elastic_train_true_negative_rate)
  elastic_train_false_positive_rate[i] <- elastic_train_table[2, 1] / sum(elastic_train_table[2, 1] + elastic_train_table[1, 1])
  elastic_train_false_positive_rate_mean <- mean(elastic_train_false_positive_rate)
  elastic_train_false_negative_rate[i] <- elastic_train_table[1, 2] / sum(elastic_train_table[1, 2] + elastic_train_table[2, 2])
  elastic_train_false_negative_rate_mean <- mean(elastic_train_false_negative_rate)
  elastic_train_accuracy[i] <- (elastic_train_table[1, 1] + elastic_train_table[2, 2]) / sum(elastic_train_table)
  elastic_train_accuracy_mean <- mean(elastic_train_accuracy)
  elastic_train_F1_score[i] <- 2 * (elastic_train_table[2, 2]) / sum(2 * elastic_train_table[2, 2] + elastic_train_table[1, 2] + elastic_train_table[2, 1])
  elastic_train_F1_score_mean <- mean(elastic_train_F1_score)
  elastic_train_positive_predictive_value[i] <- elastic_train_table[2, 2] / sum(elastic_train_table[2, 2] + elastic_train_table[2, 1])
  elastic_train_positive_predictive_value_mean <- mean(elastic_train_positive_predictive_value)
  elastic_train_negative_predictive_value[i] <- elastic_train_table[1, 1] / sum(elastic_train_table[1, 1] + elastic_train_table[1, 2])
  elastic_train_negative_predictive_value_mean <- mean(elastic_train_negative_predictive_value)

  elastic_test_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(test[, 1:ncol(test)-1]))
  elastic_test_predictions <- ifelse(elastic_test_pred > positive_rate, 1, 0)
  elastic_test_predictions <- elastic_test_predictions[!is.na(elastic_test_predictions)]
  ifelse(sd(elastic_test_predictions) == 0,
         elastic_test_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         elastic_test_table <- table(elastic_test_predictions, y_test[1:length(elastic_test_predictions)])
  )
  elastic_test_true_positive_rate[i] <- elastic_test_table[2, 2] / sum(elastic_test_table[2, 2] + elastic_test_table[1, 2])
  elastic_test_true_positive_rate_mean <- mean(elastic_test_true_positive_rate)
  elastic_test_true_negative_rate[i] <- elastic_test_table[1, 1] / sum(elastic_test_table[1, 1] + elastic_test_table[2, 1])
  elastic_test_true_negative_rate_mean <- mean(elastic_test_true_negative_rate)
  elastic_test_false_positive_rate[i] <- elastic_test_table[2, 1] / sum(elastic_test_table[2, 1] + elastic_test_table[1, 1])
  elastic_test_false_positive_rate_mean <- mean(elastic_test_false_positive_rate)
  elastic_test_false_negative_rate[i] <- elastic_test_table[1, 2] / sum(elastic_test_table[1, 2] + elastic_test_table[2, 2])
  elastic_test_false_negative_rate_mean <- mean(elastic_test_false_negative_rate)
  elastic_test_accuracy[i] <- (elastic_test_table[1, 1] + elastic_test_table[2, 2]) / sum(elastic_test_table)
  elastic_test_accuracy_mean <- mean(elastic_test_accuracy)
  elastic_test_F1_score[i] <- 2 * (elastic_test_table[2, 2]) / sum(2 * elastic_test_table[2, 2] + elastic_test_table[1, 2] + elastic_test_table[2, 1])
  elastic_test_F1_score_mean <- mean(elastic_test_F1_score)
  elastic_test_positive_predictive_value[i] <- elastic_test_table[2, 2] / sum(elastic_test_table[2, 2] + elastic_test_table[2, 1])
  elastic_test_positive_predictive_value_mean <- mean(elastic_test_positive_predictive_value)
  elastic_test_negative_predictive_value[i] <- elastic_test_table[1, 1] / sum(elastic_test_table[1, 1] + elastic_test_table[1, 2])
  elastic_test_negative_predictive_value_mean <- mean(elastic_test_negative_predictive_value)

  elastic_validation_pred <- predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(validation[, 1:ncol(validation)-1]))
  elastic_validation_predictions <- ifelse(elastic_validation_pred > positive_rate, 1, 0)
  elastic_validation_predictions <- elastic_validation_predictions[!is.na(elastic_validation_predictions)]
  ifelse(sd(elastic_validation_predictions) == 0,
         elastic_validation_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         elastic_validation_table <- table(elastic_validation_predictions, y_validation[1:length(elastic_validation_predictions)])
  )
  elastic_validation_true_positive_rate[i] <- elastic_validation_table[2, 2] / sum(elastic_validation_table[2, 2] + elastic_validation_table[1, 2])
  elastic_validation_true_positive_rate_mean <- mean(elastic_validation_true_positive_rate)
  elastic_validation_true_negative_rate[i] <- elastic_validation_table[1, 1] / sum(elastic_validation_table[1, 1] + elastic_validation_table[2, 1])
  elastic_validation_true_negative_rate_mean <- mean(elastic_validation_true_negative_rate)
  elastic_validation_false_positive_rate[i] <- elastic_validation_table[2, 1] / sum(elastic_validation_table[2, 1] + elastic_validation_table[1, 1])
  elastic_validation_false_positive_rate_mean <- mean(elastic_validation_false_positive_rate)
  elastic_validation_false_negative_rate[i] <- elastic_validation_table[1, 2] / sum(elastic_validation_table[1, 2] + elastic_validation_table[2, 2])
  elastic_validation_false_negative_rate_mean <- mean(elastic_validation_false_negative_rate)
  elastic_validation_accuracy[i] <- (elastic_validation_table[1, 1] + elastic_validation_table[2, 2]) / sum(elastic_validation_table)
  elastic_validation_accuracy_mean <- mean(elastic_validation_accuracy)
  elastic_validation_F1_score[i] <- 2 * (elastic_validation_table[2, 2]) / sum(2 * elastic_validation_table[2, 2] + elastic_validation_table[1, 2] + elastic_validation_table[2, 1])
  elastic_validation_F1_score_mean <- mean(elastic_validation_F1_score)
  elastic_validation_positive_predictive_value[i] <- elastic_validation_table[2, 2] / sum(elastic_validation_table[2, 2] + elastic_validation_table[2, 1])
  elastic_validation_positive_predictive_value_mean <- mean(elastic_validation_positive_predictive_value)
  elastic_validation_negative_predictive_value[i] <- elastic_validation_table[1, 1] / sum(elastic_validation_table[1, 1] + elastic_validation_table[1, 2])
  elastic_validation_negative_predictive_value_mean <- mean(elastic_validation_negative_predictive_value)

  elastic_holdout_true_positive_rate[i] <- (elastic_test_true_positive_rate[i] + elastic_validation_true_positive_rate[i]) / 2
  elastic_holdout_true_positive_rate_mean <- mean(elastic_holdout_true_positive_rate)
  elastic_holdout_true_negative_rate[i] <- (elastic_test_true_negative_rate[i] + elastic_validation_true_negative_rate[i]) / 2
  elastic_holdout_true_negative_rate_mean <- mean(elastic_holdout_true_negative_rate)
  elastic_holdout_false_positive_rate[i] <- (elastic_test_false_positive_rate[i] + elastic_validation_false_positive_rate[i]) / 2
  elastic_holdout_false_positive_rate_mean <- mean(elastic_holdout_false_positive_rate)
  elastic_holdout_false_negative_rate[i] <- (elastic_test_false_negative_rate[i] + elastic_validation_false_negative_rate[i]) / 2
  elastic_holdout_false_negative_rate_mean <- mean(elastic_holdout_false_negative_rate)
  elastic_holdout_accuracy[i] <- (elastic_test_accuracy[i] + elastic_validation_accuracy[i]) / 2
  elastic_holdout_accuracy_mean <- mean(elastic_holdout_accuracy)
  elastic_holdout_accuracy_sd <- sd(elastic_holdout_accuracy)
  elastic_holdout_F1_score[i] <- (elastic_test_F1_score[i] + elastic_validation_F1_score[i]) / 2
  elastic_holdout_F1_score_mean <- mean(elastic_holdout_F1_score)
  elastic_holdout_positive_predictive_value[i] <- (elastic_test_positive_predictive_value[i] + elastic_validation_positive_predictive_value[i]) / 2
  elastic_holdout_positive_predictive_value_mean <- mean(elastic_holdout_positive_predictive_value)
  elastic_holdout_negative_predictive_value[i] <- (elastic_test_negative_predictive_value[i] + elastic_validation_negative_predictive_value[i]) / 2
  elastic_holdout_negative_predictive_value_mean <- mean(elastic_holdout_negative_predictive_value)
  elastic_holdout_overfitting[i] <- elastic_holdout_accuracy[i] / elastic_train_accuracy[i]
  elastic_holdout_overfitting_mean <- mean(elastic_holdout_overfitting)
  elastic_holdout_overfitting_range <- range(elastic_holdout_overfitting)
  elastic_holdout_overfitting_sd <- sd(elastic_holdout_overfitting)
  elastic_AUC[i] <- pROC::auc(c(test01$y, validation01$y), as.numeric(c(elastic_test_pred, elastic_validation_pred)) - 1)[1]
  elastic_AUC_mean <- mean(elastic_AUC)

  elastic_table <- elastic_test_table + elastic_validation_table
  elastic_table_total <- elastic_table_total + elastic_table

  elastic_end <- Sys.time()
  elastic_duration[i] <- elastic_end - elastic_start
  elastic_duration_mean <- mean(elastic_duration)
  elastic_duration_sd <- sd(elastic_duration)


  #### 05 Flexible Discriminant Analysis ####

  fda_start <- Sys.time()
  message("Working on Flexible Discriminant Analysis (FDA)")

  if(set_seed == "Y"){
    set.seed(seed = seed)
    fda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "FDAModel", family = binomial(link = "logit"))
  }
  if(set_seed == "N"){
    fda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "FDAModel", family = binomial(link = "logit"))
  }
  fda_train_pred <- as.numeric(as.character(stats::predict(fda_train_fit, train01, type = "response")))
  fda_train_predictions <- fda_train_pred[!is.na(fda_train_pred)]
  fda_train_predictions <- fda_train_predictions[!is.na(fda_train_predictions)]
  ifelse(sd(fda_train_predictions) == 0,
         fda_train_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         fda_train_table <- table(fda_train_predictions, y_train[1:length(fda_train_predictions)])
  )
  fda_train_true_positive_rate[i] <- fda_train_table[2, 2] / sum(fda_train_table[2, 2] + fda_train_table[1, 2])
  fda_train_true_positive_rate_mean <- mean(fda_train_true_positive_rate)
  fda_train_true_negative_rate[i] <- fda_train_table[1, 1] / sum(fda_train_table[1, 1] + fda_train_table[2, 1])
  fda_train_true_negative_rate_mean <- mean(fda_train_true_negative_rate)
  fda_train_false_positive_rate[i] <- fda_train_table[2, 1] / sum(fda_train_table[2, 1] + fda_train_table[1, 1])
  fda_train_false_positive_rate_mean <- mean(fda_train_false_positive_rate)
  fda_train_false_negative_rate[i] <- fda_train_table[1, 2] / sum(fda_train_table[1, 2] + fda_train_table[2, 2])
  fda_train_false_negative_rate_mean <- mean(fda_train_false_negative_rate)
  fda_train_accuracy[i] <- (fda_train_table[1, 1] + fda_train_table[2, 2]) / sum(fda_train_table)
  fda_train_accuracy_mean <- mean(fda_train_accuracy)
  fda_train_F1_score[i] <- 2 * (fda_train_table[2, 2]) / sum(2 * fda_train_table[2, 2] + fda_train_table[1, 2] + fda_train_table[2, 1])
  fda_train_F1_score_mean <- mean(fda_train_F1_score)
  fda_train_positive_predictive_value[i] <- fda_train_table[2, 2] / sum(fda_train_table[2, 2] + fda_train_table[2, 1])
  fda_train_positive_predictive_value_mean <- mean(fda_train_positive_predictive_value)
  fda_train_negative_predictive_value[i] <- fda_train_table[1, 1] / sum(fda_train_table[1, 1] + fda_train_table[1, 2])
  fda_train_negative_predictive_value_mean <- mean(fda_train_negative_predictive_value)

  fda_test_pred <- as.numeric(as.character(stats::predict(fda_train_fit, test01, type = "response")))
  fda_test_predictions <- fda_test_pred[!is.na(fda_test_pred)]
  ifelse(sd(fda_test_predictions) == 0,
         fda_test_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         fda_test_table <- table(fda_test_predictions, y_test[1:length(fda_test_predictions)])
  )
  fda_test_true_positive_rate[i] <- fda_test_table[2, 2] / sum(fda_test_table[2, 2] + fda_test_table[1, 2])
  fda_test_true_positive_rate_mean <- mean(fda_test_true_positive_rate)
  fda_test_true_negative_rate[i] <- fda_test_table[1, 1] / sum(fda_test_table[1, 1] + fda_test_table[2, 1])
  fda_test_true_negative_rate_mean <- mean(fda_test_true_negative_rate)
  fda_test_false_positive_rate[i] <- fda_test_table[2, 1] / sum(fda_test_table[2, 1] + fda_test_table[1, 1])
  fda_test_false_positive_rate_mean <- mean(fda_test_false_positive_rate)
  fda_test_false_negative_rate[i] <- fda_test_table[1, 2] / sum(fda_test_table[1, 2] + fda_test_table[2, 2])
  fda_test_false_negative_rate_mean <- mean(fda_test_false_negative_rate)
  fda_test_accuracy[i] <- (fda_test_table[1, 1] + fda_test_table[2, 2]) / sum(fda_test_table)
  fda_test_accuracy_mean <- mean(fda_test_accuracy)
  fda_test_F1_score[i] <- 2 * (fda_test_table[2, 2]) / sum(2 * fda_test_table[2, 2] + fda_test_table[1, 2] + fda_test_table[2, 1])
  fda_test_F1_score_mean <- mean(fda_test_F1_score)
  fda_test_positive_predictive_value[i] <- fda_test_table[2, 2] / sum(fda_test_table[2, 2] + fda_test_table[2, 1])
  fda_test_positive_predictive_value_mean <- mean(fda_test_positive_predictive_value)
  fda_test_negative_predictive_value[i] <- fda_test_table[1, 1] / sum(fda_test_table[1, 1] + fda_test_table[1, 2])
  fda_test_negative_predictive_value_mean <- mean(fda_test_negative_predictive_value)

  fda_validation_pred <- as.numeric(as.character(stats::predict(fda_train_fit, validation01, type = "response")))
  fda_validation_predictions <- fda_validation_pred[!is.na(fda_validation_pred)]
  ifelse(sd(fda_validation_predictions) == 0,
         fda_validation_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         fda_validation_table <- table(fda_validation_predictions, y_validation[1:length(fda_validation_predictions)])
  )
  fda_validation_true_positive_rate[i] <- fda_validation_table[2, 2] / sum(fda_validation_table[2, 2] + fda_validation_table[1, 2])
  fda_validation_true_positive_rate_mean <- mean(fda_validation_true_positive_rate)
  fda_validation_true_negative_rate[i] <- fda_validation_table[1, 1] / sum(fda_validation_table[1, 1] + fda_validation_table[2, 1])
  fda_validation_true_negative_rate_mean <- mean(fda_validation_true_negative_rate)
  fda_validation_false_positive_rate[i] <- fda_validation_table[2, 1] / sum(fda_validation_table[2, 1] + fda_validation_table[1, 1])
  fda_validation_false_positive_rate_mean <- mean(fda_validation_false_positive_rate)
  fda_validation_false_negative_rate[i] <- fda_validation_table[1, 2] / sum(fda_validation_table[1, 2] + fda_validation_table[2, 2])
  fda_validation_false_negative_rate_mean <- mean(fda_validation_false_negative_rate)
  fda_validation_accuracy[i] <- (fda_validation_table[1, 1] + fda_validation_table[2, 2]) / sum(fda_validation_table)
  fda_validation_accuracy_mean <- mean(fda_validation_accuracy)
  fda_validation_F1_score[i] <- 2 * (fda_validation_table[2, 2]) / sum(2 * fda_validation_table[2, 2] + fda_validation_table[1, 2] + fda_validation_table[2, 1])
  fda_validation_F1_score_mean <- mean(fda_validation_F1_score)
  fda_validation_positive_predictive_value[i] <- fda_validation_table[2, 2] / sum(fda_validation_table[2, 2] + fda_validation_table[2, 1])
  fda_validation_positive_predictive_value_mean <- mean(fda_validation_positive_predictive_value)
  fda_validation_negative_predictive_value[i] <- fda_validation_table[1, 1] / sum(fda_validation_table[1, 1] + fda_validation_table[1, 2])
  fda_validation_negative_predictive_value_mean <- mean(fda_validation_negative_predictive_value)

  fda_holdout_true_positive_rate[i] <- (fda_test_true_positive_rate[i] + fda_validation_true_positive_rate[i]) / 2
  fda_holdout_true_positive_rate_mean <- mean(fda_holdout_true_positive_rate)
  fda_holdout_true_negative_rate[i] <- (fda_test_true_negative_rate[i] + fda_validation_true_negative_rate[i]) / 2
  fda_holdout_true_negative_rate_mean <- mean(fda_holdout_true_negative_rate)
  fda_holdout_false_positive_rate[i] <- (fda_test_false_positive_rate[i] + fda_validation_false_positive_rate[i]) / 2
  fda_holdout_false_positive_rate_mean <- mean(fda_holdout_false_positive_rate)
  fda_holdout_false_negative_rate[i] <- (fda_test_false_negative_rate[i] + fda_validation_false_negative_rate[i]) / 2
  fda_holdout_false_negative_rate_mean <- mean(fda_holdout_false_negative_rate)
  fda_holdout_accuracy[i] <- (fda_test_accuracy[i] + fda_validation_accuracy[i]) / 2
  fda_holdout_accuracy_mean <- mean(fda_holdout_accuracy)
  fda_holdout_accuracy_sd <- sd(fda_holdout_accuracy)
  fda_holdout_F1_score[i] <- (fda_test_F1_score[i] + fda_validation_F1_score[i]) / 2
  fda_holdout_F1_score_mean <- mean(fda_holdout_F1_score)
  fda_holdout_positive_predictive_value[i] <- (fda_test_positive_predictive_value[i] + fda_validation_positive_predictive_value[i]) / 2
  fda_holdout_positive_predictive_value_mean <- mean(fda_holdout_positive_predictive_value)
  fda_holdout_negative_predictive_value[i] <- (fda_test_negative_predictive_value[i] + fda_validation_negative_predictive_value[i]) / 2
  fda_holdout_negative_predictive_value_mean <- mean(fda_holdout_negative_predictive_value)
  fda_holdout_overfitting[i] <- fda_holdout_accuracy[i] / fda_train_accuracy[i]
  fda_holdout_overfitting_mean <- mean(fda_holdout_overfitting)
  fda_holdout_overfitting_range <- range(fda_holdout_overfitting)
  fda_holdout_overfitting_sd <- sd(fda_holdout_overfitting)
  fda_AUC[i] <- pROC::auc(c(test01$y, validation01$y), as.numeric(c(fda_test_pred, fda_validation_pred)) - 1)[1]
  fda_AUC_mean <- mean(fda_AUC)

  fda_table <- fda_test_table + fda_validation_table
  fda_table_total <- fda_table_total + fda_table

  fda_end <- Sys.time()
  fda_duration[i] <- fda_end - fda_start
  fda_duration_mean <- mean(fda_duration)
  fda_duration_sd <- sd(fda_duration)


  #### 06 Generalized Additive Models ####
  gam_start <- Sys.time()
  message("Working on Generalized Additive Models (GAM)")

  if(set_seed == "Y"){
    set.seed(seed = seed)
    gam_train_fit <- gam::gam(y ~ ., data = train, family = binomial(link = "logit"))
  }
  if(set_seed == "N"){
    gam_train_fit <- gam::gam(y ~ ., data = train, family = binomial(link = "logit"))
  }
  gam_train_pred <- stats::predict(gam_train_fit, train01, type = "response")
  gam_train_predictions <- ifelse(gam_train_pred >positive_rate, 1, 0)
  gam_train_predictions <- gam_train_predictions[!is.na(gam_train_predictions)]
  ifelse(sd(gam_train_predictions) == 0,
         gam_train_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         gam_train_table <- table(gam_train_predictions, y_train[1:length(gam_train_predictions)])
  )
  gam_train_true_positive_rate[i] <- gam_train_table[2, 2] / sum(gam_train_table[2, 2] + gam_train_table[1, 2])
  gam_train_true_positive_rate_mean <- mean(gam_train_true_positive_rate)
  gam_train_true_negative_rate[i] <- gam_train_table[1, 1] / sum(gam_train_table[1, 1] + gam_train_table[2, 1])
  gam_train_true_negative_rate_mean <- mean(gam_train_true_negative_rate)
  gam_train_false_positive_rate[i] <- gam_train_table[2, 1] / sum(gam_train_table[2, 1] + gam_train_table[1, 1])
  gam_train_false_positive_rate_mean <- mean(gam_train_false_positive_rate)
  gam_train_false_negative_rate[i] <- gam_train_table[1, 2] / sum(gam_train_table[1, 2] + gam_train_table[2, 2])
  gam_train_false_negative_rate_mean <- mean(gam_train_false_negative_rate)
  gam_train_accuracy[i] <- (gam_train_table[1, 1] + gam_train_table[2, 2]) / sum(gam_train_table)
  gam_train_accuracy_mean <- mean(gam_train_accuracy)
  gam_train_F1_score[i] <- 2 * (gam_train_table[2, 2]) / sum(2 * gam_train_table[2, 2] + gam_train_table[1, 2] + gam_train_table[2, 1])
  gam_train_F1_score_mean <- mean(gam_train_F1_score)
  gam_train_positive_predictive_value[i] <- gam_train_table[2, 2] / sum(gam_train_table[2, 2] + gam_train_table[2, 1])
  gam_train_positive_predictive_value_mean <- mean(gam_train_positive_predictive_value)
  gam_train_negative_predictive_value[i] <- gam_train_table[1, 1] / sum(gam_train_table[1, 1] + gam_train_table[1, 2])
  gam_train_negative_predictive_value_mean <- mean(gam_train_negative_predictive_value)

  gam_test_pred <- stats::predict(gam_train_fit, test01, type = "response")
  gam_test_predictions <- ifelse(gam_test_pred > positive_rate, 1, 0)
  gam_test_predictions <- gam_test_predictions[!is.na(gam_test_predictions)]
  ifelse(sd(gam_test_predictions) == 0,
         gam_test_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         gam_test_table <- table(gam_test_predictions, y_test[1:length(gam_test_predictions)])
  )
  gam_test_true_positive_rate[i] <- gam_test_table[2, 2] / sum(gam_test_table[2, 2] + gam_test_table[1, 2])
  gam_test_true_positive_rate_mean <- mean(gam_test_true_positive_rate)
  gam_test_true_negative_rate[i] <- gam_test_table[1, 1] / sum(gam_test_table[1, 1] + gam_test_table[2, 1])
  gam_test_true_negative_rate_mean <- mean(gam_test_true_negative_rate)
  gam_test_false_positive_rate[i] <- gam_test_table[2, 1] / sum(gam_test_table[2, 1] + gam_test_table[1, 1])
  gam_test_false_positive_rate_mean <- mean(gam_test_false_positive_rate)
  gam_test_false_negative_rate[i] <- gam_test_table[1, 2] / sum(gam_test_table[1, 2] + gam_test_table[2, 2])
  gam_test_false_negative_rate_mean <- mean(gam_test_false_negative_rate)
  gam_test_accuracy[i] <- (gam_test_table[1, 1] + gam_test_table[2, 2]) / sum(gam_test_table)
  gam_test_accuracy_mean <- mean(gam_test_accuracy)
  gam_test_F1_score[i] <- 2 * (gam_test_table[2, 2]) / sum(2 * gam_test_table[2, 2] + gam_test_table[1, 2] + gam_test_table[2, 1])
  gam_test_F1_score_mean <- mean(gam_test_F1_score)
  gam_test_positive_predictive_value[i] <- gam_test_table[2, 2] / sum(gam_test_table[2, 2] + gam_test_table[2, 1])
  gam_test_positive_predictive_value_mean <- mean(gam_test_positive_predictive_value)
  gam_test_negative_predictive_value[i] <- gam_test_table[1, 1] / sum(gam_test_table[1, 1] + gam_test_table[1, 2])
  gam_test_negative_predictive_value_mean <- mean(gam_test_negative_predictive_value)

  gam_validation_pred <- stats::predict(gam_train_fit, validation01, type = "response")
  gam_validation_predictions <- ifelse(gam_validation_pred > positive_rate, 1, 0)
  gam_validation_predictions <- gam_validation_predictions[!is.na(gam_validation_predictions)]
  ifelse(sd(gam_validation_predictions) == 0,
         gam_validation_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         gam_validation_table <- table(gam_validation_predictions, y_validation[1:length(gam_validation_predictions)])
  )
  gam_validation_true_positive_rate[i] <- gam_validation_table[2, 2] / sum(gam_validation_table[2, 2] + gam_validation_table[1, 2])
  gam_validation_true_positive_rate_mean <- mean(gam_validation_true_positive_rate)
  gam_validation_true_negative_rate[i] <- gam_validation_table[1, 1] / sum(gam_validation_table[1, 1] + gam_validation_table[2, 1])
  gam_validation_true_negative_rate_mean <- mean(gam_validation_true_negative_rate)
  gam_validation_false_positive_rate[i] <- gam_validation_table[2, 1] / sum(gam_validation_table[2, 1] + gam_validation_table[1, 1])
  gam_validation_false_positive_rate_mean <- mean(gam_validation_false_positive_rate)
  gam_validation_false_negative_rate[i] <- gam_validation_table[1, 2] / sum(gam_validation_table[1, 2] + gam_validation_table[2, 2])
  gam_validation_false_negative_rate_mean <- mean(gam_validation_false_negative_rate)
  gam_validation_accuracy[i] <- (gam_validation_table[1, 1] + gam_validation_table[2, 2]) / sum(gam_validation_table)
  gam_validation_accuracy_mean <- mean(gam_validation_accuracy)
  gam_validation_F1_score[i] <- 2 * (gam_validation_table[2, 2]) / sum(2 * gam_validation_table[2, 2] + gam_validation_table[1, 2] + gam_validation_table[2, 1])
  gam_validation_F1_score_mean <- mean(gam_validation_F1_score)
  gam_validation_positive_predictive_value[i] <- gam_validation_table[2, 2] / sum(gam_validation_table[2, 2] + gam_validation_table[2, 1])
  gam_validation_positive_predictive_value_mean <- mean(gam_validation_positive_predictive_value)
  gam_validation_negative_predictive_value[i] <- gam_validation_table[1, 1] / sum(gam_validation_table[1, 1] + gam_validation_table[1, 2])
  gam_validation_negative_predictive_value_mean <- mean(gam_validation_negative_predictive_value)

  gam_holdout_true_positive_rate[i] <- (gam_test_true_positive_rate[i] + gam_validation_true_positive_rate[i]) / 2
  gam_holdout_true_positive_rate_mean <- mean(gam_holdout_true_positive_rate)
  gam_holdout_true_negative_rate[i] <- (gam_test_true_negative_rate[i] + gam_validation_true_negative_rate[i]) / 2
  gam_holdout_true_negative_rate_mean <- mean(gam_holdout_true_negative_rate)
  gam_holdout_false_positive_rate[i] <- (gam_test_false_positive_rate[i] + gam_validation_false_positive_rate[i]) / 2
  gam_holdout_false_positive_rate_mean <- mean(gam_holdout_false_positive_rate)
  gam_holdout_false_negative_rate[i] <- (gam_test_false_negative_rate[i] + gam_validation_false_negative_rate[i]) / 2
  gam_holdout_false_negative_rate_mean <- mean(gam_holdout_false_negative_rate)
  gam_holdout_accuracy[i] <- (gam_test_accuracy[i] + gam_validation_accuracy[i]) / 2
  gam_holdout_accuracy_mean <- mean(gam_holdout_accuracy)
  gam_holdout_accuracy_sd <- sd(gam_holdout_accuracy)
  gam_holdout_F1_score[i] <- (gam_test_F1_score[i] + gam_validation_F1_score[i]) / 2
  gam_holdout_F1_score_mean <- mean(gam_holdout_F1_score)
  gam_holdout_positive_predictive_value[i] <- (gam_test_positive_predictive_value[i] + gam_validation_positive_predictive_value[i]) / 2
  gam_holdout_positive_predictive_value_mean <- mean(gam_holdout_positive_predictive_value)
  gam_holdout_negative_predictive_value[i] <- (gam_test_negative_predictive_value[i] + gam_validation_negative_predictive_value[i]) / 2
  gam_holdout_negative_predictive_value_mean <- mean(gam_holdout_negative_predictive_value)
  gam_holdout_overfitting[i] <- gam_holdout_accuracy[i] / gam_train_accuracy[i]
  gam_holdout_overfitting_mean <- mean(gam_holdout_overfitting)
  gam_holdout_overfitting_range <- range(gam_holdout_overfitting)
  gam_holdout_overfitting_sd <- sd(gam_holdout_overfitting)
  gam_AUC[i] <- pROC::auc(c(test01$y, validation01$y), as.numeric(c(gam_test_pred, gam_validation_pred)) - 1)[1]
  gam_AUC_mean <- mean(gam_AUC)

  gam_table <- gam_test_table + gam_validation_table
  gam_table_total <- gam_table_total + gam_table

  gam_end <- Sys.time()
  gam_duration[i] <- gam_end - gam_start
  gam_duration_mean <- mean(gam_duration)
  gam_duration_sd <- sd(gam_duration)


  #### Gradient Boosted #####
  gb_start <- Sys.time()
  message("Working on Gradient Boosted")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    gb_train_fit <- gbm::gbm(train$y ~ ., data = train, distribution = "bernoulli")
  }
  if(set_seed == "N"){
    gb_train_fit <- gbm::gbm(train$y ~ ., data = train, distribution = "bernoulli")
  }
  gb_train_pred <- stats::predict(gb_train_fit, train01, type = "response")
  gb_train_predictions <- ifelse(gb_train_pred >positive_rate, 1, 0)
  gb_train_predictions <- gb_train_predictions[!is.na(gb_train_predictions)]
  ifelse(sd(gb_train_predictions) == 0,
         gb_train_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         gb_train_table <- table(gb_train_predictions, y_train[1:length(gb_train_predictions)])
  )
  gb_train_true_positive_rate[i] <- gb_train_table[2, 2] / sum(gb_train_table[2, 2] + gb_train_table[1, 2])
  gb_train_true_positive_rate_mean <- mean(gb_train_true_positive_rate)
  gb_train_true_negative_rate[i] <- gb_train_table[1, 1] / sum(gb_train_table[1, 1] + gb_train_table[2, 1])
  gb_train_true_negative_rate_mean <- mean(gb_train_true_negative_rate)
  gb_train_false_positive_rate[i] <- gb_train_table[2, 1] / sum(gb_train_table[2, 1] + gb_train_table[1, 1])
  gb_train_false_positive_rate_mean <- mean(gb_train_false_positive_rate)
  gb_train_false_negative_rate[i] <- gb_train_table[1, 2] / sum(gb_train_table[1, 2] + gb_train_table[2, 2])
  gb_train_false_negative_rate_mean <- mean(gb_train_false_negative_rate)
  gb_train_accuracy[i] <- (gb_train_table[1, 1] + gb_train_table[2, 2]) / sum(gb_train_table)
  gb_train_accuracy_mean <- mean(gb_train_accuracy)
  gb_train_F1_score[i] <- 2 * (gb_train_table[2, 2]) / sum(2 * gb_train_table[2, 2] + gb_train_table[1, 2] + gb_train_table[2, 1])
  gb_train_F1_score_mean <- mean(gb_train_F1_score)
  gb_train_positive_predictive_value[i] <- gb_train_table[2, 2] / sum(gb_train_table[2, 2] + gb_train_table[2, 1])
  gb_train_positive_predictive_value_mean <- mean(gb_train_positive_predictive_value)
  gb_train_negative_predictive_value[i] <- gb_train_table[1, 1] / sum(gb_train_table[1, 1] + gb_train_table[1, 2])
  gb_train_negative_predictive_value_mean <- mean(gb_train_negative_predictive_value)

  gb_test_pred <- stats::predict(gb_train_fit, test01, type = "response")
  gb_test_predictions <- ifelse(gb_test_pred > positive_rate, 1, 0)
  gb_test_predictions <- gb_test_predictions[!is.na(gb_test_predictions)]
  ifelse(sd(gb_test_predictions) == 0,
         gb_test_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         gb_test_table <- table(gb_test_predictions, y_test[1:length(gb_test_predictions)])
  )
  gb_test_true_positive_rate[i] <- gb_test_table[2, 2] / sum(gb_test_table[2, 2] + gb_test_table[1, 2])
  gb_test_true_positive_rate_mean <- mean(gb_test_true_positive_rate)
  gb_test_true_negative_rate[i] <- gb_test_table[1, 1] / sum(gb_test_table[1, 1] + gb_test_table[2, 1])
  gb_test_true_negative_rate_mean <- mean(gb_test_true_negative_rate)
  gb_test_false_positive_rate[i] <- gb_test_table[2, 1] / sum(gb_test_table[2, 1] + gb_test_table[1, 1])
  gb_test_false_positive_rate_mean <- mean(gb_test_false_positive_rate)
  gb_test_false_negative_rate[i] <- gb_test_table[1, 2] / sum(gb_test_table[1, 2] + gb_test_table[2, 2])
  gb_test_false_negative_rate_mean <- mean(gb_test_false_negative_rate)
  gb_test_accuracy[i] <- (gb_test_table[1, 1] + gb_test_table[2, 2]) / sum(gb_test_table)
  gb_test_accuracy_mean <- mean(gb_test_accuracy)
  gb_test_F1_score[i] <- 2 * (gb_test_table[2, 2]) / sum(2 * gb_test_table[2, 2] + gb_test_table[1, 2] + gb_test_table[2, 1])
  gb_test_F1_score_mean <- mean(gb_test_F1_score)
  gb_test_positive_predictive_value[i] <- gb_test_table[2, 2] / sum(gb_test_table[2, 2] + gb_test_table[2, 1])
  gb_test_positive_predictive_value_mean <- mean(gb_test_positive_predictive_value)
  gb_test_negative_predictive_value[i] <- gb_test_table[1, 1] / sum(gb_test_table[1, 1] + gb_test_table[1, 2])
  gb_test_negative_predictive_value_mean <- mean(gb_test_negative_predictive_value)

  gb_validation_pred <- stats::predict(gb_train_fit, validation01, type = "response")
  gb_validation_predictions <- ifelse(gb_validation_pred > positive_rate, 1, 0)
  gb_validation_predictions <- gb_validation_predictions[!is.na(gb_validation_predictions)]
  ifelse(sd(gb_validation_predictions) == 0,
         gb_validation_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         gb_validation_table <- table(gb_validation_predictions, y_validation[1:length(gb_validation_predictions)])
  )
  gb_validation_true_positive_rate[i] <- gb_validation_table[2, 2] / sum(gb_validation_table[2, 2] + gb_validation_table[1, 2])
  gb_validation_true_positive_rate_mean <- mean(gb_validation_true_positive_rate)
  gb_validation_true_negative_rate[i] <- gb_validation_table[1, 1] / sum(gb_validation_table[1, 1] + gb_validation_table[2, 1])
  gb_validation_true_negative_rate_mean <- mean(gb_validation_true_negative_rate)
  gb_validation_false_positive_rate[i] <- gb_validation_table[2, 1] / sum(gb_validation_table[2, 1] + gb_validation_table[1, 1])
  gb_validation_false_positive_rate_mean <- mean(gb_validation_false_positive_rate)
  gb_validation_false_negative_rate[i] <- gb_validation_table[1, 2] / sum(gb_validation_table[1, 2] + gb_validation_table[2, 2])
  gb_validation_false_negative_rate_mean <- mean(gb_validation_false_negative_rate)
  gb_validation_accuracy[i] <- (gb_validation_table[1, 1] + gb_validation_table[2, 2]) / sum(gb_validation_table)
  gb_validation_accuracy_mean <- mean(gb_validation_accuracy)
  gb_validation_F1_score[i] <- 2 * (gb_validation_table[2, 2]) / sum(2 * gb_validation_table[2, 2] + gb_validation_table[1, 2] + gb_validation_table[2, 1])
  gb_validation_F1_score_mean <- mean(gb_validation_F1_score)
  gb_validation_positive_predictive_value[i] <- gb_validation_table[2, 2] / sum(gb_validation_table[2, 2] + gb_validation_table[2, 1])
  gb_validation_positive_predictive_value_mean <- mean(gb_validation_positive_predictive_value)
  gb_validation_negative_predictive_value[i] <- gb_validation_table[1, 1] / sum(gb_validation_table[1, 1] + gb_validation_table[1, 2])
  gb_validation_negative_predictive_value_mean <- mean(gb_validation_negative_predictive_value)

  gb_holdout_true_positive_rate[i] <- (gb_test_true_positive_rate[i] + gb_validation_true_positive_rate[i]) / 2
  gb_holdout_true_positive_rate_mean <- mean(gb_holdout_true_positive_rate)
  gb_holdout_true_negative_rate[i] <- (gb_test_true_negative_rate[i] + gb_validation_true_negative_rate[i]) / 2
  gb_holdout_true_negative_rate_mean <- mean(gb_holdout_true_negative_rate)
  gb_holdout_false_positive_rate[i] <- (gb_test_false_positive_rate[i] + gb_validation_false_positive_rate[i]) / 2
  gb_holdout_false_positive_rate_mean <- mean(gb_holdout_false_positive_rate)
  gb_holdout_false_negative_rate[i] <- (gb_test_false_negative_rate[i] + gb_validation_false_negative_rate[i]) / 2
  gb_holdout_false_negative_rate_mean <- mean(gb_holdout_false_negative_rate)
  gb_holdout_accuracy[i] <- (gb_test_accuracy[i] + gb_validation_accuracy[i]) / 2
  gb_holdout_accuracy_mean <- mean(gb_holdout_accuracy)
  gb_holdout_accuracy_sd <- sd(gb_holdout_accuracy)
  gb_holdout_F1_score[i] <- (gb_test_F1_score[i] + gb_validation_F1_score[i]) / 2
  gb_holdout_F1_score_mean <- mean(gb_holdout_F1_score)
  gb_holdout_positive_predictive_value[i] <- (gb_test_positive_predictive_value[i] + gb_validation_positive_predictive_value[i]) / 2
  gb_holdout_positive_predictive_value_mean <- mean(gb_holdout_positive_predictive_value)
  gb_holdout_negative_predictive_value[i] <- (gb_test_negative_predictive_value[i] + gb_validation_negative_predictive_value[i]) / 2
  gb_holdout_negative_predictive_value_mean <- mean(gb_holdout_negative_predictive_value)
  gb_holdout_overfitting[i] <- gb_holdout_accuracy[i] / gb_train_accuracy[i]
  gb_holdout_overfitting_mean <- mean(gb_holdout_overfitting)
  gb_holdout_overfitting_range <- range(gb_holdout_overfitting)
  gb_holdout_overfitting_sd <- sd(gb_holdout_overfitting)
  gb_AUC[i] <- pROC::auc(c(test01$y, validation01$y), as.numeric(c(gb_test_pred, gb_validation_pred)) - 1)[1]
  gb_AUC_mean <- mean(gb_AUC)

  gb_table <- gb_test_table + gb_validation_table
  gb_table_total <- gb_table_total + gb_table

  gb_end <- Sys.time()
  gb_duration[i] <- gb_end - gb_start
  gb_duration_mean <- mean(gb_duration)
  gb_duration_sd <- sd(gb_duration)


  #### 07 Generalized Linear Models Using Glmnet ####
  glmnet_start <- Sys.time()
  message("Working on glmnet")
  y <- train$y
  x <- data.matrix(train %>% dplyr::select(-y))
  glmnet_model <- glmnet::glmnet(x, y, alpha = 0.5)
  glmnet_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_glmnet_lambda <- glmnet_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    best_glmnet_model <- glmnet::glmnet(x, y, alpha = 0.5, family = "binomial")
  }
  if(set_seed == "N"){
    best_glmnet_model <- glmnet::glmnet(x, y, alpha = 0.5, family = "binomial")
  }
  glmnet_train_pred <- predict(best_glmnet_model, s = best_glmnet_lambda, newx = data.matrix(train[, 1:ncol(train)-1]))
  glmnet_train_predictions <- ifelse(glmnet_train_pred > positive_rate, 1, 0)
  glmnet_train_predictions <- glmnet_train_predictions[!is.na(glmnet_train_predictions)]
  ifelse(sd(glmnet_train_predictions) == 0,
         glmnet_train_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         glmnet_train_table <- table(glmnet_train_predictions, y_train[1:length(glmnet_train_predictions)])
  )
  glmnet_train_true_positive_rate[i] <- glmnet_train_table[2, 2] / sum(glmnet_train_table[2, 2] + glmnet_train_table[1, 2])
  glmnet_train_true_positive_rate_mean <- mean(glmnet_train_true_positive_rate)
  glmnet_train_true_negative_rate[i] <- glmnet_train_table[1, 1] / sum(glmnet_train_table[1, 1] + glmnet_train_table[2, 1])
  glmnet_train_true_negative_rate_mean <- mean(glmnet_train_true_negative_rate)
  glmnet_train_false_positive_rate[i] <- glmnet_train_table[2, 1] / sum(glmnet_train_table[2, 1] + glmnet_train_table[1, 1])
  glmnet_train_false_positive_rate_mean <- mean(glmnet_train_false_positive_rate)
  glmnet_train_false_negative_rate[i] <- glmnet_train_table[1, 2] / sum(glmnet_train_table[1, 2] + glmnet_train_table[2, 2])
  glmnet_train_false_negative_rate_mean <- mean(glmnet_train_false_negative_rate)
  glmnet_train_accuracy[i] <- (glmnet_train_table[1, 1] + glmnet_train_table[2, 2]) / sum(glmnet_train_table)
  glmnet_train_accuracy_mean <- mean(glmnet_train_accuracy)
  glmnet_train_F1_score[i] <- 2 * (glmnet_train_table[2, 2]) / sum(2 * glmnet_train_table[2, 2] + glmnet_train_table[1, 2] + glmnet_train_table[2, 1])
  glmnet_train_F1_score_mean <- mean(glmnet_train_F1_score)
  glmnet_train_positive_predictive_value[i] <- glmnet_train_table[2, 2] / sum(glmnet_train_table[2, 2] + glmnet_train_table[2, 1])
  glmnet_train_positive_predictive_value_mean <- mean(glmnet_train_positive_predictive_value)
  glmnet_train_negative_predictive_value[i] <- glmnet_train_table[1, 1] / sum(glmnet_train_table[1, 1] + glmnet_train_table[1, 2])
  glmnet_train_negative_predictive_value_mean <- mean(glmnet_train_negative_predictive_value)

  glmnet_test_pred <- predict(best_glmnet_model, s = best_glmnet_lambda, newx = data.matrix(test[, 1:ncol(test)-1]))
  glmnet_test_predictions <- ifelse(glmnet_test_pred > positive_rate, 1, 0)
  glmnet_test_predictions <- glmnet_test_predictions[!is.na(glmnet_test_predictions)]
  ifelse(sd(glmnet_test_predictions) == 0,
         glmnet_test_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         glmnet_test_table <- table(glmnet_test_predictions, y_test[1:length(glmnet_test_predictions)])
  )
  glmnet_test_true_positive_rate[i] <- glmnet_test_table[2, 2] / sum(glmnet_test_table[2, 2] + glmnet_test_table[1, 2])
  glmnet_test_true_positive_rate_mean <- mean(glmnet_test_true_positive_rate)
  glmnet_test_true_negative_rate[i] <- glmnet_test_table[1, 1] / sum(glmnet_test_table[1, 1] + glmnet_test_table[2, 1])
  glmnet_test_true_negative_rate_mean <- mean(glmnet_test_true_negative_rate)
  glmnet_test_false_positive_rate[i] <- glmnet_test_table[2, 1] / sum(glmnet_test_table[2, 1] + glmnet_test_table[1, 1])
  glmnet_test_false_positive_rate_mean <- mean(glmnet_test_false_positive_rate)
  glmnet_test_false_negative_rate[i] <- glmnet_test_table[1, 2] / sum(glmnet_test_table[1, 2] + glmnet_test_table[2, 2])
  glmnet_test_false_negative_rate_mean <- mean(glmnet_test_false_negative_rate)
  glmnet_test_accuracy[i] <- (glmnet_test_table[1, 1] + glmnet_test_table[2, 2]) / sum(glmnet_test_table)
  glmnet_test_accuracy_mean <- mean(glmnet_test_accuracy)
  glmnet_test_F1_score[i] <- 2 * (glmnet_test_table[2, 2]) / sum(2 * glmnet_test_table[2, 2] + glmnet_test_table[1, 2] + glmnet_test_table[2, 1])
  glmnet_test_F1_score_mean <- mean(glmnet_test_F1_score)
  glmnet_test_positive_predictive_value[i] <- glmnet_test_table[2, 2] / sum(glmnet_test_table[2, 2] + glmnet_test_table[2, 1])
  glmnet_test_positive_predictive_value_mean <- mean(glmnet_test_positive_predictive_value)
  glmnet_test_negative_predictive_value[i] <- glmnet_test_table[1, 1] / sum(glmnet_test_table[1, 1] + glmnet_test_table[1, 2])
  glmnet_test_negative_predictive_value_mean <- mean(glmnet_test_negative_predictive_value)

  glmnet_validation_pred <- predict(best_glmnet_model, s = best_glmnet_lambda, newx = data.matrix(validation[, 1:ncol(validation)-1]))
  glmnet_validation_predictions <- ifelse(glmnet_validation_pred > positive_rate, 1, 0)
  glmnet_validation_predictions <- glmnet_validation_predictions[!is.na(glmnet_validation_predictions)]
  ifelse(sd(glmnet_validation_predictions) == 0,
         glmnet_validation_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         glmnet_validation_table <- table(glmnet_validation_predictions, y_validation[1:length(glmnet_validation_predictions)])
  )
  glmnet_validation_true_positive_rate[i] <- glmnet_validation_table[2, 2] / sum(glmnet_validation_table[2, 2] + glmnet_validation_table[1, 2])
  glmnet_validation_true_positive_rate_mean <- mean(glmnet_validation_true_positive_rate)
  glmnet_validation_true_negative_rate[i] <- glmnet_validation_table[1, 1] / sum(glmnet_validation_table[1, 1] + glmnet_validation_table[2, 1])
  glmnet_validation_true_negative_rate_mean <- mean(glmnet_validation_true_negative_rate)
  glmnet_validation_false_positive_rate[i] <- glmnet_validation_table[2, 1] / sum(glmnet_validation_table[2, 1] + glmnet_validation_table[1, 1])
  glmnet_validation_false_positive_rate_mean <- mean(glmnet_validation_false_positive_rate)
  glmnet_validation_false_negative_rate[i] <- glmnet_validation_table[1, 2] / sum(glmnet_validation_table[1, 2] + glmnet_validation_table[2, 2])
  glmnet_validation_false_negative_rate_mean <- mean(glmnet_validation_false_negative_rate)
  glmnet_validation_accuracy[i] <- (glmnet_validation_table[1, 1] + glmnet_validation_table[2, 2]) / sum(glmnet_validation_table)
  glmnet_validation_accuracy_mean <- mean(glmnet_validation_accuracy)
  glmnet_validation_F1_score[i] <- 2 * (glmnet_validation_table[2, 2]) / sum(2 * glmnet_validation_table[2, 2] + glmnet_validation_table[1, 2] + glmnet_validation_table[2, 1])
  glmnet_validation_F1_score_mean <- mean(glmnet_validation_F1_score)
  glmnet_validation_positive_predictive_value[i] <- glmnet_validation_table[2, 2] / sum(glmnet_validation_table[2, 2] + glmnet_validation_table[2, 1])
  glmnet_validation_positive_predictive_value_mean <- mean(glmnet_validation_positive_predictive_value)
  glmnet_validation_negative_predictive_value[i] <- glmnet_validation_table[1, 1] / sum(glmnet_validation_table[1, 1] + glmnet_validation_table[1, 2])
  glmnet_validation_negative_predictive_value_mean <- mean(glmnet_validation_negative_predictive_value)

  glmnet_holdout_true_positive_rate[i] <- (glmnet_test_true_positive_rate[i] + glmnet_validation_true_positive_rate[i]) / 2
  glmnet_holdout_true_positive_rate_mean <- mean(glmnet_holdout_true_positive_rate)
  glmnet_holdout_true_negative_rate[i] <- (glmnet_test_true_negative_rate[i] + glmnet_validation_true_negative_rate[i]) / 2
  glmnet_holdout_true_negative_rate_mean <- mean(glmnet_holdout_true_negative_rate)
  glmnet_holdout_false_positive_rate[i] <- (glmnet_test_false_positive_rate[i] + glmnet_validation_false_positive_rate[i]) / 2
  glmnet_holdout_false_positive_rate_mean <- mean(glmnet_holdout_false_positive_rate)
  glmnet_holdout_false_negative_rate[i] <- (glmnet_test_false_negative_rate[i] + glmnet_validation_false_negative_rate[i]) / 2
  glmnet_holdout_false_negative_rate_mean <- mean(glmnet_holdout_false_negative_rate)
  glmnet_holdout_accuracy[i] <- (glmnet_test_accuracy[i] + glmnet_validation_accuracy[i]) / 2
  glmnet_holdout_accuracy_mean <- mean(glmnet_holdout_accuracy)
  glmnet_holdout_accuracy_sd <- sd(glmnet_holdout_accuracy)
  glmnet_holdout_F1_score[i] <- (glmnet_test_F1_score[i] + glmnet_validation_F1_score[i]) / 2
  glmnet_holdout_F1_score_mean <- mean(glmnet_holdout_F1_score)
  glmnet_holdout_positive_predictive_value[i] <- (glmnet_test_positive_predictive_value[i] + glmnet_validation_positive_predictive_value[i]) / 2
  glmnet_holdout_positive_predictive_value_mean <- mean(glmnet_holdout_positive_predictive_value)
  glmnet_holdout_negative_predictive_value[i] <- (glmnet_test_negative_predictive_value[i] + glmnet_validation_negative_predictive_value[i]) / 2
  glmnet_holdout_negative_predictive_value_mean <- mean(glmnet_holdout_negative_predictive_value)
  glmnet_holdout_overfitting[i] <- glmnet_holdout_accuracy[i] / glmnet_train_accuracy[i]
  glmnet_holdout_overfitting_mean <- mean(glmnet_holdout_overfitting)
  glmnet_holdout_overfitting_range <- range(glmnet_holdout_overfitting)
  glmnet_holdout_overfitting_sd <- sd(glmnet_holdout_overfitting)
  glmnet_AUC[i] <- pROC::auc(c(test01$y, validation01$y), as.numeric(c(glmnet_test_pred, glmnet_validation_pred)) - 1)[1]
  glmnet_AUC_mean <- mean(glmnet_AUC)

  glmnet_table <- glmnet_test_table + glmnet_validation_table
  glmnet_table_total <- glmnet_table_total + glmnet_table

  glmnet_end <- Sys.time()
  glmnet_duration[i] <- glmnet_end - glmnet_start
  glmnet_duration_mean <- mean(glmnet_duration)
  glmnet_duration_sd <- sd(glmnet_duration)

  #### Neuralnet ####
  neuralnet_start <- Sys.time()
  message("Working on Neuralnet")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    neuralnet_train_fit <- nnet::nnet(train$y ~ ., data = train, size = 0, linout = TRUE, skip = TRUE, family = binomial(link = "logit"))
  }
  if(set_seed == "N"){
    neuralnet_train_fit <- nnet::nnet(train$y ~ ., data = train, size = 0, linout = TRUE, skip = TRUE, family = binomial(link = "logit"))
  }
  neuralnet_train_pred <- as.numeric(stats::predict(neuralnet_train_fit, train, type = "raw"))
  neuralnet_train_predictions <- ifelse(neuralnet_train_pred > positive_rate, 1, 0)
  neuralnet_train_predictions <- neuralnet_train_predictions[!is.na(neuralnet_train_predictions)]
  ifelse(sd(neuralnet_train_predictions) == 0,
         neuralnet_train_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         neuralnet_train_table <- table(neuralnet_train_predictions, y_train[1:length(neuralnet_train_predictions)])
  )
  neuralnet_train_true_positive_rate[i] <- neuralnet_train_table[2, 2] / sum(neuralnet_train_table[2, 2] + neuralnet_train_table[1, 2])
  neuralnet_train_true_positive_rate_mean <- mean(neuralnet_train_true_positive_rate)
  neuralnet_train_true_negative_rate[i] <- neuralnet_train_table[1, 1] / sum(neuralnet_train_table[1, 1] + neuralnet_train_table[2, 1])
  neuralnet_train_true_negative_rate_mean <- mean(neuralnet_train_true_negative_rate)
  neuralnet_train_false_positive_rate[i] <- neuralnet_train_table[2, 1] / sum(neuralnet_train_table[2, 1] + neuralnet_train_table[1, 1])
  neuralnet_train_false_positive_rate_mean <- mean(neuralnet_train_false_positive_rate)
  neuralnet_train_false_negative_rate[i] <- neuralnet_train_table[1, 2] / sum(neuralnet_train_table[1, 2] + neuralnet_train_table[2, 2])
  neuralnet_train_false_negative_rate_mean <- mean(neuralnet_train_false_negative_rate)
  neuralnet_train_accuracy[i] <- (neuralnet_train_table[1, 1] + neuralnet_train_table[2, 2]) / sum(neuralnet_train_table)
  neuralnet_train_accuracy_mean <- mean(neuralnet_train_accuracy)
  neuralnet_train_F1_score[i] <- 2 * (neuralnet_train_table[2, 2]) / sum(2 * neuralnet_train_table[2, 2] + neuralnet_train_table[1, 2] + neuralnet_train_table[2, 1])
  neuralnet_train_F1_score_mean <- mean(neuralnet_train_F1_score)
  neuralnet_train_positive_predictive_value[i] <- neuralnet_train_table[2, 2] / sum(neuralnet_train_table[2, 2] + neuralnet_train_table[2, 1])
  neuralnet_train_positive_predictive_value_mean <- mean(neuralnet_train_positive_predictive_value)
  neuralnet_train_negative_predictive_value[i] <- neuralnet_train_table[1, 1] / sum(neuralnet_train_table[1, 1] + neuralnet_train_table[1, 2])
  neuralnet_train_negative_predictive_value_mean <- mean(neuralnet_train_negative_predictive_value)

  neuralnet_test_pred <- as.numeric(stats::predict(neuralnet_train_fit, test, type = "raw"))
  neuralnet_test_predictions <- ifelse(neuralnet_test_pred > positive_rate, 1, 0)
  neuralnet_test_predictions <- neuralnet_test_predictions[!is.na(neuralnet_test_predictions)]
  ifelse(sd(neuralnet_test_predictions) == 0,
         neuralnet_test_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         neuralnet_test_table <- table(neuralnet_test_predictions, y_test[1:length(neuralnet_test_predictions)])
  )
  neuralnet_test_true_positive_rate[i] <- neuralnet_test_table[2, 2] / sum(neuralnet_test_table[2, 2] + neuralnet_test_table[1, 2])
  neuralnet_test_true_positive_rate_mean <- mean(neuralnet_test_true_positive_rate)
  neuralnet_test_true_negative_rate[i] <- neuralnet_test_table[1, 1] / sum(neuralnet_test_table[1, 1] + neuralnet_test_table[2, 1])
  neuralnet_test_true_negative_rate_mean <- mean(neuralnet_test_true_negative_rate)
  neuralnet_test_false_positive_rate[i] <- neuralnet_test_table[2, 1] / sum(neuralnet_test_table[2, 1] + neuralnet_test_table[1, 1])
  neuralnet_test_false_positive_rate_mean <- mean(neuralnet_test_false_positive_rate)
  neuralnet_test_false_negative_rate[i] <- neuralnet_test_table[1, 2] / sum(neuralnet_test_table[1, 2] + neuralnet_test_table[2, 2])
  neuralnet_test_false_negative_rate_mean <- mean(neuralnet_test_false_negative_rate)
  neuralnet_test_accuracy[i] <- (neuralnet_test_table[1, 1] + neuralnet_test_table[2, 2]) / sum(neuralnet_test_table)
  neuralnet_test_accuracy_mean <- mean(neuralnet_test_accuracy)
  neuralnet_test_F1_score[i] <- 2 * (neuralnet_test_table[2, 2]) / sum(2 * neuralnet_test_table[2, 2] + neuralnet_test_table[1, 2] + neuralnet_test_table[2, 1])
  neuralnet_test_F1_score_mean <- mean(neuralnet_test_F1_score)
  neuralnet_test_positive_predictive_value[i] <- neuralnet_test_table[2, 2] / sum(neuralnet_test_table[2, 2] + neuralnet_test_table[2, 1])
  neuralnet_test_positive_predictive_value_mean <- mean(neuralnet_test_positive_predictive_value)
  neuralnet_test_negative_predictive_value[i] <- neuralnet_test_table[1, 1] / sum(neuralnet_test_table[1, 1] + neuralnet_test_table[1, 2])
  neuralnet_test_negative_predictive_value_mean <- mean(neuralnet_test_negative_predictive_value)

  neuralnet_validation_pred <- as.numeric(stats::predict(neuralnet_train_fit, validation, type = "raw"))
  neuralnet_validation_predictions <- ifelse(neuralnet_validation_pred > positive_rate, 1, 0)
  neuralnet_validation_predictions <- neuralnet_validation_predictions[!is.na(neuralnet_validation_predictions)]
  ifelse(sd(neuralnet_validation_predictions) == 0,
         neuralnet_validation_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         neuralnet_validation_table <- table(neuralnet_validation_predictions, y_validation[1:length(neuralnet_validation_predictions)])
  )
  neuralnet_validation_true_positive_rate[i] <- neuralnet_validation_table[2, 2] / sum(neuralnet_validation_table[2, 2] + neuralnet_validation_table[1, 2])
  neuralnet_validation_true_positive_rate_mean <- mean(neuralnet_validation_true_positive_rate)
  neuralnet_validation_true_negative_rate[i] <- neuralnet_validation_table[1, 1] / sum(neuralnet_validation_table[1, 1] + neuralnet_validation_table[2, 1])
  neuralnet_validation_true_negative_rate_mean <- mean(neuralnet_validation_true_negative_rate)
  neuralnet_validation_false_positive_rate[i] <- neuralnet_validation_table[2, 1] / sum(neuralnet_validation_table[2, 1] + neuralnet_validation_table[1, 1])
  neuralnet_validation_false_positive_rate_mean <- mean(neuralnet_validation_false_positive_rate)
  neuralnet_validation_false_negative_rate[i] <- neuralnet_validation_table[1, 2] / sum(neuralnet_validation_table[1, 2] + neuralnet_validation_table[2, 2])
  neuralnet_validation_false_negative_rate_mean <- mean(neuralnet_validation_false_negative_rate)
  neuralnet_validation_accuracy[i] <- (neuralnet_validation_table[1, 1] + neuralnet_validation_table[2, 2]) / sum(neuralnet_validation_table)
  neuralnet_validation_accuracy_mean <- mean(neuralnet_validation_accuracy)
  neuralnet_validation_F1_score[i] <- 2 * (neuralnet_validation_table[2, 2]) / sum(2 * neuralnet_validation_table[2, 2] + neuralnet_validation_table[1, 2] + neuralnet_validation_table[2, 1])
  neuralnet_validation_F1_score_mean <- mean(neuralnet_validation_F1_score)
  neuralnet_validation_positive_predictive_value[i] <- neuralnet_validation_table[2, 2] / sum(neuralnet_validation_table[2, 2] + neuralnet_validation_table[2, 1])
  neuralnet_validation_positive_predictive_value_mean <- mean(neuralnet_validation_positive_predictive_value)
  neuralnet_validation_negative_predictive_value[i] <- neuralnet_validation_table[1, 1] / sum(neuralnet_validation_table[1, 1] + neuralnet_validation_table[1, 2])
  neuralnet_validation_negative_predictive_value_mean <- mean(neuralnet_validation_negative_predictive_value)

  neuralnet_holdout_true_positive_rate[i] <- (neuralnet_test_true_positive_rate[i] + neuralnet_validation_true_positive_rate[i]) / 2
  neuralnet_holdout_true_positive_rate_mean <- mean(neuralnet_holdout_true_positive_rate)
  neuralnet_holdout_true_negative_rate[i] <- (neuralnet_test_true_negative_rate[i] + neuralnet_validation_true_negative_rate[i]) / 2
  neuralnet_holdout_true_negative_rate_mean <- mean(neuralnet_holdout_true_negative_rate)
  neuralnet_holdout_false_positive_rate[i] <- (neuralnet_test_false_positive_rate[i] + neuralnet_validation_false_positive_rate[i]) / 2
  neuralnet_holdout_false_positive_rate_mean <- mean(neuralnet_holdout_false_positive_rate)
  neuralnet_holdout_false_negative_rate[i] <- (neuralnet_test_false_negative_rate[i] + neuralnet_validation_false_negative_rate[i]) / 2
  neuralnet_holdout_false_negative_rate_mean <- mean(neuralnet_holdout_false_negative_rate)
  neuralnet_holdout_accuracy[i] <- (neuralnet_test_accuracy[i] + neuralnet_validation_accuracy[i]) / 2
  neuralnet_holdout_accuracy_mean <- mean(neuralnet_holdout_accuracy)
  neuralnet_holdout_accuracy_sd <- sd(neuralnet_holdout_accuracy)
  neuralnet_holdout_F1_score[i] <- (neuralnet_test_F1_score[i] + neuralnet_validation_F1_score[i]) / 2
  neuralnet_holdout_F1_score_mean <- mean(neuralnet_holdout_F1_score)
  neuralnet_holdout_positive_predictive_value[i] <- (neuralnet_test_positive_predictive_value[i] + neuralnet_validation_positive_predictive_value[i]) / 2
  neuralnet_holdout_positive_predictive_value_mean <- mean(neuralnet_holdout_positive_predictive_value)
  neuralnet_holdout_negative_predictive_value[i] <- (neuralnet_test_negative_predictive_value[i] + neuralnet_validation_negative_predictive_value[i]) / 2
  neuralnet_holdout_negative_predictive_value_mean <- mean(neuralnet_holdout_negative_predictive_value)
  neuralnet_holdout_overfitting[i] <- neuralnet_holdout_accuracy[i] / neuralnet_train_accuracy[i]
  neuralnet_holdout_overfitting_mean <- mean(neuralnet_holdout_overfitting)
  neuralnet_holdout_overfitting_range <- range(neuralnet_holdout_overfitting)
  neuralnet_holdout_overfitting_sd <- sd(neuralnet_holdout_overfitting)
  neuralnet_AUC[i] <- pROC::auc(c(test01$y, validation01$y), as.numeric(c(neuralnet_test_pred, neuralnet_validation_pred)) - 1)[1]
  neuralnet_AUC_mean <- mean(neuralnet_AUC)

  neuralnet_table <- neuralnet_test_table + neuralnet_validation_table
  neuralnet_table_total <- neuralnet_table_total + neuralnet_table

  neuralnet_end <- Sys.time()
  neuralnet_duration[i] <- neuralnet_end - neuralnet_start
  neuralnet_duration_mean <- mean(neuralnet_duration)
  neuralnet_duration_sd <- sd(neuralnet_duration)


  #### 14 XGBoost ####
  xgb_start <- Sys.time()
  message("Working on XGBoost")

  train_x = data.matrix(train[, 1 : ncol(train)])
  train_y = train[,ncol(train) : ncol(train)]

  #define predictor and response variables in test set
  test_x = data.matrix(test[, 1 : ncol(test)])
  test_y = test[, ncol(test) : ncol(test)]

  #define predictor and response variables in validation set
  validation_x = data.matrix(validation[, 1 : ncol(validation)])
  validation_y = validation[, ncol(validation): ncol(validation)]

  #define final train, test and validation sets
  xgb_train = xgb.DMatrix(data = train_x, label = as.matrix(train_y))
  xgb_test <- xgb.DMatrix(data = test_x, label = as.matrix(test_y))
  xgb_validation = xgb.DMatrix(data = validation_x, label = as.matrix(validation_y))

  #define watchlist
  watchlist = list(train = xgb_train, validation=xgb_validation)
  watchlist_test <- list(train = xgb_train, test = xgb_test)
  watchlist_validation <- list(train = xgb_train, validation = xgb_validation)

  xgb_model <- xgb.train(data = xgb_train, params = xgb.params(max_depth = 3), nrounds = 70)

  xgb_min = which.min(xgb_model$evaluation_log$validation_rmse)

  xgb_train_pred <- stats::predict(object = xgb_model, newdata = train_x, type = "prob")
  xgb_train_predictions <- ifelse(xgb_train_pred > positive_rate, 1, 0)
  xgb_train_predictions <- xgb_train_predictions[!is.na(xgb_train_predictions)]
  ifelse(sd(xgb_train_predictions) == 0,
         xgb_train_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         xgb_train_table <- table(xgb_train_predictions, y_train[1:length(xgb_train_predictions)])
  )
  xgb_train_true_positive_rate[i] <- xgb_train_table[2, 2] / sum(xgb_train_table[2, 2] + xgb_train_table[1, 2])
  xgb_train_true_positive_rate_mean <- mean(xgb_train_true_positive_rate)
  xgb_train_true_negative_rate[i] <- xgb_train_table[1, 1] / sum(xgb_train_table[1, 1] + xgb_train_table[2, 1])
  xgb_train_true_negative_rate_mean <- mean(xgb_train_true_negative_rate)
  xgb_train_false_positive_rate[i] <- xgb_train_table[2, 1] / sum(xgb_train_table[2, 1] + xgb_train_table[1, 1])
  xgb_train_false_positive_rate_mean <- mean(xgb_train_false_positive_rate)
  xgb_train_false_negative_rate[i] <- xgb_train_table[1, 2] / sum(xgb_train_table[1, 2] + xgb_train_table[2, 2])
  xgb_train_false_negative_rate_mean <- mean(xgb_train_false_negative_rate)
  xgb_train_sensitivity[i] <- xgb_train_table[2,2] / sum(xgb_train_table[2,1] + xgb_train_table[2,2])
  xgb_train_sensitivity_mean <- mean(xgb_train_sensitivity)
  xgb_train_specificity[i] <- xgb_train_table[1,1] / sum(xgb_train_table[1,1] + xgb_train_table[2,1])
  xgb_train_specificity_mean <- mean(xgb_train_specificity)
  xgb_train_precision[i] <- xgb_train_table[2,2] / sum(xgb_train_table[1,2] + xgb_train_table[2,2])
  xgb_train_precision_mean <- mean(xgb_train_precision)
  xgb_train_negative_predictive_value[i] <- xgb_train_table[1,1] / sum(xgb_train_table[1,1] + xgb_train_table[2,1])
  xgb_train_negative_predictive_value_mean <- mean(xgb_train_negative_predictive_value)
  xgb_train_accuracy[i] <- (xgb_train_table[1,1] + xgb_train_table[2,2]) / sum(xgb_train_table)
  xgb_train_accuracy_mean <- mean(xgb_train_accuracy)
  xgb_train_F1_score[i] <- 2*(xgb_train_table[2,2]) / sum(2 * xgb_train_table[2,2] + xgb_train_table[1,2] + xgb_train_table[2,1])
  xgb_train_F1_score_mean <- mean(xgb_train_F1_score)
  xgb_train_positive_predictive_value[i] = xgb_train_table[2, 2] / sum(xgb_train_table[2, 2] + xgb_train_table[2, 1])
  xgb_train_positive_predictive_value_mean <- mean(xgb_train_positive_predictive_value)
  xgb_train_negative_predictive_value[i] <- xgb_train_table[1, 1]/ sum(xgb_train_table[1, 1] + xgb_train_table[1, 2])
  xgb_train_negative_predictive_value_mean <- mean(xgb_train_negative_predictive_value)

  xgb_test_pred <- stats::predict(object = xgb_model, newdata = test_x, type = "prob")
  xgb_test_predictions <- ifelse(xgb_test_pred > positive_rate, 1, 0)
  xgb_test_predictions <- xgb_test_predictions[!is.na(xgb_test_predictions)]
  ifelse(sd(xgb_test_predictions) == 0,
         xgb_test_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         xgb_test_table <- table(xgb_test_predictions, y_test[1:length(xgb_test_predictions)])
  )
  xgb_test_true_positive_rate[i] <- xgb_test_table[2, 2] / sum(xgb_test_table[2, 2] + xgb_test_table[1, 2])
  xgb_test_true_positive_rate_mean <- mean(xgb_test_true_positive_rate)
  xgb_test_true_negative_rate[i] <- xgb_test_table[1, 1] / sum(xgb_test_table[1, 1] + xgb_test_table[2, 1])
  xgb_test_true_negative_rate_mean <- mean(xgb_test_true_negative_rate)
  xgb_test_false_positive_rate[i] <- xgb_test_table[2, 1] / sum(xgb_test_table[2, 1] + xgb_test_table[1, 1])
  xgb_test_false_positive_rate_mean <- mean(xgb_test_false_positive_rate)
  xgb_test_false_negative_rate[i] <- xgb_test_table[1, 2] / sum(xgb_test_table[1, 2] + xgb_test_table[2, 2])
  xgb_test_false_negative_rate_mean <- mean(xgb_test_false_negative_rate)
  xgb_test_sensitivity[i] <- xgb_test_table[2,2] / sum(xgb_test_table[2,1] + xgb_test_table[2,2])
  xgb_test_sensitivity_mean <- mean(xgb_test_sensitivity)
  xgb_test_specificity[i] <- xgb_test_table[1,1] / sum(xgb_test_table[1,1] + xgb_test_table[2,1])
  xgb_test_specificity_mean <- mean(xgb_test_specificity)
  xgb_test_precision[i] <- xgb_test_table[2,2] / sum(xgb_test_table[1,2] + xgb_test_table[2,2])
  xgb_test_precision_mean <- mean(xgb_test_precision)
  xgb_test_negative_predictive_value[i] <- xgb_test_table[1,1] / sum(xgb_test_table[1,1] + xgb_test_table[2,1])
  xgb_test_negative_predictive_value_mean <- mean(xgb_test_negative_predictive_value)
  xgb_test_accuracy[i] <- (xgb_test_table[1,1] + xgb_test_table[2,2]) / sum(xgb_test_table) # work on this one!!
  xgb_test_accuracy_mean <- mean(xgb_test_accuracy)
  xgb_test_F1_score[i] <- 2*(xgb_test_table[2,2]) / sum(2 * xgb_test_table[2,2] + xgb_test_table[1,2] + xgb_test_table[2,1])
  xgb_test_F1_score_mean <- mean(xgb_test_F1_score)
  xgb_test_positive_predictive_value[i] = xgb_test_table[2, 2] / sum(xgb_test_table[2, 2] + xgb_test_table[2, 1])
  xgb_test_positive_predictive_value_mean <- mean(xgb_test_positive_predictive_value)
  xgb_test_negative_predictive_value[i] <- xgb_test_table[1, 1]/ sum(xgb_test_table[1, 1] + xgb_test_table[1, 2])
  xgb_test_negative_predictive_value_mean <- mean(xgb_test_negative_predictive_value)

  xgb_validation_pred <- stats::predict(object = xgb_model, newdata = validation_x, type = "prob")
  xgb_validation_predictions <- ifelse(xgb_validation_pred > positive_rate, 1, 0)
  xgb_validation_predictions <- xgb_validation_predictions[!is.na(xgb_validation_predictions)]
  ifelse(sd(xgb_validation_predictions) == 0,
         xgb_validation_table <- (as.table(matrix(c(0,0,0,0), ncol = 2, dimnames = list(c("0", "1"), c("0", "1"))))),
         xgb_validation_table <- table(xgb_validation_predictions, y_validation[1:length(xgb_validation_predictions)])
  )
  xgb_validation_true_positive_rate[i] <- xgb_validation_table[2, 2] / sum(xgb_validation_table[2, 2] + xgb_validation_table[1, 2])
  xgb_validation_true_positive_rate_mean <- mean(xgb_validation_true_positive_rate)
  xgb_validation_true_negative_rate[i] <- xgb_validation_table[1, 1] / sum(xgb_validation_table[1, 1] + xgb_validation_table[2, 1])
  xgb_validation_true_negative_rate_mean <- mean(xgb_validation_true_negative_rate)
  xgb_validation_false_positive_rate[i] <- xgb_validation_table[2, 1] / sum(xgb_validation_table[2, 1] + xgb_validation_table[1, 1])
  xgb_validation_false_positive_rate_mean <- mean(xgb_validation_false_positive_rate)
  xgb_validation_false_negative_rate[i] <- xgb_validation_table[1, 2] / sum(xgb_validation_table[1, 2] + xgb_validation_table[2, 2])
  xgb_validation_false_negative_rate_mean <- mean(xgb_validation_false_negative_rate)
  xgb_validation_sensitivity[i] <- xgb_validation_table[2,2] / sum(xgb_validation_table[2,1] + xgb_validation_table[2,2])
  xgb_validation_sensitivity_mean <- mean(xgb_validation_sensitivity)
  xgb_validation_specificity[i] <- xgb_validation_table[1,1] / sum(xgb_validation_table[1,1] + xgb_validation_table[2,1])
  xgb_validation_specificity_mean <- mean(xgb_validation_specificity)
  xgb_validation_precision[i] <- xgb_validation_table[2,2] / sum(xgb_validation_table[1,2] + xgb_validation_table[2,2])
  xgb_validation_precision_mean <- mean(xgb_validation_precision)
  xgb_validation_negative_predictive_value[i] <- xgb_validation_table[1,1] / sum(xgb_validation_table[1,1] + xgb_validation_table[2,1])
  xgb_validation_negative_predictive_value_mean <- mean(xgb_validation_negative_predictive_value)
  xgb_validation_accuracy[i] <- (xgb_validation_table[1,1] + xgb_validation_table[2,2]) / sum(xgb_validation_table) # work on this one!!
  xgb_validation_accuracy_mean <- mean(xgb_validation_accuracy)
  xgb_validation_F1_score[i] <- 2*(xgb_validation_table[2,2]) / sum(2 * xgb_validation_table[2,2] + xgb_validation_table[1,2] + xgb_validation_table[2,1])
  xgb_validation_F1_score_mean <- mean(xgb_validation_F1_score)
  xgb_validation_positive_predictive_value[i] = xgb_validation_table[2, 2] / sum(xgb_validation_table[2, 2] + xgb_validation_table[2, 1])
  xgb_validation_positive_predictive_value_mean <- mean(xgb_validation_positive_predictive_value)
  xgb_validation_negative_predictive_value[i] <- xgb_validation_table[1, 1]/ sum(xgb_validation_table[1, 1] + xgb_validation_table[1, 2])
  xgb_validation_negative_predictive_value_mean <- mean(xgb_validation_negative_predictive_value)
  xgb_AUC[i] <- pROC::auc(c(test01$y, validation01$y), as.numeric(c(xgb_test_pred, xgb_validation_pred)) - 1)[1]
  xgb_AUC_mean <- mean(xgb_AUC)

  xgb_holdout_true_positive_rate[i] <- (xgb_test_true_positive_rate[i] + xgb_validation_true_positive_rate[i])/2
  xgb_holdout_true_positive_rate_mean <- mean(xgb_holdout_true_positive_rate)
  xgb_holdout_true_negative_rate[i] <- (xgb_test_true_negative_rate[i] + xgb_validation_true_negative_rate[i])/2
  xgb_holdout_true_negative_rate_mean <- mean(xgb_holdout_true_negative_rate)
  xgb_holdout_false_positive_rate[i] <- (xgb_test_false_positive_rate[i] + xgb_validation_false_positive_rate[i])/2
  xgb_holdout_false_positive_rate_mean <- mean(xgb_holdout_false_positive_rate)
  xgb_holdout_false_negative_rate[i] <- (xgb_test_false_negative_rate[i] + xgb_validation_false_negative_rate[i])/2
  xgb_holdout_false_negative_rate_mean <- mean(xgb_holdout_false_negative_rate)
  xgb_holdout_sensitivity[i] <- (xgb_test_specificity[i] + xgb_validation_sensitivity[i])/2
  xgb_holdout_sensitivity_mean <- mean(xgb_holdout_sensitivity)
  xgb_holdout_specificity[i] <- (xgb_test_specificity[i] + xgb_validation_specificity[i])/2
  xgb_holdout_specificity_mean <- mean(xgb_holdout_specificity)
  xgb_holdout_precision[i] <- (xgb_test_precision[i] + xgb_validation_precision[i])/2
  xgb_holdout_precision_mean <- mean(xgb_holdout_precision)
  xgb_holdout_negative_predictive_value[i] <- (xgb_test_negative_predictive_value[i] + xgb_validation_negative_predictive_value[i])/2
  xgb_holdout_negative_predictive_value_mean <- mean(xgb_holdout_negative_predictive_value)
  xgb_holdout_accuracy[i] <- (xgb_test_accuracy[i] + xgb_validation_accuracy[i])/2
  xgb_holdout_accuracy_mean <- mean(xgb_holdout_accuracy)
  xgb_holdout_F1_score[i] <- (xgb_test_F1_score[i] + xgb_validation_F1_score[i])/2
  xgb_holdout_F1_score_mean <- mean(xgb_holdout_F1_score)
  xgb_holdout_positive_predictive_value[i] <- (xgb_test_positive_predictive_value[i] + xgb_validation_positive_predictive_value[i]) /2
  xgb_holdout_positive_predictive_value_mean <- mean(xgb_holdout_positive_predictive_value)
  xgb_holdout_negative_predictive_value[i] <- (xgb_test_negative_predictive_value[i] + xgb_validation_negative_predictive_value[i]) /2
  xgb_holdout_negative_predictive_value_mean <- mean(xgb_holdout_negative_predictive_value)
  xgb_holdout_overfitting[i] <- xgb_holdout_accuracy[i] / xgb_train_accuracy[i]
  xgb_holdout_overfitting_mean <- mean(xgb_holdout_overfitting)
  xgb_holdout_overfitting_range <- range(xgb_holdout_overfitting)
  xgb_holdout_overfitting_sd <- sd(xgb_holdout_overfitting)

  xgb_table <- xgb_test_table + xgb_validation_table
  xgb_table_total <- xgb_table_total + xgb_table

  xgb_end <- Sys.time()
  xgb_duration[i] = xgb_end - xgb_start
  xgb_duration_mean <- mean(xgb_duration)
  xgb_duration_sd <- sd(xgb_duration)

  ######################################## Ensembles start here ####################################################

  ensemble1 <- data.frame(
    "Elastic" = c(elastic_test_predictions, elastic_validation_predictions),
    "Flexible_Discriminant_Analysis" = c(fda_test_pred, fda_validation_pred),
    "Generalized_Additive_Models" = c(gam_test_predictions, gam_validation_predictions),
    "Generalized_Linear_Models" = as.numeric(c(glmnet_test_predictions, glmnet_validation_predictions)),
    "Gradient_Boosted" = as.numeric(c(gb_test_predictions, gb_validation_predictions)),
    "Neuralnet" = c(neuralnet_test_predictions, neuralnet_validation_predictions),
    "XGBoost" = c(as.numeric(xgb_test_predictions), as.numeric(xgb_validation_predictions))
  )

  ensemble_row_numbers <- as.numeric(c(row.names(test), row.names(validation)))
  ensemble1$y <- c(test$y, validation$y)

  tmp <- stats::cor(ensemble1)
  tmp[upper.tri(tmp)] <- 0
  diag(tmp) <- 0
  data_01 <- ensemble1[, !apply(tmp, 2, function(x) any(abs(x) > remove_ensemble_correlations_greater_than, na.rm = TRUE))]
  ensemble1 <- data_01 # removed columns above a given correlation (I recommend 0.99)

  if(sum(is.na(ensemble1 >0))){
    ensemble1 <- ensemble1[stats::complete.cases(ensemble1), ]
  }

  ensemble1 <- Filter(function(x) stats::var(x) != 0, ensemble1) # Removes columns with no variation

  headensemble <- round(head(ensemble1), 4)

  head_ensemble <- reactable::reactable(round(head(ensemble1), 4),
                                        searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                        striped = TRUE, highlight = TRUE, resizable = TRUE
  )

  htmltools::div(class = "table",
                 htmltools::div(class = "title", "head_ensemble")
  )

  head_ensemble <- htmlwidgets::prependContent(head_ensemble, htmltools::h2(class = "title", "Head of ensemble"))

  ensemblecorrelation <- round(cor(ensemble1), 4)

  ensemble_correlation <- reactable::reactable(round(cor(ensemble1), 4),
                                               searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                               striped = TRUE, highlight = TRUE, resizable = TRUE
  )

  htmltools::div(class = "table",
                 htmltools::div(class = "title", "ensemble_correlation")
  )

  ensemble_correlation <- htmlwidgets::prependContent(ensemble_correlation, htmltools::h2(class = "title", "Ensemble correlation"))

  if(set_seed == "N"){
    ensemble_index <- sample(c(1:3), nrow(ensemble1), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))
    ensemble_train <- ensemble1[ensemble_index == 1, ]
    ensemble_test <- ensemble1[ensemble_index == 2, ]
    ensemble_validation <- ensemble1[ensemble_index == 3, ]
    ensemble_y_train <- ensemble_train$y
    ensemble_y_test <- ensemble_test$y
    ensemble_y_validation <- ensemble_validation$y
  }

  if(set_seed == "Y"){
    ensemble_train <- ensemble1[1:round(train_amount*nrow(ensemble1)), ]
    ensemble_test <- ensemble1[round(train_amount*nrow(ensemble1)) +1:round(test_amount*nrow(ensemble1)), ]
    ensemble_validation <- ensemble1[(nrow(ensemble_train) + nrow(ensemble_test) +1) : nrow(ensemble1), ]
    ensemble_y_train <- ensemble_train$y
    ensemble_y_test <- ensemble_test$y
    ensemble_y_validation <- ensemble_validation$y
  }

  message(noquote(""))
  message("Working on the Ensembles section")
  message(noquote(""))


  #### 16. Ensemble Using ensemble_C50 ####
  ensemble_C50_start <- Sys.time()
  message("Working on Ensemble C50")

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_C50_train_fit <- C50::C5.0(as.factor(ensemble_y_train) ~ ., data = ensemble_train, family = binomial(link = "logit"))
  }
  if(set_seed == "N"){
    ensemble_C50_train_fit <- C50::C5.0(as.factor(ensemble_y_train) ~ ., data = ensemble_train)
  }
  ensemble_C50_train_pred <- stats::predict(ensemble_C50_train_fit, ensemble_train, type = "class")
  ensemble_C50_train_table <- table(ensemble_C50_train_pred, ensemble_y_train)
  ensemble_C50_train_true_positive_rate[i] <- ensemble_C50_train_table[2, 2] / sum(ensemble_C50_train_table[2, 2] + ensemble_C50_train_table[1, 2])
  ensemble_C50_train_true_positive_rate_mean <- mean(ensemble_C50_train_true_positive_rate)
  ensemble_C50_train_true_negative_rate[i] <- ensemble_C50_train_table[1, 1] / sum(ensemble_C50_train_table[1, 1] + ensemble_C50_train_table[2, 1])
  ensemble_C50_train_true_negative_rate_mean <- mean(ensemble_C50_train_true_negative_rate)
  ensemble_C50_train_false_positive_rate[i] <- ensemble_C50_train_table[2, 1] / sum(ensemble_C50_train_table[2, 1] + ensemble_C50_train_table[1, 1])
  ensemble_C50_train_false_positive_rate_mean <- mean(ensemble_C50_train_false_positive_rate)
  ensemble_C50_train_false_negative_rate[i] <- ensemble_C50_train_table[1, 2] / sum(ensemble_C50_train_table[1, 2] + ensemble_C50_train_table[2, 2])
  ensemble_C50_train_false_negative_rate_mean <- mean(ensemble_C50_train_false_negative_rate)
  ensemble_C50_train_accuracy[i] <- (ensemble_C50_train_table[1, 1] + ensemble_C50_train_table[2, 2]) / sum(ensemble_C50_train_table)
  ensemble_C50_train_accuracy_mean <- mean(ensemble_C50_train_accuracy)
  ensemble_C50_train_F1_score[i] <- 2 * (ensemble_C50_train_table[2, 2]) / sum(2 * ensemble_C50_train_table[2, 2] + ensemble_C50_train_table[1, 2] + ensemble_C50_train_table[2, 1])
  ensemble_C50_train_F1_score_mean <- mean(ensemble_C50_train_F1_score)
  ensemble_C50_train_positive_predictive_value[i] <- ensemble_C50_train_table[2, 2] / sum(ensemble_C50_train_table[2, 2] + ensemble_C50_train_table[2, 1])
  ensemble_C50_train_positive_predictive_value_mean <- mean(ensemble_C50_train_positive_predictive_value)
  ensemble_C50_train_negative_predictive_value[i] <- ensemble_C50_train_table[1, 1] / sum(ensemble_C50_train_table[1, 1] + ensemble_C50_train_table[1, 2])
  ensemble_C50_train_negative_predictive_value_mean <- mean(ensemble_C50_train_negative_predictive_value)

  ensemble_C50_test_pred <- stats::predict(ensemble_C50_train_fit, ensemble_test, type = "class")
  ensemble_C50_test_table <- table(ensemble_C50_test_pred, ensemble_y_test)
  ensemble_C50_test_true_positive_rate[i] <- ensemble_C50_test_table[2, 2] / sum(ensemble_C50_test_table[2, 2] + ensemble_C50_test_table[1, 2])
  ensemble_C50_test_true_positive_rate_mean <- mean(ensemble_C50_test_true_positive_rate)
  ensemble_C50_test_true_negative_rate[i] <- ensemble_C50_test_table[1, 1] / sum(ensemble_C50_test_table[1, 1] + ensemble_C50_test_table[2, 1])
  ensemble_C50_test_true_negative_rate_mean <- mean(ensemble_C50_test_true_negative_rate)
  ensemble_C50_test_false_positive_rate[i] <- ensemble_C50_test_table[2, 1] / sum(ensemble_C50_test_table[2, 1] + ensemble_C50_test_table[1, 1])
  ensemble_C50_test_false_positive_rate_mean <- mean(ensemble_C50_test_false_positive_rate)
  ensemble_C50_test_false_negative_rate[i] <- ensemble_C50_test_table[1, 2] / sum(ensemble_C50_test_table[1, 2] + ensemble_C50_test_table[2, 2])
  ensemble_C50_test_false_negative_rate_mean <- mean(ensemble_C50_test_false_negative_rate)
  ensemble_C50_test_accuracy[i] <- (ensemble_C50_test_table[1, 1] + ensemble_C50_test_table[2, 2]) / sum(ensemble_C50_test_table)
  ensemble_C50_test_accuracy_mean <- mean(ensemble_C50_test_accuracy)
  ensemble_C50_test_F1_score[i] <- 2 * (ensemble_C50_test_table[2, 2]) / sum(2 * ensemble_C50_test_table[2, 2] + ensemble_C50_test_table[1, 2] + ensemble_C50_test_table[2, 1])
  ensemble_C50_test_F1_score_mean <- mean(ensemble_C50_test_F1_score)
  ensemble_C50_test_positive_predictive_value[i] <- ensemble_C50_test_table[2, 2] / sum(ensemble_C50_test_table[2, 2] + ensemble_C50_test_table[2, 1])
  ensemble_C50_test_positive_predictive_value_mean <- mean(ensemble_C50_test_positive_predictive_value)
  ensemble_C50_test_negative_predictive_value[i] <- ensemble_C50_test_table[1, 1] / sum(ensemble_C50_test_table[1, 1] + ensemble_C50_test_table[1, 2])
  ensemble_C50_test_negative_predictive_value_mean <- mean(ensemble_C50_test_negative_predictive_value)

  ensemble_C50_validation_pred <- stats::predict(ensemble_C50_train_fit, ensemble_validation, type = "class")
  ensemble_C50_validation_table <- table(ensemble_C50_validation_pred, ensemble_y_validation)
  ensemble_C50_validation_true_positive_rate[i] <- ensemble_C50_validation_table[2, 2] / sum(ensemble_C50_validation_table[2, 2] + ensemble_C50_validation_table[1, 2])
  ensemble_C50_validation_true_positive_rate_mean <- mean(ensemble_C50_validation_true_positive_rate)
  ensemble_C50_validation_true_negative_rate[i] <- ensemble_C50_validation_table[1, 1] / sum(ensemble_C50_validation_table[1, 1] + ensemble_C50_validation_table[2, 1])
  ensemble_C50_validation_true_negative_rate_mean <- mean(ensemble_C50_validation_true_negative_rate)
  ensemble_C50_validation_false_positive_rate[i] <- ensemble_C50_validation_table[2, 1] / sum(ensemble_C50_validation_table[2, 1] + ensemble_C50_validation_table[1, 1])
  ensemble_C50_validation_false_positive_rate_mean <- mean(ensemble_C50_validation_false_positive_rate)
  ensemble_C50_validation_false_negative_rate[i] <- ensemble_C50_validation_table[1, 2] / sum(ensemble_C50_validation_table[1, 2] + ensemble_C50_validation_table[2, 2])
  ensemble_C50_validation_false_negative_rate_mean <- mean(ensemble_C50_validation_false_negative_rate)
  ensemble_C50_validation_accuracy[i] <- (ensemble_C50_validation_table[1, 1] + ensemble_C50_validation_table[2, 2]) / sum(ensemble_C50_validation_table)
  ensemble_C50_validation_accuracy_mean <- mean(ensemble_C50_validation_accuracy)
  ensemble_C50_validation_F1_score[i] <- 2 * (ensemble_C50_validation_table[2, 2]) / sum(2 * ensemble_C50_validation_table[2, 2] + ensemble_C50_validation_table[1, 2] + ensemble_C50_validation_table[2, 1])
  ensemble_C50_validation_F1_score_mean <- mean(ensemble_C50_validation_F1_score)
  ensemble_C50_validation_positive_predictive_value[i] <- ensemble_C50_validation_table[2, 2] / sum(ensemble_C50_validation_table[2, 2] + ensemble_C50_validation_table[2, 1])
  ensemble_C50_validation_positive_predictive_value_mean <- mean(ensemble_C50_validation_positive_predictive_value)
  ensemble_C50_validation_negative_predictive_value[i] <- ensemble_C50_validation_table[1, 1] / sum(ensemble_C50_validation_table[1, 1] + ensemble_C50_validation_table[1, 2])
  ensemble_C50_validation_negative_predictive_value_mean <- mean(ensemble_C50_validation_negative_predictive_value)

  ensemble_C50_holdout_true_positive_rate[i] <- (ensemble_C50_test_true_positive_rate[i] + ensemble_C50_validation_true_positive_rate[i]) / 2
  ensemble_C50_holdout_true_positive_rate_mean <- mean(ensemble_C50_holdout_true_positive_rate)
  ensemble_C50_holdout_true_negative_rate[i] <- (ensemble_C50_test_true_negative_rate[i] + ensemble_C50_validation_true_negative_rate[i]) / 2
  ensemble_C50_holdout_true_negative_rate_mean <- mean(ensemble_C50_holdout_true_negative_rate)
  ensemble_C50_holdout_false_positive_rate[i] <- (ensemble_C50_test_false_positive_rate[i] + ensemble_C50_validation_false_positive_rate[i]) / 2
  ensemble_C50_holdout_false_positive_rate_mean <- mean(ensemble_C50_holdout_false_positive_rate)
  ensemble_C50_holdout_false_negative_rate[i] <- (ensemble_C50_test_false_negative_rate[i] + ensemble_C50_validation_false_negative_rate[i]) / 2
  ensemble_C50_holdout_false_negative_rate_mean <- mean(ensemble_C50_holdout_false_negative_rate)
  ensemble_C50_holdout_accuracy[i] <- (ensemble_C50_test_accuracy[i] + ensemble_C50_validation_accuracy[i]) / 2
  ensemble_C50_holdout_accuracy_mean <- mean(ensemble_C50_holdout_accuracy)
  ensemble_C50_holdout_accuracy_sd <- sd(ensemble_C50_holdout_accuracy)
  ensemble_C50_holdout_F1_score[i] <- (ensemble_C50_test_F1_score[i] + ensemble_C50_validation_F1_score[i]) / 2
  ensemble_C50_holdout_F1_score_mean <- mean(ensemble_C50_holdout_F1_score)
  ensemble_C50_holdout_positive_predictive_value[i] <- (ensemble_C50_test_positive_predictive_value[i] + ensemble_C50_validation_positive_predictive_value[i]) / 2
  ensemble_C50_holdout_positive_predictive_value_mean <- mean(ensemble_C50_holdout_positive_predictive_value)
  ensemble_C50_holdout_negative_predictive_value[i] <- (ensemble_C50_test_negative_predictive_value[i] + ensemble_C50_validation_negative_predictive_value[i]) / 2
  ensemble_C50_holdout_negative_predictive_value_mean <- mean(ensemble_C50_holdout_negative_predictive_value)
  ensemble_C50_holdout_overfitting[i] <- ensemble_C50_holdout_accuracy[i] / ensemble_C50_train_accuracy[i]
  ensemble_C50_holdout_overfitting_mean <- mean(ensemble_C50_holdout_overfitting)
  ensemble_C50_holdout_overfitting_range <- range(ensemble_C50_holdout_overfitting)
  ensemble_C50_holdout_overfitting_sd <- sd(ensemble_C50_holdout_overfitting)
  ensemble_C50_AUC[i] <- pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_C50_test_pred, ensemble_C50_validation_pred)) - 1)[1]
  ensemble_C50_AUC_mean <- mean(ensemble_C50_AUC)

  ensemble_C50_table <- ensemble_C50_test_table + ensemble_C50_validation_table
  ensemble_C50_table_total <- ensemble_C50_table_total + ensemble_C50_table

  ensemble_C50_end <- Sys.time()
  ensemble_C50_duration[i] <- ensemble_C50_end - ensemble_C50_start
  ensemble_C50_duration_mean <- mean(ensemble_C50_duration)
  ensemble_C50_duration_sd <- sd(ensemble_C50_duration)


  #### Model #6  Ensemble Elastic Net ####
  ensemble_elastic_start <- Sys.time()
  message("Working on ensemble_elastic")
  y <- ensemble_train$y
  x <- data.matrix(ensemble_train[, 1:ncol(ensemble_train)])
  ensemble_elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  ensemble_elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_ensemble_elastic_lambda <- ensemble_elastic_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    best_ensemble_elastic_model <- glmnet::glmnet(x, y, alpha = 0.5, family = "binomial")
  }
  if(set_seed == "N"){
    best_ensemble_elastic_model <- glmnet::glmnet(x, y, alpha = 0.5, family = "binomial")
  }
  y <- ensemble_train$y
  x <- data.matrix(ensemble_train[, 1:ncol(ensemble_train)])
  ensemble_elastic_model <- glmnet::glmnet(x, y, alpha = 0.5)
  ensemble_elastic_cv <- glmnet::cv.glmnet(x, y, alpha = 0.5)
  best_ensemble_elastic_lambda <- ensemble_elastic_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    best_ensemble_elastic_model <- glmnet::glmnet(x, y, alpha = 0.5, family = "binomial")
  }
  if(set_seed == "N"){
    best_ensemble_elastic_model <- glmnet::glmnet(x, y, alpha = 0.5, family = "binomial")
  }
  ensemble_elastic_train_pred <- glmnet::predict.glmnet(best_ensemble_elastic_model, newx = as.matrix(ensemble_train), s = best_ensemble_elastic_lambda)
  ensemble_elastic_train_pred <- ifelse(ensemble_elastic_train_pred > positive_rate, 1, 0)
  ensemble_elastic_train_table <- table(ensemble_elastic_train_pred, ensemble_y_train)
  ensemble_elastic_train_true_positive_rate[i] <- ensemble_elastic_train_table[2, 2] / sum(ensemble_elastic_train_table[2, 2] + ensemble_elastic_train_table[1, 2])
  ensemble_elastic_train_true_positive_rate_mean <- mean(ensemble_elastic_train_true_positive_rate)
  ensemble_elastic_train_true_negative_rate[i] <- ensemble_elastic_train_table[1, 1] / sum(ensemble_elastic_train_table[1, 1] + ensemble_elastic_train_table[2, 1])
  ensemble_elastic_train_true_negative_rate_mean <- mean(ensemble_elastic_train_true_negative_rate)
  ensemble_elastic_train_false_positive_rate[i] <- ensemble_elastic_train_table[2, 1] / sum(ensemble_elastic_train_table[2, 1] + ensemble_elastic_train_table[1, 1])
  ensemble_elastic_train_false_positive_rate_mean <- mean(ensemble_elastic_train_false_positive_rate)
  ensemble_elastic_train_false_negative_rate[i] <- ensemble_elastic_train_table[1, 2] / sum(ensemble_elastic_train_table[1, 2] + ensemble_elastic_train_table[2, 2])
  ensemble_elastic_train_false_negative_rate_mean <- mean(ensemble_elastic_train_false_negative_rate)
  ensemble_elastic_train_accuracy[i] <- (ensemble_elastic_train_table[1, 1] + ensemble_elastic_train_table[2, 2]) / sum(ensemble_elastic_train_table)
  ensemble_elastic_train_accuracy_mean <- mean(ensemble_elastic_train_accuracy)
  ensemble_elastic_train_F1_score[i] <- 2 * (ensemble_elastic_train_table[2, 2]) / sum(2 * ensemble_elastic_train_table[2, 2] + ensemble_elastic_train_table[1, 2] + ensemble_elastic_train_table[2, 1])
  ensemble_elastic_train_F1_score_mean <- mean(ensemble_elastic_train_F1_score)
  ensemble_elastic_train_positive_predictive_value[i] <- ensemble_elastic_train_table[2, 2] / sum(ensemble_elastic_train_table[2, 2] + ensemble_elastic_train_table[2, 1])
  ensemble_elastic_train_positive_predictive_value_mean <- mean(ensemble_elastic_train_positive_predictive_value)
  ensemble_elastic_train_negative_predictive_value[i] <- ensemble_elastic_train_table[1, 1] / sum(ensemble_elastic_train_table[1, 1] + ensemble_elastic_train_table[1, 2])
  ensemble_elastic_train_negative_predictive_value_mean <- mean(ensemble_elastic_train_negative_predictive_value)

  ensemble_elastic_test_pred <- glmnet::predict.glmnet(best_ensemble_elastic_model, newx = as.matrix(ensemble_test), s = best_ensemble_elastic_lambda)
  ensemble_elastic_test_pred <- ifelse(ensemble_elastic_test_pred > positive_rate, 1, 0)
  ensemble_elastic_test_table <- table(ensemble_elastic_test_pred, ensemble_y_test)
  ensemble_elastic_test_true_positive_rate[i] <- ensemble_elastic_test_table[2, 2] / sum(ensemble_elastic_test_table[2, 2] + ensemble_elastic_test_table[1, 2])
  ensemble_elastic_test_true_positive_rate_mean <- mean(ensemble_elastic_test_true_positive_rate)
  ensemble_elastic_test_true_negative_rate[i] <- ensemble_elastic_test_table[1, 1] / sum(ensemble_elastic_test_table[1, 1] + ensemble_elastic_test_table[2, 1])
  ensemble_elastic_test_true_negative_rate_mean <- mean(ensemble_elastic_test_true_negative_rate)
  ensemble_elastic_test_false_positive_rate[i] <- ensemble_elastic_test_table[2, 1] / sum(ensemble_elastic_test_table[2, 1] + ensemble_elastic_test_table[1, 1])
  ensemble_elastic_test_false_positive_rate_mean <- mean(ensemble_elastic_test_false_positive_rate)
  ensemble_elastic_test_false_negative_rate[i] <- ensemble_elastic_test_table[1, 2] / sum(ensemble_elastic_test_table[1, 2] + ensemble_elastic_test_table[2, 2])
  ensemble_elastic_test_false_negative_rate_mean <- mean(ensemble_elastic_test_false_negative_rate)
  ensemble_elastic_test_accuracy[i] <- (ensemble_elastic_test_table[1, 1] + ensemble_elastic_test_table[2, 2]) / sum(ensemble_elastic_test_table)
  ensemble_elastic_test_accuracy_mean <- mean(ensemble_elastic_test_accuracy)
  ensemble_elastic_test_F1_score[i] <- 2 * (ensemble_elastic_test_table[2, 2]) / sum(2 * ensemble_elastic_test_table[2, 2] + ensemble_elastic_test_table[1, 2] + ensemble_elastic_test_table[2, 1])
  ensemble_elastic_test_F1_score_mean <- mean(ensemble_elastic_test_F1_score)
  ensemble_elastic_test_positive_predictive_value[i] <- ensemble_elastic_test_table[2, 2] / sum(ensemble_elastic_test_table[2, 2] + ensemble_elastic_test_table[2, 1])
  ensemble_elastic_test_positive_predictive_value_mean <- mean(ensemble_elastic_test_positive_predictive_value)
  ensemble_elastic_test_negative_predictive_value[i] <- ensemble_elastic_test_table[1, 1] / sum(ensemble_elastic_test_table[1, 1] + ensemble_elastic_test_table[1, 2])
  ensemble_elastic_test_negative_predictive_value_mean <- mean(ensemble_elastic_test_negative_predictive_value)

  ensemble_elastic_validation_pred <- glmnet::predict.glmnet(best_ensemble_elastic_model, newx = as.matrix(ensemble_validation), s = best_ensemble_elastic_lambda)
  ensemble_elastic_validation_pred <- ifelse(ensemble_elastic_validation_pred > positive_rate, 1, 0)
  ensemble_elastic_validation_table <- table(ensemble_elastic_validation_pred, ensemble_y_validation)
  ensemble_elastic_validation_true_positive_rate[i] <- ensemble_elastic_validation_table[2, 2] / sum(ensemble_elastic_validation_table[2, 2] + ensemble_elastic_validation_table[1, 2])
  ensemble_elastic_validation_true_positive_rate_mean <- mean(ensemble_elastic_validation_true_positive_rate)
  ensemble_elastic_validation_true_negative_rate[i] <- ensemble_elastic_validation_table[1, 1] / sum(ensemble_elastic_validation_table[1, 1] + ensemble_elastic_validation_table[2, 1])
  ensemble_elastic_validation_true_negative_rate_mean <- mean(ensemble_elastic_validation_true_negative_rate)
  ensemble_elastic_validation_false_positive_rate[i] <- ensemble_elastic_validation_table[2, 1] / sum(ensemble_elastic_validation_table[2, 1] + ensemble_elastic_validation_table[1, 1])
  ensemble_elastic_validation_false_positive_rate_mean <- mean(ensemble_elastic_validation_false_positive_rate)
  ensemble_elastic_validation_false_negative_rate[i] <- ensemble_elastic_validation_table[1, 2] / sum(ensemble_elastic_validation_table[1, 2] + ensemble_elastic_validation_table[2, 2])
  ensemble_elastic_validation_false_negative_rate_mean <- mean(ensemble_elastic_validation_false_negative_rate)
  ensemble_elastic_validation_accuracy[i] <- (ensemble_elastic_validation_table[1, 1] + ensemble_elastic_validation_table[2, 2]) / sum(ensemble_elastic_validation_table)
  ensemble_elastic_validation_accuracy_mean <- mean(ensemble_elastic_validation_accuracy)
  ensemble_elastic_validation_F1_score[i] <- 2 * (ensemble_elastic_validation_table[2, 2]) / sum(2 * ensemble_elastic_validation_table[2, 2] + ensemble_elastic_validation_table[1, 2] + ensemble_elastic_validation_table[2, 1])
  ensemble_elastic_validation_F1_score_mean <- mean(ensemble_elastic_validation_F1_score)
  ensemble_elastic_validation_positive_predictive_value[i] <- ensemble_elastic_validation_table[2, 2] / sum(ensemble_elastic_validation_table[2, 2] + ensemble_elastic_validation_table[2, 1])
  ensemble_elastic_validation_positive_predictive_value_mean <- mean(ensemble_elastic_validation_positive_predictive_value)
  ensemble_elastic_validation_negative_predictive_value[i] <- ensemble_elastic_validation_table[1, 1] / sum(ensemble_elastic_validation_table[1, 1] + ensemble_elastic_validation_table[1, 2])
  ensemble_elastic_validation_negative_predictive_value_mean <- mean(ensemble_elastic_validation_negative_predictive_value)

  ensemble_elastic_holdout_true_positive_rate[i] <- (ensemble_elastic_test_true_positive_rate[i] + ensemble_elastic_validation_true_positive_rate[i]) / 2
  ensemble_elastic_holdout_true_positive_rate_mean <- mean(ensemble_elastic_holdout_true_positive_rate)
  ensemble_elastic_holdout_true_negative_rate[i] <- (ensemble_elastic_test_true_negative_rate[i] + ensemble_elastic_validation_true_negative_rate[i]) / 2
  ensemble_elastic_holdout_true_negative_rate_mean <- mean(ensemble_elastic_holdout_true_negative_rate)
  ensemble_elastic_holdout_false_positive_rate[i] <- (ensemble_elastic_test_false_positive_rate[i] + ensemble_elastic_validation_false_positive_rate[i]) / 2
  ensemble_elastic_holdout_false_positive_rate_mean <- mean(ensemble_elastic_holdout_false_positive_rate)
  ensemble_elastic_holdout_false_negative_rate[i] <- (ensemble_elastic_test_false_negative_rate[i] + ensemble_elastic_validation_false_negative_rate[i]) / 2
  ensemble_elastic_holdout_false_negative_rate_mean <- mean(ensemble_elastic_holdout_false_negative_rate)
  ensemble_elastic_holdout_accuracy[i] <- (ensemble_elastic_test_accuracy[i] + ensemble_elastic_validation_accuracy[i]) / 2
  ensemble_elastic_holdout_accuracy_mean <- mean(ensemble_elastic_holdout_accuracy)
  ensemble_elastic_holdout_accuracy_sd <- sd(ensemble_elastic_holdout_accuracy)
  ensemble_elastic_holdout_F1_score[i] <- (ensemble_elastic_test_F1_score[i] + ensemble_elastic_validation_F1_score[i]) / 2
  ensemble_elastic_holdout_F1_score_mean <- mean(ensemble_elastic_holdout_F1_score)
  ensemble_elastic_holdout_positive_predictive_value[i] <- (ensemble_elastic_test_positive_predictive_value[i] + ensemble_elastic_validation_positive_predictive_value[i]) / 2
  ensemble_elastic_holdout_positive_predictive_value_mean <- mean(ensemble_elastic_holdout_positive_predictive_value)
  ensemble_elastic_holdout_negative_predictive_value[i] <- (ensemble_elastic_test_negative_predictive_value[i] + ensemble_elastic_validation_negative_predictive_value[i]) / 2
  ensemble_elastic_holdout_negative_predictive_value_mean <- mean(ensemble_elastic_holdout_negative_predictive_value)
  ensemble_elastic_holdout_overfitting[i] <- ensemble_elastic_holdout_accuracy[i] / ensemble_elastic_train_accuracy[i]
  ensemble_elastic_holdout_overfitting_mean <- mean(ensemble_elastic_holdout_overfitting)
  ensemble_elastic_holdout_overfitting_range <- range(ensemble_elastic_holdout_overfitting)
  ensemble_elastic_holdout_overfitting_sd <- sd(ensemble_elastic_holdout_overfitting)
  ensemble_elastic_AUC[i] <- pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_elastic_test_pred, ensemble_elastic_validation_pred)) - 1)[1]
  ensemble_elastic_AUC_mean <- mean(ensemble_elastic_AUC)
  ensemble_elastic_AUC[i] <- pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_elastic_test_pred, ensemble_elastic_validation_pred)) - 1)[1]
  ensemble_elastic_AUC_mean <- mean(ensemble_elastic_AUC)

  ensemble_elastic_table <- ensemble_elastic_test_table + ensemble_elastic_validation_table
  ensemble_elastic_table_total <- ensemble_elastic_table_total + ensemble_elastic_table

  ensemble_elastic_end <- Sys.time()
  ensemble_elastic_duration[i] <- ensemble_elastic_end - ensemble_elastic_start
  ensemble_elastic_duration_mean <- mean(ensemble_elastic_duration)
  ensemble_elastic_duration_sd <- sd(ensemble_elastic_duration)


  #### Ensemble Generalized Linear Models ####
  ensemble_glmnet_start <- Sys.time()
  message("Working on ensemble_glmnet")
  y <- ensemble_train$y
  x <- data.matrix(ensemble_train %>% dplyr::select(-y))
  ensemble_glmnet_model <- glmnet::glmnet(x, y)
  ensemble_glmnet_cv <- glmnet::cv.glmnet(x = x,y = y)
  best_ensemble_glmnet_lambda <- ensemble_glmnet_cv$lambda.min
  if(set_seed == "Y"){
    set.seed(seed = seed)
    best_ensemble_glmnet_model <- glmnet::glmnet(x, y, alpha = best_ensemble_glmnet_lambda, family = "binomial")
  }
  if(set_seed == "N"){
    best_ensemble_glmnet_model <- glmnet::glmnet(x, y, alpha = best_ensemble_glmnet_lambda, family = "binomial")
  }
  ensemble_glmnet_train_pred <- glmnet::predict.glmnet(best_ensemble_glmnet_model, newx = as.matrix(ensemble_train[, 1:ncol(ensemble_train)-1]), s = best_ensemble_glmnet_lambda)
  ensemble_glmnet_train_pred <- ifelse(ensemble_glmnet_train_pred > positive_rate, 1, 0)
  ensemble_glmnet_train_table <- table(ensemble_glmnet_train_pred, ensemble_y_train)
  ensemble_glmnet_train_true_positive_rate[i] <- ensemble_glmnet_train_table[2, 2] / sum(ensemble_glmnet_train_table[2, 2] + ensemble_glmnet_train_table[1, 2])
  ensemble_glmnet_train_true_positive_rate_mean <- mean(ensemble_glmnet_train_true_positive_rate)
  ensemble_glmnet_train_true_negative_rate[i] <- ensemble_glmnet_train_table[1, 1] / sum(ensemble_glmnet_train_table[1, 1] + ensemble_glmnet_train_table[2, 1])
  ensemble_glmnet_train_true_negative_rate_mean <- mean(ensemble_glmnet_train_true_negative_rate)
  ensemble_glmnet_train_false_positive_rate[i] <- ensemble_glmnet_train_table[2, 1] / sum(ensemble_glmnet_train_table[2, 1] + ensemble_glmnet_train_table[1, 1])
  ensemble_glmnet_train_false_positive_rate_mean <- mean(ensemble_glmnet_train_false_positive_rate)
  ensemble_glmnet_train_false_negative_rate[i] <- ensemble_glmnet_train_table[1, 2] / sum(ensemble_glmnet_train_table[1, 2] + ensemble_glmnet_train_table[2, 2])
  ensemble_glmnet_train_false_negative_rate_mean <- mean(ensemble_glmnet_train_false_negative_rate)
  ensemble_glmnet_train_accuracy[i] <- (ensemble_glmnet_train_table[1, 1] + ensemble_glmnet_train_table[2, 2]) / sum(ensemble_glmnet_train_table)
  ensemble_glmnet_train_accuracy_mean <- mean(ensemble_glmnet_train_accuracy)
  ensemble_glmnet_train_F1_score[i] <- 2 * (ensemble_glmnet_train_table[2, 2]) / sum(2 * ensemble_glmnet_train_table[2, 2] + ensemble_glmnet_train_table[1, 2] + ensemble_glmnet_train_table[2, 1])
  ensemble_glmnet_train_F1_score_mean <- mean(ensemble_glmnet_train_F1_score)
  ensemble_glmnet_train_positive_predictive_value[i] <- ensemble_glmnet_train_table[2, 2] / sum(ensemble_glmnet_train_table[2, 2] + ensemble_glmnet_train_table[2, 1])
  ensemble_glmnet_train_positive_predictive_value_mean <- mean(ensemble_glmnet_train_positive_predictive_value)
  ensemble_glmnet_train_negative_predictive_value[i] <- ensemble_glmnet_train_table[1, 1] / sum(ensemble_glmnet_train_table[1, 1] + ensemble_glmnet_train_table[1, 2])
  ensemble_glmnet_train_negative_predictive_value_mean <- mean(ensemble_glmnet_train_negative_predictive_value)

  ensemble_glmnet_test_pred <- glmnet::predict.glmnet(best_ensemble_glmnet_model, newx = as.matrix(ensemble_test[, 1:ncol(ensemble_test)-1]), s = best_ensemble_glmnet_lambda)
  ensemble_glmnet_test_pred <- ifelse(ensemble_glmnet_test_pred > positive_rate, 1, 0)
  ensemble_glmnet_test_table <- table(ensemble_glmnet_test_pred, ensemble_y_test)
  ensemble_glmnet_test_true_positive_rate[i] <- ensemble_glmnet_test_table[2, 2] / sum(ensemble_glmnet_test_table[2, 2] + ensemble_glmnet_test_table[1, 2])
  ensemble_glmnet_test_true_positive_rate_mean <- mean(ensemble_glmnet_test_true_positive_rate)
  ensemble_glmnet_test_true_negative_rate[i] <- ensemble_glmnet_test_table[1, 1] / sum(ensemble_glmnet_test_table[1, 1] + ensemble_glmnet_test_table[2, 1])
  ensemble_glmnet_test_true_negative_rate_mean <- mean(ensemble_glmnet_test_true_negative_rate)
  ensemble_glmnet_test_false_positive_rate[i] <- ensemble_glmnet_test_table[2, 1] / sum(ensemble_glmnet_test_table[2, 1] + ensemble_glmnet_test_table[1, 1])
  ensemble_glmnet_test_false_positive_rate_mean <- mean(ensemble_glmnet_test_false_positive_rate)
  ensemble_glmnet_test_false_negative_rate[i] <- ensemble_glmnet_test_table[1, 2] / sum(ensemble_glmnet_test_table[1, 2] + ensemble_glmnet_test_table[2, 2])
  ensemble_glmnet_test_false_negative_rate_mean <- mean(ensemble_glmnet_test_false_negative_rate)
  ensemble_glmnet_test_accuracy[i] <- (ensemble_glmnet_test_table[1, 1] + ensemble_glmnet_test_table[2, 2]) / sum(ensemble_glmnet_test_table)
  ensemble_glmnet_test_accuracy_mean <- mean(ensemble_glmnet_test_accuracy)
  ensemble_glmnet_test_F1_score[i] <- 2 * (ensemble_glmnet_test_table[2, 2]) / sum(2 * ensemble_glmnet_test_table[2, 2] + ensemble_glmnet_test_table[1, 2] + ensemble_glmnet_test_table[2, 1])
  ensemble_glmnet_test_F1_score_mean <- mean(ensemble_glmnet_test_F1_score)
  ensemble_glmnet_test_positive_predictive_value[i] <- ensemble_glmnet_test_table[2, 2] / sum(ensemble_glmnet_test_table[2, 2] + ensemble_glmnet_test_table[2, 1])
  ensemble_glmnet_test_positive_predictive_value_mean <- mean(ensemble_glmnet_test_positive_predictive_value)
  ensemble_glmnet_test_negative_predictive_value[i] <- ensemble_glmnet_test_table[1, 1] / sum(ensemble_glmnet_test_table[1, 1] + ensemble_glmnet_test_table[1, 2])
  ensemble_glmnet_test_negative_predictive_value_mean <- mean(ensemble_glmnet_test_negative_predictive_value)

  ensemble_glmnet_validation_pred <- glmnet::predict.glmnet(best_ensemble_glmnet_model, newx = as.matrix(ensemble_validation[, 1:ncol(ensemble_validation)-1]), s = best_ensemble_glmnet_lambda)
  ensemble_glmnet_validation_pred <- ifelse(ensemble_glmnet_validation_pred > positive_rate, 1, 0)
  ensemble_glmnet_validation_table <- table(ensemble_glmnet_validation_pred, ensemble_y_validation)
  ensemble_glmnet_validation_true_positive_rate[i] <- ensemble_glmnet_validation_table[2, 2] / sum(ensemble_glmnet_validation_table[2, 2] + ensemble_glmnet_validation_table[1, 2])
  ensemble_glmnet_validation_true_positive_rate_mean <- mean(ensemble_glmnet_validation_true_positive_rate)
  ensemble_glmnet_validation_true_negative_rate[i] <- ensemble_glmnet_validation_table[1, 1] / sum(ensemble_glmnet_validation_table[1, 1] + ensemble_glmnet_validation_table[2, 1])
  ensemble_glmnet_validation_true_negative_rate_mean <- mean(ensemble_glmnet_validation_true_negative_rate)
  ensemble_glmnet_validation_false_positive_rate[i] <- ensemble_glmnet_validation_table[2, 1] / sum(ensemble_glmnet_validation_table[2, 1] + ensemble_glmnet_validation_table[1, 1])
  ensemble_glmnet_validation_false_positive_rate_mean <- mean(ensemble_glmnet_validation_false_positive_rate)
  ensemble_glmnet_validation_false_negative_rate[i] <- ensemble_glmnet_validation_table[1, 2] / sum(ensemble_glmnet_validation_table[1, 2] + ensemble_glmnet_validation_table[2, 2])
  ensemble_glmnet_validation_false_negative_rate_mean <- mean(ensemble_glmnet_validation_false_negative_rate)
  ensemble_glmnet_validation_accuracy[i] <- (ensemble_glmnet_validation_table[1, 1] + ensemble_glmnet_validation_table[2, 2]) / sum(ensemble_glmnet_validation_table)
  ensemble_glmnet_validation_accuracy_mean <- mean(ensemble_glmnet_validation_accuracy)
  ensemble_glmnet_validation_F1_score[i] <- 2 * (ensemble_glmnet_validation_table[2, 2]) / sum(2 * ensemble_glmnet_validation_table[2, 2] + ensemble_glmnet_validation_table[1, 2] + ensemble_glmnet_validation_table[2, 1])
  ensemble_glmnet_validation_F1_score_mean <- mean(ensemble_glmnet_validation_F1_score)
  ensemble_glmnet_validation_positive_predictive_value[i] <- ensemble_glmnet_validation_table[2, 2] / sum(ensemble_glmnet_validation_table[2, 2] + ensemble_glmnet_validation_table[2, 1])
  ensemble_glmnet_validation_positive_predictive_value_mean <- mean(ensemble_glmnet_validation_positive_predictive_value)
  ensemble_glmnet_validation_negative_predictive_value[i] <- ensemble_glmnet_validation_table[1, 1] / sum(ensemble_glmnet_validation_table[1, 1] + ensemble_glmnet_validation_table[1, 2])
  ensemble_glmnet_validation_negative_predictive_value_mean <- mean(ensemble_glmnet_validation_negative_predictive_value)

  ensemble_glmnet_holdout_true_positive_rate[i] <- (ensemble_glmnet_test_true_positive_rate[i] + ensemble_glmnet_validation_true_positive_rate[i]) / 2
  ensemble_glmnet_holdout_true_positive_rate_mean <- mean(ensemble_glmnet_holdout_true_positive_rate)
  ensemble_glmnet_holdout_true_negative_rate[i] <- (ensemble_glmnet_test_true_negative_rate[i] + ensemble_glmnet_validation_true_negative_rate[i]) / 2
  ensemble_glmnet_holdout_true_negative_rate_mean <- mean(ensemble_glmnet_holdout_true_negative_rate)
  ensemble_glmnet_holdout_false_positive_rate[i] <- (ensemble_glmnet_test_false_positive_rate[i] + ensemble_glmnet_validation_false_positive_rate[i]) / 2
  ensemble_glmnet_holdout_false_positive_rate_mean <- mean(ensemble_glmnet_holdout_false_positive_rate)
  ensemble_glmnet_holdout_false_negative_rate[i] <- (ensemble_glmnet_test_false_negative_rate[i] + ensemble_glmnet_validation_false_negative_rate[i]) / 2
  ensemble_glmnet_holdout_false_negative_rate_mean <- mean(ensemble_glmnet_holdout_false_negative_rate)
  ensemble_glmnet_holdout_accuracy[i] <- (ensemble_glmnet_test_accuracy[i] + ensemble_glmnet_validation_accuracy[i]) / 2
  ensemble_glmnet_holdout_accuracy_mean <- mean(ensemble_glmnet_holdout_accuracy)
  ensemble_glmnet_holdout_accuracy_sd <- sd(ensemble_glmnet_holdout_accuracy)
  ensemble_glmnet_holdout_F1_score[i] <- (ensemble_glmnet_test_F1_score[i] + ensemble_glmnet_validation_F1_score[i]) / 2
  ensemble_glmnet_holdout_F1_score_mean <- mean(ensemble_glmnet_holdout_F1_score)
  ensemble_glmnet_holdout_positive_predictive_value[i] <- (ensemble_glmnet_test_positive_predictive_value[i] + ensemble_glmnet_validation_positive_predictive_value[i]) / 2
  ensemble_glmnet_holdout_positive_predictive_value_mean <- mean(ensemble_glmnet_holdout_positive_predictive_value)
  ensemble_glmnet_holdout_negative_predictive_value[i] <- (ensemble_glmnet_test_negative_predictive_value[i] + ensemble_glmnet_validation_negative_predictive_value[i]) / 2
  ensemble_glmnet_holdout_negative_predictive_value_mean <- mean(ensemble_glmnet_holdout_negative_predictive_value)
  ensemble_glmnet_holdout_overfitting[i] <- ensemble_glmnet_holdout_accuracy[i] / ensemble_glmnet_train_accuracy[i]
  ensemble_glmnet_holdout_overfitting_mean <- mean(ensemble_glmnet_holdout_overfitting)
  ensemble_glmnet_holdout_overfitting_range <- range(ensemble_glmnet_holdout_overfitting)
  ensemble_glmnet_holdout_overfitting_sd <- sd(ensemble_glmnet_holdout_overfitting)
  ensemble_glmnet_AUC[i] <- pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_glmnet_test_pred, ensemble_glmnet_validation_pred)) - 1)[1]
  ensemble_glmnet_AUC_mean <- mean(ensemble_glmnet_AUC)

  ensemble_glmnet_table <- ensemble_glmnet_test_table + ensemble_glmnet_validation_table
  ensemble_glmnet_table_total <- ensemble_glmnet_table_total + ensemble_glmnet_table

  ensemble_glmnet_end <- Sys.time()
  ensemble_glmnet_duration[i] <- ensemble_glmnet_end - ensemble_glmnet_start
  ensemble_glmnet_duration_mean <- mean(ensemble_glmnet_duration)
  ensemble_glmnet_duration_sd <- sd(ensemble_glmnet_duration)


  #### Ensemble Neuralnet ####

  ensemble_neuralnet_start <- Sys.time()
  message("Working on Ensemble Neuralnet")
  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_neuralnet_train_fit <- nnet::nnet(ensemble_train$y ~ ., data = ensemble_train, size = 0, linout = TRUE, skip = TRUE, family = binomial(link = "logit"))
  }
  if(set_seed == "N"){
    ensemble_neuralnet_train_fit <- nnet::nnet(ensemble_train$y ~ ., data = ensemble_train, size = 0, linout = TRUE, skip = TRUE, family = binomial(link = "logit"))
  }
  ensemble_neuralnet_train_pred <- predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_train)
  ensemble_neuralnet_train_pred <- ifelse(ensemble_neuralnet_train_pred > positive_rate, 1, 0)
  ensemble_neuralnet_train_table <- table(ensemble_neuralnet_train_pred, ensemble_y_train)
  ensemble_neuralnet_train_true_positive_rate[i] <- ensemble_neuralnet_train_table[2, 2] / sum(ensemble_neuralnet_train_table[2, 2] + ensemble_neuralnet_train_table[1, 2])
  ensemble_neuralnet_train_true_positive_rate_mean <- mean(ensemble_neuralnet_train_true_positive_rate)
  ensemble_neuralnet_train_true_negative_rate[i] <- ensemble_neuralnet_train_table[1, 1] / sum(ensemble_neuralnet_train_table[1, 1] + ensemble_neuralnet_train_table[2, 1])
  ensemble_neuralnet_train_true_negative_rate_mean <- mean(ensemble_neuralnet_train_true_negative_rate)
  ensemble_neuralnet_train_false_positive_rate[i] <- ensemble_neuralnet_train_table[2, 1] / sum(ensemble_neuralnet_train_table[2, 1] + ensemble_neuralnet_train_table[1, 1])
  ensemble_neuralnet_train_false_positive_rate_mean <- mean(ensemble_neuralnet_train_false_positive_rate)
  ensemble_neuralnet_train_false_negative_rate[i] <- ensemble_neuralnet_train_table[1, 2] / sum(ensemble_neuralnet_train_table[1, 2] + ensemble_neuralnet_train_table[2, 2])
  ensemble_neuralnet_train_false_negative_rate_mean <- mean(ensemble_neuralnet_train_false_negative_rate)
  ensemble_neuralnet_train_accuracy[i] <- (ensemble_neuralnet_train_table[1, 1] + ensemble_neuralnet_train_table[2, 2]) / sum(ensemble_neuralnet_train_table)
  ensemble_neuralnet_train_accuracy_mean <- mean(ensemble_neuralnet_train_accuracy)
  ensemble_neuralnet_train_F1_score[i] <- 2 * (ensemble_neuralnet_train_table[2, 2]) / sum(2 * ensemble_neuralnet_train_table[2, 2] + ensemble_neuralnet_train_table[1, 2] + ensemble_neuralnet_train_table[2, 1])
  ensemble_neuralnet_train_F1_score_mean <- mean(ensemble_neuralnet_train_F1_score)
  ensemble_neuralnet_train_positive_predictive_value[i] <- ensemble_neuralnet_train_table[2, 2] / sum(ensemble_neuralnet_train_table[2, 2] + ensemble_neuralnet_train_table[2, 1])
  ensemble_neuralnet_train_positive_predictive_value_mean <- mean(ensemble_neuralnet_train_positive_predictive_value)
  ensemble_neuralnet_train_negative_predictive_value[i] <- ensemble_neuralnet_train_table[1, 1] / sum(ensemble_neuralnet_train_table[1, 1] + ensemble_neuralnet_train_table[1, 2])
  ensemble_neuralnet_train_negative_predictive_value_mean <- mean(ensemble_neuralnet_train_negative_predictive_value)

  ensemble_neuralnet_test_pred <-  predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_test)
  ensemble_neuralnet_test_pred <- ifelse(ensemble_neuralnet_test_pred > positive_rate, 1, 0)
  ensemble_neuralnet_test_table <- table(ensemble_neuralnet_test_pred, ensemble_y_test)
  ensemble_neuralnet_test_true_positive_rate[i] <- ensemble_neuralnet_test_table[2, 2] / sum(ensemble_neuralnet_test_table[2, 2] + ensemble_neuralnet_test_table[1, 2])
  ensemble_neuralnet_test_true_positive_rate_mean <- mean(ensemble_neuralnet_test_true_positive_rate)
  ensemble_neuralnet_test_true_negative_rate[i] <- ensemble_neuralnet_test_table[1, 1] / sum(ensemble_neuralnet_test_table[1, 1] + ensemble_neuralnet_test_table[2, 1])
  ensemble_neuralnet_test_true_negative_rate_mean <- mean(ensemble_neuralnet_test_true_negative_rate)
  ensemble_neuralnet_test_false_positive_rate[i] <- ensemble_neuralnet_test_table[2, 1] / sum(ensemble_neuralnet_test_table[2, 1] + ensemble_neuralnet_test_table[1, 1])
  ensemble_neuralnet_test_false_positive_rate_mean <- mean(ensemble_neuralnet_test_false_positive_rate)
  ensemble_neuralnet_test_false_negative_rate[i] <- ensemble_neuralnet_test_table[1, 2] / sum(ensemble_neuralnet_test_table[1, 2] + ensemble_neuralnet_test_table[2, 2])
  ensemble_neuralnet_test_false_negative_rate_mean <- mean(ensemble_neuralnet_test_false_negative_rate)
  ensemble_neuralnet_test_accuracy[i] <- (ensemble_neuralnet_test_table[1, 1] + ensemble_neuralnet_test_table[2, 2]) / sum(ensemble_neuralnet_test_table)
  ensemble_neuralnet_test_accuracy_mean <- mean(ensemble_neuralnet_test_accuracy)
  ensemble_neuralnet_test_F1_score[i] <- 2 * (ensemble_neuralnet_test_table[2, 2]) / sum(2 * ensemble_neuralnet_test_table[2, 2] + ensemble_neuralnet_test_table[1, 2] + ensemble_neuralnet_test_table[2, 1])
  ensemble_neuralnet_test_F1_score_mean <- mean(ensemble_neuralnet_test_F1_score)
  ensemble_neuralnet_test_positive_predictive_value[i] <- ensemble_neuralnet_test_table[2, 2] / sum(ensemble_neuralnet_test_table[2, 2] + ensemble_neuralnet_test_table[2, 1])
  ensemble_neuralnet_test_positive_predictive_value_mean <- mean(ensemble_neuralnet_test_positive_predictive_value)
  ensemble_neuralnet_test_negative_predictive_value[i] <- ensemble_neuralnet_test_table[1, 1] / sum(ensemble_neuralnet_test_table[1, 1] + ensemble_neuralnet_test_table[1, 2])
  ensemble_neuralnet_test_negative_predictive_value_mean <- mean(ensemble_neuralnet_test_negative_predictive_value)

  ensemble_neuralnet_validation_pred <-  predict(object = ensemble_neuralnet_train_fit, newdata = ensemble_validation)
  ensemble_neuralnet_validation_pred <- ifelse(ensemble_neuralnet_validation_pred > positive_rate, 1, 0)
  ensemble_neuralnet_validation_table <- table(ensemble_neuralnet_validation_pred, ensemble_y_validation)
  ensemble_neuralnet_validation_true_positive_rate[i] <- ensemble_neuralnet_validation_table[2, 2] / sum(ensemble_neuralnet_validation_table[2, 2] + ensemble_neuralnet_validation_table[1, 2])
  ensemble_neuralnet_validation_true_positive_rate_mean <- mean(ensemble_neuralnet_validation_true_positive_rate)
  ensemble_neuralnet_validation_true_negative_rate[i] <- ensemble_neuralnet_validation_table[1, 1] / sum(ensemble_neuralnet_validation_table[1, 1] + ensemble_neuralnet_validation_table[2, 1])
  ensemble_neuralnet_validation_true_negative_rate_mean <- mean(ensemble_neuralnet_validation_true_negative_rate)
  ensemble_neuralnet_validation_false_positive_rate[i] <- ensemble_neuralnet_validation_table[2, 1] / sum(ensemble_neuralnet_validation_table[2, 1] + ensemble_neuralnet_validation_table[1, 1])
  ensemble_neuralnet_validation_false_positive_rate_mean <- mean(ensemble_neuralnet_validation_false_positive_rate)
  ensemble_neuralnet_validation_false_negative_rate[i] <- ensemble_neuralnet_validation_table[1, 2] / sum(ensemble_neuralnet_validation_table[1, 2] + ensemble_neuralnet_validation_table[2, 2])
  ensemble_neuralnet_validation_false_negative_rate_mean <- mean(ensemble_neuralnet_validation_false_negative_rate)
  ensemble_neuralnet_validation_accuracy[i] <- (ensemble_neuralnet_validation_table[1, 1] + ensemble_neuralnet_validation_table[2, 2]) / sum(ensemble_neuralnet_validation_table)
  ensemble_neuralnet_validation_accuracy_mean <- mean(ensemble_neuralnet_validation_accuracy)
  ensemble_neuralnet_validation_F1_score[i] <- 2 * (ensemble_neuralnet_validation_table[2, 2]) / sum(2 * ensemble_neuralnet_validation_table[2, 2] + ensemble_neuralnet_validation_table[1, 2] + ensemble_neuralnet_validation_table[2, 1])
  ensemble_neuralnet_validation_F1_score_mean <- mean(ensemble_neuralnet_validation_F1_score)
  ensemble_neuralnet_validation_positive_predictive_value[i] <- ensemble_neuralnet_validation_table[2, 2] / sum(ensemble_neuralnet_validation_table[2, 2] + ensemble_neuralnet_validation_table[2, 1])
  ensemble_neuralnet_validation_positive_predictive_value_mean <- mean(ensemble_neuralnet_validation_positive_predictive_value)
  ensemble_neuralnet_validation_negative_predictive_value[i] <- ensemble_neuralnet_validation_table[1, 1] / sum(ensemble_neuralnet_validation_table[1, 1] + ensemble_neuralnet_validation_table[1, 2])
  ensemble_neuralnet_validation_negative_predictive_value_mean <- mean(ensemble_neuralnet_validation_negative_predictive_value)

  ensemble_neuralnet_holdout_true_positive_rate[i] <- (ensemble_neuralnet_test_true_positive_rate[i] + ensemble_neuralnet_validation_true_positive_rate[i]) / 2
  ensemble_neuralnet_holdout_true_positive_rate_mean <- mean(ensemble_neuralnet_holdout_true_positive_rate)
  ensemble_neuralnet_holdout_true_negative_rate[i] <- (ensemble_neuralnet_test_true_negative_rate[i] + ensemble_neuralnet_validation_true_negative_rate[i]) / 2
  ensemble_neuralnet_holdout_true_negative_rate_mean <- mean(ensemble_neuralnet_holdout_true_negative_rate)
  ensemble_neuralnet_holdout_false_positive_rate[i] <- (ensemble_neuralnet_test_false_positive_rate[i] + ensemble_neuralnet_validation_false_positive_rate[i]) / 2
  ensemble_neuralnet_holdout_false_positive_rate_mean <- mean(ensemble_neuralnet_holdout_false_positive_rate)
  ensemble_neuralnet_holdout_false_negative_rate[i] <- (ensemble_neuralnet_test_false_negative_rate[i] + ensemble_neuralnet_validation_false_negative_rate[i]) / 2
  ensemble_neuralnet_holdout_false_negative_rate_mean <- mean(ensemble_neuralnet_holdout_false_negative_rate)
  ensemble_neuralnet_holdout_accuracy[i] <- (ensemble_neuralnet_test_accuracy[i] + ensemble_neuralnet_validation_accuracy[i]) / 2
  ensemble_neuralnet_holdout_accuracy_mean <- mean(ensemble_neuralnet_holdout_accuracy)
  ensemble_neuralnet_holdout_accuracy_sd <- sd(ensemble_neuralnet_holdout_accuracy)
  ensemble_neuralnet_holdout_F1_score[i] <- (ensemble_neuralnet_test_F1_score[i] + ensemble_neuralnet_validation_F1_score[i]) / 2
  ensemble_neuralnet_holdout_F1_score_mean <- mean(ensemble_neuralnet_holdout_F1_score)
  ensemble_neuralnet_holdout_positive_predictive_value[i] <- (ensemble_neuralnet_test_positive_predictive_value[i] + ensemble_neuralnet_validation_positive_predictive_value[i]) / 2
  ensemble_neuralnet_holdout_positive_predictive_value_mean <- mean(ensemble_neuralnet_holdout_positive_predictive_value)
  ensemble_neuralnet_holdout_negative_predictive_value[i] <- (ensemble_neuralnet_test_negative_predictive_value[i] + ensemble_neuralnet_validation_negative_predictive_value[i]) / 2
  ensemble_neuralnet_holdout_negative_predictive_value_mean <- mean(ensemble_neuralnet_holdout_negative_predictive_value)
  ensemble_neuralnet_holdout_overfitting[i] <- ensemble_neuralnet_holdout_accuracy[i] / ensemble_neuralnet_train_accuracy[i]
  ensemble_neuralnet_holdout_overfitting_mean <- mean(ensemble_neuralnet_holdout_overfitting)
  ensemble_neuralnet_holdout_overfitting_range <- range(ensemble_neuralnet_holdout_overfitting)
  ensemble_neuralnet_holdout_overfitting_sd <- sd(ensemble_neuralnet_holdout_overfitting)
  ensemble_neuralnet_AUC[i] <- pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_neuralnet_test_pred, ensemble_neuralnet_validation_pred)) - 1)[1]
  ensemble_neuralnet_AUC_mean <- mean(ensemble_neuralnet_AUC)

  ensemble_neuralnet_table <- ensemble_neuralnet_test_table + ensemble_neuralnet_validation_table
  ensemble_neuralnet_table_total <- ensemble_neuralnet_table_total + ensemble_neuralnet_table

  ensemble_neuralnet_end <- Sys.time()
  ensemble_neuralnet_duration[i] <- ensemble_neuralnet_end - ensemble_neuralnet_start
  ensemble_neuralnet_duration_mean <- mean(ensemble_neuralnet_duration)
  ensemble_neuralnet_duration_sd <- sd(ensemble_neuralnet_duration)


  #### 18. Ensemble_XGBoost ####
  ensemble_xgb_start <- Sys.time()
  message("Working on Ensembles using XGBoost (XGB)")

  ensemble_train_x = data.matrix(ensemble_train[, 1 : ncol(ensemble_train)])
  ensemble_train_y = ensemble_train[,ncol(ensemble_train) : ncol(ensemble_train)]

  #define predictor and response variables in ensemble_test set
  ensemble_test_x = data.matrix(ensemble_test[, 1 : ncol(ensemble_test)])
  ensemble_test_y = ensemble_test[, ncol(ensemble_test) : ncol(ensemble_test)]

  #define predictor and response variables in ensemble_validation set
  ensemble_validation_x = data.matrix(ensemble_validation[, 1 : ncol(ensemble_validation)])
  ensemble_validation_y = ensemble_validation[, ncol(ensemble_validation): ncol(ensemble_validation)]

  #define final ensemble_train, ensemble_test and ensemble_validation sets
  ensemble_xgb_train <-  xgboost::xgb.DMatrix(data = ensemble_train_x, label = as.matrix(ensemble_train_y))
  ensemble_xgb_test <- xgboost::xgb.DMatrix(data = ensemble_test_x, label = as.matrix(ensemble_test_y))
  ensemble_xgb_validation <-  xgboost::xgb.DMatrix(data = ensemble_validation_x, label = as.matrix(ensemble_validation_y))

  #define watchlist
  watchlist = list(ensemble_train = ensemble_xgb_train, ensemble_validation=ensemble_xgb_validation)
  watchlist_ensemble_test <- list(ensemble_train = ensemble_xgb_train, ensemble_test = ensemble_xgb_test)
  watchlist_ensemble_validation <- list(ensemble_train = ensemble_xgb_train, ensemble_validation = ensemble_xgb_validation)

  ensemble_xgb_model <- xgboost::xgb.train(data = ensemble_xgb_train, evals = watchlist_ensemble_test, nrounds = 70)

  ensemble_xgb_min = which.min(ensemble_xgb_model$evaluation_log$ensemble_validation_rmse)

  ensemble_xgb_train_pred <- predict(object = ensemble_xgb_model, newdata = ensemble_train)
  ensemble_xgb_train_pred <- ifelse(ensemble_xgb_train_pred > positive_rate, 1, 0)
  ensemble_xgb_train_table <- table(ensemble_xgb_train_pred, ensemble_y_train)
  ensemble_xgb_train_true_positive_rate[i] <- ensemble_xgb_train_table[2, 2] / sum(ensemble_xgb_train_table[2, 2] + ensemble_xgb_train_table[1, 2])
  ensemble_xgb_train_true_positive_rate_mean <- mean(ensemble_xgb_train_true_positive_rate)
  ensemble_xgb_train_true_negative_rate[i] <- ensemble_xgb_train_table[1, 1] / sum(ensemble_xgb_train_table[1, 1] + ensemble_xgb_train_table[2, 1])
  ensemble_xgb_train_true_negative_rate_mean <- mean(ensemble_xgb_train_true_negative_rate)
  ensemble_xgb_train_false_positive_rate[i] <- ensemble_xgb_train_table[2, 1] / sum(ensemble_xgb_train_table[2, 1] + ensemble_xgb_train_table[1, 1])
  ensemble_xgb_train_false_positive_rate_mean <- mean(ensemble_xgb_train_false_positive_rate)
  ensemble_xgb_train_false_negative_rate[i] <- ensemble_xgb_train_table[1, 2] / sum(ensemble_xgb_train_table[1, 2] + ensemble_xgb_train_table[2, 2])
  ensemble_xgb_train_false_negative_rate_mean <- mean(ensemble_xgb_train_false_negative_rate)
  ensemble_xgb_train_accuracy[i] <- (ensemble_xgb_train_table[1, 1] + ensemble_xgb_train_table[2, 2]) / sum(ensemble_xgb_train_table)
  ensemble_xgb_train_accuracy_mean <- mean(ensemble_xgb_train_accuracy)
  ensemble_xgb_train_F1_score[i] <- 2 * (ensemble_xgb_train_table[2, 2]) / sum(2 * ensemble_xgb_train_table[2, 2] + ensemble_xgb_train_table[1, 2] + ensemble_xgb_train_table[2, 1])
  ensemble_xgb_train_F1_score_mean <- mean(ensemble_xgb_train_F1_score)
  ensemble_xgb_train_positive_predictive_value[i] <- ensemble_xgb_train_table[2, 2] / sum(ensemble_xgb_train_table[2, 2] + ensemble_xgb_train_table[2, 1])
  ensemble_xgb_train_positive_predictive_value_mean <- mean(ensemble_xgb_train_positive_predictive_value)
  ensemble_xgb_train_negative_predictive_value[i] <- ensemble_xgb_train_table[1, 1] / sum(ensemble_xgb_train_table[1, 1] + ensemble_xgb_train_table[1, 2])
  ensemble_xgb_train_negative_predictive_value_mean <- mean(ensemble_xgb_train_negative_predictive_value)

  ensemble_xgb_test_pred <-  predict(object = ensemble_xgb_model, newdata = ensemble_test)
  ensemble_xgb_test_pred <- ifelse(ensemble_xgb_test_pred > positive_rate, 1, 0)
  ensemble_xgb_test_table <- table(ensemble_xgb_test_pred, ensemble_y_test)
  ensemble_xgb_test_true_positive_rate[i] <- ensemble_xgb_test_table[2, 2] / sum(ensemble_xgb_test_table[2, 2] + ensemble_xgb_test_table[1, 2])
  ensemble_xgb_test_true_positive_rate_mean <- mean(ensemble_xgb_test_true_positive_rate)
  ensemble_xgb_test_true_negative_rate[i] <- ensemble_xgb_test_table[1, 1] / sum(ensemble_xgb_test_table[1, 1] + ensemble_xgb_test_table[2, 1])
  ensemble_xgb_test_true_negative_rate_mean <- mean(ensemble_xgb_test_true_negative_rate)
  ensemble_xgb_test_false_positive_rate[i] <- ensemble_xgb_test_table[2, 1] / sum(ensemble_xgb_test_table[2, 1] + ensemble_xgb_test_table[1, 1])
  ensemble_xgb_test_false_positive_rate_mean <- mean(ensemble_xgb_test_false_positive_rate)
  ensemble_xgb_test_false_negative_rate[i] <- ensemble_xgb_test_table[1, 2] / sum(ensemble_xgb_test_table[1, 2] + ensemble_xgb_test_table[2, 2])
  ensemble_xgb_test_false_negative_rate_mean <- mean(ensemble_xgb_test_false_negative_rate)
  ensemble_xgb_test_accuracy[i] <- (ensemble_xgb_test_table[1, 1] + ensemble_xgb_test_table[2, 2]) / sum(ensemble_xgb_test_table)
  ensemble_xgb_test_accuracy_mean <- mean(ensemble_xgb_test_accuracy)
  ensemble_xgb_test_F1_score[i] <- 2 * (ensemble_xgb_test_table[2, 2]) / sum(2 * ensemble_xgb_test_table[2, 2] + ensemble_xgb_test_table[1, 2] + ensemble_xgb_test_table[2, 1])
  ensemble_xgb_test_F1_score_mean <- mean(ensemble_xgb_test_F1_score)
  ensemble_xgb_test_positive_predictive_value[i] <- ensemble_xgb_test_table[2, 2] / sum(ensemble_xgb_test_table[2, 2] + ensemble_xgb_test_table[2, 1])
  ensemble_xgb_test_positive_predictive_value_mean <- mean(ensemble_xgb_test_positive_predictive_value)
  ensemble_xgb_test_negative_predictive_value[i] <- ensemble_xgb_test_table[1, 1] / sum(ensemble_xgb_test_table[1, 1] + ensemble_xgb_test_table[1, 2])
  ensemble_xgb_test_negative_predictive_value_mean <- mean(ensemble_xgb_test_negative_predictive_value)

  ensemble_xgb_validation_pred <-  predict(object = ensemble_xgb_model, newdata = ensemble_validation)
  ensemble_xgb_validation_pred <- ifelse(ensemble_xgb_validation_pred > positive_rate, 1, 0)
  ensemble_xgb_validation_table <- table(ensemble_xgb_validation_pred, ensemble_y_validation)
  ensemble_xgb_validation_true_positive_rate[i] <- ensemble_xgb_validation_table[2, 2] / sum(ensemble_xgb_validation_table[2, 2] + ensemble_xgb_validation_table[1, 2])
  ensemble_xgb_validation_true_positive_rate_mean <- mean(ensemble_xgb_validation_true_positive_rate)
  ensemble_xgb_validation_true_negative_rate[i] <- ensemble_xgb_validation_table[1, 1] / sum(ensemble_xgb_validation_table[1, 1] + ensemble_xgb_validation_table[2, 1])
  ensemble_xgb_validation_true_negative_rate_mean <- mean(ensemble_xgb_validation_true_negative_rate)
  ensemble_xgb_validation_false_positive_rate[i] <- ensemble_xgb_validation_table[2, 1] / sum(ensemble_xgb_validation_table[2, 1] + ensemble_xgb_validation_table[1, 1])
  ensemble_xgb_validation_false_positive_rate_mean <- mean(ensemble_xgb_validation_false_positive_rate)
  ensemble_xgb_validation_false_negative_rate[i] <- ensemble_xgb_validation_table[1, 2] / sum(ensemble_xgb_validation_table[1, 2] + ensemble_xgb_validation_table[2, 2])
  ensemble_xgb_validation_false_negative_rate_mean <- mean(ensemble_xgb_validation_false_negative_rate)
  ensemble_xgb_validation_accuracy[i] <- (ensemble_xgb_validation_table[1, 1] + ensemble_xgb_validation_table[2, 2]) / sum(ensemble_xgb_validation_table)
  ensemble_xgb_validation_accuracy_mean <- mean(ensemble_xgb_validation_accuracy)
  ensemble_xgb_validation_F1_score[i] <- 2 * (ensemble_xgb_validation_table[2, 2]) / sum(2 * ensemble_xgb_validation_table[2, 2] + ensemble_xgb_validation_table[1, 2] + ensemble_xgb_validation_table[2, 1])
  ensemble_xgb_validation_F1_score_mean <- mean(ensemble_xgb_validation_F1_score)
  ensemble_xgb_validation_positive_predictive_value[i] <- ensemble_xgb_validation_table[2, 2] / sum(ensemble_xgb_validation_table[2, 2] + ensemble_xgb_validation_table[2, 1])
  ensemble_xgb_validation_positive_predictive_value_mean <- mean(ensemble_xgb_validation_positive_predictive_value)
  ensemble_xgb_validation_negative_predictive_value[i] <- ensemble_xgb_validation_table[1, 1] / sum(ensemble_xgb_validation_table[1, 1] + ensemble_xgb_validation_table[1, 2])
  ensemble_xgb_validation_negative_predictive_value_mean <- mean(ensemble_xgb_validation_negative_predictive_value)

  ensemble_xgb_holdout_true_positive_rate[i] <- (ensemble_xgb_test_true_positive_rate[i] + ensemble_xgb_validation_true_positive_rate[i]) / 2
  ensemble_xgb_holdout_true_positive_rate_mean <- mean(ensemble_xgb_holdout_true_positive_rate)
  ensemble_xgb_holdout_true_negative_rate[i] <- (ensemble_xgb_test_true_negative_rate[i] + ensemble_xgb_validation_true_negative_rate[i]) / 2
  ensemble_xgb_holdout_true_negative_rate_mean <- mean(ensemble_xgb_holdout_true_negative_rate)
  ensemble_xgb_holdout_false_positive_rate[i] <- (ensemble_xgb_test_false_positive_rate[i] + ensemble_xgb_validation_false_positive_rate[i]) / 2
  ensemble_xgb_holdout_false_positive_rate_mean <- mean(ensemble_xgb_holdout_false_positive_rate)
  ensemble_xgb_holdout_false_negative_rate[i] <- (ensemble_xgb_test_false_negative_rate[i] + ensemble_xgb_validation_false_negative_rate[i]) / 2
  ensemble_xgb_holdout_false_negative_rate_mean <- mean(ensemble_xgb_holdout_false_negative_rate)
  ensemble_xgb_holdout_accuracy[i] <- (ensemble_xgb_test_accuracy[i] + ensemble_xgb_validation_accuracy[i]) / 2
  ensemble_xgb_holdout_accuracy_mean <- mean(ensemble_xgb_holdout_accuracy)
  ensemble_xgb_holdout_accuracy_sd <- sd(ensemble_xgb_holdout_accuracy)
  ensemble_xgb_holdout_F1_score[i] <- (ensemble_xgb_test_F1_score[i] + ensemble_xgb_validation_F1_score[i]) / 2
  ensemble_xgb_holdout_F1_score_mean <- mean(ensemble_xgb_holdout_F1_score)
  ensemble_xgb_holdout_positive_predictive_value[i] <- (ensemble_xgb_test_positive_predictive_value[i] + ensemble_xgb_validation_positive_predictive_value[i]) / 2
  ensemble_xgb_holdout_positive_predictive_value_mean <- mean(ensemble_xgb_holdout_positive_predictive_value)
  ensemble_xgb_holdout_negative_predictive_value[i] <- (ensemble_xgb_test_negative_predictive_value[i] + ensemble_xgb_validation_negative_predictive_value[i]) / 2
  ensemble_xgb_holdout_negative_predictive_value_mean <- mean(ensemble_xgb_holdout_negative_predictive_value)
  ensemble_xgb_holdout_overfitting[i] <- ensemble_xgb_holdout_accuracy[i] / ensemble_xgb_train_accuracy[i]
  ensemble_xgb_holdout_overfitting_mean <- mean(ensemble_xgb_holdout_overfitting)
  ensemble_xgb_holdout_overfitting_range <- range(ensemble_xgb_holdout_overfitting)
  ensemble_xgb_holdout_overfitting_sd <- sd(ensemble_xgb_holdout_overfitting)
  ensemble_xgb_AUC[i] <- pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_xgb_test_pred, ensemble_xgb_validation_pred)) - 1)[1]
  ensemble_xgb_AUC_mean <- mean(ensemble_xgb_AUC)

  ensemble_xgb_table <- ensemble_xgb_test_table + ensemble_xgb_validation_table
  ensemble_xgb_table_total <- ensemble_xgb_table + ensemble_xgb_table_total

  ensemble_xgb_end <- Sys.time()
  ensemble_xgb_duration[i] = ensemble_xgb_end - ensemble_xgb_start
  ensemble_xgb_duration_mean <- mean(ensemble_xgb_duration)
  ensemble_xgb_duration_sd <- sd(ensemble_xgb_duration)
}


#### ROC Curves start here ####

elastic_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(elastic_test_pred, elastic_validation_pred)))
elastic_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(elastic_test_predictions, elastic_validation_predictions)) - 1)), 4)
elastic_ROC <- pROC::ggroc(elastic_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Elastic Analysis ", "(AUC = ", elastic_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

fda_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(fda_test_pred, fda_validation_pred)))
fda_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(fda_test_pred, fda_validation_pred)) - 1)), 4)
fda_ROC <- pROC::ggroc(fda_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Flexible Discriminant Analysis ", "(AUC = ", fda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

gam_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(gam_test_pred, gam_validation_pred)))
gam_auc <- round((pROC::auc(c(test$y, validation$y), as.numeric(c(gam_test_predictions, gam_validation_predictions)) - 1)), 4)
gam_ROC <- pROC::ggroc(gam_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Generalized Additve Models ", "(AUC = ", gam_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

gb_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(gb_test_pred, gb_validation_pred)))
gb_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(gb_test_predictions, gb_validation_predictions)) - 1)), 4)
gb_ROC <- pROC::ggroc(gb_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Gradient Boosted Models ", "(AUC = ", gb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

glm_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), c(glmnet_test_pred, glmnet_validation_pred))
glm_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(glmnet_test_predictions, glmnet_validation_predictions)) - 1)), 4)
glm_ROC <- pROC::ggroc(glm_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Generalized Linear Models ", "(AUC = ", glm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

neuralnet_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(neuralnet_test_pred, neuralnet_validation_pred)))
neuralnet_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(neuralnet_test_predictions, neuralnet_validation_predictions)) - 1)), 4)
neuralnet_ROC <- pROC::ggroc(neuralnet_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Neuralnet Analysis ", "(AUC = ", neuralnet_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

xgb_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(xgb_test_pred, xgb_validation_pred)))
xgb_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(xgb_test_predictions, xgb_validation_predictions)) - 1)), 4)
xgb_ROC <- pROC::ggroc(xgb_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("XGBoost ", "(AUC = ", xgb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_C50_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_C50_test_pred, ensemble_C50_validation_pred)))
ensemble_C50_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_C50_test_pred, ensemble_C50_validation_pred)) - 1)), 4)
ensemble_C50_ROC <- pROC::ggroc(ensemble_C50_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Ensemble C50 ", "(AUC = ", ensemble_C50_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_elastic_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_elastic_test_pred, ensemble_elastic_validation_pred)))
ensemble_elastic_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_elastic_test_pred, ensemble_elastic_validation_pred)) - 1)), 4)
ensemble_elastic_ROC <- pROC::ggroc(ensemble_elastic_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Ensemble Elastic ", "(AUC = ", ensemble_elastic_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_glm_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_glmnet_test_pred, ensemble_glmnet_validation_pred)))
ensemble_glm_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_glmnet_test_pred, ensemble_glmnet_validation_pred)) - 1)), 4)
ensemble_glm_ROC <- pROC::ggroc(ensemble_glm_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Ensemble GLM ", "(AUC = ", ensemble_glm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_neuralnet_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_neuralnet_test_pred, ensemble_neuralnet_validation_pred)))
ensemble_neuralnet_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_neuralnet_test_pred, ensemble_neuralnet_validation_pred)) - 1)), 4)
ensemble_neuralnet_ROC <- pROC::ggroc(ensemble_neuralnet_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Ensemble Neuralnet ", "(AUC = ", ensemble_neuralnet_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_xgb_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_xgb_test_pred, ensemble_xgb_validation_pred)))
ensemble_xgb_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_xgb_test_pred, ensemble_xgb_validation_pred)) - 1)), 4)
ensemble_xgb_ROC <- pROC::ggroc(ensemble_xgb_roc_obj, color = "steelblue", linewidth = 2) +
  ggplot2::ggtitle(paste0("Ensemble XGBoost ", "(AUC = ", ensemble_xgb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")



ROC_curves <- gridExtra::grid.arrange(elastic_ROC, fda_ROC, gam_ROC,
                                      gb_ROC,
                                      glm_ROC,
                                      neuralnet_ROC,
                                      xgb_ROC,
                                      ensemble_C50_ROC,
                                      ensemble_elastic_ROC,
                                      ensemble_glm_ROC,
                                      ensemble_neuralnet_ROC,
                                      ensemble_xgb_ROC,
                                      ncol = 3
)

ROC_curves <- ggplotify::as.ggplot(ROC_curves)

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("ROC_curves.eps", plot = ROC_curves, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("ROC_curves.jpeg", plot = ROC_curves, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("ROC_curves.pdf", plot = ROC_curves, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("ROC_curves.png", plot = ROC_curves, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("ROC_curves.svg", plot = ROC_curves, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("ROC_curves.tiff", plot = ROC_curves, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### AUC data and plots start here ####

AUC_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Elastic", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Gradient Boosted", numresamples),
    rep("Neuralnet", numresamples),
    rep("XGBoost", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Elastic", numresamples),
    rep("Ensemble GLM", numresamples),
    rep("Ensemble Neuralnet", numresamples),
    rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    elastic_AUC, fda_AUC, gam_AUC,
    glmnet_AUC,
    gb_AUC,
    neuralnet_AUC,
    xgb_AUC,
    ensemble_C50_AUC,
    ensemble_elastic_AUC,
    ensemble_glmnet_AUC,
    ensemble_neuralnet_AUC,
    ensemble_xgb_AUC
  ),
  "mean" = rep(c(
    elastic_AUC_mean, fda_AUC_mean, gam_AUC_mean,
    glmnet_AUC_mean,
    gb_AUC_mean,
    neuralnet_AUC_mean,
    xgb_AUC_mean,
    ensemble_C50_AUC_mean,
    ensemble_elastic_AUC_mean,
    ensemble_glmnet_AUC_mean,
    ensemble_neuralnet_AUC_mean,
    ensemble_xgb_AUC_mean
  ), each = numresamples)
)

AUC_plot_fixed_scales <- ggplot2::ggplot(data = AUC_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3 , scales = "fixed") +
  ggplot2::ggtitle("AUC by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "AUC by model fixed scales, higher is better \n The black horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("AUC_plot_fixed_scales.eps", plot = AUC_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("AUC_plot_fixed_scales.jpeg", plot = AUC_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("AUC_plot_fixed_scales.pdf", plot = AUC_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("AUC_plot_fixed_scales.png", plot = AUC_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("AUC_plot_fixed_scales.svg", plot = AUC_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("AUC_plot_fixed_scales.tiff", plot = AUC_plot_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

AUC_plot_free_scales <- ggplot2::ggplot(data = AUC_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free") +
  ggplot2::ggtitle("AUC by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "AUC by model free scales, higher is better \n The black horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("AUC_plot_free_scales.eps", plot = AUC_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("AUC_plot_free_scales.jpeg", plot = AUC_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("AUC_plot_free_scales.pdf", plot = AUC_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("AUC_plot_free_scales.png", plot = AUC_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("AUC_plot_free_scales.svg", plot = AUC_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("AUC_plot_free_scales.tiff", plot = AUC_plot_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Holdout results (tables and plots) start here ####

holdout_results <- data.frame(
  Model = c(
    "Elastic", "Flexible Discriminant Analysis",
    "Generalized Additive Models", "Generalized Linear Models",
    "Gradient Boosted",
    "Neuralnet",
    "XGBoost",
    "Ensemble C50",
    "Ensemble Elastic",
    "Ensemble GLM",
    "Ensemble Neuralnet",
    "Ensemble XGBoost"
  ),
  "Area_Under_Curve" = c(
    elastic_auc, fda_auc,
    gam_auc, glm_auc,
    gb_auc,
    neuralnet_auc,
    xgb_auc,
    ensemble_C50_auc,
    ensemble_elastic_auc,
    ensemble_glm_auc,
    ensemble_neuralnet_auc,
    ensemble_xgb_auc
  ),
  "True_Positive_Rate_aka_Sensitivity" = round(c(
    elastic_holdout_true_positive_rate_mean, fda_holdout_true_positive_rate_mean,
    gam_holdout_true_positive_rate_mean, glmnet_holdout_true_positive_rate_mean,
    gb_holdout_true_positive_rate_mean,
    neuralnet_holdout_true_positive_rate_mean,
    xgb_holdout_true_positive_rate_mean,
    ensemble_C50_holdout_true_positive_rate_mean,
    ensemble_elastic_holdout_true_positive_rate_mean,
    ensemble_glmnet_holdout_true_positive_rate_mean,
    ensemble_neuralnet_holdout_true_positive_rate_mean,
    ensemble_xgb_holdout_true_positive_rate_mean
  ), 4),
  "True_Negative_Rate_aka_Specificity" = round(c(
    elastic_holdout_true_positive_rate_mean, fda_holdout_true_negative_rate_mean,
    gam_holdout_true_negative_rate_mean, glmnet_holdout_true_negative_rate_mean,
    gb_holdout_true_negative_rate_mean,
    neuralnet_holdout_true_negative_rate_mean,
    xgb_holdout_true_negative_rate_mean,
    ensemble_C50_holdout_true_negative_rate_mean,
    ensemble_elastic_holdout_true_negative_rate_mean,
    ensemble_glmnet_holdout_true_negative_rate_mean,
    ensemble_neuralnet_holdout_true_negative_rate_mean,
    ensemble_xgb_holdout_true_negative_rate_mean
  ), 4),
  "False_Positive_Rate_aka_Type_I_Error" = round(c(
    elastic_holdout_false_positive_mean, fda_holdout_false_positive_rate_mean,
    gam_holdout_false_positive_rate_mean, glmnet_holdout_false_positive_rate_mean,
    gb_holdout_false_positive_rate_mean,
    neuralnet_holdout_false_positive_rate_mean,
    xgb_holdout_false_positive_rate_mean,
    ensemble_C50_holdout_false_positive_rate_mean,
    ensemble_elastic_holdout_false_positive_rate_mean,
    ensemble_glmnet_holdout_false_positive_rate_mean,
    ensemble_neuralnet_holdout_false_positive_rate_mean,
    ensemble_xgb_holdout_false_positive_rate_mean
  ), 4),
  "False_Negative_Rate_aka_Type_II_Error" = round(c(
    elastic_holdout_false_negative_rate_mean, fda_holdout_false_negative_rate_mean,
    gam_holdout_false_negative_rate_mean, glmnet_holdout_false_negative_rate_mean,
    gb_holdout_false_negative_rate_mean,
    neuralnet_holdout_false_negative_rate_mean,
    xgb_holdout_false_negative_rate_mean,
    ensemble_C50_holdout_false_negative_rate_mean,
    ensemble_elastic_holdout_false_negative_rate_mean,
    ensemble_glmnet_holdout_false_negative_rate_mean,
    ensemble_neuralnet_holdout_false_negative_rate_mean,
    ensemble_xgb_holdout_false_negative_rate_mean
  ), 4),
  "Positive_Predictive_Value_aka_Precision" = round(c(
    elastic_holdout_positive_predictive_value_mean, fda_holdout_positive_predictive_value_mean,
    gam_holdout_positive_predictive_value_mean, glmnet_holdout_positive_predictive_value_mean,
    gb_holdout_positive_predictive_value_mean,
    neuralnet_holdout_positive_predictive_value_mean,
    xgb_holdout_positive_predictive_value_mean,
    ensemble_C50_holdout_positive_predictive_value_mean,
    ensemble_elastic_holdout_positive_predictive_value_mean,
    ensemble_glmnet_holdout_positive_predictive_value_mean,
    ensemble_neuralnet_holdout_positive_predictive_value_mean,
    ensemble_xgb_holdout_positive_predictive_value_mean
  ), 4),
  "Negative_Predictive_Value" = round(c(
    elastic_holdout_negative_predictive_value_mean, fda_holdout_negative_predictive_value_mean,
    gam_holdout_negative_predictive_value_mean, glmnet_holdout_negative_predictive_value_mean,
    gb_holdout_negative_predictive_value_mean,
    neuralnet_holdout_negative_predictive_value_mean,
    xgb_holdout_negative_predictive_value_mean,
    ensemble_C50_holdout_negative_predictive_value_mean,
    ensemble_elastic_holdout_negative_predictive_value_mean,
    ensemble_glmnet_holdout_negative_predictive_value_mean,
    ensemble_neuralnet_holdout_negative_predictive_value_mean,
    ensemble_xgb_holdout_negative_predictive_value_mean
  ), 4),
  "F1_Score" = round(c(
    elastic_holdout_F1_score_mean, fda_holdout_F1_score_mean,
    gam_holdout_F1_score_mean, glmnet_holdout_F1_score_mean,
    gb_holdout_F1_score_mean,
    neuralnet_holdout_F1_score_mean,
    xgb_holdout_F1_score_mean,
    ensemble_C50_holdout_F1_score_mean,
    ensemble_elastic_holdout_F1_score_mean,
    ensemble_glmnet_holdout_F1_score_mean,
    ensemble_neuralnet_holdout_F1_score_mean,
    ensemble_xgb_holdout_F1_score_mean
  ), 4),
  "Duration" = round(c(
    elastic_duration_mean, fda_duration_mean,
    gam_duration_mean, glmnet_duration_mean,
    gb_duration_mean,
    neuralnet_duration_mean,
    xgb_duration_mean,
    ensemble_C50_duration_mean,
    ensemble_elastic_duration_mean,
    ensemble_glmnet_duration_mean,
    ensemble_neuralnet_duration_mean,
    ensemble_xgb_duration_mean
  ), 4),
  "Duration_sd" = round(c(
    elastic_duration_sd, fda_duration_sd,
    gam_duration_sd, glmnet_duration_sd,
    gb_duration_sd,
    neuralnet_duration_sd,
    xgb_duration_sd,
    ensemble_C50_duration_sd,
    ensemble_elastic_duration_sd,
    ensemble_glmnet_duration_sd,
    ensemble_neuralnet_duration_sd,
    ensemble_xgb_duration_sd
  ), 4)
)

holdout_results <- holdout_results %>% dplyr::arrange(dplyr::desc(Area_Under_Curve))

holdout_results_final <- reactable::reactable(holdout_results,
                                              searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                              striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "holdout_results_final")
)

holdout_results_final <- htmlwidgets::prependContent(holdout_results_final, htmltools::h2(class = "title", "Holdout results"))


#### True Positive Rate data starts here ####

true_positive_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Elastic", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Gradient Boosted", numresamples),
    rep("Neuralnet", numresamples),
    rep("XGBoost", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Elastic", numresamples),
    rep("Ensemble GLM", numresamples),
    rep("Ensemble Neuralnet",numresamples),
    rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    elastic_holdout_true_positive_rate, fda_holdout_true_positive_rate, gam_holdout_true_positive_rate,
    glmnet_holdout_true_positive_rate,
    gb_holdout_true_positive_rate,
    neuralnet_holdout_true_positive_rate,
    xgb_holdout_true_positive_rate,
    ensemble_C50_holdout_true_positive_rate,
    ensemble_elastic_holdout_true_positive_rate,
    ensemble_glmnet_holdout_true_positive_rate,
    ensemble_neuralnet_holdout_true_positive_rate,
    ensemble_xgb_holdout_true_negative_rate
  ),
  "mean" = rep(c(
    elastic_holdout_true_positive_rate_mean, fda_holdout_true_positive_rate_mean, gam_holdout_true_positive_rate_mean,
    glmnet_holdout_true_positive_rate_mean,
    gb_holdout_true_positive_rate_mean,
    neuralnet_holdout_true_positive_rate_mean,
    xgb_holdout_true_positive_rate_mean,
    ensemble_C50_holdout_true_positive_rate_mean,
    ensemble_elastic_holdout_true_positive_rate_mean,
    ensemble_glmnet_holdout_true_positive_rate_mean,
    ensemble_neuralnet_holdout_true_positive_rate_mean,
    ensemble_xgb_holdout_true_negative_rate_mean
  ), each = numresamples)
)

true_positive_rate_fixed_scales <- ggplot2::ggplot(data = true_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "fixed") +
  ggplot2::ggtitle("True Positive Rate by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True Positive Rate by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_positive_rate_fixed_scales.eps", plot = true_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_positive_rate_fixed_scales.jpeg", plot = true_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_positive_rate_fixed_scales.pdf", plot = true_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_positive_rate_fixed_scales.png", plot = true_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_positive_rate_fixed_scales.svg", plot = true_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_positive_rate_fixed_scales.tiff", plot = true_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

true_positive_rate_free_scales <- ggplot2::ggplot(data = true_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free") +
  ggplot2::ggtitle("True positive rate by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True positive rate by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_positive_rate_free_scales.eps", plot = true_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_positive_rate_free_scales.jpeg", plot = true_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_positive_rate_free_scales.pdf", plot = true_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_positive_rate_free_scales.png", plot = true_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_positive_rate_free_scales.svg", plot = true_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_positive_rate_free_scales.tiff", plot = true_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### True Negative Rate data starts here ####

true_negative_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Elastic", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Gradient Boosted", numresamples),
    rep("Neuralnet", numresamples),
    rep("XGBoost", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Elastic", numresamples),
    rep("Ensemble GLM", numresamples),
    rep("Ensemble Neuralnet", numresamples),
    rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    elastic_holdout_true_negative_rate, fda_holdout_true_negative_rate, gam_holdout_true_negative_rate,
    glmnet_holdout_true_negative_rate,
    gb_holdout_true_negative_rate,
    neuralnet_holdout_true_negative_rate,
    xgb_holdout_true_negative_rate,
    ensemble_C50_holdout_true_negative_rate,
    ensemble_elastic_holdout_true_negative_rate,
    ensemble_glmnet_holdout_true_negative_rate,
    ensemble_neuralnet_holdout_true_negative_rate,
    ensemble_xgb_holdout_true_negative_rate
  ),
  "mean" = rep(c(
    elastic_holdout_true_negative_rate_mean, fda_holdout_true_negative_rate_mean, gam_holdout_true_negative_rate_mean,
    glmnet_holdout_true_negative_rate_mean,
    gb_holdout_true_negative_rate_mean,
    neuralnet_holdout_true_negative_rate_mean,
    xgb_holdout_true_negative_rate_mean,
    ensemble_C50_holdout_true_negative_rate_mean,
    ensemble_elastic_holdout_true_negative_rate_mean,
    ensemble_glmnet_holdout_true_negative_rate_mean,
    ensemble_neuralnet_holdout_true_negative_rate_mean,
    ensemble_xgb_holdout_true_negative_rate_mean
  ), each = numresamples)
)

true_negative_rate_fixed_scales <- ggplot2::ggplot(data = true_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "fixed") +
  ggplot2::ggtitle("True negative rate by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True negative rate by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_negative_rate_fixed_scales.eps", plot = true_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_negative_rate_fixed_scales.jpeg", plot = true_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_negative_rate_fixed_scales.pdf", plot = true_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_negative_rate_fixed_scales.png", plot = true_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_negative_rate_fixed_scales.svg", plot = true_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_negative_rate_fixed_scales.tiff", plot = true_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

true_negative_rate_free_scales <- ggplot2::ggplot(data = true_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free") +
  ggplot2::ggtitle("True negative rate by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True negative rate by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_negative_rate_free_scales.eps", plot = true_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_negative_rate_free_scales.jpeg", plot = true_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_negative_rate_free_scales.pdf", plot = true_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_negative_rate_free_scales.png", plot = true_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_negative_rate_free_scales.svg", plot = true_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_negative_rate_free_scales.tiff", plot = true_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### False Positive Rate data starts here ####

false_positive_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Elastic", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Gradient Boosted", numresamples),
    rep("Neuranlet", numresamples),
    rep("XGBoost", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Elastic", numresamples),
    rep("Ensemble GLM", numresamples),
    rep("Ensemble Neuralnet", numresamples),
    rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    elastic_holdout_false_positive_rate, fda_holdout_false_positive_rate, gam_holdout_false_positive_rate,
    glmnet_holdout_false_positive_rate,
    gb_holdout_false_positive_rate,
    neuralnet_holdout_false_positive_rate,
    xgb_holdout_false_positive_rate,
    ensemble_C50_holdout_false_positive_rate,
    ensemble_elastic_holdout_false_positive_rate,
    ensemble_glmnet_holdout_false_positive_rate,
    ensemble_neuralnet_holdout_false_positive_rate,
    ensemble_xgb_holdout_false_positive_rate
  ),
  "mean" = rep(c(
    elastic_holdout_false_positive_rate_mean, fda_holdout_false_positive_rate_mean, gam_holdout_false_positive_rate_mean,
    glmnet_holdout_false_positive_rate_mean,
    gb_holdout_false_positive_rate_mean,
    neuralnet_holdout_false_positive_rate_mean,
    xgb_holdout_false_positive_rate_mean,
    ensemble_C50_holdout_false_positive_rate_mean,
    ensemble_elastic_holdout_false_positive_rate_mean,
    ensemble_glmnet_holdout_false_positive_rate_mean,
    ensemble_neuralnet_holdout_false_positive_rate_mean,
    ensemble_xgb_holdout_false_positive_rate_mean
  ), each = numresamples)
)

false_positive_rate_fixed_scales <- ggplot2::ggplot(data = false_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "fixed") +
  ggplot2::ggtitle("False positive rate by model fixed scales, lower is better. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "False positive rate by model fixed scales, lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_positive_rate_fixed_scales.eps", plot = false_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_positive_rate_fixed_scales.jpeg", plot = false_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_positive_rate_fixed_scales.pdf", plot = false_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_positive_rate_fixed_scales.png", plot = false_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_positive_rate_fixed_scales.svg", plot = false_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_positive_rate_fixed_scales.tiff", plot = false_positive_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

false_positive_rate_free_scales <- ggplot2::ggplot(data = false_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free") +
  ggplot2::ggtitle("False positive rate by model free scales, lower is better. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "False positive rate by model free scales, lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_positive_rate_free_scales.eps", plot = false_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_positive_rate_free_scales.jpeg", plot = false_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_positive_rate_free_scales.pdf", plot = false_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_positive_rate_free_scales.png", plot = false_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_positive_rate_free_scales.svg", plot = false_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_positive_rate_free_scales.tiff", plot = false_positive_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### False Negative Rate data starts here ####

false_negative_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Elastic", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Gradient Boosted", numresamples),
    rep("Neuralnet",numresamples),
    rep("XGBoost", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Elastic", numresamples),
    rep("Ensemble GLM", numresamples),
    rep("Ensemble Neuralnet", numresamples),
    rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    elastic_holdout_false_negative_rate, fda_holdout_false_negative_rate, gam_holdout_false_negative_rate,
    glmnet_holdout_false_negative_rate,
    gb_holdout_false_negative_rate,
    neuralnet_holdout_false_negative_rate,
    xgb_holdout_false_negative_rate,
    ensemble_C50_holdout_false_negative_rate,
    ensemble_elastic_holdout_false_negative_rate,
    ensemble_glmnet_holdout_false_negative_rate,
    ensemble_neuralnet_holdout_false_negative_rate,
    ensemble_xgb_holdout_false_negative_rate
  ),
  "mean" = rep(c(
    elastic_holdout_false_negative_rate_mean, fda_holdout_false_negative_rate_mean, gam_holdout_false_negative_rate_mean,
    glmnet_holdout_false_negative_rate_mean,
    gb_holdout_false_negative_rate_mean,
    neuralnet_holdout_false_negative_rate_mean,
    xgb_holdout_false_negative_rate_mean,
    ensemble_C50_holdout_false_negative_rate_mean,
    ensemble_elastic_holdout_false_negative_rate_mean,
    ensemble_glmnet_holdout_false_negative_rate_mean,
    ensemble_neuralnet_holdout_false_negative_rate_mean,
    ensemble_xgb_holdout_false_negative_rate_mean
  ), each = numresamples)
)

false_negative_rate_fixed_scales <- ggplot2::ggplot(data = false_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "fixed") +
  ggplot2::ggtitle("False negative rate by model fixed scales, lower is better. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "False negative rate by model fixed scales, lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_negative_rate_fixed_scales.eps", plot = false_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_negative_rate_fixed_scales.jpeg", plot = false_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_negative_rate_fixed_scales.pdf", plot = false_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_negative_rate_fixed_scales.png", plot = false_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_negative_rate_fixed_scales.svg", plot = false_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_negative_rate_fixed_scales.tiff", plot = false_negative_rate_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

false_negative_rate_free_scales <- ggplot2::ggplot(data = false_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free") +
  ggplot2::ggtitle("False negative rate by model free scales, lower is better. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "False negative rate by model free scales, lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_negative_rate_free_scales.eps", plot = false_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_negative_rate_free_scales.jpeg", plot = false_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_negative_rate_free_scales.pdf", plot = false_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_negative_rate_free_scales.png", plot = false_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_negative_rate_free_scales.svg", plot = false_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_negative_rate_free_scales.tiff", plot = false_negative_rate_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

##### F1 data starts here ######

F1_score_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Elastic", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Gradient Boosted", numresamples),
    rep("Neuralnet", numresamples),
    rep("XGBoost", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Elastic", numresamples),
    rep("Ensemble GLM", numresamples),
    rep("Ensemble Neuralnet", numresamples),
    rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    elastic_holdout_F1_score, fda_holdout_F1_score, gam_holdout_F1_score,
    glmnet_holdout_F1_score,
    gb_holdout_F1_score,
    neuralnet_holdout_F1_score,
    xgb_holdout_F1_score,
    ensemble_C50_holdout_F1_score,
    ensemble_elastic_holdout_F1_score,
    ensemble_glmnet_holdout_F1_score,
    ensemble_neuralnet_holdout_F1_score,
    ensemble_xgb_holdout_F1_score
  ),
  "mean" = rep(c(
    elastic_holdout_F1_score_mean, fda_holdout_F1_score_mean, gam_holdout_F1_score_mean,
    glmnet_holdout_F1_score_mean,
    gb_holdout_F1_score_mean,
    neuralnet_holdout_F1_score_mean,
    xgb_holdout_F1_score_mean,
    ensemble_C50_holdout_F1_score_mean,
    ensemble_elastic_holdout_F1_score_mean,
    ensemble_glmnet_holdout_F1_score_mean,
    ensemble_neuralnet_holdout_F1_score_mean,
    ensemble_xgb_holdout_F1_score_mean
  ), each = numresamples)
)

F1_score_fixed_scales <- ggplot2::ggplot(data = F1_score_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "fixed") +
  ggplot2::ggtitle("F1 score by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "F1 score by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("F1_score_fixed_scales.eps", plot = F1_score_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("F1_score_fixed_scales.jpeg", plot = F1_score_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("F1_score_fixed_scales.pdf", plot = F1_score_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("F1_score_fixed_scales.png", plot = F1_score_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("F1_score_fixed_scales.svg", plot = F1_score_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("F1_score_fixed_scales.tiff", plot = F1_score_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

F1_score_free_scales <- ggplot2::ggplot(data = F1_score_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free") +
  ggplot2::ggtitle("F1 score by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "F1 score by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("F1_score_free_scales.eps", plot = F1_score_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("F1_score_free_scales.jpeg", plot = F1_score_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("F1_score_free_scales.pdf", plot = F1_score_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("F1_score_free_scales.png", plot = F1_score_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("F1_score_free_scales.svg", plot = F1_score_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("F1_score_free_scales.tiff", plot = F1_score_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Positive Predictive Value #### starts here

positive_predictive_value_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Elastic", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Gradient Boosted", numresamples),
    rep("Neuralnet", numresamples),
    rep("XGBoost", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Elastic", numresamples),
    rep("Ensemble GLM", numresamples),
    rep("Ensemble Neuralnet", numresamples),
    rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    elastic_holdout_positive_predictive_value, fda_holdout_positive_predictive_value, gam_holdout_positive_predictive_value,
    glmnet_holdout_positive_predictive_value,
    gb_holdout_positive_predictive_value,
    neuralnet_holdout_positive_predictive_value,
    xgb_holdout_positive_predictive_value,
    ensemble_C50_holdout_positive_predictive_value,
    ensemble_elastic_holdout_positive_predictive_value,
    ensemble_glmnet_holdout_positive_predictive_value,
    ensemble_neuralnet_holdout_positive_predictive_value,
    ensemble_xgb_holdout_positive_predictive_value
  ),
  "mean" = rep(c(
    elastic_holdout_positive_predictive_value_mean, fda_holdout_positive_predictive_value_mean, gam_holdout_positive_predictive_value_mean,
    glmnet_holdout_positive_predictive_value_mean,
    gb_holdout_positive_predictive_value_mean,
    neuralnet_holdout_positive_predictive_value_mean,
    xgb_holdout_positive_predictive_value_mean,
    ensemble_C50_holdout_positive_predictive_value_mean,
    ensemble_elastic_holdout_positive_predictive_value_mean,
    ensemble_glmnet_holdout_positive_predictive_value_mean,
    ensemble_neuralnet_holdout_positive_predictive_value_mean,
    ensemble_xgb_holdout_positive_predictive_value_mean
  ), each = numresamples)
)

positive_predictive_value_fixed_scales <- ggplot2::ggplot(data = positive_predictive_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "fixed") +
  ggplot2::ggtitle("Positive predictive value by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Positive predictive value by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("positive_predictive_value_fixed_scales.eps", plot = positive_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("positive_predictive_value_fixed_scales.jpeg", plot = positive_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("positive_predictive_value_fixed_scales.pdf", plot = positive_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("positive_predictive_value_fixed_scales.png", plot = positive_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("positive_predictive_value_fixed_scales.svg", plot = positive_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("positive_predictive_value_fixed_scales.tiff", plot = positive_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

positive_predictive_value_free_scales <- ggplot2::ggplot(data = positive_predictive_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free") +
  ggplot2::ggtitle("Positive predictive value by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Positive predictive value by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("positive_predictive_value_free_scales.eps", plot = positive_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("positive_predictive_value_free_scales.jpeg", plot = positive_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("positive_predictive_value_free_scales.pdf", plot = positive_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("positive_predictive_value_free_scales.png", plot = positive_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("positive_predictive_value_free_scales.svg", plot = positive_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("positive_predictive_value_free_scales.tiff", plot = positive_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Negative Predictive Value ####

negative_predictive_value_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Elastic", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Gradient Boosted", numresamples),
    rep("Neuralnet", numresamples),
    rep("XGBoost", numresamples),
    rep("Ensemble C50", numresamples),
    rep("Ensemble Elastic", numresamples),
    rep("Ensemble GLM", numresamples),
    rep("Ensemble Neuralnet", numresamples),
    rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    elastic_holdout_negative_predictive_value, fda_holdout_negative_predictive_value, gam_holdout_negative_predictive_value,
    glmnet_holdout_negative_predictive_value,
    gb_holdout_negative_predictive_value,
    neuralnet_holdout_negative_predictive_value,
    xgb_holdout_negative_predictive_value,
    ensemble_C50_holdout_negative_predictive_value,
    ensemble_elastic_holdout_negative_predictive_value,
    ensemble_glmnet_holdout_negative_predictive_value,
    ensemble_neuralnet_holdout_negative_predictive_value,
    ensemble_xgb_holdout_negative_predictive_value
  ),
  "mean" = rep(c(
    elastic_holdout_negative_predictive_value_mean, fda_holdout_negative_predictive_value_mean, gam_holdout_negative_predictive_value_mean,
    glmnet_holdout_negative_predictive_value_mean,
    gb_holdout_negative_predictive_value_mean,
    neuralnet_holdout_negative_predictive_value_mean,
    xgb_holdout_negative_predictive_value_mean,
    ensemble_C50_holdout_negative_predictive_value_mean,
    ensemble_elastic_holdout_negative_predictive_value_mean,
    ensemble_glmnet_holdout_negative_predictive_value_mean,
    ensemble_neuralnet_holdout_negative_predictive_value_mean,
    ensemble_xgb_holdout_negative_predictive_value_mean
  ), each = numresamples)
)

negative_predictive_value_fixed_scales <- ggplot2::ggplot(data = negative_predictive_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "fixed") +
  ggplot2::ggtitle("Negative predictive value by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Negative predictive value by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("negative_predictive_value_fixed_scales.eps", plot = negative_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("negative_predictive_value_fixed_scales.jpeg", plot = negative_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("negative_predictive_value_fixed_scales.pdf", plot = negative_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("negative_predictive_value_fixed_scales.png", plot = negative_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("negative_predictive_value_fixed_scales.svg", plot = negative_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("negative_predictive_value_fixed_scales.tiff", plot = negative_predictive_value_fixed_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

negative_predictive_value_free_scales <- ggplot2::ggplot(data = negative_predictive_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 3, scales = "free") +
  ggplot2::ggtitle("Negative predictive value by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Negative predictive value by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("negative_predictive_value_free_scales.eps", plot = negative_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("negative_predictive_value_free_scales.jpeg", plot = negative_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("negative_predictive_value_free_scales.pdf", plot = negative_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("negative_predictive_value_free_scales.png", plot = negative_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("negative_predictive_value_free_scales.svg", plot = negative_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("negative_predictive_value_free_scales.tiff", plot = negative_predictive_value_free_scales, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


#### Duration Barchart ####

duration_barchart <- ggplot2::ggplot(holdout_results, aes(x = reorder(Model, Duration), y = Duration)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Duration", title = "Duration, shorter is better") +
  ggplot2::geom_text(aes(label = Duration), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, 1.25*max(holdout_results$Duration)) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Duration - Duration_sd, ymax = Duration + Duration_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("duration_barchart.eps", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("duration_barchart.jpeg", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("duration_barchart.pdf", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("duration_barchart.png", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("duration_barchart.svg", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("duration_barchart.tiff", plot = duration_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Save all trained models ####

if (save_all_trained_models == "Y") {
  fil <- tempfile("fda_train_fit", fileext = ".RDS")
  saveRDS(fda_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("best_elastic_model", fileext = ".RDS")
  saveRDS(best_elastic_model, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("gam_train_fit", fileext = ".RDS")
  saveRDS(gam_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("glmnet_train_fit", fileext = ".RDS")
  saveRDS(glmnet_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("gb_train_fit", fileext = ".RDS")
  saveRDS(gb_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("neuralnet_train_fit", fileext = ".RDS")
  saveRDS(neuralnet_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("xgb_model", fileext = ".RDS")
  saveRDS(xgb_model, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensemble_C50_train_fit", fileext = ".RDS")
  saveRDS(ensemble_C50_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("best_ensemble_elastic_model", fileext = ".RDS")
  saveRDS(best_ensemble_elastic_model, fil)
}

if (save_all_trained_models == "Y"){
  fil <- tempfile("ensemble_glmnet_model", fileext = ".RDS")
  saveRDS(ensemble_glmnet_model, fil)
}

if (save_all_trained_models == "Y"){
  fil <- tempfile("ensemble_neuralnet_train_fit", fileext = ".RDS")
  saveRDS(ensemble_neuralnet_train_fit, fil)
}

if(save_all_trained_models == "Y"){
  fil <- tempfile("ensemble_XGBoost_train_fit", fileext = ".RDS")
  saveRDS(ensemble_xgbModel, fil)
}

if(save_all_trained_models == "Y"){
  fil <- tempfile("df_head", fileext = ".RDS")
  saveRDS(df_head, fil)
}

if(save_all_trained_models == "Y"){
  fil <- tempfile("datasummary", fileext = ".RDS")
  saveRDS(datasummary, fil)
}

if(save_all_trained_models == "Y"){
  fil <- tempfile("holdout_results", fileext = ".RDS")
  saveRDS(holdout_results, fil)
}

if(save_all_trained_models == "Y") {
  fil <- tempfile("VIF_table", fileext = ".RDS")
  saveRDS(VIF_table, fil)
}

if(save_all_trained_models == "Y"){
  fil <- tempfile("ensemblecorrelation", fileext = ".RDS")
  saveRDS(ensemblecorrelation, fil)
}

if(save_all_trained_models == "Y"){
  fil <- tempfile("correlationtable", fileext = ".RDS")
  saveRDS(correlationtable, fil)
}

if(save_all_trained_models == "Y"){
  fil <- tempfile("headensemble", fileext = ".RDS")
  saveRDS(headensemble, fil)
}

lm_vip <- stats::lm(y ~ ., data = df)
vip_df <- vip::vi(lm_vip)
vip_df$Percentage <- round(vip_df$Importance / sum(vip_df$Importance), 4)
vip_df$Total_Percentage <- cumsum(vip_df$Percentage)
vip_df <- vip_df %>% dplyr::arrange(dplyr::desc(Percentage))
if (save_all_trained_models == "Y"){
  fil <- tempfile("vip_df", fileext = ".RDS")
  saveRDS(vip_df, fil)
}
variable_importance <- reactable::reactable(as.data.frame(vip_df),
                                            searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                            striped = TRUE, highlight = TRUE, resizable = TRUE
)

htmltools::div(class = "table",
               htmltools::div(class = "title", "variable_importance")
)

variable_importance <- htmlwidgets::prependContent(variable_importance, htmltools::h2(class = "title", "Variable importance report"))

variable_importance_barchart <- ggplot2::ggplot(data = vip_df, mapping = aes(x = stats::reorder(Variable, -Percentage), y = Percentage)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(mapping = aes(label = paste0(100*Percentage, "%"), y = 1.03 * Percentage)) +
  ggplot2::ggtitle("Variable Importance (based on a linear model applied to the full data set)") +
  ggplot2::xlab(label = "Features") +
  ggplot2::scale_y_continuous(labels = scales::label_percent())
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("variable_importance_barchart.eps", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("variable_importance_barchart.jpeg", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("variable_importance_barchart.pdf", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("variable_importance_barchart.png", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("variable_importance_barchart.svg", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("variable_importance_barchart.tiff", plot = variable_importance_barchart, width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}



#### Predicting on new data ####

if (do_you_have_new_data == "Y") {
  Elastic <- as.numeric(predict(best_elastic_model, s = best_elastic_lambda, newx = data.matrix(newdata)))
  Elastic <- ifelse(Elastic > positive_rate, 1, 0)
  Flexible_Discriminant_Analysis <- as.numeric(predict(object = fda_train_fit, newdata = newdata)) -1
  Generalized_Linear_Models <- as.numeric(predict(best_glmnet_model, s = best_glmnet_lambda, newx = data.matrix(newdata)))
  Generalized_Linear_Models <- ifelse(Generalized_Linear_Models > positive_rate, 1, 0)
  Generalized_Additive_Models <- as.numeric(predict(object = gam_train_fit, newdata = newdata))
  Generalized_Additive_Models <- ifelse(Generalized_Additive_Models > positive_rate, 1, 0)
  Gradient_Boosted <- predict(object = gb_train_fit, newdata = newdata)
  Gradient_Boosted <- ifelse(Gradient_Boosted > positive_rate, 1, 0)
  Neuralnet <- as.numeric(predict(object = neuralnet_train_fit, newdata = newdata))
  Neuralnet <- ifelse(Neuralnet > positive_rate, 1, 0)

  new_ensemble <- data.frame(
    Elastic,
    Flexible_Discriminant_Analysis,
    Generalized_Additive_Models,
    Generalized_Linear_Models,
    Gradient_Boosted,
    Neuralnet,
    "y" = Elastic
  )

  new_ensemble_C50 <- predict(object = ensemble_C50_train_fit, newdata = new_ensemble)
  new_ensemble_elastic <- as.numeric(glmnet::predict.glmnet(best_ensemble_elastic_model, newx = as.matrix(new_ensemble), s = best_ensemble_elastic_lambda))
  new_ensemble_elastic <- ifelse(new_ensemble_elastic > positive_rate, 1, 0)
  new_ensemble_glm <- as.numeric(glmnet::predict.glmnet(best_ensemble_glmnet_model, newx = as.matrix(new_ensemble[, 1:ncol(new_ensemble)-1]), s = best_ensemble_glmnet_lambda))
  new_ensemble_glm <- ifelse(new_ensemble_glm > positive_rate, 1, 0)
  new_ensemble_elastic <- ifelse(new_ensemble_elastic > positive_rate, 1, 0)
  new_ensemble_neuralnet <- as.numeric(predict(object = ensemble_neuralnet_train_fit, newdata = new_ensemble))
  new_ensemble_neuralnet <- ifelse(new_ensemble_neuralnet > positive_rate, 1, 0)

  new_data_results <- data.frame(
    "Elastic" = Elastic,
    "Flexible_Discriminant_Analysis" = Flexible_Discriminant_Analysis,
    "Generalized_Linear_Models" = Generalized_Linear_Models,
    "Generalized_Additive_Models" = Generalized_Additive_Models,
    "Gradient_Boosted" = Gradient_Boosted,
    "Neuralnet" = Neuralnet,
    "Ensemble_C50" = new_ensemble_C50,
    "Ensemble_Elastic" = new_ensemble_elastic,
    "Ensemble GLM" = new_ensemble_glm,
    "Ensemble_Neuralnet" = new_ensemble_neuralnet
  )

  new_data_results <- reactable::reactable(new_data_results,
                                           searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                           striped = TRUE, highlight = TRUE, resizable = TRUE
  )

  htmltools::div(class = "table",
                 htmltools::div(class = "title", "new_data_results")
  )

  new_data_results <- htmlwidgets::prependContent(new_data_results, htmltools::h2(class = "title", "New data results"))

  #### Summary tables for new data ####

  return(list(
    "Head of data" = head_df, "Summary tables" = summary_tables,
    "AUC fixed scales" = AUC_plot_fixed_scales, "AUC free scales" = AUC_plot_free_scales,
    "Duration barchart" = duration_barchart, "ROC curves" = ROC_curves,
    "Boxplots" = boxplots, "Barchart" = barchart, "Barchart percentage" = barchart_percentage, "Correlation table" = correlation_table, 'VIF results' = VIF_report,
    'True positive rate fixed scales' = true_positive_rate_fixed_scales, 'True positive rate free scales' = true_positive_rate_free_scales,
    'True negative rate fixed scales' = true_negative_rate_fixed_scales, 'True negative rate free scales' = true_negative_rate_free_scales,
    'False positive rate fixed scales' = false_positive_rate_fixed_scales, 'False positive rate free scales' = false_positive_rate_free_scales,
    'False negative rate fixed scales' = false_negative_rate_fixed_scales, 'False negative rate free scales' = false_negative_rate_free_scales,
    'F1 score fixed scales' = F1_score_fixed_scales, 'F1 score free scales' = F1_score_free_scales, "Stratified sampling report" = stratified_sampling_report,
    'Positive predictive value fixed scales' = positive_predictive_value_fixed_scales, 'Positive predictive value free scales' = positive_predictive_value_free_scales,
    'Negative predictive value fixed scales' = negative_predictive_value_fixed_scales, 'Negative predictive value free scales' = negative_predictive_value_free_scales,
    "Ensemble Correlation" = ensemble_correlation, "Ensemble head" = head_ensemble, "New data results" = new_data_results,
    "Data Summary" = data_summary, "Holdout results" = holdout_results_final, "Outlier list" = outlier_list,
    "Variable importance" = variable_importance, "Variable importance barchart" = variable_importance_barchart,
    "How to handle strings" = how_to_handle_strings, "Train amount" = train_amount, "Test amount" = test_amount, "Validation amount" = validation_amount
  )
  )
}

#### Summary tables if there is no new data ####


summary_tables <- list(
  "Elastic" = elastic_table_total,
  "Fixture Discrmininant Analysis" = fda_table_total, "Generalized Additive Methods" = gam_table_total,
  "Generalized Linear Models" = glmnet_table_total,
  "Gradient Boosted" = gb_table_total,
  "Neuralnet" = neuralnet_table_total,
  "XGBoost" = xgb_table_total,
  "Ensemble C50" = ensemble_C50_table_total,
  "Ensemble Elastic" = ensemble_elastic_table_total,
  "Ensemble GLM" = ensemble_glmnet_table_total,
  "Ensemble Neuralnet" = ensemble_neuralnet_table_total,
  "Ensemble XGBoost" = ensemble_xgb_table_total
)

return(list(
  "Head of data" = head_df, "Summary tables" = summary_tables,
  "AUC fixed scales" = AUC_plot_fixed_scales, "AUC free scales" = AUC_plot_free_scales,
  "Duration barchart" = duration_barchart, "ROC curves" = ROC_curves,
  "Boxplots" = boxplots, "Barchart" = barchart, "Barchart percentage" = barchart_percentage, "Correlation table" = correlation_table, 'VIF results' = VIF_report,
  'True positive rate fixed scales' = true_positive_rate_fixed_scales, 'True positive rate free scales' = true_positive_rate_free_scales,
  'True negative rate fixed scales' = true_negative_rate_fixed_scales, 'True negative rate free scales' = true_negative_rate_free_scales,
  'False positive rate fixed scales' = false_positive_rate_fixed_scales, 'False positive rate free scales' = false_positive_rate_free_scales,
  'False negative rate fixed scales' = false_negative_rate_fixed_scales, 'False negative rate free scales' = false_negative_rate_free_scales,
  'F1 score fixed scales' = F1_score_fixed_scales, 'F1 score free scales' = F1_score_free_scales, "Stratified sampling report" = stratified_sampling_report,
  'Positive predictive value fixed scales' = positive_predictive_value_fixed_scales, 'Positive predictive value free scales' = positive_predictive_value_free_scales,
  'Negative predictive value fixed scales' = negative_predictive_value_fixed_scales, 'Negative predictive value free scales' = negative_predictive_value_free_scales,
  "Ensemble Correlation" = ensemble_correlation, "Ensemble head" = head_ensemble,
  "Data Summary" = data_summary, "Holdout results" = holdout_results_final, "Outlier list" = outlier_list,
  "Variable importance" = variable_importance, "Variable importance barchart" = variable_importance_barchart,
  "How to handle strings" = how_to_handle_strings, "Train amount" = train_amount, "Test amount" = test_amount, "Validation amount" = validation_amount
)
)
}
