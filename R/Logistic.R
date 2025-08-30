#' logistic—function to perform logistic analysis and return the results to the user.

#' @param data data can be a CSV file or within an R package, such as MASS::Pima.te
#' @param colnum the column number with the logistic data
#' @param numresamples the number of resamples
#' @param save_all_trained_models "Y" or "N". Places all the trained models in the Environment
#' @param save_all_plots Options to save all plots
#' @param how_to_handle_strings 0: No strings, 1: Factor values
#' @param set_seed Asks the user to set a seed to create reproducible results
#' @param do_you_have_new_data "Y" or "N". If "Y", then you will be asked for the new data
#' @param remove_VIF_greater_than Removes features with VIGF value above the given amount (default = 5.00)
#' @param remove_ensemble_correlations_greater_than Enter a number to remove correlations in the ensembles
#' @param use_parallel "Y" or "N" for parallel processing
#' @param train_amount set the amount for the training data
#' @param test_amount set the amount for the testing data
#' @param validation_amount Set the amount for the validation data

#' @return a real number
#' @export Logistic Automatically builds 36 binary models (23 individual models and 13 ensembles of models)

#' @importFrom adabag bagging
#' @importFrom arm bayesglm
#' @importFrom brnn brnn
#' @importFrom C50 C5.0
#' @importFrom car vif
#' @importFrom corrplot corrplot
#' @importFrom Cubist cubist
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr across count mutate relocate select %>%
#' @importFrom e1071 naiveBayes svm
#' @importFrom gam gam
#' @importFrom gbm gbm
#' @importFrom ggplot2 geom_boxplot geom_histogram ggplot facet_wrap labs theme_bw labs aes
#' @importFrom ggplotify as.ggplot
#' @importFrom graphics hist panel.smooth par rect
#' @importFrom gridExtra grid.arrange
#' @importFrom gt gt
#' @importFrom ipred bagging
#' @importFrom klaR rda
#' @importFrom MachineShop fit
#' @importFrom magrittr %>%
#' @importFrom MASS lda qda
#' @importFrom mda mda fda
#' @importFrom parallel makeCluster
#' @importFrom pls pcr
#' @importFrom pROC roc ggroc
#' @importFrom purrr keep
#' @importFrom randomForest randomForest
#' @importFrom ranger ranger
#' @importFrom reactable reactable
#' @importFrom reactablefmtr add_title
#' @importFrom readr cols
#' @importFrom rpart rpart
#' @importFrom scales percent
#' @importFrom stats binomial cor lm plogis predict rbinom reorder sd
#' @importFrom tidyr gather pivot_longer
#' @importFrom tree tree
#' @importFrom utils head read.csv str
#' @importFrom xgboost xgb.DMatrix xgb.train


Logistic <- function(data, colnum, numresamples, remove_VIF_greater_than, remove_ensemble_correlations_greater_than,
                     save_all_trained_models = c("Y", "N"), save_all_plots = c("Y", "N"), set_seed = c("Y", "N"), how_to_handle_strings = c("0", "1"),
                     do_you_have_new_data = c("Y", "N"),  use_parallel = c("Y", "N"),
                     train_amount, test_amount, validation_amount) {

use_parallel <- 0
no_cores <- 0

if (use_parallel == "Y") {
  cl <- parallel::makeCluster(no_cores, type = "FORK")
  doParallel::registerDoParallel(cl)
}

colnames(data)[colnum] <- "y"

df <- data %>% dplyr::relocate(y, .after = dplyr::last_col()) # Moves the target column to the last column on the right
if(set_seed == "N"){
  df <- df[sample(1:nrow(df)), ] # randomizes the rows
}

if(set_seed == "Y"){
  seed = as.integer(readline("Which integer would you like to use for the seed? "))
}

if (how_to_handle_strings == 1) {
  df <- dplyr::mutate_if(df, is.character, as.factor)
  df <- dplyr::mutate_if(df, is.factor, as.numeric)
}

vif <- car::vif(stats::lm(y ~ ., data = df[, 1:ncol(df)]))
for (i in 1:ncol(df)) {
  if(max(vif) > remove_VIF_greater_than){
    df <- df %>% dplyr::select(-which.max(vif))
    vif <- car::vif(stats::lm(y ~ ., data = df[, 1:ncol(df)]))
  }
}

VIF_results <- reactable::reactable(as.data.frame(vif),
                                    searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                    striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Variance Inflation Factor (VIF)")

if (do_you_have_new_data == "Y") {
  newdata <- readline("What is the URL for the new data? ")
  newdata <- read.csv(newdata)
  colnames(newdata)[colnum] <- "y"
  newdata <- newdata %>% dplyr::relocate(y, .after = dplyr::last_col()) # Moves the target column to the last column on the right
}

tempdir1 <- tempdir()
if(save_all_plots == "Y"){
  width = as.numeric(readline("Width of the graphics: "))
  height = as.numeric(readline("Height of the graphics: "))
  units = readline("Which units? You may use in, cm, mm or px. ")
  scale = as.numeric(readline("What multiplicative scaling factor? "))
  device = readline("Which device to use? You may enter eps, jpeg, pdf, png, svg or tiff: ")
  dpi <- as.numeric(readline("Plot resolution. Applies only to raster output types (jpeg, png, tiff): "))
}

head_df <- reactable::reactable(head(df),
                                searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Head of the data frame")


#### Initialize values to 0, in alphabetical order ####

cubist_train_true_positive_rate <- 0
cubist_train_true_negative_rate <- 0
cubist_train_false_positive_rate <- 0
cubist_train_false_negative_rate <- 0
cubist_train_accuracy <- 0
cubist_train_F1_score <- 0
cubist_test_true_positive_rate <- 0
cubist_test_true_negative_rate <- 0
cubist_test_false_positive_rate <- 0
cubist_test_false_negative_rate <- 0
cubist_test_accuracy <- 0
cubist_test_F1_score <- 0
cubist_validation_true_positive_rate <- 0
cubist_validation_true_negative_rate <- 0
cubist_validation_false_positive_rate <- 0
cubist_validation_false_negative_rate <- 0
cubist_validation_accuracy <- 0
cubist_validation_F1_score <- 0
cubist_holdout_true_positive_rate <- 0
cubist_holdout_true_negative_rate <- 0
cubist_holdout_false_positive_rate <- 0
cubist_holdout_false_negative_rate <- 0
cubist_holdout_accuracy <- 0
cubist_holdout_F1_score <- 0
cubist_duration <- 0
cubist_train_positive_predictive_value <- 0
cubist_train_negative_predictive_value <- 0
cubist_test_positive_predictive_value <- 0
cubist_test_negative_predictive_value <- 0
cubist_validation_positive_predictive_value <- 0
cubist_validation_negative_predictive_value <- 0
cubist_holdout_positive_predictive_value <- 0
cubist_holdout_negative_predictive_value <- 0
cubist_holdout_overfitting <- 0
cubist_holdout_overfitting_mean <- 0
cubist_table_total <- 0

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

glm_train_true_positive_rate <- 0
glm_train_true_negative_rate <- 0
glm_train_false_positive_rate <- 0
glm_train_false_negative_rate <- 0
glm_train_accuracy <- 0
glm_train_F1_score <- 0
glm_test_true_positive_rate <- 0
glm_test_true_negative_rate <- 0
glm_test_false_positive_rate <- 0
glm_test_false_negative_rate <- 0
glm_test_accuracy <- 0
glm_test_F1_score <- 0
glm_validation_true_positive_rate <- 0
glm_validation_true_negative_rate <- 0
glm_validation_false_positive_rate <- 0
glm_validation_false_negative_rate <- 0
glm_validation_accuracy <- 0
glm_validation_F1_score <- 0
glm_holdout_true_positive_rate <- 0
glm_holdout_true_negative_rate <- 0
glm_holdout_false_positive_rate <- 0
glm_holdout_false_negative_rate <- 0
glm_holdout_accuracy <- 0
glm_holdout_F1_score <- 0
glm_duration <- 0
glm_train_positive_predictive_value <- 0
glm_train_negative_predictive_value <- 0
glm_test_positive_predictive_value <- 0
glm_test_negative_predictive_value <- 0
glm_validation_positive_predictive_value <- 0
glm_validation_negative_predictive_value <- 0
glm_holdout_positive_predictive_value <- 0
glm_holdout_negative_predictive_value <- 0
glm_holdout_overfitting <- 0
glm_holdout_overfitting_mean <- 0
glm_table_total <- 0

linear_train_true_positive_rate <- 0
linear_train_true_negative_rate <- 0
linear_train_false_positive_rate <- 0
linear_train_false_negative_rate <- 0
linear_train_accuracy <- 0
linear_train_F1_score <- 0
linear_test_true_positive_rate <- 0
linear_test_true_negative_rate <- 0
linear_test_false_positive_rate <- 0
linear_test_false_negative_rate <- 0
linear_test_accuracy <- 0
linear_test_F1_score <- 0
linear_validation_true_positive_rate <- 0
linear_validation_true_negative_rate <- 0
linear_validation_false_positive_rate <- 0
linear_validation_false_negative_rate <- 0
linear_validation_accuracy <- 0
linear_validation_F1_score <- 0
linear_holdout_true_positive_rate <- 0
linear_holdout_true_negative_rate <- 0
linear_holdout_false_positive_rate <- 0
linear_holdout_false_negative_rate <- 0
linear_holdout_accuracy <- 0
linear_holdout_F1_score <- 0
linear_duration <- 0
linear_train_positive_predictive_value <- 0
linear_train_negative_predictive_value <- 0
linear_test_positive_predictive_value <- 0
linear_test_negative_predictive_value <- 0
linear_validation_positive_predictive_value <- 0
linear_validation_negative_predictive_value <- 0
linear_holdout_positive_predictive_value <- 0
linear_holdout_negative_predictive_value <- 0
linear_holdout_overfitting <- 0
linear_holdout_overfitting_mean <- 0
linear_table_total <- 0

lda_train_true_positive_rate <- 0
lda_train_true_negative_rate <- 0
lda_train_false_positive_rate <- 0
lda_train_false_negative_rate <- 0
lda_train_accuracy <- 0
lda_train_F1_score <- 0
lda_test_true_positive_rate <- 0
lda_test_true_negative_rate <- 0
lda_test_false_positive_rate <- 0
lda_test_false_negative_rate <- 0
lda_test_accuracy <- 0
lda_test_F1_score <- 0
lda_validation_true_positive_rate <- 0
lda_validation_true_negative_rate <- 0
lda_validation_false_positive_rate <- 0
lda_validation_false_negative_rate <- 0
lda_validation_accuracy <- 0
lda_validation_F1_score <- 0
lda_holdout_true_positive_rate <- 0
lda_holdout_true_negative_rate <- 0
lda_holdout_false_positive_rate <- 0
lda_holdout_false_negative_rate <- 0
lda_holdout_accuracy <- 0
lda_holdout_F1_score <- 0
lda_duration <- 0
lda_train_positive_predictive_value <- 0
lda_train_negative_predictive_value <- 0
lda_test_positive_predictive_value <- 0
lda_test_negative_predictive_value <- 0
lda_validation_positive_predictive_value <- 0
lda_validation_negative_predictive_value <- 0
lda_holdout_positive_predictive_value <- 0
lda_holdout_negative_predictive_value <- 0
lda_holdout_overfitting <- 0
lda_holdout_overfitting_mean <- 0
lda_table_total <- 0

pda_train_true_positive_rate <- 0
pda_train_true_negative_rate <- 0
pda_train_false_positive_rate <- 0
pda_train_false_negative_rate <- 0
pda_train_accuracy <- 0
pda_train_F1_score <- 0
pda_test_true_positive_rate <- 0
pda_test_true_negative_rate <- 0
pda_test_false_positive_rate <- 0
pda_test_false_negative_rate <- 0
pda_test_accuracy <- 0
pda_test_F1_score <- 0
pda_validation_true_positive_rate <- 0
pda_validation_true_negative_rate <- 0
pda_validation_false_positive_rate <- 0
pda_validation_false_negative_rate <- 0
pda_validation_accuracy <- 0
pda_validation_F1_score <- 0
pda_holdout_true_positive_rate <- 0
pda_holdout_true_negative_rate <- 0
pda_holdout_false_positive_rate <- 0
pda_holdout_false_negative_rate <- 0
pda_holdout_accuracy <- 0
pda_holdout_F1_score <- 0
pda_duration <- 0
pda_train_positive_predictive_value <- 0
pda_train_negative_predictive_value <- 0
pda_test_positive_predictive_value <- 0
pda_test_negative_predictive_value <- 0
pda_validation_positive_predictive_value <- 0
pda_validation_negative_predictive_value <- 0
pda_holdout_positive_predictive_value <- 0
pda_holdout_negative_predictive_value <- 0
pda_holdout_overfitting <- 0
pda_holdout_overfitting_mean <- 0
pda_table_total <- 0

qda_train_true_positive_rate <- 0
qda_train_true_negative_rate <- 0
qda_train_false_positive_rate <- 0
qda_train_false_negative_rate <- 0
qda_train_accuracy <- 0
qda_train_F1_score <- 0
qda_test_true_positive_rate <- 0
qda_test_true_negative_rate <- 0
qda_test_false_positive_rate <- 0
qda_test_false_negative_rate <- 0
qda_test_accuracy <- 0
qda_test_F1_score <- 0
qda_validation_true_positive_rate <- 0
qda_validation_true_negative_rate <- 0
qda_validation_false_positive_rate <- 0
qda_validation_false_negative_rate <- 0
qda_validation_accuracy <- 0
qda_validation_F1_score <- 0
qda_holdout_true_positive_rate <- 0
qda_holdout_true_negative_rate <- 0
qda_holdout_false_positive_rate <- 0
qda_holdout_false_negative_rate <- 0
qda_holdout_accuracy <- 0
qda_holdout_F1_score <- 0
qda_duration <- 0
qda_train_positive_predictive_value <- 0
qda_train_negative_predictive_value <- 0
qda_test_positive_predictive_value <- 0
qda_test_negative_predictive_value <- 0
qda_validation_positive_predictive_value <- 0
qda_validation_negative_predictive_value <- 0
qda_holdout_positive_predictive_value <- 0
qda_holdout_negative_predictive_value <- 0
qda_holdout_overfitting <- 0
qda_holdout_overfitting_mean <- 0
qda_table_total <- 0

rf_train_true_positive_rate <- 0
rf_train_true_negative_rate <- 0
rf_train_false_positive_rate <- 0
rf_train_false_negative_rate <- 0
rf_train_accuracy <- 0
rf_train_F1_score <- 0
rf_test_true_positive_rate <- 0
rf_test_true_negative_rate <- 0
rf_test_false_positive_rate <- 0
rf_test_false_negative_rate <- 0
rf_test_accuracy <- 0
rf_test_F1_score <- 0
rf_validation_true_positive_rate <- 0
rf_validation_true_negative_rate <- 0
rf_validation_false_positive_rate <- 0
rf_validation_false_negative_rate <- 0
rf_validation_accuracy <- 0
rf_validation_F1_score <- 0
rf_holdout_true_positive_rate <- 0
rf_holdout_true_negative_rate <- 0
rf_holdout_false_positive_rate <- 0
rf_holdout_false_negative_rate <- 0
rf_holdout_accuracy <- 0
rf_holdout_F1_score <- 0
rf_duration <- 0
rf_train_positive_predictive_value <- 0
rf_train_negative_predictive_value <- 0
rf_test_positive_predictive_value <- 0
rf_test_negative_predictive_value <- 0
rf_validation_positive_predictive_value <- 0
rf_validation_negative_predictive_value <- 0
rf_holdout_positive_predictive_value <- 0
rf_holdout_negative_predictive_value <- 0
rf_holdout_overfitting <- 0
rf_holdout_overfitting_mean <- 0
rf_table_total <- 0

svm_train_true_positive_rate <- 0
svm_train_true_negative_rate <- 0
svm_train_false_positive_rate <- 0
svm_train_false_negative_rate <- 0
svm_train_accuracy <- 0
svm_train_F1_score <- 0
svm_test_true_positive_rate <- 0
svm_test_true_negative_rate <- 0
svm_test_false_positive_rate <- 0
svm_test_false_negative_rate <- 0
svm_test_accuracy <- 0
svm_test_F1_score <- 0
svm_validation_true_positive_rate <- 0
svm_validation_true_negative_rate <- 0
svm_validation_false_positive_rate <- 0
svm_validation_false_negative_rate <- 0
svm_validation_accuracy <- 0
svm_validation_F1_score <- 0
svm_holdout_true_positive_rate <- 0
svm_holdout_true_negative_rate <- 0
svm_holdout_false_positive_rate <- 0
svm_holdout_false_negative_rate <- 0
svm_holdout_accuracy <- 0
svm_holdout_F1_score <- 0
svm_duration <- 0
svm_train_positive_predictive_value <- 0
svm_train_negative_predictive_value <- 0
svm_test_positive_predictive_value <- 0
svm_test_negative_predictive_value <- 0
svm_validation_positive_predictive_value <- 0
svm_validation_negative_predictive_value <- 0
svm_holdout_positive_predictive_value <- 0
svm_holdout_negative_predictive_value <- 0
svm_holdout_overfitting <- 0
svm_holdout_overfitting_mean <- 0
svm_table_total <- 0

tree_train_true_positive_rate <- 0
tree_train_true_negative_rate <- 0
tree_train_false_positive_rate <- 0
tree_train_false_negative_rate <- 0
tree_train_accuracy <- 0
tree_train_F1_score <- 0
tree_test_true_positive_rate <- 0
tree_test_true_negative_rate <- 0
tree_test_false_positive_rate <- 0
tree_test_false_negative_rate <- 0
tree_test_accuracy <- 0
tree_test_F1_score <- 0
tree_validation_true_positive_rate <- 0
tree_validation_true_negative_rate <- 0
tree_validation_false_positive_rate <- 0
tree_validation_false_negative_rate <- 0
tree_validation_accuracy <- 0
tree_validation_F1_score <- 0
tree_holdout_true_positive_rate <- 0
tree_holdout_true_negative_rate <- 0
tree_holdout_false_positive_rate <- 0
tree_holdout_false_negative_rate <- 0
tree_holdout_accuracy <- 0
tree_holdout_F1_score <- 0
tree_duration <- 0
tree_train_positive_predictive_value <- 0
tree_train_negative_predictive_value <- 0
tree_test_positive_predictive_value <- 0
tree_test_negative_predictive_value <- 0
tree_validation_positive_predictive_value <- 0
tree_validation_negative_predictive_value <- 0
tree_holdout_positive_predictive_value <- 0
tree_holdout_negative_predictive_value <- 0
tree_holdout_overfitting <- 0
tree_holdout_overfitting_mean <- 0
tree_table_total <- 0

ensemble_bagging_train_true_positive_rate <- 0
ensemble_bagging_train_true_negative_rate <- 0
ensemble_bagging_train_false_positive_rate <- 0
ensemble_bagging_train_false_negative_rate <- 0
ensemble_bagging_train_accuracy <- 0
ensemble_bagging_train_F1_score <- 0
ensemble_bagging_test_true_positive_rate <- 0
ensemble_bagging_test_true_negative_rate <- 0
ensemble_bagging_test_false_positive_rate <- 0
ensemble_bagging_test_false_negative_rate <- 0
ensemble_bagging_test_accuracy <- 0
ensemble_bagging_test_F1_score <- 0
ensemble_bagging_validation_true_positive_rate <- 0
ensemble_bagging_validation_true_negative_rate <- 0
ensemble_bagging_validation_false_positive_rate <- 0
ensemble_bagging_validation_false_negative_rate <- 0
ensemble_bagging_validation_accuracy <- 0
ensemble_bagging_validation_F1_score <- 0
ensemble_bagging_holdout_true_positive_rate <- 0
ensemble_bagging_holdout_true_negative_rate <- 0
ensemble_bagging_holdout_false_positive_rate <- 0
ensemble_bagging_holdout_false_negative_rate <- 0
ensemble_bagging_holdout_accuracy <- 0
ensemble_bagging_holdout_F1_score <- 0
ensemble_bagging_duration <- 0
ensemble_bagging_train_positive_predictive_value <- 0
ensemble_bagging_train_negative_predictive_value <- 0
ensemble_bagging_test_positive_predictive_value <- 0
ensemble_bagging_test_negative_predictive_value <- 0
ensemble_bagging_validation_positive_predictive_value <- 0
ensemble_bagging_validation_negative_predictive_value <- 0
ensemble_bagging_holdout_positive_predictive_value <- 0
ensemble_bagging_holdout_negative_predictive_value <- 0
ensemble_bagging_holdout_overfitting <- 0
ensemble_bagging_holdout_overfitting_mean <- 0
ensemble_bagging_table_total <- 0

ensemble_C50_train_true_positive_rate <- 0
ensemble_C50_train_true_negative_rate <- 0
ensemble_C50_train_false_positive_rate <- 0
ensemble_C50_train_false_negative_rate <- 0
ensemble_C50_train_accuracy <- 0
ensemble_C50_train_F1_score <- 0
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

ensemble_gb_train_true_positive_rate <- 0
ensemble_gb_train_true_negative_rate <- 0
ensemble_gb_train_false_positive_rate <- 0
ensemble_gb_train_false_negative_rate <- 0
ensemble_gb_train_accuracy <- 0
ensemble_gb_train_F1_score <- 0
ensemble_gb_test_true_positive_rate <- 0
ensemble_gb_test_true_negative_rate <- 0
ensemble_gb_test_false_positive_rate <- 0
ensemble_gb_test_false_negative_rate <- 0
ensemble_gb_test_accuracy <- 0
ensemble_gb_test_F1_score <- 0
ensemble_gb_validation_true_positive_rate <- 0
ensemble_gb_validation_true_negative_rate <- 0
ensemble_gb_validation_false_positive_rate <- 0
ensemble_gb_validation_false_negative_rate <- 0
ensemble_gb_validation_accuracy <- 0
ensemble_gb_validation_F1_score <- 0
ensemble_gb_holdout_true_positive_rate <- 0
ensemble_gb_holdout_true_negative_rate <- 0
ensemble_gb_holdout_false_positive_rate <- 0
ensemble_gb_holdout_false_negative_rate <- 0
ensemble_gb_holdout_accuracy <- 0
ensemble_gb_holdout_F1_score <- 0
ensemble_gb_duration <- 0
ensemble_gb_train_positive_predictive_value <- 0
ensemble_gb_train_negative_predictive_value <- 0
ensemble_gb_test_positive_predictive_value <- 0
ensemble_gb_test_negative_predictive_value <- 0
ensemble_gb_validation_positive_predictive_value <- 0
ensemble_gb_validation_negative_predictive_value <- 0
ensemble_gb_holdout_positive_predictive_value <- 0
ensemble_gb_holdout_negative_predictive_value <- 0
ensemble_gb_holdout_overfitting <- 0
ensemble_gb_holdout_overfitting_mean <- 0
ensemble_gb_table_total <- 0

ensemble_pls_train_true_positive_rate <- 0
ensemble_pls_train_true_negative_rate <- 0
ensemble_pls_train_false_positive_rate <- 0
ensemble_pls_train_false_negative_rate <- 0
ensemble_pls_train_accuracy <- 0
ensemble_pls_train_F1_score <- 0
ensemble_pls_test_true_positive_rate <- 0
ensemble_pls_test_true_negative_rate <- 0
ensemble_pls_test_false_positive_rate <- 0
ensemble_pls_test_false_negative_rate <- 0
ensemble_pls_test_accuracy <- 0
ensemble_pls_test_F1_score <- 0
ensemble_pls_validation_true_positive_rate <- 0
ensemble_pls_validation_true_negative_rate <- 0
ensemble_pls_validation_false_positive_rate <- 0
ensemble_pls_validation_false_negative_rate <- 0
ensemble_pls_validation_accuracy <- 0
ensemble_pls_validation_F1_score <- 0
ensemble_pls_holdout_true_positive_rate <- 0
ensemble_pls_holdout_true_negative_rate <- 0
ensemble_pls_holdout_false_positive_rate <- 0
ensemble_pls_holdout_false_negative_rate <- 0
ensemble_pls_holdout_accuracy <- 0
ensemble_pls_holdout_F1_score <- 0
ensemble_pls_duration <- 0
ensemble_pls_train_positive_predictive_value <- 0
ensemble_pls_train_negative_predictive_value <- 0
ensemble_pls_test_positive_predictive_value <- 0
ensemble_pls_test_negative_predictive_value <- 0
ensemble_pls_validation_positive_predictive_value <- 0
ensemble_pls_validation_negative_predictive_value <- 0
ensemble_pls_holdout_positive_predictive_value <- 0
ensemble_pls_holdout_negative_predictive_value <- 0
ensemble_pls_holdout_overfitting <- 0
ensemble_pls_holdout_overfitting_mean <- 0
ensemble_pls_table_total <- 0

ensemble_pda_train_true_positive_rate <- 0
ensemble_pda_train_true_negative_rate <- 0
ensemble_pda_train_false_positive_rate <- 0
ensemble_pda_train_false_negative_rate <- 0
ensemble_pda_train_accuracy <- 0
ensemble_pda_train_F1_score <- 0
ensemble_pda_test_true_positive_rate <- 0
ensemble_pda_test_true_negative_rate <- 0
ensemble_pda_test_false_positive_rate <- 0
ensemble_pda_test_false_negative_rate <- 0
ensemble_pda_test_accuracy <- 0
ensemble_pda_test_F1_score <- 0
ensemble_pda_validation_true_positive_rate <- 0
ensemble_pda_validation_true_negative_rate <- 0
ensemble_pda_validation_false_positive_rate <- 0
ensemble_pda_validation_false_negative_rate <- 0
ensemble_pda_validation_accuracy <- 0
ensemble_pda_validation_F1_score <- 0
ensemble_pda_holdout_true_positive_rate <- 0
ensemble_pda_holdout_true_negative_rate <- 0
ensemble_pda_holdout_false_positive_rate <- 0
ensemble_pda_holdout_false_negative_rate <- 0
ensemble_pda_holdout_accuracy <- 0
ensemble_pda_holdout_F1_score <- 0
ensemble_pda_duration <- 0
ensemble_pda_train_positive_predictive_value <- 0
ensemble_pda_train_negative_predictive_value <- 0
ensemble_pda_test_positive_predictive_value <- 0
ensemble_pda_test_negative_predictive_value <- 0
ensemble_pda_validation_positive_predictive_value <- 0
ensemble_pda_validation_negative_predictive_value <- 0
ensemble_pda_holdout_positive_predictive_value <- 0
ensemble_pda_holdout_negative_predictive_value <- 0
ensemble_pda_holdout_overfitting <- 0
ensemble_pda_holdout_overfitting_mean <- 0
ensemble_pda_table_total <- 0

ensemble_rpart_train_true_positive_rate <- 0
ensemble_rpart_train_true_negative_rate <- 0
ensemble_rpart_train_false_positive_rate <- 0
ensemble_rpart_train_false_negative_rate <- 0
ensemble_rpart_train_accuracy <- 0
ensemble_rpart_train_F1_score <- 0
ensemble_rpart_test_true_positive_rate <- 0
ensemble_rpart_test_true_negative_rate <- 0
ensemble_rpart_test_false_positive_rate <- 0
ensemble_rpart_test_false_negative_rate <- 0
ensemble_rpart_test_accuracy <- 0
ensemble_rpart_test_F1_score <- 0
ensemble_rpart_validation_true_positive_rate <- 0
ensemble_rpart_validation_true_negative_rate <- 0
ensemble_rpart_validation_false_positive_rate <- 0
ensemble_rpart_validation_false_negative_rate <- 0
ensemble_rpart_validation_accuracy <- 0
ensemble_rpart_validation_F1_score <- 0
ensemble_rpart_holdout_true_positive_rate <- 0
ensemble_rpart_holdout_true_negative_rate <- 0
ensemble_rpart_holdout_false_positive_rate <- 0
ensemble_rpart_holdout_false_negative_rate <- 0
ensemble_rpart_holdout_accuracy <- 0
ensemble_rpart_holdout_F1_score <- 0
ensemble_rpart_duration <- 0
ensemble_rpart_train_positive_predictive_value <- 0
ensemble_rpart_train_negative_predictive_value <- 0
ensemble_rpart_test_positive_predictive_value <- 0
ensemble_rpart_test_negative_predictive_value <- 0
ensemble_rpart_validation_positive_predictive_value <- 0
ensemble_rpart_validation_negative_predictive_value <- 0
ensemble_rpart_holdout_positive_predictive_value <- 0
ensemble_rpart_holdout_negative_predictive_value <- 0
ensemble_rpart_holdout_overfitting <- 0
ensemble_rpart_holdout_overfitting_mean <- 0
ensemble_rpart_table_total <- 0

ensemble_svm_train_true_positive_rate <- 0
ensemble_svm_train_true_negative_rate <- 0
ensemble_svm_train_false_positive_rate <- 0
ensemble_svm_train_false_negative_rate <- 0
ensemble_svm_train_accuracy <- 0
ensemble_svm_train_F1_score <- 0
ensemble_svm_test_true_positive_rate <- 0
ensemble_svm_test_true_negative_rate <- 0
ensemble_svm_test_false_positive_rate <- 0
ensemble_svm_test_false_negative_rate <- 0
ensemble_svm_test_accuracy <- 0
ensemble_svm_test_F1_score <- 0
ensemble_svm_validation_true_positive_rate <- 0
ensemble_svm_validation_true_negative_rate <- 0
ensemble_svm_validation_false_positive_rate <- 0
ensemble_svm_validation_false_negative_rate <- 0
ensemble_svm_validation_accuracy <- 0
ensemble_svm_validation_F1_score <- 0
ensemble_svm_holdout_true_positive_rate <- 0
ensemble_svm_holdout_true_negative_rate <- 0
ensemble_svm_holdout_false_positive_rate <- 0
ensemble_svm_holdout_false_negative_rate <- 0
ensemble_svm_holdout_accuracy <- 0
ensemble_svm_holdout_F1_score <- 0
ensemble_svm_duration <- 0
ensemble_svm_train_positive_predictive_value <- 0
ensemble_svm_train_negative_predictive_value <- 0
ensemble_svm_test_positive_predictive_value <- 0
ensemble_svm_test_negative_predictive_value <- 0
ensemble_svm_validation_positive_predictive_value <- 0
ensemble_svm_validation_negative_predictive_value <- 0
ensemble_svm_holdout_positive_predictive_value <- 0
ensemble_svm_holdout_negative_predictive_value <- 0
ensemble_svm_holdout_overfitting <- 0
ensemble_svm_holdout_overfitting_mean <- 0
ensemble_svm_table_total <- 0

ensemble_tree_train_true_positive_rate <- 0
ensemble_tree_train_true_negative_rate <- 0
ensemble_tree_train_false_positive_rate <- 0
ensemble_tree_train_false_negative_rate <- 0
ensemble_tree_train_accuracy <- 0
ensemble_tree_train_F1_score <- 0
ensemble_tree_test_true_positive_rate <- 0
ensemble_tree_test_true_negative_rate <- 0
ensemble_tree_test_false_positive_rate <- 0
ensemble_tree_test_false_negative_rate <- 0
ensemble_tree_test_accuracy <- 0
ensemble_tree_test_F1_score <- 0
ensemble_tree_validation_true_positive_rate <- 0
ensemble_tree_validation_true_negative_rate <- 0
ensemble_tree_validation_false_positive_rate <- 0
ensemble_tree_validation_false_negative_rate <- 0
ensemble_tree_validation_accuracy <- 0
ensemble_tree_validation_F1_score <- 0
ensemble_tree_holdout_true_positive_rate <- 0
ensemble_tree_holdout_true_negative_rate <- 0
ensemble_tree_holdout_false_positive_rate <- 0
ensemble_tree_holdout_false_negative_rate <- 0
ensemble_tree_holdout_accuracy <- 0
ensemble_tree_holdout_F1_score <- 0
ensemble_tree_duration <- 0
ensemble_tree_train_positive_predictive_value <- 0
ensemble_tree_train_negative_predictive_value <- 0
ensemble_tree_test_positive_predictive_value <- 0
ensemble_tree_test_negative_predictive_value <- 0
ensemble_tree_validation_positive_predictive_value <- 0
ensemble_tree_validation_negative_predictive_value <- 0
ensemble_tree_holdout_positive_predictive_value <- 0
ensemble_tree_holdout_negative_predictive_value <- 0
ensemble_tree_holdout_overfitting <- 0
ensemble_tree_holdout_overfitting_mean <- 0
ensemble_tree_table_total <- 0

ensemble_xgb_train_true_positive_rate <- 0
ensemble_xgb_train_true_negative_rate <- 0
ensemble_xgb_train_false_positive_rate <- 0
ensemble_xgb_train_false_negative_rate <- 0
ensemble_xgb_train_accuracy <- 0
ensemble_xgb_train_F1_score <- 0
ensemble_xgb_test_true_positive_rate <- 0
ensemble_xgb_test_true_negative_rate <- 0
ensemble_xgb_test_false_positive_rate <- 0
ensemble_xgb_test_false_negative_rate <- 0
ensemble_xgb_test_accuracy <- 0
ensemble_xgb_test_F1_score <- 0
ensemble_xgb_validation_true_positive_rate <- 0
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
ensemble_xgb_holdout_overfitting <- 0
ensemble_xgb_holdout_overfitting_mean <- 0
ensemble_xgb_table_total <- 0


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
  ggplot2::ggsave("barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

#### Summary of the dataset ####
data_summary <- reactable::reactable(round(as.data.frame(do.call(cbind, lapply(df, summary))), 4),
                                     searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                     striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Data summary")

#### Correlation plot of numeric data ####
df1 <- df %>% purrr::keep(is.numeric)
M1 <- cor(df1)
title <- "Correlation plot of the numerical data"
corrplot::corrplot(M1, method = "number", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow_com/a/14754408/54964)
corrplot::corrplot(M1, method = "circle", title = title, mar = c(0, 0, 1, 0)) # http://stackoverflow_com/a/14754408/54964)

#### message correlation matrix of numeric data ####
correlation_table <- reactable::reactable(round(cor(df), 4),
                                          searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                          striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Correlation of the data")

#### Boxplots of the numeric data ####
boxplots <- df1 %>%
  tidyr::gather(key = "var", value = "value") %>%
  ggplot2::ggplot(aes(x = "", y = value)) +
  ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  ggplot2::facet_wrap(~var, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Boxplots of the numeric data")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("boxplots.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("boxplots.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("boxplots.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("boxplots.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("boxplots.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("boxplots.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
# Thanks to https://rstudio-pubs-static_s3_amazonaws_com/388596_e21196f1adf04e0ea7cd68edd9eba966_html

histograms <- ggplot2::ggplot(tidyr::gather(df1, cols, value), aes(x = value)) +
  ggplot2::geom_histogram(bins = round(nrow(df1) / 10)) +
  ggplot2::facet_wrap(. ~ cols, scales = "free") +
  ggplot2::labs(title = "Histograms of each numeric column. Each bar = 10 rows of data")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("histograms.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("histograms.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("histograms.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("histograms.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("histograms.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("histograms.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


# Break into train, test and validation sets

for (i in 1:numresamples) {
  message(noquote(""))
  message(paste0("Resampling number ", i, " of ", numresamples, sep = ','))
  message(noquote(""))

  if(set_seed == "N"){

    index <- sample(c(1:3), nrow(df), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))

    train <- df[index == 1, ]
    test <- df[index == 2, ]
    validation <- df[index == 3, ]

    train01 <- train # needed to run xgboost
    test01 <- test # needed to run xgboost
    validation01 <- validation

    y_train <- train$y
    y_test <- test$y
    y_validation <- validation$y
  }

  if(set_seed == "Y"){
    train <- df[1:round(train_amount*nrow(df)), ]
    test <- df[round(train_amount*nrow(df)) +1:round(test_amount*nrow(df)), ]
    validation <- df[(nrow(test) + nrow(train) +1) : nrow(df), ]

    train01 <- train # needed to run xgboost
    test01 <- test # needed to run xgboost
    validation01 <- validation

    y_train <- train$y
    y_test <- test$y
    y_validation <- validation$y
  }

  #### cubist ####
  cubist_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    cubist_train_fit <- Cubist::cubist(x = as.data.frame(train), y = train$y)
  }
  if(set_seed == "N"){
    cubist_train_fit <- Cubist::cubist(x = as.data.frame(train), y = train$y)
  }
  cubist_train_pred <- stats::predict(cubist_train_fit, train01, type = "numeric")
  cubist_train_predictions <- plogis(cubist_train_pred)
  cubist_train_predictions_binomial <- rbinom(n = length(cubist_train_predictions), size = 1, prob = cubist_train_predictions)
  cubist_train_table <- table(cubist_train_predictions_binomial, y_train)
  cubist_train_true_positive_rate[i] <- cubist_train_table[2, 2] / sum(cubist_train_table[2, 2] + cubist_train_table[1, 2])
  cubist_train_true_positive_rate_mean <- mean(cubist_train_true_positive_rate)
  cubist_train_true_negative_rate[i] <- cubist_train_table[1, 1] / sum(cubist_train_table[1, 1] + cubist_train_table[2, 1])
  cubist_train_true_negative_rate_mean <- mean(cubist_train_true_negative_rate)
  cubist_train_false_positive_rate[i] <- cubist_train_table[2, 1] / sum(cubist_train_table[2, 1] + cubist_train_table[1, 1])
  cubist_train_false_positive_rate_mean <- mean(cubist_train_false_positive_rate)
  cubist_train_false_negative_rate[i] <- cubist_train_table[1, 2] / sum(cubist_train_table[1, 2] + cubist_train_table[2, 2])
  cubist_train_false_negative_rate_mean <- mean(cubist_train_false_negative_rate)
  cubist_train_accuracy[i] <- (cubist_train_table[1, 1] + cubist_train_table[2, 2]) / sum(cubist_train_table)
  cubist_train_accuracy_mean <- mean(cubist_train_accuracy)
  cubist_train_F1_score[i] <- 2 * (cubist_train_table[2, 2]) / sum(2 * cubist_train_table[2, 2] + cubist_train_table[1, 2] + cubist_train_table[2, 1])
  cubist_train_F1_score_mean <- mean(cubist_train_F1_score)
  cubist_train_positive_predictive_value[i] <- cubist_train_table[2, 2] / sum(cubist_train_table[2, 2] + cubist_train_table[2, 1])
  cubist_train_positive_predictive_value_mean <- mean(cubist_train_positive_predictive_value)
  cubist_train_negative_predictive_value[i] <- cubist_train_table[1, 1] / sum(cubist_train_table[1, 1] + cubist_train_table[1, 2])
  cubist_train_negative_predictive_value_mean <- mean(cubist_train_negative_predictive_value)

  cubist_test_pred <- stats::predict(cubist_train_fit, test01, type = "numeric")
  cubist_test_predictions <- plogis(cubist_test_pred)
  cubist_test_predictions_binomial <- rbinom(n = length(cubist_test_predictions), size = 1, prob = cubist_test_predictions)
  cubist_test_table <- table(cubist_test_predictions_binomial, y_test)
  cubist_test_true_positive_rate[i] <- cubist_test_table[2, 2] / sum(cubist_test_table[2, 2] + cubist_test_table[1, 2])
  cubist_test_true_positive_rate_mean <- mean(cubist_test_true_positive_rate)
  cubist_test_true_negative_rate[i] <- cubist_test_table[1, 1] / sum(cubist_test_table[1, 1] + cubist_test_table[2, 1])
  cubist_test_true_negative_rate_mean <- mean(cubist_test_true_negative_rate)
  cubist_test_false_positive_rate[i] <- cubist_test_table[2, 1] / sum(cubist_test_table[2, 1] + cubist_test_table[1, 1])
  cubist_test_false_positive_rate_mean <- mean(cubist_test_false_positive_rate)
  cubist_test_false_negative_rate[i] <- cubist_test_table[1, 2] / sum(cubist_test_table[1, 2] + cubist_test_table[2, 2])
  cubist_test_false_negative_rate_mean <- mean(cubist_test_false_negative_rate)
  cubist_test_accuracy[i] <- (cubist_test_table[1, 1] + cubist_test_table[2, 2]) / sum(cubist_test_table)
  cubist_test_accuracy_mean <- mean(cubist_test_accuracy)
  cubist_test_F1_score[i] <- 2 * (cubist_test_table[2, 2]) / sum(2 * cubist_test_table[2, 2] + cubist_test_table[1, 2] + cubist_test_table[2, 1])
  cubist_test_F1_score_mean <- mean(cubist_test_F1_score)
  cubist_test_positive_predictive_value[i] <- cubist_test_table[2, 2] / sum(cubist_test_table[2, 2] + cubist_test_table[2, 1])
  cubist_test_positive_predictive_value_mean <- mean(cubist_test_positive_predictive_value)
  cubist_test_negative_predictive_value[i] <- cubist_test_table[1, 1] / sum(cubist_test_table[1, 1] + cubist_test_table[1, 2])
  cubist_test_negative_predictive_value_mean <- mean(cubist_test_negative_predictive_value)

  cubist_validation_pred <- stats::predict(cubist_train_fit, validation01, type = "numeric")
  cubist_validation_predictions <- plogis(cubist_validation_pred)
  cubist_validation_predictions_binomial <- rbinom(n = length(cubist_validation_predictions), size = 1, prob = cubist_validation_predictions)
  cubist_validation_table <- table(cubist_validation_predictions_binomial, y_validation)
  cubist_validation_true_positive_rate[i] <- cubist_validation_table[2, 2] / sum(cubist_validation_table[2, 2] + cubist_validation_table[1, 2])
  cubist_validation_true_positive_rate_mean <- mean(cubist_validation_true_positive_rate)
  cubist_validation_true_negative_rate[i] <- cubist_validation_table[1, 1] / sum(cubist_validation_table[1, 1] + cubist_validation_table[2, 1])
  cubist_validation_true_negative_rate_mean <- mean(cubist_validation_true_negative_rate)
  cubist_validation_false_positive_rate[i] <- cubist_validation_table[2, 1] / sum(cubist_validation_table[2, 1] + cubist_validation_table[1, 1])
  cubist_validation_false_positive_rate_mean <- mean(cubist_validation_false_positive_rate)
  cubist_validation_false_negative_rate[i] <- cubist_validation_table[1, 2] / sum(cubist_validation_table[1, 2] + cubist_validation_table[2, 2])
  cubist_validation_false_negative_rate_mean <- mean(cubist_validation_false_negative_rate)
  cubist_validation_accuracy[i] <- (cubist_validation_table[1, 1] + cubist_validation_table[2, 2]) / sum(cubist_validation_table)
  cubist_validation_accuracy_mean <- mean(cubist_validation_accuracy)
  cubist_validation_F1_score[i] <- 2 * (cubist_validation_table[2, 2]) / sum(2 * cubist_validation_table[2, 2] + cubist_validation_table[1, 2] + cubist_validation_table[2, 1])
  cubist_validation_F1_score_mean <- mean(cubist_validation_F1_score)
  cubist_validation_positive_predictive_value[i] <- cubist_validation_table[2, 2] / sum(cubist_validation_table[2, 2] + cubist_validation_table[2, 1])
  cubist_validation_positive_predictive_value_mean <- mean(cubist_validation_positive_predictive_value)
  cubist_validation_negative_predictive_value[i] <- cubist_validation_table[1, 1] / sum(cubist_validation_table[1, 1] + cubist_validation_table[1, 2])
  cubist_validation_negative_predictive_value_mean <- mean(cubist_validation_negative_predictive_value)

  cubist_holdout_true_positive_rate[i] <- (cubist_test_true_positive_rate[i] + cubist_validation_true_positive_rate[i]) / 2
  cubist_holdout_true_positive_rate_mean <- mean(cubist_holdout_true_positive_rate)
  cubist_holdout_true_negative_rate[i] <- (cubist_test_true_negative_rate[i] + cubist_validation_true_negative_rate[i]) / 2
  cubist_holdout_true_negative_rate_mean <- mean(cubist_holdout_true_negative_rate)
  cubist_holdout_false_positive_rate[i] <- (cubist_test_false_positive_rate[i] + cubist_validation_false_positive_rate[i]) / 2
  cubist_holdout_false_positive_rate_mean <- mean(cubist_holdout_false_positive_rate)
  cubist_holdout_false_negative_rate[i] <- (cubist_test_false_negative_rate[i] + cubist_validation_false_negative_rate[i]) / 2
  cubist_holdout_false_negative_rate_mean <- mean(cubist_holdout_false_negative_rate)
  cubist_holdout_accuracy[i] <- (cubist_test_accuracy[i] + cubist_validation_accuracy[i]) / 2
  cubist_holdout_accuracy_mean <- mean(cubist_holdout_accuracy)
  cubist_holdout_accuracy_sd <- sd(cubist_holdout_accuracy)
  cubist_holdout_F1_score[i] <- (cubist_test_F1_score[i] + cubist_validation_F1_score[i]) / 2
  cubist_holdout_F1_score_mean <- mean(cubist_holdout_F1_score)
  cubist_holdout_positive_predictive_value[i] <- (cubist_test_positive_predictive_value[i] + cubist_validation_positive_predictive_value[i]) / 2
  cubist_holdout_positive_predictive_value_mean <- mean(cubist_holdout_positive_predictive_value)
  cubist_holdout_negative_predictive_value[i] <- (cubist_test_negative_predictive_value[i] + cubist_validation_negative_predictive_value[i]) / 2
  cubist_holdout_negative_predictive_value_mean <- mean(cubist_holdout_negative_predictive_value)
  cubist_holdout_overfitting[i] <- cubist_holdout_accuracy[i] / cubist_train_accuracy[i]
  cubist_holdout_overfitting_mean <- mean(cubist_holdout_overfitting)
  cubist_holdout_overfitting_range <- range(cubist_holdout_overfitting)
  cubist_holdout_overfitting_sd <- sd(cubist_holdout_overfitting)

  cubist_table <- cubist_test_table + cubist_validation_table
  cubist_table_total <- cubist_table_total + cubist_table

  cubist_end <- Sys.time()
  cubist_duration[i] <- cubist_end - cubist_start
  cubist_duration_mean <- mean(cubist_duration)
  cubist_duration_sd <- sd(cubist_duration)

  #### Flexible Discriminant Analysis ####

  fda_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    fda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "FDAModel")
  }
  if(set_seed == "N"){
    fda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "FDAModel")
  }
  fda_train_pred <- stats::predict(fda_train_fit, train01, type = "response")
  fda_train_predictions <- plogis(as.numeric(fda_train_pred))
  fda_train_predictions_binomial <- rbinom(n = length(fda_train_predictions), size = 1, prob = fda_train_predictions)
  fda_train_table <- table(fda_train_predictions_binomial, y_train)
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

  fda_test_pred <- stats::predict(fda_train_fit, test01, type = "response")
  fda_test_predictions <- plogis(as.numeric(fda_test_pred))
  fda_test_predictions_binomial <- rbinom(n = length(fda_test_predictions), size = 1, prob = fda_test_predictions)
  fda_test_table <- table(fda_test_predictions_binomial, y_test)
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

  fda_validation_pred <- stats::predict(fda_train_fit, validation01, type = "response")
  fda_validation_predictions <- plogis(as.numeric(fda_validation_pred))
  fda_validation_predictions_binomial <- rbinom(n = length(fda_validation_predictions), size = 1, prob = fda_validation_predictions)
  fda_validation_table <- table(fda_validation_predictions_binomial, y_validation)
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

  fda_table <- fda_test_table + fda_validation_table
  fda_table_total <- fda_table_total + fda_table

  fda_end <- Sys.time()
  fda_duration[i] <- fda_end - fda_start
  fda_duration_mean <- mean(fda_duration)
  fda_duration_sd <- sd(fda_duration)


  #### Generalized Additive Models ####
  gam_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    gam_train_fit <- gam::gam(y ~ ., data = train)
  }
  if(set_seed == "N"){
    gam_train_fit <- gam::gam(y ~ ., data = train)
  }
  gam_train_pred <- stats::predict(gam_train_fit, train01, type = "response")
  gam_train_predictions <- as.numeric(plogis(gam_train_pred))
  gam_train_predictions_binomial <- rbinom(n = length(gam_train_predictions), size = 1, prob = gam_train_predictions)
  gam_train_table <- table(gam_train_predictions_binomial, y_train)
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
  gam_test_predictions <- as.numeric(plogis(gam_test_pred))
  gam_test_predictions_binomial <- rbinom(n = length(gam_test_predictions), size = 1, prob = gam_test_predictions)
  gam_test_table <- table(gam_test_predictions_binomial, y_test)
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
  gam_validation_predictions <- as.numeric(plogis(gam_validation_pred))
  gam_validation_predictions_binomial <- rbinom(n = length(gam_validation_predictions), size = 1, prob = gam_validation_predictions)
  gam_validation_table <- table(gam_validation_predictions_binomial, y_validation)
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

  gam_table <- gam_test_table + gam_validation_table
  gam_table_total <- gam_table_total + gam_table

  gam_end <- Sys.time()
  gam_duration[i] <- gam_end - gam_start
  gam_duration_mean <- mean(gam_duration)
  gam_duration_sd <- sd(gam_duration)


  #### Generalized Linear Models ####
  glm_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    glm_train_fit <- stats::glm(y ~ ., data = train, family = binomial(link = "logit"))
  }
  if(set_seed == "N"){
    glm_train_fit <- stats::glm(y ~ ., data = train, family = binomial(link = "logit"))
  }
  glm_train_pred <- stats::predict(glm_train_fit, train01, type = "response")
  glm_train_predictions <- plogis(glm_train_pred)
  glm_train_predictions_binomial <- rbinom(n = length(glm_train_predictions), size = 1, prob = glm_train_predictions)
  glm_train_table <- table(glm_train_predictions_binomial, y_train)
  glm_train_true_positive_rate[i] <- glm_train_table[2, 2] / sum(glm_train_table[2, 2] + glm_train_table[1, 2])
  glm_train_true_positive_rate_mean <- mean(glm_train_true_positive_rate)
  glm_train_true_negative_rate[i] <- glm_train_table[1, 1] / sum(glm_train_table[1, 1] + glm_train_table[2, 1])
  glm_train_true_negative_rate_mean <- mean(glm_train_true_negative_rate)
  glm_train_false_positive_rate[i] <- glm_train_table[2, 1] / sum(glm_train_table[2, 1] + glm_train_table[1, 1])
  glm_train_false_positive_rate_mean <- mean(glm_train_false_positive_rate)
  glm_train_false_negative_rate[i] <- glm_train_table[1, 2] / sum(glm_train_table[1, 2] + glm_train_table[2, 2])
  glm_train_false_negative_rate_mean <- mean(glm_train_false_negative_rate)
  glm_train_accuracy[i] <- (glm_train_table[1, 1] + glm_train_table[2, 2]) / sum(glm_train_table)
  glm_train_accuracy_mean <- mean(glm_train_accuracy)
  glm_train_F1_score[i] <- 2 * (glm_train_table[2, 2]) / sum(2 * glm_train_table[2, 2] + glm_train_table[1, 2] + glm_train_table[2, 1])
  glm_train_F1_score_mean <- mean(glm_train_F1_score)
  glm_train_positive_predictive_value[i] <- glm_train_table[2, 2] / sum(glm_train_table[2, 2] + glm_train_table[2, 1])
  glm_train_positive_predictive_value_mean <- mean(glm_train_positive_predictive_value)
  glm_train_negative_predictive_value[i] <- glm_train_table[1, 1] / sum(glm_train_table[1, 1] + glm_train_table[1, 2])
  glm_train_negative_predictive_value_mean <- mean(glm_train_negative_predictive_value)

  glm_test_pred <- stats::predict(glm_train_fit, test01, type = "response")
  glm_test_predictions <- plogis(glm_test_pred)
  glm_test_predictions_binomial <- rbinom(n = length(glm_test_predictions), size = 1, prob = glm_test_predictions)
  glm_test_table <- table(glm_test_predictions_binomial, y_test)
  glm_test_true_positive_rate[i] <- glm_test_table[2, 2] / sum(glm_test_table[2, 2] + glm_test_table[1, 2])
  glm_test_true_positive_rate_mean <- mean(glm_test_true_positive_rate)
  glm_test_true_negative_rate[i] <- glm_test_table[1, 1] / sum(glm_test_table[1, 1] + glm_test_table[2, 1])
  glm_test_true_negative_rate_mean <- mean(glm_test_true_negative_rate)
  glm_test_false_positive_rate[i] <- glm_test_table[2, 1] / sum(glm_test_table[2, 1] + glm_test_table[1, 1])
  glm_test_false_positive_rate_mean <- mean(glm_test_false_positive_rate)
  glm_test_false_negative_rate[i] <- glm_test_table[1, 2] / sum(glm_test_table[1, 2] + glm_test_table[2, 2])
  glm_test_false_negative_rate_mean <- mean(glm_test_false_negative_rate)
  glm_test_accuracy[i] <- (glm_test_table[1, 1] + glm_test_table[2, 2]) / sum(glm_test_table)
  glm_test_accuracy_mean <- mean(glm_test_accuracy)
  glm_test_F1_score[i] <- 2 * (glm_test_table[2, 2]) / sum(2 * glm_test_table[2, 2] + glm_test_table[1, 2] + glm_test_table[2, 1])
  glm_test_F1_score_mean <- mean(glm_test_F1_score)
  glm_test_positive_predictive_value[i] <- glm_test_table[2, 2] / sum(glm_test_table[2, 2] + glm_test_table[2, 1])
  glm_test_positive_predictive_value_mean <- mean(glm_test_positive_predictive_value)
  glm_test_negative_predictive_value[i] <- glm_test_table[1, 1] / sum(glm_test_table[1, 1] + glm_test_table[1, 2])
  glm_test_negative_predictive_value_mean <- mean(glm_test_negative_predictive_value)

  glm_validation_pred <- stats::predict(glm_train_fit, validation01, type = "response")
  glm_validation_predictions <- plogis(glm_validation_pred)
  glm_validation_predictions_binomial <- rbinom(n = length(glm_validation_predictions), size = 1, prob = glm_validation_predictions)
  glm_validation_table <- table(glm_validation_predictions_binomial, y_validation)
  glm_validation_true_positive_rate[i] <- glm_validation_table[2, 2] / sum(glm_validation_table[2, 2] + glm_validation_table[1, 2])
  glm_validation_true_positive_rate_mean <- mean(glm_validation_true_positive_rate)
  glm_validation_true_negative_rate[i] <- glm_validation_table[1, 1] / sum(glm_validation_table[1, 1] + glm_validation_table[2, 1])
  glm_validation_true_negative_rate_mean <- mean(glm_validation_true_negative_rate)
  glm_validation_false_positive_rate[i] <- glm_validation_table[2, 1] / sum(glm_validation_table[2, 1] + glm_validation_table[1, 1])
  glm_validation_false_positive_rate_mean <- mean(glm_validation_false_positive_rate)
  glm_validation_false_negative_rate[i] <- glm_validation_table[1, 2] / sum(glm_validation_table[1, 2] + glm_validation_table[2, 2])
  glm_validation_false_negative_rate_mean <- mean(glm_validation_false_negative_rate)
  glm_validation_accuracy[i] <- (glm_validation_table[1, 1] + glm_validation_table[2, 2]) / sum(glm_validation_table)
  glm_validation_accuracy_mean <- mean(glm_validation_accuracy)
  glm_validation_F1_score[i] <- 2 * (glm_validation_table[2, 2]) / sum(2 * glm_validation_table[2, 2] + glm_validation_table[1, 2] + glm_validation_table[2, 1])
  glm_validation_F1_score_mean <- mean(glm_validation_F1_score)
  glm_validation_positive_predictive_value[i] <- glm_validation_table[2, 2] / sum(glm_validation_table[2, 2] + glm_validation_table[2, 1])
  glm_validation_positive_predictive_value_mean <- mean(glm_validation_positive_predictive_value)
  glm_validation_negative_predictive_value[i] <- glm_validation_table[1, 1] / sum(glm_validation_table[1, 1] + glm_validation_table[1, 2])
  glm_validation_negative_predictive_value_mean <- mean(glm_validation_negative_predictive_value)

  glm_holdout_true_positive_rate[i] <- (glm_test_true_positive_rate[i] + glm_validation_true_positive_rate[i]) / 2
  glm_holdout_true_positive_rate_mean <- mean(glm_holdout_true_positive_rate)
  glm_holdout_true_negative_rate[i] <- (glm_test_true_negative_rate[i] + glm_validation_true_negative_rate[i]) / 2
  glm_holdout_true_negative_rate_mean <- mean(glm_holdout_true_negative_rate)
  glm_holdout_false_positive_rate[i] <- (glm_test_false_positive_rate[i] + glm_validation_false_positive_rate[i]) / 2
  glm_holdout_false_positive_rate_mean <- mean(glm_holdout_false_positive_rate)
  glm_holdout_false_negative_rate[i] <- (glm_test_false_negative_rate[i] + glm_validation_false_negative_rate[i]) / 2
  glm_holdout_false_negative_rate_mean <- mean(glm_holdout_false_negative_rate)
  glm_holdout_accuracy[i] <- (glm_test_accuracy[i] + glm_validation_accuracy[i]) / 2
  glm_holdout_accuracy_mean <- mean(glm_holdout_accuracy)
  glm_holdout_accuracy_sd <- sd(glm_holdout_accuracy)
  glm_holdout_F1_score[i] <- (glm_test_F1_score[i] + glm_validation_F1_score[i]) / 2
  glm_holdout_F1_score_mean <- mean(glm_holdout_F1_score)
  glm_holdout_positive_predictive_value[i] <- (glm_test_positive_predictive_value[i] + glm_validation_positive_predictive_value[i]) / 2
  glm_holdout_positive_predictive_value_mean <- mean(glm_holdout_positive_predictive_value)
  glm_holdout_negative_predictive_value[i] <- (glm_test_negative_predictive_value[i] + glm_validation_negative_predictive_value[i]) / 2
  glm_holdout_negative_predictive_value_mean <- mean(glm_holdout_negative_predictive_value)
  glm_holdout_overfitting[i] <- glm_holdout_accuracy[i] / glm_train_accuracy[i]
  glm_holdout_overfitting_mean <- mean(glm_holdout_overfitting)
  glm_holdout_overfitting_range <- range(glm_holdout_overfitting)
  glm_holdout_overfitting_sd <- sd(glm_holdout_overfitting)

  glm_table <- glm_test_table + glm_validation_table
  glm_table_total <- glm_table_total + glm_table

  glm_end <- Sys.time()
  glm_duration[i] <- glm_end - glm_start
  glm_duration_mean <- mean(glm_duration)
  glm_duration_sd <- sd(glm_duration)


  #### Linear ####
  linear_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    linear_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "LMModel")
  }
  if(set_seed == "N"){
    linear_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "LMModel")
  }
  linear_train_pred <- stats::predict(linear_train_fit, train01, type = "response")
  linear_train_predictions <- plogis(as.numeric(linear_train_pred))
  linear_train_predictions_binomial <- rbinom(n = length(linear_train_predictions), size = 1, prob = linear_train_predictions)
  linear_train_table <- table(linear_train_predictions_binomial, y_train)
  linear_train_true_positive_rate[i] <- linear_train_table[2, 2] / sum(linear_train_table[2, 2] + linear_train_table[1, 2])
  linear_train_true_positive_rate_mean <- mean(linear_train_true_positive_rate)
  linear_train_true_negative_rate[i] <- linear_train_table[1, 1] / sum(linear_train_table[1, 1] + linear_train_table[2, 1])
  linear_train_true_negative_rate_mean <- mean(linear_train_true_negative_rate)
  linear_train_false_positive_rate[i] <- linear_train_table[2, 1] / sum(linear_train_table[2, 1] + linear_train_table[1, 1])
  linear_train_false_positive_rate_mean <- mean(linear_train_false_positive_rate)
  linear_train_false_negative_rate[i] <- linear_train_table[1, 2] / sum(linear_train_table[1, 2] + linear_train_table[2, 2])
  linear_train_false_negative_rate_mean <- mean(linear_train_false_negative_rate)
  linear_train_accuracy[i] <- (linear_train_table[1, 1] + linear_train_table[2, 2]) / sum(linear_train_table)
  linear_train_accuracy_mean <- mean(linear_train_accuracy)
  linear_train_F1_score[i] <- 2 * (linear_train_table[2, 2]) / sum(2 * linear_train_table[2, 2] + linear_train_table[1, 2] + linear_train_table[2, 1])
  linear_train_F1_score_mean <- mean(linear_train_F1_score)
  linear_train_positive_predictive_value[i] <- linear_train_table[2, 2] / sum(linear_train_table[2, 2] + linear_train_table[2, 1])
  linear_train_positive_predictive_value_mean <- mean(linear_train_positive_predictive_value)
  linear_train_negative_predictive_value[i] <- linear_train_table[1, 1] / sum(linear_train_table[1, 1] + linear_train_table[1, 2])
  linear_train_negative_predictive_value_mean <- mean(linear_train_negative_predictive_value)

  linear_test_pred <- stats::predict(linear_train_fit, test01, type = "response")
  linear_test_predictions <- plogis(as.numeric(linear_test_pred))
  linear_test_predictions_binomial <- rbinom(n = length(linear_test_predictions), size = 1, prob = linear_test_predictions)
  linear_test_table <- table(linear_test_predictions_binomial, y_test)
  linear_test_true_positive_rate[i] <- linear_test_table[2, 2] / sum(linear_test_table[2, 2] + linear_test_table[1, 2])
  linear_test_true_positive_rate_mean <- mean(linear_test_true_positive_rate)
  linear_test_true_negative_rate[i] <- linear_test_table[1, 1] / sum(linear_test_table[1, 1] + linear_test_table[2, 1])
  linear_test_true_negative_rate_mean <- mean(linear_test_true_negative_rate)
  linear_test_false_positive_rate[i] <- linear_test_table[2, 1] / sum(linear_test_table[2, 1] + linear_test_table[1, 1])
  linear_test_false_positive_rate_mean <- mean(linear_test_false_positive_rate)
  linear_test_false_negative_rate[i] <- linear_test_table[1, 2] / sum(linear_test_table[1, 2] + linear_test_table[2, 2])
  linear_test_false_negative_rate_mean <- mean(linear_test_false_negative_rate)
  linear_test_accuracy[i] <- (linear_test_table[1, 1] + linear_test_table[2, 2]) / sum(linear_test_table)
  linear_test_accuracy_mean <- mean(linear_test_accuracy)
  linear_test_F1_score[i] <- 2 * (linear_test_table[2, 2]) / sum(2 * linear_test_table[2, 2] + linear_test_table[1, 2] + linear_test_table[2, 1])
  linear_test_F1_score_mean <- mean(linear_test_F1_score)
  linear_test_positive_predictive_value[i] <- linear_test_table[2, 2] / sum(linear_test_table[2, 2] + linear_test_table[2, 1])
  linear_test_positive_predictive_value_mean <- mean(linear_test_positive_predictive_value)
  linear_test_negative_predictive_value[i] <- linear_test_table[1, 1] / sum(linear_test_table[1, 1] + linear_test_table[1, 2])
  linear_test_negative_predictive_value_mean <- mean(linear_test_negative_predictive_value)

  linear_validation_pred <- stats::predict(linear_train_fit, validation01, type = "response")
  linear_validation_predictions <- plogis(as.numeric(linear_validation_pred))
  linear_validation_predictions_binomial <- rbinom(n = length(linear_validation_predictions), size = 1, prob = linear_validation_predictions)
  linear_validation_table <- table(linear_validation_predictions_binomial, y_validation)
  linear_validation_true_positive_rate[i] <- linear_validation_table[2, 2] / sum(linear_validation_table[2, 2] + linear_validation_table[1, 2])
  linear_validation_true_positive_rate_mean <- mean(linear_validation_true_positive_rate)
  linear_validation_true_negative_rate[i] <- linear_validation_table[1, 1] / sum(linear_validation_table[1, 1] + linear_validation_table[2, 1])
  linear_validation_true_negative_rate_mean <- mean(linear_validation_true_negative_rate)
  linear_validation_false_positive_rate[i] <- linear_validation_table[2, 1] / sum(linear_validation_table[2, 1] + linear_validation_table[1, 1])
  linear_validation_false_positive_rate_mean <- mean(linear_validation_false_positive_rate)
  linear_validation_false_negative_rate[i] <- linear_validation_table[1, 2] / sum(linear_validation_table[1, 2] + linear_validation_table[2, 2])
  linear_validation_false_negative_rate_mean <- mean(linear_validation_false_negative_rate)
  linear_validation_accuracy[i] <- (linear_validation_table[1, 1] + linear_validation_table[2, 2]) / sum(linear_validation_table)
  linear_validation_accuracy_mean <- mean(linear_validation_accuracy)
  linear_validation_F1_score[i] <- 2 * (linear_validation_table[2, 2]) / sum(2 * linear_validation_table[2, 2] + linear_validation_table[1, 2] + linear_validation_table[2, 1])
  linear_validation_F1_score_mean <- mean(linear_validation_F1_score)
  linear_validation_positive_predictive_value[i] <- linear_validation_table[2, 2] / sum(linear_validation_table[2, 2] + linear_validation_table[2, 1])
  linear_validation_positive_predictive_value_mean <- mean(linear_validation_positive_predictive_value)
  linear_validation_negative_predictive_value[i] <- linear_validation_table[1, 1] / sum(linear_validation_table[1, 1] + linear_validation_table[1, 2])
  linear_validation_negative_predictive_value_mean <- mean(linear_validation_negative_predictive_value)

  linear_holdout_true_positive_rate[i] <- (linear_test_true_positive_rate[i] + linear_validation_true_positive_rate[i]) / 2
  linear_holdout_true_positive_rate_mean <- mean(linear_holdout_true_positive_rate)
  linear_holdout_true_negative_rate[i] <- (linear_test_true_negative_rate[i] + linear_validation_true_negative_rate[i]) / 2
  linear_holdout_true_negative_rate_mean <- mean(linear_holdout_true_negative_rate)
  linear_holdout_false_positive_rate[i] <- (linear_test_false_positive_rate[i] + linear_validation_false_positive_rate[i]) / 2
  linear_holdout_false_positive_rate_mean <- mean(linear_holdout_false_positive_rate)
  linear_holdout_false_negative_rate[i] <- (linear_test_false_negative_rate[i] + linear_validation_false_negative_rate[i]) / 2
  linear_holdout_false_negative_rate_mean <- mean(linear_holdout_false_negative_rate)
  linear_holdout_accuracy[i] <- (linear_test_accuracy[i] + linear_validation_accuracy[i]) / 2
  linear_holdout_accuracy_mean <- mean(linear_holdout_accuracy)
  linear_holdout_accuracy_sd <- sd(linear_holdout_accuracy)
  linear_holdout_F1_score[i] <- (linear_test_F1_score[i] + linear_validation_F1_score[i]) / 2
  linear_holdout_F1_score_mean <- mean(linear_holdout_F1_score)
  linear_end <- Sys.time()
  linear_duration[i] <- linear_end - linear_start
  linear_duration_mean <- mean(linear_duration)
  linear_holdout_positive_predictive_value[i] <- (linear_test_positive_predictive_value[i] + linear_validation_positive_predictive_value[i]) / 2
  linear_holdout_positive_predictive_value_mean <- mean(linear_holdout_positive_predictive_value)
  linear_holdout_negative_predictive_value[i] <- (linear_test_negative_predictive_value[i] + linear_validation_negative_predictive_value[i]) / 2
  linear_holdout_negative_predictive_value_mean <- mean(linear_holdout_negative_predictive_value)
  linear_holdout_overfitting[i] <- linear_holdout_accuracy[i] / linear_train_accuracy[i]
  linear_holdout_overfitting_mean <- mean(linear_holdout_overfitting)
  linear_holdout_overfitting_range <- range(linear_holdout_overfitting)
  linear_holdout_overfitting_sd <- sd(linear_holdout_overfitting)

  linear_table <- linear_test_table + linear_validation_table
  linear_table_total <- linear_table_total + linear_table

  linear_end <- Sys.time()
  linear_duration[i] <- linear_end - linear_start
  linear_duration_mean <- mean(linear_duration)
  linear_duration_sd <- sd(linear_duration)

  #### Linear Discriminant Analysis ####
  lda_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    lda_train_fit <- MASS::lda(as.factor(y) ~ ., data = train01, model = "LMModel")
  }
  if(set_seed == "N"){
    lda_train_fit <- MASS::lda(as.factor(y) ~ ., data = train01, model = "LMModel")
  }
  lda_train_pred <- stats::predict(lda_train_fit, train01, type = "response")
  lda_train_predictions <- plogis(as.numeric(lda_train_pred$class))
  lda_train_predictions_binomial <- rbinom(n = length(lda_train_predictions), size = 1, prob = lda_train_predictions)
  lda_train_table <- table(lda_train_predictions_binomial, y_train)
  lda_train_true_positive_rate[i] <- lda_train_table[2, 2] / sum(lda_train_table[2, 2] + lda_train_table[1, 2])
  lda_train_true_positive_rate_mean <- mean(lda_train_true_positive_rate)
  lda_train_true_negative_rate[i] <- lda_train_table[1, 1] / sum(lda_train_table[1, 1] + lda_train_table[2, 1])
  lda_train_true_negative_rate_mean <- mean(lda_train_true_negative_rate)
  lda_train_false_positive_rate[i] <- lda_train_table[2, 1] / sum(lda_train_table[2, 1] + lda_train_table[1, 1])
  lda_train_false_positive_rate_mean <- mean(lda_train_false_positive_rate)
  lda_train_false_negative_rate[i] <- lda_train_table[1, 2] / sum(lda_train_table[1, 2] + lda_train_table[2, 2])
  lda_train_false_negative_rate_mean <- mean(lda_train_false_negative_rate)
  lda_train_accuracy[i] <- (lda_train_table[1, 1] + lda_train_table[2, 2]) / sum(lda_train_table)
  lda_train_accuracy_mean <- mean(lda_train_accuracy)
  lda_train_F1_score[i] <- 2 * (lda_train_table[2, 2]) / sum(2 * lda_train_table[2, 2] + lda_train_table[1, 2] + lda_train_table[2, 1])
  lda_train_F1_score_mean <- mean(lda_train_F1_score)
  lda_train_positive_predictive_value[i] <- lda_train_table[2, 2] / sum(lda_train_table[2, 2] + lda_train_table[2, 1])
  lda_train_positive_predictive_value_mean <- mean(lda_train_positive_predictive_value)
  lda_train_negative_predictive_value[i] <- lda_train_table[1, 1] / sum(lda_train_table[1, 1] + lda_train_table[1, 2])
  lda_train_negative_predictive_value_mean <- mean(lda_train_negative_predictive_value)

  lda_test_pred <- stats::predict(lda_train_fit, test01, type = "response")
  lda_test_predictions <- plogis(as.numeric(lda_test_pred$class))
  lda_test_predictions_binomial <- rbinom(n = length(lda_test_predictions), size = 1, prob = lda_test_predictions)
  lda_test_table <- table(lda_test_predictions_binomial, y_test)
  lda_test_true_positive_rate[i] <- lda_test_table[2, 2] / sum(lda_test_table[2, 2] + lda_test_table[1, 2])
  lda_test_true_positive_rate_mean <- mean(lda_test_true_positive_rate)
  lda_test_true_negative_rate[i] <- lda_test_table[1, 1] / sum(lda_test_table[1, 1] + lda_test_table[2, 1])
  lda_test_true_negative_rate_mean <- mean(lda_test_true_negative_rate)
  lda_test_false_positive_rate[i] <- lda_test_table[2, 1] / sum(lda_test_table[2, 1] + lda_test_table[1, 1])
  lda_test_false_positive_rate_mean <- mean(lda_test_false_positive_rate)
  lda_test_false_negative_rate[i] <- lda_test_table[1, 2] / sum(lda_test_table[1, 2] + lda_test_table[2, 2])
  lda_test_false_negative_rate_mean <- mean(lda_test_false_negative_rate)
  lda_test_accuracy[i] <- (lda_test_table[1, 1] + lda_test_table[2, 2]) / sum(lda_test_table)
  lda_test_accuracy_mean <- mean(lda_test_accuracy)
  lda_test_F1_score[i] <- 2 * (lda_test_table[2, 2]) / sum(2 * lda_test_table[2, 2] + lda_test_table[1, 2] + lda_test_table[2, 1])
  lda_test_F1_score_mean <- mean(lda_test_F1_score)
  lda_test_positive_predictive_value[i] <- lda_test_table[2, 2] / sum(lda_test_table[2, 2] + lda_test_table[2, 1])
  lda_test_positive_predictive_value_mean <- mean(lda_test_positive_predictive_value)
  lda_test_negative_predictive_value[i] <- lda_test_table[1, 1] / sum(lda_test_table[1, 1] + lda_test_table[1, 2])
  lda_test_negative_predictive_value_mean <- mean(lda_test_negative_predictive_value)

  lda_validation_pred <- stats::predict(lda_train_fit, validation01, type = "response")
  lda_validation_predictions <- plogis(as.numeric(lda_validation_pred$class))
  lda_validation_predictions_binomial <- rbinom(n = length(lda_validation_predictions), size = 1, prob = lda_validation_predictions)
  lda_validation_table <- table(lda_validation_predictions_binomial, y_validation)
  lda_validation_true_positive_rate[i] <- lda_validation_table[2, 2] / sum(lda_validation_table[2, 2] + lda_validation_table[1, 2])
  lda_validation_true_positive_rate_mean <- mean(lda_validation_true_positive_rate)
  lda_validation_true_negative_rate[i] <- lda_validation_table[1, 1] / sum(lda_validation_table[1, 1] + lda_validation_table[2, 1])
  lda_validation_true_negative_rate_mean <- mean(lda_validation_true_negative_rate)
  lda_validation_false_positive_rate[i] <- lda_validation_table[2, 1] / sum(lda_validation_table[2, 1] + lda_validation_table[1, 1])
  lda_validation_false_positive_rate_mean <- mean(lda_validation_false_positive_rate)
  lda_validation_false_negative_rate[i] <- lda_validation_table[1, 2] / sum(lda_validation_table[1, 2] + lda_validation_table[2, 2])
  lda_validation_false_negative_rate_mean <- mean(lda_validation_false_negative_rate)
  lda_validation_accuracy[i] <- (lda_validation_table[1, 1] + lda_validation_table[2, 2]) / sum(lda_validation_table)
  lda_validation_accuracy_mean <- mean(lda_validation_accuracy)
  lda_validation_F1_score[i] <- 2 * (lda_validation_table[2, 2]) / sum(2 * lda_validation_table[2, 2] + lda_validation_table[1, 2] + lda_validation_table[2, 1])
  lda_validation_F1_score_mean <- mean(lda_validation_F1_score)
  lda_validation_positive_predictive_value[i] <- lda_validation_table[2, 2] / sum(lda_validation_table[2, 2] + lda_validation_table[2, 1])
  lda_validation_positive_predictive_value_mean <- mean(lda_validation_positive_predictive_value)
  lda_validation_negative_predictive_value[i] <- lda_validation_table[1, 1] / sum(lda_validation_table[1, 1] + lda_validation_table[1, 2])
  lda_validation_negative_predictive_value_mean <- mean(lda_validation_negative_predictive_value)

  lda_holdout_true_positive_rate[i] <- (lda_test_true_positive_rate[i] + lda_validation_true_positive_rate[i]) / 2
  lda_holdout_true_positive_rate_mean <- mean(lda_holdout_true_positive_rate)
  lda_holdout_true_negative_rate[i] <- (lda_test_true_negative_rate[i] + lda_validation_true_negative_rate[i]) / 2
  lda_holdout_true_negative_rate_mean <- mean(lda_holdout_true_negative_rate)
  lda_holdout_false_positive_rate[i] <- (lda_test_false_positive_rate[i] + lda_validation_false_positive_rate[i]) / 2
  lda_holdout_false_positive_rate_mean <- mean(lda_holdout_false_positive_rate)
  lda_holdout_false_negative_rate[i] <- (lda_test_false_negative_rate[i] + lda_validation_false_negative_rate[i]) / 2
  lda_holdout_false_negative_rate_mean <- mean(lda_holdout_false_negative_rate)
  lda_holdout_accuracy[i] <- (lda_test_accuracy[i] + lda_validation_accuracy[i]) / 2
  lda_holdout_accuracy_mean <- mean(lda_holdout_accuracy)
  lda_holdout_accuracy_sd <- sd(lda_holdout_accuracy)
  lda_holdout_F1_score[i] <- (lda_test_F1_score[i] + lda_validation_F1_score[i]) / 2
  lda_holdout_F1_score_mean <- mean(lda_holdout_F1_score)
  lda_holdout_positive_predictive_value[i] <- (lda_test_positive_predictive_value[i] + lda_validation_positive_predictive_value[i]) / 2
  lda_holdout_positive_predictive_value_mean <- mean(lda_holdout_positive_predictive_value)
  lda_holdout_negative_predictive_value[i] <- (lda_test_negative_predictive_value[i] + lda_validation_negative_predictive_value[i]) / 2
  lda_holdout_negative_predictive_value_mean <- mean(lda_holdout_negative_predictive_value)
  lda_holdout_overfitting[i] <- lda_holdout_accuracy[i] / lda_train_accuracy[i]
  lda_holdout_overfitting_mean <- mean(lda_holdout_overfitting)
  lda_holdout_overfitting_range <- range(lda_holdout_overfitting)
  lda_holdout_overfitting_sd <- sd(lda_holdout_overfitting)

  lda_table <- lda_test_table + lda_validation_table
  lda_table_total <- lda_table_total + lda_table

  lda_end <- Sys.time()
  lda_duration[i] <- lda_end - lda_start
  lda_duration_mean <- mean(lda_duration)
  lda_duration_sd <- sd(lda_duration)


  #### Penalized Discriminant Analysis ####
  pda_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    pda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "PDAModel")
  }
  if(set_seed == "N"){
    pda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "PDAModel")
  }
  pda_train_pred <- stats::predict(pda_train_fit, train01, type = "response")
  pda_train_predictions <- plogis(as.numeric(pda_train_pred))
  pda_train_predictions_binomial <- rbinom(n = length(pda_train_predictions), size = 1, prob = pda_train_predictions)
  pda_train_table <- table(pda_train_predictions_binomial, y_train)
  pda_train_true_positive_rate[i] <- pda_train_table[2, 2] / sum(pda_train_table[2, 2] + pda_train_table[1, 2])
  pda_train_true_positive_rate_mean <- mean(pda_train_true_positive_rate)
  pda_train_true_negative_rate[i] <- pda_train_table[1, 1] / sum(pda_train_table[1, 1] + pda_train_table[2, 1])
  pda_train_true_negative_rate_mean <- mean(pda_train_true_negative_rate)
  pda_train_false_positive_rate[i] <- pda_train_table[2, 1] / sum(pda_train_table[2, 1] + pda_train_table[1, 1])
  pda_train_false_positive_rate_mean <- mean(pda_train_false_positive_rate)
  pda_train_false_negative_rate[i] <- pda_train_table[1, 2] / sum(pda_train_table[1, 2] + pda_train_table[2, 2])
  pda_train_false_negative_rate_mean <- mean(pda_train_false_negative_rate)
  pda_train_accuracy[i] <- (pda_train_table[1, 1] + pda_train_table[2, 2]) / sum(pda_train_table)
  pda_train_accuracy_mean <- mean(pda_train_accuracy)
  pda_train_F1_score[i] <- 2 * (pda_train_table[2, 2]) / sum(2 * pda_train_table[2, 2] + pda_train_table[1, 2] + pda_train_table[2, 1])
  pda_train_F1_score_mean <- mean(pda_train_F1_score)
  pda_train_positive_predictive_value[i] <- pda_train_table[2, 2] / sum(pda_train_table[2, 2] + pda_train_table[2, 1])
  pda_train_positive_predictive_value_mean <- mean(pda_train_positive_predictive_value)
  pda_train_negative_predictive_value[i] <- pda_train_table[1, 1] / sum(pda_train_table[1, 1] + pda_train_table[1, 2])
  pda_train_negative_predictive_value_mean <- mean(pda_train_negative_predictive_value)

  pda_test_pred <- stats::predict(pda_train_fit, test01, type = "response")
  pda_test_predictions <- plogis(as.numeric(pda_test_pred))
  pda_test_predictions_binomial <- rbinom(n = length(pda_test_predictions), size = 1, prob = pda_test_predictions)
  pda_test_table <- table(pda_test_predictions_binomial, y_test)
  pda_test_true_positive_rate[i] <- pda_test_table[2, 2] / sum(pda_test_table[2, 2] + pda_test_table[1, 2])
  pda_test_true_positive_rate_mean <- mean(pda_test_true_positive_rate)
  pda_test_true_negative_rate[i] <- pda_test_table[1, 1] / sum(pda_test_table[1, 1] + pda_test_table[2, 1])
  pda_test_true_negative_rate_mean <- mean(pda_test_true_negative_rate)
  pda_test_false_positive_rate[i] <- pda_test_table[2, 1] / sum(pda_test_table[2, 1] + pda_test_table[1, 1])
  pda_test_false_positive_rate_mean <- mean(pda_test_false_positive_rate)
  pda_test_false_negative_rate[i] <- pda_test_table[1, 2] / sum(pda_test_table[1, 2] + pda_test_table[2, 2])
  pda_test_false_negative_rate_mean <- mean(pda_test_false_negative_rate)
  pda_test_accuracy[i] <- (pda_test_table[1, 1] + pda_test_table[2, 2]) / sum(pda_test_table)
  pda_test_accuracy_mean <- mean(pda_test_accuracy)
  pda_test_F1_score[i] <- 2 * (pda_test_table[2, 2]) / sum(2 * pda_test_table[2, 2] + pda_test_table[1, 2] + pda_test_table[2, 1])
  pda_test_F1_score_mean <- mean(pda_test_F1_score)
  pda_test_positive_predictive_value[i] <- pda_test_table[2, 2] / sum(pda_test_table[2, 2] + pda_test_table[2, 1])
  pda_test_positive_predictive_value_mean <- mean(pda_test_positive_predictive_value)
  pda_test_negative_predictive_value[i] <- pda_test_table[1, 1] / sum(pda_test_table[1, 1] + pda_test_table[1, 2])
  pda_test_negative_predictive_value_mean <- mean(pda_test_negative_predictive_value)

  pda_validation_pred <- stats::predict(pda_train_fit, validation01, type = "response")
  pda_validation_predictions <- plogis(as.numeric(pda_validation_pred))
  pda_validation_predictions_binomial <- rbinom(n = length(pda_validation_predictions), size = 1, prob = pda_validation_predictions)
  pda_validation_table <- table(pda_validation_predictions_binomial, y_validation)
  pda_validation_true_positive_rate[i] <- pda_validation_table[2, 2] / sum(pda_validation_table[2, 2] + pda_validation_table[1, 2])
  pda_validation_true_positive_rate_mean <- mean(pda_validation_true_positive_rate)
  pda_validation_true_negative_rate[i] <- pda_validation_table[1, 1] / sum(pda_validation_table[1, 1] + pda_validation_table[2, 1])
  pda_validation_true_negative_rate_mean <- mean(pda_validation_true_negative_rate)
  pda_validation_false_positive_rate[i] <- pda_validation_table[2, 1] / sum(pda_validation_table[2, 1] + pda_validation_table[1, 1])
  pda_validation_false_positive_rate_mean <- mean(pda_validation_false_positive_rate)
  pda_validation_false_negative_rate[i] <- pda_validation_table[1, 2] / sum(pda_validation_table[1, 2] + pda_validation_table[2, 2])
  pda_validation_false_negative_rate_mean <- mean(pda_validation_false_negative_rate)
  pda_validation_accuracy[i] <- (pda_validation_table[1, 1] + pda_validation_table[2, 2]) / sum(pda_validation_table)
  pda_validation_accuracy_mean <- mean(pda_validation_accuracy)
  pda_validation_F1_score[i] <- 2 * (pda_validation_table[2, 2]) / sum(2 * pda_validation_table[2, 2] + pda_validation_table[1, 2] + pda_validation_table[2, 1])
  pda_validation_F1_score_mean <- mean(pda_validation_F1_score)
  pda_validation_positive_predictive_value[i] <- pda_validation_table[2, 2] / sum(pda_validation_table[2, 2] + pda_validation_table[2, 1])
  pda_validation_positive_predictive_value_mean <- mean(pda_validation_positive_predictive_value)
  pda_validation_negative_predictive_value[i] <- pda_validation_table[1, 1] / sum(pda_validation_table[1, 1] + pda_validation_table[1, 2])
  pda_validation_negative_predictive_value_mean <- mean(pda_validation_negative_predictive_value)

  pda_holdout_true_positive_rate[i] <- (pda_test_true_positive_rate[i] + pda_validation_true_positive_rate[i]) / 2
  pda_holdout_true_positive_rate_mean <- mean(pda_holdout_true_positive_rate)
  pda_holdout_true_negative_rate[i] <- (pda_test_true_negative_rate[i] + pda_validation_true_negative_rate[i]) / 2
  pda_holdout_true_negative_rate_mean <- mean(pda_holdout_true_negative_rate)
  pda_holdout_false_positive_rate[i] <- (pda_test_false_positive_rate[i] + pda_validation_false_positive_rate[i]) / 2
  pda_holdout_false_positive_rate_mean <- mean(pda_holdout_false_positive_rate)
  pda_holdout_false_negative_rate[i] <- (pda_test_false_negative_rate[i] + pda_validation_false_negative_rate[i]) / 2
  pda_holdout_false_negative_rate_mean <- mean(pda_holdout_false_negative_rate)
  pda_holdout_accuracy[i] <- (pda_test_accuracy[i] + pda_validation_accuracy[i]) / 2
  pda_holdout_accuracy_mean <- mean(pda_holdout_accuracy)
  pda_holdout_accuracy_sd <- sd(pda_holdout_accuracy)
  pda_holdout_F1_score[i] <- (pda_test_F1_score[i] + pda_validation_F1_score[i]) / 2
  pda_holdout_F1_score_mean <- mean(pda_holdout_F1_score)
  pda_holdout_positive_predictive_value[i] <- (pda_test_positive_predictive_value[i] + pda_validation_positive_predictive_value[i]) / 2
  pda_holdout_positive_predictive_value_mean <- mean(pda_holdout_positive_predictive_value)
  pda_holdout_negative_predictive_value[i] <- (pda_test_negative_predictive_value[i] + pda_validation_negative_predictive_value[i]) / 2
  pda_holdout_negative_predictive_value_mean <- mean(pda_holdout_negative_predictive_value)
  pda_holdout_overfitting[i] <- pda_holdout_accuracy[i] / pda_train_accuracy[i]
  pda_holdout_overfitting_mean <- mean(pda_holdout_overfitting)
  pda_holdout_overfitting_range <- range(pda_holdout_overfitting)
  pda_holdout_overfitting_sd <- sd(pda_holdout_overfitting)

  pda_table <- pda_test_table + pda_validation_table
  pda_table_total <- pda_table_total + pda_table

  pda_end <- Sys.time()
  pda_duration[i] <- pda_end - pda_start
  pda_duration_mean <- mean(pda_duration)
  pda_duration_sd <- sd(pda_duration)


  #### Quadratic Discriminant Analysis ####
  qda_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    qda_train_fit <- MASS::qda(as.factor(y) ~ ., data = train01)
  }
  if(set_seed == "N"){
    qda_train_fit <- MASS::qda(as.factor(y) ~ ., data = train01)
  }
  qda_train_pred <- stats::predict(qda_train_fit, train01, type = "response")
  qda_train_predictions <- plogis(as.numeric(qda_train_pred$class))
  qda_train_predictions_binomial <- rbinom(n = length(qda_train_predictions), size = 1, prob = qda_train_predictions)
  qda_train_table <- table(qda_train_predictions_binomial, y_train)
  qda_train_true_positive_rate[i] <- qda_train_table[2, 2] / sum(qda_train_table[2, 2] + qda_train_table[1, 2])
  qda_train_true_positive_rate_mean <- mean(qda_train_true_positive_rate)
  qda_train_true_negative_rate[i] <- qda_train_table[1, 1] / sum(qda_train_table[1, 1] + qda_train_table[2, 1])
  qda_train_true_negative_rate_mean <- mean(qda_train_true_negative_rate)
  qda_train_false_positive_rate[i] <- qda_train_table[2, 1] / sum(qda_train_table[2, 1] + qda_train_table[1, 1])
  qda_train_false_positive_rate_mean <- mean(qda_train_false_positive_rate)
  qda_train_false_negative_rate[i] <- qda_train_table[1, 2] / sum(qda_train_table[1, 2] + qda_train_table[2, 2])
  qda_train_false_negative_rate_mean <- mean(qda_train_false_negative_rate)
  qda_train_accuracy[i] <- (qda_train_table[1, 1] + qda_train_table[2, 2]) / sum(qda_train_table)
  qda_train_accuracy_mean <- mean(qda_train_accuracy)
  qda_train_F1_score[i] <- 2 * (qda_train_table[2, 2]) / sum(2 * qda_train_table[2, 2] + qda_train_table[1, 2] + qda_train_table[2, 1])
  qda_train_F1_score_mean <- mean(qda_train_F1_score)
  qda_train_positive_predictive_value[i] <- qda_train_table[2, 2] / sum(qda_train_table[2, 2] + qda_train_table[2, 1])
  qda_train_positive_predictive_value_mean <- mean(qda_train_positive_predictive_value)
  qda_train_negative_predictive_value[i] <- qda_train_table[1, 1] / sum(qda_train_table[1, 1] + qda_train_table[1, 2])
  qda_train_negative_predictive_value_mean <- mean(qda_train_negative_predictive_value)

  qda_test_pred <- stats::predict(qda_train_fit, test01, type = "response")
  qda_test_predictions <- plogis(as.numeric(qda_test_pred$class))
  qda_test_predictions_binomial <- rbinom(n = length(qda_test_predictions), size = 1, prob = qda_test_predictions)
  qda_test_table <- table(qda_test_predictions_binomial, y_test)
  qda_test_true_positive_rate[i] <- qda_test_table[2, 2] / sum(qda_test_table[2, 2] + qda_test_table[1, 2])
  qda_test_true_positive_rate_mean <- mean(qda_test_true_positive_rate)
  qda_test_true_negative_rate[i] <- qda_test_table[1, 1] / sum(qda_test_table[1, 1] + qda_test_table[2, 1])
  qda_test_true_negative_rate_mean <- mean(qda_test_true_negative_rate)
  qda_test_false_positive_rate[i] <- qda_test_table[2, 1] / sum(qda_test_table[2, 1] + qda_test_table[1, 1])
  qda_test_false_positive_rate_mean <- mean(qda_test_false_positive_rate)
  qda_test_false_negative_rate[i] <- qda_test_table[1, 2] / sum(qda_test_table[1, 2] + qda_test_table[2, 2])
  qda_test_false_negative_rate_mean <- mean(qda_test_false_negative_rate)
  qda_test_accuracy[i] <- (qda_test_table[1, 1] + qda_test_table[2, 2]) / sum(qda_test_table)
  qda_test_accuracy_mean <- mean(qda_test_accuracy)
  qda_test_F1_score[i] <- 2 * (qda_test_table[2, 2]) / sum(2 * qda_test_table[2, 2] + qda_test_table[1, 2] + qda_test_table[2, 1])
  qda_test_F1_score_mean <- mean(qda_test_F1_score)
  qda_test_positive_predictive_value[i] <- qda_test_table[2, 2] / sum(qda_test_table[2, 2] + qda_test_table[2, 1])
  qda_test_positive_predictive_value_mean <- mean(qda_test_positive_predictive_value)
  qda_test_negative_predictive_value[i] <- qda_test_table[1, 1] / sum(qda_test_table[1, 1] + qda_test_table[1, 2])
  qda_test_negative_predictive_value_mean <- mean(qda_test_negative_predictive_value)

  qda_validation_pred <- stats::predict(qda_train_fit, validation01, type = "response")
  qda_validation_predictions <- plogis(as.numeric(qda_validation_pred$class))
  qda_validation_predictions_binomial <- rbinom(n = length(qda_validation_predictions), size = 1, prob = qda_validation_predictions)
  qda_validation_table <- table(qda_validation_predictions_binomial, y_validation)
  qda_validation_true_positive_rate[i] <- qda_validation_table[2, 2] / sum(qda_validation_table[2, 2] + qda_validation_table[1, 2])
  qda_validation_true_positive_rate_mean <- mean(qda_validation_true_positive_rate)
  qda_validation_true_negative_rate[i] <- qda_validation_table[1, 1] / sum(qda_validation_table[1, 1] + qda_validation_table[2, 1])
  qda_validation_true_negative_rate_mean <- mean(qda_validation_true_negative_rate)
  qda_validation_false_positive_rate[i] <- qda_validation_table[2, 1] / sum(qda_validation_table[2, 1] + qda_validation_table[1, 1])
  qda_validation_false_positive_rate_mean <- mean(qda_validation_false_positive_rate)
  qda_validation_false_negative_rate[i] <- qda_validation_table[1, 2] / sum(qda_validation_table[1, 2] + qda_validation_table[2, 2])
  qda_validation_false_negative_rate_mean <- mean(qda_validation_false_negative_rate)
  qda_validation_accuracy[i] <- (qda_validation_table[1, 1] + qda_validation_table[2, 2]) / sum(qda_validation_table)
  qda_validation_accuracy_mean <- mean(qda_validation_accuracy)
  qda_validation_F1_score[i] <- 2 * (qda_validation_table[2, 2]) / sum(2 * qda_validation_table[2, 2] + qda_validation_table[1, 2] + qda_validation_table[2, 1])
  qda_validation_F1_score_mean <- mean(qda_validation_F1_score)
  qda_validation_positive_predictive_value[i] <- qda_validation_table[2, 2] / sum(qda_validation_table[2, 2] + qda_validation_table[2, 1])
  qda_validation_positive_predictive_value_mean <- mean(qda_validation_positive_predictive_value)
  qda_validation_negative_predictive_value[i] <- qda_validation_table[1, 1] / sum(qda_validation_table[1, 1] + qda_validation_table[1, 2])
  qda_validation_negative_predictive_value_mean <- mean(qda_validation_negative_predictive_value)

  qda_holdout_true_positive_rate[i] <- (qda_test_true_positive_rate[i] + qda_validation_true_positive_rate[i]) / 2
  qda_holdout_true_positive_rate_mean <- mean(qda_holdout_true_positive_rate)
  qda_holdout_true_negative_rate[i] <- (qda_test_true_negative_rate[i] + qda_validation_true_negative_rate[i]) / 2
  qda_holdout_true_negative_rate_mean <- mean(qda_holdout_true_negative_rate)
  qda_holdout_false_positive_rate[i] <- (qda_test_false_positive_rate[i] + qda_validation_false_positive_rate[i]) / 2
  qda_holdout_false_positive_rate_mean <- mean(qda_holdout_false_positive_rate)
  qda_holdout_false_negative_rate[i] <- (qda_test_false_negative_rate[i] + qda_validation_false_negative_rate[i]) / 2
  qda_holdout_false_negative_rate_mean <- mean(qda_holdout_false_negative_rate)
  qda_holdout_accuracy[i] <- (qda_test_accuracy[i] + qda_validation_accuracy[i]) / 2
  qda_holdout_accuracy_mean <- mean(qda_holdout_accuracy)
  qda_holdout_accuracy_sd <- sd(qda_holdout_accuracy)
  qda_holdout_F1_score[i] <- (qda_test_F1_score[i] + qda_validation_F1_score[i]) / 2
  qda_holdout_F1_score_mean <- mean(qda_holdout_F1_score)
  qda_holdout_positive_predictive_value[i] <- (qda_test_positive_predictive_value[i] + qda_validation_positive_predictive_value[i]) / 2
  qda_holdout_positive_predictive_value_mean <- mean(qda_holdout_positive_predictive_value)
  qda_holdout_negative_predictive_value[i] <- (qda_test_negative_predictive_value[i] + qda_validation_negative_predictive_value[i]) / 2
  qda_holdout_negative_predictive_value_mean <- mean(qda_holdout_negative_predictive_value)
  qda_holdout_overfitting[i] <- qda_holdout_accuracy[i] / qda_train_accuracy[i]
  qda_holdout_overfitting_mean <- mean(qda_holdout_overfitting)
  qda_holdout_overfitting_range <- range(qda_holdout_overfitting)
  qda_holdout_overfitting_sd <- sd(qda_holdout_overfitting)

  qda_table <- qda_test_table + qda_validation_table
  qda_table_total <- qda_table_total + qda_table

  qda_end <- Sys.time()
  qda_duration[i] <- qda_end - qda_start
  qda_duration_mean <- mean(qda_duration)
  qda_duration_sd <- sd(qda_duration)

  #### Random Forest ####
  rf_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    rf_train_fit <- randomForest(x = train, y = as.factor(y_train), data = df, family = binomial(link = "logit"))
  }
  if(set_seed == "N"){
    rf_train_fit <- randomForest(x = train, y = as.factor(y_train), data = df, family = binomial(link = "logit"))
  }
  rf_train_pred <- stats::predict(rf_train_fit, train01, type = "response")
  rf_train_predictions <- plogis(as.numeric(rf_train_pred))
  rf_train_predictions_binomial <- rbinom(n = length(rf_train_predictions), size = 1, prob = rf_train_predictions)
  rf_train_table <- table(rf_train_predictions_binomial, y_train)
  rf_train_true_positive_rate[i] <- rf_train_table[2, 2] / sum(rf_train_table[2, 2] + rf_train_table[1, 2])
  rf_train_true_positive_rate_mean <- mean(rf_train_true_positive_rate)
  rf_train_true_negative_rate[i] <- rf_train_table[1, 1] / sum(rf_train_table[1, 1] + rf_train_table[2, 1])
  rf_train_true_negative_rate_mean <- mean(rf_train_true_negative_rate)
  rf_train_false_positive_rate[i] <- rf_train_table[2, 1] / sum(rf_train_table[2, 1] + rf_train_table[1, 1])
  rf_train_false_positive_rate_mean <- mean(rf_train_false_positive_rate)
  rf_train_false_negative_rate[i] <- rf_train_table[1, 2] / sum(rf_train_table[1, 2] + rf_train_table[2, 2])
  rf_train_false_negative_rate_mean <- mean(rf_train_false_negative_rate)
  rf_train_accuracy[i] <- (rf_train_table[1, 1] + rf_train_table[2, 2]) / sum(rf_train_table)
  rf_train_accuracy_mean <- mean(rf_train_accuracy)
  rf_train_F1_score[i] <- 2 * (rf_train_table[2, 2]) / sum(2 * rf_train_table[2, 2] + rf_train_table[1, 2] + rf_train_table[2, 1])
  rf_train_F1_score_mean <- mean(rf_train_F1_score)
  rf_train_positive_predictive_value[i] <- rf_train_table[2, 2] / sum(rf_train_table[2, 2] + rf_train_table[2, 1])
  rf_train_positive_predictive_value_mean <- mean(rf_train_positive_predictive_value)
  rf_train_negative_predictive_value[i] <- rf_train_table[1, 1] / sum(rf_train_table[1, 1] + rf_train_table[1, 2])
  rf_train_negative_predictive_value_mean <- mean(rf_train_negative_predictive_value)

  rf_test_pred <- stats::predict(rf_train_fit, test01, type = "response")
  rf_test_predictions <- plogis(as.numeric(rf_test_pred))
  rf_test_predictions_binomial <- rbinom(n = length(rf_test_predictions), size = 1, prob = rf_test_predictions)
  rf_test_table <- table(rf_test_predictions_binomial, y_test)
  rf_test_true_positive_rate[i] <- rf_test_table[2, 2] / sum(rf_test_table[2, 2] + rf_test_table[1, 2])
  rf_test_true_positive_rate_mean <- mean(rf_test_true_positive_rate)
  rf_test_true_negative_rate[i] <- rf_test_table[1, 1] / sum(rf_test_table[1, 1] + rf_test_table[2, 1])
  rf_test_true_negative_rate_mean <- mean(rf_test_true_negative_rate)
  rf_test_false_positive_rate[i] <- rf_test_table[2, 1] / sum(rf_test_table[2, 1] + rf_test_table[1, 1])
  rf_test_false_positive_rate_mean <- mean(rf_test_false_positive_rate)
  rf_test_false_negative_rate[i] <- rf_test_table[1, 2] / sum(rf_test_table[1, 2] + rf_test_table[2, 2])
  rf_test_false_negative_rate_mean <- mean(rf_test_false_negative_rate)
  rf_test_accuracy[i] <- (rf_test_table[1, 1] + rf_test_table[2, 2]) / sum(rf_test_table)
  rf_test_accuracy_mean <- mean(rf_test_accuracy)
  rf_test_F1_score[i] <- 2 * (rf_test_table[2, 2]) / sum(2 * rf_test_table[2, 2] + rf_test_table[1, 2] + rf_test_table[2, 1])
  rf_test_F1_score_mean <- mean(rf_test_F1_score)
  rf_test_positive_predictive_value[i] <- rf_test_table[2, 2] / sum(rf_test_table[2, 2] + rf_test_table[2, 1])
  rf_test_positive_predictive_value_mean <- mean(rf_test_positive_predictive_value)
  rf_test_negative_predictive_value[i] <- rf_test_table[1, 1] / sum(rf_test_table[1, 1] + rf_test_table[1, 2])
  rf_test_negative_predictive_value_mean <- mean(rf_test_negative_predictive_value)

  rf_validation_pred <- stats::predict(rf_train_fit, validation01, type = "response")
  rf_validation_predictions <- plogis(as.numeric(rf_validation_pred))
  rf_validation_predictions_binomial <- rbinom(n = length(rf_validation_predictions), size = 1, prob = rf_validation_predictions)
  rf_validation_table <- table(rf_validation_predictions_binomial, y_validation)
  rf_validation_true_positive_rate[i] <- rf_validation_table[2, 2] / sum(rf_validation_table[2, 2] + rf_validation_table[1, 2])
  rf_validation_true_positive_rate_mean <- mean(rf_validation_true_positive_rate)
  rf_validation_true_negative_rate[i] <- rf_validation_table[1, 1] / sum(rf_validation_table[1, 1] + rf_validation_table[2, 1])
  rf_validation_true_negative_rate_mean <- mean(rf_validation_true_negative_rate)
  rf_validation_false_positive_rate[i] <- rf_validation_table[2, 1] / sum(rf_validation_table[2, 1] + rf_validation_table[1, 1])
  rf_validation_false_positive_rate_mean <- mean(rf_validation_false_positive_rate)
  rf_validation_false_negative_rate[i] <- rf_validation_table[1, 2] / sum(rf_validation_table[1, 2] + rf_validation_table[2, 2])
  rf_validation_false_negative_rate_mean <- mean(rf_validation_false_negative_rate)
  rf_validation_accuracy[i] <- (rf_validation_table[1, 1] + rf_validation_table[2, 2]) / sum(rf_validation_table)
  rf_validation_accuracy_mean <- mean(rf_validation_accuracy)
  rf_validation_F1_score[i] <- 2 * (rf_validation_table[2, 2]) / sum(2 * rf_validation_table[2, 2] + rf_validation_table[1, 2] + rf_validation_table[2, 1])
  rf_validation_F1_score_mean <- mean(rf_validation_F1_score)
  rf_validation_positive_predictive_value[i] <- rf_validation_table[2, 2] / sum(rf_validation_table[2, 2] + rf_validation_table[2, 1])
  rf_validation_positive_predictive_value_mean <- mean(rf_validation_positive_predictive_value)
  rf_validation_negative_predictive_value[i] <- rf_validation_table[1, 1] / sum(rf_validation_table[1, 1] + rf_validation_table[1, 2])
  rf_validation_negative_predictive_value_mean <- mean(rf_validation_negative_predictive_value)

  rf_holdout_true_positive_rate[i] <- (rf_test_true_positive_rate[i] + rf_validation_true_positive_rate[i]) / 2
  rf_holdout_true_positive_rate_mean <- mean(rf_holdout_true_positive_rate)
  rf_holdout_true_negative_rate[i] <- (rf_test_true_negative_rate[i] + rf_validation_true_negative_rate[i]) / 2
  rf_holdout_true_negative_rate_mean <- mean(rf_holdout_true_negative_rate)
  rf_holdout_false_positive_rate[i] <- (rf_test_false_positive_rate[i] + rf_validation_false_positive_rate[i]) / 2
  rf_holdout_false_positive_rate_mean <- mean(rf_holdout_false_positive_rate)
  rf_holdout_false_negative_rate[i] <- (rf_test_false_negative_rate[i] + rf_validation_false_negative_rate[i]) / 2
  rf_holdout_false_negative_rate_mean <- mean(rf_holdout_false_negative_rate)
  rf_holdout_accuracy[i] <- (rf_test_accuracy[i] + rf_validation_accuracy[i]) / 2
  rf_holdout_accuracy_mean <- mean(rf_holdout_accuracy)
  rf_holdout_accuracy_sd <- sd(rf_holdout_accuracy)
  rf_holdout_F1_score[i] <- (rf_test_F1_score[i] + rf_validation_F1_score[i]) / 2
  rf_holdout_F1_score_mean <- mean(rf_holdout_F1_score)
  rf_holdout_positive_predictive_value[i] <- (rf_test_positive_predictive_value[i] + rf_validation_positive_predictive_value[i]) / 2
  rf_holdout_positive_predictive_value_mean <- mean(rf_holdout_positive_predictive_value)
  rf_holdout_negative_predictive_value[i] <- (rf_test_negative_predictive_value[i] + rf_validation_negative_predictive_value[i]) / 2
  rf_holdout_negative_predictive_value_mean <- mean(rf_holdout_negative_predictive_value)
  rf_holdout_overfitting[i] <- rf_holdout_accuracy[i] / rf_train_accuracy[i]
  rf_holdout_overfitting_mean <- mean(rf_holdout_overfitting)
  rf_holdout_overfitting_range <- range(rf_holdout_overfitting)
  rf_holdout_overfitting_sd <- sd(rf_holdout_overfitting)

  rf_table <- rf_test_table + rf_validation_table
  rf_table_total <- rf_table_total + rf_table

  rf_end <- Sys.time()
  rf_duration[i] <- rf_end - rf_start
  rf_duration_mean <- mean(rf_duration)
  rf_duration_sd <- sd(rf_duration)


  #### Support Vector Machines (SVM) ####
  svm_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    svm_train_fit <- e1071::svm(as.factor(y) ~ ., data = train01)
  }
  if(set_seed == "N"){
    svm_train_fit <- e1071::svm(as.factor(y) ~ ., data = train01)
  }
  svm_train_pred <- stats::predict(svm_train_fit, train01, type = "response")
  svm_train_predictions <- plogis(as.numeric(svm_train_pred))
  svm_train_predictions_binomial <- rbinom(n = length(svm_train_predictions), size = 1, prob = svm_train_predictions)
  svm_train_table <- table(svm_train_predictions_binomial, y_train)
  svm_train_true_positive_rate[i] <- svm_train_table[2, 2] / sum(svm_train_table[2, 2] + svm_train_table[1, 2])
  svm_train_true_positive_rate_mean <- mean(svm_train_true_positive_rate)
  svm_train_true_negative_rate[i] <- svm_train_table[1, 1] / sum(svm_train_table[1, 1] + svm_train_table[2, 1])
  svm_train_true_negative_rate_mean <- mean(svm_train_true_negative_rate)
  svm_train_false_positive_rate[i] <- svm_train_table[2, 1] / sum(svm_train_table[2, 1] + svm_train_table[1, 1])
  svm_train_false_positive_rate_mean <- mean(svm_train_false_positive_rate)
  svm_train_false_negative_rate[i] <- svm_train_table[1, 2] / sum(svm_train_table[1, 2] + svm_train_table[2, 2])
  svm_train_false_negative_rate_mean <- mean(svm_train_false_negative_rate)
  svm_train_accuracy[i] <- (svm_train_table[1, 1] + svm_train_table[2, 2]) / sum(svm_train_table)
  svm_train_accuracy_mean <- mean(svm_train_accuracy)
  svm_train_F1_score[i] <- 2 * (svm_train_table[2, 2]) / sum(2 * svm_train_table[2, 2] + svm_train_table[1, 2] + svm_train_table[2, 1])
  svm_train_F1_score_mean <- mean(svm_train_F1_score)
  svm_train_positive_predictive_value[i] <- svm_train_table[2, 2] / sum(svm_train_table[2, 2] + svm_train_table[2, 1])
  svm_train_positive_predictive_value_mean <- mean(svm_train_positive_predictive_value)
  svm_train_negative_predictive_value[i] <- svm_train_table[1, 1] / sum(svm_train_table[1, 1] + svm_train_table[1, 2])
  svm_train_negative_predictive_value_mean <- mean(svm_train_negative_predictive_value)

  svm_test_pred <- stats::predict(svm_train_fit, test01, type = "response")
  svm_test_predictions <- plogis(as.numeric(svm_test_pred))
  svm_test_predictions_binomial <- rbinom(n = length(svm_test_predictions), size = 1, prob = svm_test_predictions)
  svm_test_table <- table(svm_test_predictions_binomial, y_test)
  svm_test_true_positive_rate[i] <- svm_test_table[2, 2] / sum(svm_test_table[2, 2] + svm_test_table[1, 2])
  svm_test_true_positive_rate_mean <- mean(svm_test_true_positive_rate)
  svm_test_true_negative_rate[i] <- svm_test_table[1, 1] / sum(svm_test_table[1, 1] + svm_test_table[2, 1])
  svm_test_true_negative_rate_mean <- mean(svm_test_true_negative_rate)
  svm_test_false_positive_rate[i] <- svm_test_table[2, 1] / sum(svm_test_table[2, 1] + svm_test_table[1, 1])
  svm_test_false_positive_rate_mean <- mean(svm_test_false_positive_rate)
  svm_test_false_negative_rate[i] <- svm_test_table[1, 2] / sum(svm_test_table[1, 2] + svm_test_table[2, 2])
  svm_test_false_negative_rate_mean <- mean(svm_test_false_negative_rate)
  svm_test_accuracy[i] <- (svm_test_table[1, 1] + svm_test_table[2, 2]) / sum(svm_test_table)
  svm_test_accuracy_mean <- mean(svm_test_accuracy)
  svm_test_F1_score[i] <- 2 * (svm_test_table[2, 2]) / sum(2 * svm_test_table[2, 2] + svm_test_table[1, 2] + svm_test_table[2, 1])
  svm_test_F1_score_mean <- mean(svm_test_F1_score)
  svm_test_positive_predictive_value[i] <- svm_test_table[2, 2] / sum(svm_test_table[2, 2] + svm_test_table[2, 1])
  svm_test_positive_predictive_value_mean <- mean(svm_test_positive_predictive_value)
  svm_test_negative_predictive_value[i] <- svm_test_table[1, 1] / sum(svm_test_table[1, 1] + svm_test_table[1, 2])
  svm_test_negative_predictive_value_mean <- mean(svm_test_negative_predictive_value)

  svm_validation_pred <- stats::predict(svm_train_fit, validation01, type = "response")
  svm_validation_predictions <- plogis(as.numeric(svm_validation_pred))
  svm_validation_predictions_binomial <- rbinom(n = length(svm_validation_predictions), size = 1, prob = svm_validation_predictions)
  svm_validation_table <- table(svm_validation_predictions_binomial, y_validation)
  svm_validation_true_positive_rate[i] <- svm_validation_table[2, 2] / sum(svm_validation_table[2, 2] + svm_validation_table[1, 2])
  svm_validation_true_positive_rate_mean <- mean(svm_validation_true_positive_rate)
  svm_validation_true_negative_rate[i] <- svm_validation_table[1, 1] / sum(svm_validation_table[1, 1] + svm_validation_table[2, 1])
  svm_validation_true_negative_rate_mean <- mean(svm_validation_true_negative_rate)
  svm_validation_false_positive_rate[i] <- svm_validation_table[2, 1] / sum(svm_validation_table[2, 1] + svm_validation_table[1, 1])
  svm_validation_false_positive_rate_mean <- mean(svm_validation_false_positive_rate)
  svm_validation_false_negative_rate[i] <- svm_validation_table[1, 2] / sum(svm_validation_table[1, 2] + svm_validation_table[2, 2])
  svm_validation_false_negative_rate_mean <- mean(svm_validation_false_negative_rate)
  svm_validation_accuracy[i] <- (svm_validation_table[1, 1] + svm_validation_table[2, 2]) / sum(svm_validation_table)
  svm_validation_accuracy_mean <- mean(svm_validation_accuracy)
  svm_validation_F1_score[i] <- 2 * (svm_validation_table[2, 2]) / sum(2 * svm_validation_table[2, 2] + svm_validation_table[1, 2] + svm_validation_table[2, 1])
  svm_validation_F1_score_mean <- mean(svm_validation_F1_score)
  svm_validation_positive_predictive_value[i] <- svm_validation_table[2, 2] / sum(svm_validation_table[2, 2] + svm_validation_table[2, 1])
  svm_validation_positive_predictive_value_mean <- mean(svm_validation_positive_predictive_value)
  svm_validation_negative_predictive_value[i] <- svm_validation_table[1, 1] / sum(svm_validation_table[1, 1] + svm_validation_table[1, 2])
  svm_validation_negative_predictive_value_mean <- mean(svm_validation_negative_predictive_value)

  svm_holdout_true_positive_rate[i] <- (svm_test_true_positive_rate[i] + svm_validation_true_positive_rate[i]) / 2
  svm_holdout_true_positive_rate_mean <- mean(svm_holdout_true_positive_rate)
  svm_holdout_true_negative_rate[i] <- (svm_test_true_negative_rate[i] + svm_validation_true_negative_rate[i]) / 2
  svm_holdout_true_negative_rate_mean <- mean(svm_holdout_true_negative_rate)
  svm_holdout_false_positive_rate[i] <- (svm_test_false_positive_rate[i] + svm_validation_false_positive_rate[i]) / 2
  svm_holdout_false_positive_rate_mean <- mean(svm_holdout_false_positive_rate)
  svm_holdout_false_negative_rate[i] <- (svm_test_false_negative_rate[i] + svm_validation_false_negative_rate[i]) / 2
  svm_holdout_false_negative_rate_mean <- mean(svm_holdout_false_negative_rate)
  svm_holdout_accuracy[i] <- (svm_test_accuracy[i] + svm_validation_accuracy[i]) / 2
  svm_holdout_accuracy_mean <- mean(svm_holdout_accuracy)
  svm_holdout_accuracy_sd <- sd(svm_holdout_accuracy)
  svm_holdout_F1_score[i] <- (svm_test_F1_score[i] + svm_validation_F1_score[i]) / 2
  svm_holdout_F1_score_mean <- mean(svm_holdout_F1_score)
  svm_holdout_positive_predictive_value[i] <- (svm_test_positive_predictive_value[i] + svm_validation_positive_predictive_value[i]) / 2
  svm_holdout_positive_predictive_value_mean <- mean(svm_holdout_positive_predictive_value)
  svm_holdout_negative_predictive_value[i] <- (svm_test_negative_predictive_value[i] + svm_validation_negative_predictive_value[i]) / 2
  svm_holdout_negative_predictive_value_mean <- mean(svm_holdout_negative_predictive_value)
  svm_holdout_overfitting[i] <- svm_holdout_accuracy[i] / svm_train_accuracy[i]
  svm_holdout_overfitting_mean <- mean(svm_holdout_overfitting)
  svm_holdout_overfitting_range <- range(svm_holdout_overfitting)
  svm_holdout_overfitting_sd <- sd(svm_holdout_overfitting)

  svm_table <- svm_test_table + svm_validation_table
  svm_table_total <- svm_table_total + svm_table

  svm_end <- Sys.time()
  svm_duration[i] <- svm_end - svm_start
  svm_duration_mean <- mean(svm_duration)
  svm_duration_sd <- sd(svm_duration)


  #### tree ####
  tree_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    tree_train_fit <- tree::tree(y ~ ., data = train)
  }
  if(set_seed == "N"){
    tree_train_fit <- tree::tree(y ~ ., data = train)
  }
  tree_train_pred <- stats::predict(tree_train_fit, train01, type = "vector")
  tree_train_predictions <- plogis(as.numeric(tree_train_pred))
  tree_train_predictions_binomial <- rbinom(n = length(tree_train_predictions), size = 1, prob = tree_train_predictions)
  tree_train_table <- table(tree_train_predictions_binomial, y_train)
  tree_train_true_positive_rate[i] <- tree_train_table[2, 2] / sum(tree_train_table[2, 2] + tree_train_table[1, 2])
  tree_train_true_positive_rate_mean <- mean(tree_train_true_positive_rate)
  tree_train_true_negative_rate[i] <- tree_train_table[1, 1] / sum(tree_train_table[1, 1] + tree_train_table[2, 1])
  tree_train_true_negative_rate_mean <- mean(tree_train_true_negative_rate)
  tree_train_false_positive_rate[i] <- tree_train_table[2, 1] / sum(tree_train_table[2, 1] + tree_train_table[1, 1])
  tree_train_false_positive_rate_mean <- mean(tree_train_false_positive_rate)
  tree_train_false_negative_rate[i] <- tree_train_table[1, 2] / sum(tree_train_table[1, 2] + tree_train_table[2, 2])
  tree_train_false_negative_rate_mean <- mean(tree_train_false_negative_rate)
  tree_train_accuracy[i] <- (tree_train_table[1, 1] + tree_train_table[2, 2]) / sum(tree_train_table)
  tree_train_accuracy_mean <- mean(tree_train_accuracy)
  tree_train_F1_score[i] <- 2 * (tree_train_table[2, 2]) / sum(2 * tree_train_table[2, 2] + tree_train_table[1, 2] + tree_train_table[2, 1])
  tree_train_F1_score_mean <- mean(tree_train_F1_score)
  tree_train_positive_predictive_value[i] <- tree_train_table[2, 2] / sum(tree_train_table[2, 2] + tree_train_table[2, 1])
  tree_train_positive_predictive_value_mean <- mean(tree_train_positive_predictive_value)
  tree_train_negative_predictive_value[i] <- tree_train_table[1, 1] / sum(tree_train_table[1, 1] + tree_train_table[1, 2])
  tree_train_negative_predictive_value_mean <- mean(tree_train_negative_predictive_value)

  tree_test_pred <- stats::predict(tree_train_fit, test01, type = "vector")
  tree_test_predictions <- plogis(as.numeric(tree_test_pred))
  tree_test_predictions_binomial <- rbinom(n = length(tree_test_predictions), size = 1, prob = tree_test_predictions)
  tree_test_table <- table(tree_test_predictions_binomial, y_test)
  tree_test_true_positive_rate[i] <- tree_test_table[2, 2] / sum(tree_test_table[2, 2] + tree_test_table[1, 2])
  tree_test_true_positive_rate_mean <- mean(tree_test_true_positive_rate)
  tree_test_true_negative_rate[i] <- tree_test_table[1, 1] / sum(tree_test_table[1, 1] + tree_test_table[2, 1])
  tree_test_true_negative_rate_mean <- mean(tree_test_true_negative_rate)
  tree_test_false_positive_rate[i] <- tree_test_table[2, 1] / sum(tree_test_table[2, 1] + tree_test_table[1, 1])
  tree_test_false_positive_rate_mean <- mean(tree_test_false_positive_rate)
  tree_test_false_negative_rate[i] <- tree_test_table[1, 2] / sum(tree_test_table[1, 2] + tree_test_table[2, 2])
  tree_test_false_negative_rate_mean <- mean(tree_test_false_negative_rate)
  tree_test_accuracy[i] <- (tree_test_table[1, 1] + tree_test_table[2, 2]) / sum(tree_test_table)
  tree_test_accuracy_mean <- mean(tree_test_accuracy)
  tree_test_F1_score[i] <- 2 * (tree_test_table[2, 2]) / sum(2 * tree_test_table[2, 2] + tree_test_table[1, 2] + tree_test_table[2, 1])
  tree_test_F1_score_mean <- mean(tree_test_F1_score)
  tree_test_positive_predictive_value[i] <- tree_test_table[2, 2] / sum(tree_test_table[2, 2] + tree_test_table[2, 1])
  tree_test_positive_predictive_value_mean <- mean(tree_test_positive_predictive_value)
  tree_test_negative_predictive_value[i] <- tree_test_table[1, 1] / sum(tree_test_table[1, 1] + tree_test_table[1, 2])
  tree_test_negative_predictive_value_mean <- mean(tree_test_negative_predictive_value)

  tree_validation_pred <- stats::predict(tree_train_fit, validation01, type = "vector")
  tree_validation_predictions <- plogis(as.numeric(tree_validation_pred))
  tree_validation_predictions_binomial <- rbinom(n = length(tree_validation_predictions), size = 1, prob = tree_validation_predictions)
  tree_validation_table <- table(tree_validation_predictions_binomial, y_validation)
  tree_validation_true_positive_rate[i] <- tree_validation_table[2, 2] / sum(tree_validation_table[2, 2] + tree_validation_table[1, 2])
  tree_validation_true_positive_rate_mean <- mean(tree_validation_true_positive_rate)
  tree_validation_true_negative_rate[i] <- tree_validation_table[1, 1] / sum(tree_validation_table[1, 1] + tree_validation_table[2, 1])
  tree_validation_true_negative_rate_mean <- mean(tree_validation_true_negative_rate)
  tree_validation_false_positive_rate[i] <- tree_validation_table[2, 1] / sum(tree_validation_table[2, 1] + tree_validation_table[1, 1])
  tree_validation_false_positive_rate_mean <- mean(tree_validation_false_positive_rate)
  tree_validation_false_negative_rate[i] <- tree_validation_table[1, 2] / sum(tree_validation_table[1, 2] + tree_validation_table[2, 2])
  tree_validation_false_negative_rate_mean <- mean(tree_validation_false_negative_rate)
  tree_validation_accuracy[i] <- (tree_validation_table[1, 1] + tree_validation_table[2, 2]) / sum(tree_validation_table)
  tree_validation_accuracy_mean <- mean(tree_validation_accuracy)
  tree_validation_F1_score[i] <- 2 * (tree_validation_table[2, 2]) / sum(2 * tree_validation_table[2, 2] + tree_validation_table[1, 2] + tree_validation_table[2, 1])
  tree_validation_F1_score_mean <- mean(tree_validation_F1_score)
  tree_validation_positive_predictive_value[i] <- tree_validation_table[2, 2] / sum(tree_validation_table[2, 2] + tree_validation_table[2, 1])
  tree_validation_positive_predictive_value_mean <- mean(tree_validation_positive_predictive_value)
  tree_validation_negative_predictive_value[i] <- tree_validation_table[1, 1] / sum(tree_validation_table[1, 1] + tree_validation_table[1, 2])
  tree_validation_negative_predictive_value_mean <- mean(tree_validation_negative_predictive_value)

  tree_holdout_true_positive_rate[i] <- (tree_test_true_positive_rate[i] + tree_validation_true_positive_rate[i]) / 2
  tree_holdout_true_positive_rate_mean <- mean(tree_holdout_true_positive_rate)
  tree_holdout_true_negative_rate[i] <- (tree_test_true_negative_rate[i] + tree_validation_true_negative_rate[i]) / 2
  tree_holdout_true_negative_rate_mean <- mean(tree_holdout_true_negative_rate)
  tree_holdout_false_positive_rate[i] <- (tree_test_false_positive_rate[i] + tree_validation_false_positive_rate[i]) / 2
  tree_holdout_false_positive_rate_mean <- mean(tree_holdout_false_positive_rate)
  tree_holdout_false_negative_rate[i] <- (tree_test_false_negative_rate[i] + tree_validation_false_negative_rate[i]) / 2
  tree_holdout_false_negative_rate_mean <- mean(tree_holdout_false_negative_rate)
  tree_holdout_accuracy[i] <- (tree_test_accuracy[i] + tree_validation_accuracy[i]) / 2
  tree_holdout_accuracy_mean <- mean(tree_holdout_accuracy)
  tree_holdout_accuracy_sd <- sd(tree_holdout_accuracy)
  tree_holdout_F1_score[i] <- (tree_test_F1_score[i] + tree_validation_F1_score[i]) / 2
  tree_holdout_F1_score_mean <- mean(tree_holdout_F1_score)
  tree_holdout_positive_predictive_value[i] <- (tree_test_positive_predictive_value[i] + tree_validation_positive_predictive_value[i]) / 2
  tree_holdout_positive_predictive_value_mean <- mean(tree_holdout_positive_predictive_value)
  tree_holdout_negative_predictive_value[i] <- (tree_test_negative_predictive_value[i] + tree_validation_negative_predictive_value[i]) / 2
  tree_holdout_negative_predictive_value_mean <- mean(tree_holdout_negative_predictive_value)
  tree_holdout_overfitting[i] <- tree_holdout_accuracy[i] / tree_train_accuracy[i]
  tree_holdout_overfitting_mean <- mean(tree_holdout_overfitting)
  tree_holdout_overfitting_range <- range(tree_holdout_overfitting)
  tree_holdout_overfitting_sd <- sd(tree_holdout_overfitting)

  tree_table <- tree_test_table + tree_validation_table
  tree_table_total <- tree_table_total + tree_table

  tree_end <- Sys.time()
  tree_duration[i] <- tree_end - tree_start
  tree_duration_mean <- mean(tree_duration)
  tree_duration_sd <- sd(tree_duration)



  ##################################################################################################################

  ######################################## Ensembles start here ####################################################

  ##################################################################################################################

  ensemble1 <- data.frame(
    "Cubist" = c(cubist_test_pred, cubist_validation_pred),
    "Flexible_Discriminant_Analysis" = c(fda_test_predictions, fda_validation_predictions),
    "Generalized_Linear_Models" = c(glm_test_pred, glm_validation_pred),
    "Linear" = as.numeric(c(linear_test_pred, linear_validation_pred)) - 1,
    "Penalized_Discriminant_Analysis" = as.numeric(c(pda_test_pred, pda_validation_pred)) - 1,
    "Quadratic_Discriminant_Analysis" = as.numeric(c(qda_test_pred$class, qda_validation_pred$class)) - 1,
    "Random_Forest" = as.numeric(c(rf_test_pred, rf_validation_pred)) -1,
    "Support_Vector_Machines" = as.numeric(c(svm_test_pred, svm_validation_pred)) - 1,
    "Trees" = c(tree_test_predictions, tree_validation_predictions)
  )

  ensemble_row_numbers <- as.numeric(c(row.names(test), row.names(validation)))
  ensemble1$y <- c(test$y, validation$y)

  if(sum(is.na(ensemble1 >0))){
    ensemble1 <- ensemble1[stats::complete.cases(ensemble1), ]
  }

  ensemble1 <- Filter(function(x) stats::var(x) != 0, ensemble1) # Removes columns with no variation

  if (remove_ensemble_correlations_greater_than > 0) {
    tmp <- stats::cor(ensemble1)
    tmp[upper.tri(tmp)] <- 0
    diag(tmp) <- 0
    data_new <- ensemble1[, !apply(tmp, 2, function(x) any(abs(x) > remove_ensemble_correlations_greater_than, na.rm = TRUE))]
    ensemble1 <- data_new
  }

  head_ensemble <- reactable::reactable(round(head(ensemble1), 4),
                                        searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                        striped = TRUE, highlight = TRUE, resizable = TRUE
  )%>%
    reactablefmtr::add_title("Head of the ensemble")

  ensemble_correlation <- reactable::reactable(round(cor(ensemble1), 4),
                                               searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                               striped = TRUE, highlight = TRUE, resizable = TRUE
  )%>%
    reactablefmtr::add_title("Correlation of the ensemble")

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

  #### Ensemble Using Bagging ####
  ensemble_bagging_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_bagging_train_fit <- ipred::bagging(as.factor(y) ~ ., data = ensemble_train, coob = TRUE)
  }
  if(set_seed == "N"){
    ensemble_bagging_train_fit <- ipred::bagging(as.factor(y) ~ ., data = ensemble_train, coob = TRUE)
  }
  ensemble_bagging_train_pred <- stats::predict(ensemble_bagging_train_fit, ensemble_train, type = "class")
  ensemble_bagging_train_predictions <- plogis(as.numeric(ensemble_bagging_train_pred))
  ensemble_bagging_train_predictions_binomial <- rbinom(n = length(ensemble_bagging_train_predictions), size = 1, prob = ensemble_bagging_train_predictions)
  ensemble_bagging_train_table <- table(ensemble_bagging_train_predictions_binomial, ensemble_y_train)
  ensemble_bagging_train_true_positive_rate[i] <- ensemble_bagging_train_table[2, 2] / sum(ensemble_bagging_train_table[2, 2] + ensemble_bagging_train_table[1, 2])
  ensemble_bagging_train_true_positive_rate_mean <- mean(ensemble_bagging_train_true_positive_rate)
  ensemble_bagging_train_true_negative_rate[i] <- ensemble_bagging_train_table[1, 1] / sum(ensemble_bagging_train_table[1, 1] + ensemble_bagging_train_table[2, 1])
  ensemble_bagging_train_true_negative_rate_mean <- mean(ensemble_bagging_train_true_negative_rate)
  ensemble_bagging_train_false_positive_rate[i] <- ensemble_bagging_train_table[2, 1] / sum(ensemble_bagging_train_table[2, 1] + ensemble_bagging_train_table[1, 1])
  ensemble_bagging_train_false_positive_rate_mean <- mean(ensemble_bagging_train_false_positive_rate)
  ensemble_bagging_train_false_negative_rate[i] <- ensemble_bagging_train_table[1, 2] / sum(ensemble_bagging_train_table[1, 2] + ensemble_bagging_train_table[2, 2])
  ensemble_bagging_train_false_negative_rate_mean <- mean(ensemble_bagging_train_false_negative_rate)
  ensemble_bagging_train_accuracy[i] <- (ensemble_bagging_train_table[1, 1] + ensemble_bagging_train_table[2, 2]) / sum(ensemble_bagging_train_table)
  ensemble_bagging_train_accuracy_mean <- mean(ensemble_bagging_train_accuracy)
  ensemble_bagging_train_F1_score[i] <- 2 * (ensemble_bagging_train_table[2, 2]) / sum(2 * ensemble_bagging_train_table[2, 2] + ensemble_bagging_train_table[1, 2] + ensemble_bagging_train_table[2, 1])
  ensemble_bagging_train_F1_score_mean <- mean(ensemble_bagging_train_F1_score)
  ensemble_bagging_train_positive_predictive_value[i] <- ensemble_bagging_train_table[2, 2] / sum(ensemble_bagging_train_table[2, 2] + ensemble_bagging_train_table[2, 1])
  ensemble_bagging_train_positive_predictive_value_mean <- mean(ensemble_bagging_train_positive_predictive_value)
  ensemble_bagging_train_negative_predictive_value[i] <- ensemble_bagging_train_table[1, 1] / sum(ensemble_bagging_train_table[1, 1] + ensemble_bagging_train_table[1, 2])
  ensemble_bagging_train_negative_predictive_value_mean <- mean(ensemble_bagging_train_negative_predictive_value)

  ensemble_bagging_test_pred <- stats::predict(ensemble_bagging_train_fit, ensemble_test, type = "class")
  ensemble_bagging_test_predictions <- plogis(as.numeric(ensemble_bagging_test_pred))
  ensemble_bagging_test_predictions_binomial <- rbinom(n = length(ensemble_bagging_test_predictions), size = 1, prob = ensemble_bagging_test_predictions)
  ensemble_bagging_test_table <- table(ensemble_bagging_test_predictions_binomial, ensemble_y_test)
  ensemble_bagging_test_true_positive_rate[i] <- ensemble_bagging_test_table[2, 2] / sum(ensemble_bagging_test_table[2, 2] + ensemble_bagging_test_table[1, 2])
  ensemble_bagging_test_true_positive_rate_mean <- mean(ensemble_bagging_test_true_positive_rate)
  ensemble_bagging_test_true_negative_rate[i] <- ensemble_bagging_test_table[1, 1] / sum(ensemble_bagging_test_table[1, 1] + ensemble_bagging_test_table[2, 1])
  ensemble_bagging_test_true_negative_rate_mean <- mean(ensemble_bagging_test_true_negative_rate)
  ensemble_bagging_test_false_positive_rate[i] <- ensemble_bagging_test_table[2, 1] / sum(ensemble_bagging_test_table[2, 1] + ensemble_bagging_test_table[1, 1])
  ensemble_bagging_test_false_positive_rate_mean <- mean(ensemble_bagging_test_false_positive_rate)
  ensemble_bagging_test_false_negative_rate[i] <- ensemble_bagging_test_table[1, 2] / sum(ensemble_bagging_test_table[1, 2] + ensemble_bagging_test_table[2, 2])
  ensemble_bagging_test_false_negative_rate_mean <- mean(ensemble_bagging_test_false_negative_rate)
  ensemble_bagging_test_accuracy[i] <- (ensemble_bagging_test_table[1, 1] + ensemble_bagging_test_table[2, 2]) / sum(ensemble_bagging_test_table)
  ensemble_bagging_test_accuracy_mean <- mean(ensemble_bagging_test_accuracy)
  ensemble_bagging_test_F1_score[i] <- 2 * (ensemble_bagging_test_table[2, 2]) / sum(2 * ensemble_bagging_test_table[2, 2] + ensemble_bagging_test_table[1, 2] + ensemble_bagging_test_table[2, 1])
  ensemble_bagging_test_F1_score_mean <- mean(ensemble_bagging_test_F1_score)
  ensemble_bagging_test_positive_predictive_value[i] <- ensemble_bagging_test_table[2, 2] / sum(ensemble_bagging_test_table[2, 2] + ensemble_bagging_test_table[2, 1])
  ensemble_bagging_test_positive_predictive_value_mean <- mean(ensemble_bagging_test_positive_predictive_value)
  ensemble_bagging_test_negative_predictive_value[i] <- ensemble_bagging_test_table[1, 1] / sum(ensemble_bagging_test_table[1, 1] + ensemble_bagging_test_table[1, 2])
  ensemble_bagging_test_negative_predictive_value_mean <- mean(ensemble_bagging_test_negative_predictive_value)

  ensemble_bagging_validation_pred <- stats::predict(ensemble_bagging_train_fit, ensemble_validation, type = "class")
  ensemble_bagging_validation_predictions <- plogis(as.numeric(ensemble_bagging_validation_pred))
  ensemble_bagging_validation_predictions_binomial <- rbinom(n = length(ensemble_bagging_validation_predictions), size = 1, prob = ensemble_bagging_validation_predictions)
  ensemble_bagging_validation_table <- table(ensemble_bagging_validation_predictions_binomial, ensemble_y_validation)
  ensemble_bagging_validation_true_positive_rate[i] <- ensemble_bagging_validation_table[2, 2] / sum(ensemble_bagging_validation_table[2, 2] + ensemble_bagging_validation_table[1, 2])
  ensemble_bagging_validation_true_positive_rate_mean <- mean(ensemble_bagging_validation_true_positive_rate)
  ensemble_bagging_validation_true_negative_rate[i] <- ensemble_bagging_validation_table[1, 1] / sum(ensemble_bagging_validation_table[1, 1] + ensemble_bagging_validation_table[2, 1])
  ensemble_bagging_validation_true_negative_rate_mean <- mean(ensemble_bagging_validation_true_negative_rate)
  ensemble_bagging_validation_false_positive_rate[i] <- ensemble_bagging_validation_table[2, 1] / sum(ensemble_bagging_validation_table[2, 1] + ensemble_bagging_validation_table[1, 1])
  ensemble_bagging_validation_false_positive_rate_mean <- mean(ensemble_bagging_validation_false_positive_rate)
  ensemble_bagging_validation_false_negative_rate[i] <- ensemble_bagging_validation_table[1, 2] / sum(ensemble_bagging_validation_table[1, 2] + ensemble_bagging_validation_table[2, 2])
  ensemble_bagging_validation_false_negative_rate_mean <- mean(ensemble_bagging_validation_false_negative_rate)
  ensemble_bagging_validation_accuracy[i] <- (ensemble_bagging_validation_table[1, 1] + ensemble_bagging_validation_table[2, 2]) / sum(ensemble_bagging_validation_table)
  ensemble_bagging_validation_accuracy_mean <- mean(ensemble_bagging_validation_accuracy)
  ensemble_bagging_validation_F1_score[i] <- 2 * (ensemble_bagging_validation_table[2, 2]) / sum(2 * ensemble_bagging_validation_table[2, 2] + ensemble_bagging_validation_table[1, 2] + ensemble_bagging_validation_table[2, 1])
  ensemble_bagging_validation_F1_score_mean <- mean(ensemble_bagging_validation_F1_score)
  ensemble_bagging_validation_positive_predictive_value[i] <- ensemble_bagging_validation_table[2, 2] / sum(ensemble_bagging_validation_table[2, 2] + ensemble_bagging_validation_table[2, 1])
  ensemble_bagging_validation_positive_predictive_value_mean <- mean(ensemble_bagging_validation_positive_predictive_value)
  ensemble_bagging_validation_negative_predictive_value[i] <- ensemble_bagging_validation_table[1, 1] / sum(ensemble_bagging_validation_table[1, 1] + ensemble_bagging_validation_table[1, 2])
  ensemble_bagging_validation_negative_predictive_value_mean <- mean(ensemble_bagging_validation_negative_predictive_value)

  ensemble_bagging_holdout_true_positive_rate[i] <- (ensemble_bagging_test_true_positive_rate[i] + ensemble_bagging_validation_true_positive_rate[i]) / 2
  ensemble_bagging_holdout_true_positive_rate_mean <- mean(ensemble_bagging_holdout_true_positive_rate)
  ensemble_bagging_holdout_true_negative_rate[i] <- (ensemble_bagging_test_true_negative_rate[i] + ensemble_bagging_validation_true_negative_rate[i]) / 2
  ensemble_bagging_holdout_true_negative_rate_mean <- mean(ensemble_bagging_holdout_true_negative_rate)
  ensemble_bagging_holdout_false_positive_rate[i] <- (ensemble_bagging_test_false_positive_rate[i] + ensemble_bagging_validation_false_positive_rate[i]) / 2
  ensemble_bagging_holdout_false_positive_rate_mean <- mean(ensemble_bagging_holdout_false_positive_rate)
  ensemble_bagging_holdout_false_negative_rate[i] <- (ensemble_bagging_test_false_negative_rate[i] + ensemble_bagging_validation_false_negative_rate[i]) / 2
  ensemble_bagging_holdout_false_negative_rate_mean <- mean(ensemble_bagging_holdout_false_negative_rate)
  ensemble_bagging_holdout_accuracy[i] <- (ensemble_bagging_test_accuracy[i] + ensemble_bagging_validation_accuracy[i]) / 2
  ensemble_bagging_holdout_accuracy_mean <- mean(ensemble_bagging_holdout_accuracy)
  ensemble_bagging_holdout_accuracy_sd <- sd(ensemble_bagging_holdout_accuracy)
  ensemble_bagging_holdout_F1_score[i] <- (ensemble_bagging_test_F1_score[i] + ensemble_bagging_validation_F1_score[i]) / 2
  ensemble_bagging_holdout_F1_score_mean <- mean(ensemble_bagging_holdout_F1_score)
  ensemble_bagging_holdout_positive_predictive_value[i] <- (ensemble_bagging_test_positive_predictive_value[i] + ensemble_bagging_validation_positive_predictive_value[i]) / 2
  ensemble_bagging_holdout_positive_predictive_value_mean <- mean(ensemble_bagging_holdout_positive_predictive_value)
  ensemble_bagging_holdout_negative_predictive_value[i] <- (ensemble_bagging_test_negative_predictive_value[i] + ensemble_bagging_validation_negative_predictive_value[i]) / 2
  ensemble_bagging_holdout_negative_predictive_value_mean <- mean(ensemble_bagging_holdout_negative_predictive_value)
  ensemble_bagging_holdout_overfitting[i] <- ensemble_bagging_holdout_accuracy[i] / ensemble_bagging_train_accuracy[i]
  ensemble_bagging_holdout_overfitting_mean <- mean(ensemble_bagging_holdout_overfitting)
  ensemble_bagging_holdout_overfitting_range <- range(ensemble_bagging_holdout_overfitting)
  ensemble_bagging_holdout_overfitting_sd <- sd(ensemble_bagging_holdout_overfitting)

  ensemble_bagging_table <- ensemble_bagging_test_table + ensemble_bagging_validation_table
  ensemble_bagging_table_total <- ensemble_bagging_table_total + ensemble_bagging_table

  ensemble_bagging_end <- Sys.time()
  ensemble_bagging_duration[i] <- ensemble_bagging_end - ensemble_bagging_start
  ensemble_bagging_duration_mean <- mean(ensemble_bagging_duration)
  ensemble_bagging_duration_sd <- sd(ensemble_bagging_duration)


  #### Ensemble Using ensemble_C50 ####
  ensemble_C50_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_C50_train_fit <- C50::C5.0(as.factor(ensemble_y_train) ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_C50_train_fit <- C50::C5.0(as.factor(ensemble_y_train) ~ ., data = ensemble_train)
  }
  ensemble_C50_train_pred <- stats::predict(ensemble_C50_train_fit, ensemble_train, type = "class")
  ensemble_C50_train_predictions <- plogis(as.numeric(ensemble_C50_train_pred))
  ensemble_C50_train_predictions_binomial <- rbinom(n = length(ensemble_C50_train_predictions), size = 1, prob = ensemble_C50_train_predictions)
  ensemble_C50_train_table <- table(ensemble_C50_train_predictions_binomial, ensemble_y_train)
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
  ensemble_C50_test_predictions <- plogis(as.numeric(ensemble_C50_test_pred))
  ensemble_C50_test_predictions_binomial <- rbinom(n = length(ensemble_C50_test_predictions), size = 1, prob = ensemble_C50_test_predictions)
  ensemble_C50_test_table <- table(ensemble_C50_test_predictions_binomial, ensemble_y_test)
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
  ensemble_C50_validation_predictions <- plogis(as.numeric(ensemble_C50_validation_pred))
  ensemble_C50_validation_predictions_binomial <- rbinom(n = length(ensemble_C50_validation_predictions), size = 1, prob = ensemble_C50_validation_predictions)
  ensemble_C50_validation_table <- table(ensemble_C50_validation_predictions_binomial, ensemble_y_validation)
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

  ensemble_C50_table <- ensemble_C50_test_table + ensemble_C50_validation_table
  ensemble_C50_table_total <- ensemble_C50_table_total + ensemble_C50_table

  ensemble_C50_end <- Sys.time()
  ensemble_C50_duration[i] <- ensemble_C50_end - ensemble_C50_start
  ensemble_C50_duration_mean <- mean(ensemble_C50_duration)
  ensemble_C50_duration_sd <- sd(ensemble_C50_duration)


  #### Ensemble Using Gradient Boosted ####
  ensemble_gb_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_gb_train_fit <- gbm::gbm(ensemble_train$y ~ ., data = ensemble_train, distribution = "gaussian", n.trees = 100, shrinkage = 0.1, interaction.depth = 10)
  }
  if(set_seed == "N"){
    ensemble_gb_train_fit <- gbm::gbm(ensemble_train$y ~ ., data = ensemble_train, distribution = "gaussian", n.trees = 100, shrinkage = 0.1, interaction.depth = 10)
  }
  ensemble_gb_train_pred <- stats::predict(ensemble_gb_train_fit, ensemble_train, type = "response")
  ensemble_gb_train_predictions <- plogis(as.numeric(ensemble_gb_train_pred))
  ensemble_gb_train_predictions_binomial <- rbinom(n = length(ensemble_gb_train_predictions), size = 1, prob = ensemble_gb_train_predictions)
  ensemble_gb_train_table <- table(ensemble_gb_train_predictions_binomial, ensemble_y_train)
  ensemble_gb_train_true_positive_rate[i] <- ensemble_gb_train_table[2, 2] / sum(ensemble_gb_train_table[2, 2] + ensemble_gb_train_table[1, 2])
  ensemble_gb_train_true_positive_rate_mean <- mean(ensemble_gb_train_true_positive_rate)
  ensemble_gb_train_true_negative_rate[i] <- ensemble_gb_train_table[1, 1] / sum(ensemble_gb_train_table[1, 1] + ensemble_gb_train_table[2, 1])
  ensemble_gb_train_true_negative_rate_mean <- mean(ensemble_gb_train_true_negative_rate)
  ensemble_gb_train_false_positive_rate[i] <- ensemble_gb_train_table[2, 1] / sum(ensemble_gb_train_table[2, 1] + ensemble_gb_train_table[1, 1])
  ensemble_gb_train_false_positive_rate_mean <- mean(ensemble_gb_train_false_positive_rate)
  ensemble_gb_train_false_negative_rate[i] <- ensemble_gb_train_table[1, 2] / sum(ensemble_gb_train_table[1, 2] + ensemble_gb_train_table[2, 2])
  ensemble_gb_train_false_negative_rate_mean <- mean(ensemble_gb_train_false_negative_rate)
  ensemble_gb_train_accuracy[i] <- (ensemble_gb_train_table[1, 1] + ensemble_gb_train_table[2, 2]) / sum(ensemble_gb_train_table)
  ensemble_gb_train_accuracy_mean <- mean(ensemble_gb_train_accuracy)
  ensemble_gb_train_F1_score[i] <- 2 * (ensemble_gb_train_table[2, 2]) / sum(2 * ensemble_gb_train_table[2, 2] + ensemble_gb_train_table[1, 2] + ensemble_gb_train_table[2, 1])
  ensemble_gb_train_F1_score_mean <- mean(ensemble_gb_train_F1_score)
  ensemble_gb_train_positive_predictive_value[i] <- ensemble_gb_train_table[2, 2] / sum(ensemble_gb_train_table[2, 2] + ensemble_gb_train_table[2, 1])
  ensemble_gb_train_positive_predictive_value_mean <- mean(ensemble_gb_train_positive_predictive_value)
  ensemble_gb_train_negative_predictive_value[i] <- ensemble_gb_train_table[1, 1] / sum(ensemble_gb_train_table[1, 1] + ensemble_gb_train_table[1, 2])
  ensemble_gb_train_negative_predictive_value_mean <- mean(ensemble_gb_train_negative_predictive_value)

  ensemble_gb_test_pred <- stats::predict(ensemble_gb_train_fit, ensemble_test, type = "response")
  ensemble_gb_test_predictions <- plogis(as.numeric(ensemble_gb_test_pred))
  ensemble_gb_test_predictions_binomial <- rbinom(n = length(ensemble_gb_test_predictions), size = 1, prob = ensemble_gb_test_predictions)
  ensemble_gb_test_table <- table(ensemble_gb_test_predictions_binomial, ensemble_y_test)
  ensemble_gb_test_true_positive_rate[i] <- ensemble_gb_test_table[2, 2] / sum(ensemble_gb_test_table[2, 2] + ensemble_gb_test_table[1, 2])
  ensemble_gb_test_true_positive_rate_mean <- mean(ensemble_gb_test_true_positive_rate)
  ensemble_gb_test_true_negative_rate[i] <- ensemble_gb_test_table[1, 1] / sum(ensemble_gb_test_table[1, 1] + ensemble_gb_test_table[2, 1])
  ensemble_gb_test_true_negative_rate_mean <- mean(ensemble_gb_test_true_negative_rate)
  ensemble_gb_test_false_positive_rate[i] <- ensemble_gb_test_table[2, 1] / sum(ensemble_gb_test_table[2, 1] + ensemble_gb_test_table[1, 1])
  ensemble_gb_test_false_positive_rate_mean <- mean(ensemble_gb_test_false_positive_rate)
  ensemble_gb_test_false_negative_rate[i] <- ensemble_gb_test_table[1, 2] / sum(ensemble_gb_test_table[1, 2] + ensemble_gb_test_table[2, 2])
  ensemble_gb_test_false_negative_rate_mean <- mean(ensemble_gb_test_false_negative_rate)
  ensemble_gb_test_accuracy[i] <- (ensemble_gb_test_table[1, 1] + ensemble_gb_test_table[2, 2]) / sum(ensemble_gb_test_table)
  ensemble_gb_test_accuracy_mean <- mean(ensemble_gb_test_accuracy)
  ensemble_gb_test_F1_score[i] <- 2 * (ensemble_gb_test_table[2, 2]) / sum(2 * ensemble_gb_test_table[2, 2] + ensemble_gb_test_table[1, 2] + ensemble_gb_test_table[2, 1])
  ensemble_gb_test_F1_score_mean <- mean(ensemble_gb_test_F1_score)
  ensemble_gb_test_positive_predictive_value[i] <- ensemble_gb_test_table[2, 2] / sum(ensemble_gb_test_table[2, 2] + ensemble_gb_test_table[2, 1])
  ensemble_gb_test_positive_predictive_value_mean <- mean(ensemble_gb_test_positive_predictive_value)
  ensemble_gb_test_negative_predictive_value[i] <- ensemble_gb_test_table[1, 1] / sum(ensemble_gb_test_table[1, 1] + ensemble_gb_test_table[1, 2])
  ensemble_gb_test_negative_predictive_value_mean <- mean(ensemble_gb_test_negative_predictive_value)

  ensemble_gb_validation_pred <- stats::predict(ensemble_gb_train_fit, ensemble_validation, type = "response")
  ensemble_gb_validation_predictions <- plogis(as.numeric(ensemble_gb_validation_pred))
  ensemble_gb_validation_predictions_binomial <- rbinom(n = length(ensemble_gb_validation_predictions), size = 1, prob = ensemble_gb_validation_predictions)
  ensemble_gb_validation_table <- table(ensemble_gb_validation_predictions_binomial, ensemble_y_validation)
  ensemble_gb_validation_true_positive_rate[i] <- ensemble_gb_validation_table[2, 2] / sum(ensemble_gb_validation_table[2, 2] + ensemble_gb_validation_table[1, 2])
  ensemble_gb_validation_true_positive_rate_mean <- mean(ensemble_gb_validation_true_positive_rate)
  ensemble_gb_validation_true_negative_rate[i] <- ensemble_gb_validation_table[1, 1] / sum(ensemble_gb_validation_table[1, 1] + ensemble_gb_validation_table[2, 1])
  ensemble_gb_validation_true_negative_rate_mean <- mean(ensemble_gb_validation_true_negative_rate)
  ensemble_gb_validation_false_positive_rate[i] <- ensemble_gb_validation_table[2, 1] / sum(ensemble_gb_validation_table[2, 1] + ensemble_gb_validation_table[1, 1])
  ensemble_gb_validation_false_positive_rate_mean <- mean(ensemble_gb_validation_false_positive_rate)
  ensemble_gb_validation_false_negative_rate[i] <- ensemble_gb_validation_table[1, 2] / sum(ensemble_gb_validation_table[1, 2] + ensemble_gb_validation_table[2, 2])
  ensemble_gb_validation_false_negative_rate_mean <- mean(ensemble_gb_validation_false_negative_rate)
  ensemble_gb_validation_accuracy[i] <- (ensemble_gb_validation_table[1, 1] + ensemble_gb_validation_table[2, 2]) / sum(ensemble_gb_validation_table)
  ensemble_gb_validation_accuracy_mean <- mean(ensemble_gb_validation_accuracy)
  ensemble_gb_validation_F1_score[i] <- 2 * (ensemble_gb_validation_table[2, 2]) / sum(2 * ensemble_gb_validation_table[2, 2] + ensemble_gb_validation_table[1, 2] + ensemble_gb_validation_table[2, 1])
  ensemble_gb_validation_F1_score_mean <- mean(ensemble_gb_validation_F1_score)
  ensemble_gb_validation_positive_predictive_value[i] <- ensemble_gb_validation_table[2, 2] / sum(ensemble_gb_validation_table[2, 2] + ensemble_gb_validation_table[2, 1])
  ensemble_gb_validation_positive_predictive_value_mean <- mean(ensemble_gb_validation_positive_predictive_value)
  ensemble_gb_validation_negative_predictive_value[i] <- ensemble_gb_validation_table[1, 1] / sum(ensemble_gb_validation_table[1, 1] + ensemble_gb_validation_table[1, 2])
  ensemble_gb_validation_negative_predictive_value_mean <- mean(ensemble_gb_validation_negative_predictive_value)

  ensemble_gb_holdout_true_positive_rate[i] <- (ensemble_gb_test_true_positive_rate[i] + ensemble_gb_validation_true_positive_rate[i]) / 2
  ensemble_gb_holdout_true_positive_rate_mean <- mean(ensemble_gb_holdout_true_positive_rate)
  ensemble_gb_holdout_true_negative_rate[i] <- (ensemble_gb_test_true_negative_rate[i] + ensemble_gb_validation_true_negative_rate[i]) / 2
  ensemble_gb_holdout_true_negative_rate_mean <- mean(ensemble_gb_holdout_true_negative_rate)
  ensemble_gb_holdout_false_positive_rate[i] <- (ensemble_gb_test_false_positive_rate[i] + ensemble_gb_validation_false_positive_rate[i]) / 2
  ensemble_gb_holdout_false_positive_rate_mean <- mean(ensemble_gb_holdout_false_positive_rate)
  ensemble_gb_holdout_false_negative_rate[i] <- (ensemble_gb_test_false_negative_rate[i] + ensemble_gb_validation_false_negative_rate[i]) / 2
  ensemble_gb_holdout_false_negative_rate_mean <- mean(ensemble_gb_holdout_false_negative_rate)
  ensemble_gb_holdout_accuracy[i] <- (ensemble_gb_test_accuracy[i] + ensemble_gb_validation_accuracy[i]) / 2
  ensemble_gb_holdout_accuracy_mean <- mean(ensemble_gb_holdout_accuracy)
  ensemble_gb_holdout_accuracy_sd <- sd(ensemble_gb_holdout_accuracy)
  ensemble_gb_holdout_F1_score[i] <- (ensemble_gb_test_F1_score[i] + ensemble_gb_validation_F1_score[i]) / 2
  ensemble_gb_holdout_F1_score_mean <- mean(ensemble_gb_holdout_F1_score)
  ensemble_gb_holdout_positive_predictive_value[i] <- (ensemble_gb_test_positive_predictive_value[i] + ensemble_gb_validation_positive_predictive_value[i]) / 2
  ensemble_gb_holdout_positive_predictive_value_mean <- mean(ensemble_gb_holdout_positive_predictive_value)
  ensemble_gb_holdout_negative_predictive_value[i] <- (ensemble_gb_test_negative_predictive_value[i] + ensemble_gb_validation_negative_predictive_value[i]) / 2
  ensemble_gb_holdout_negative_predictive_value_mean <- mean(ensemble_gb_holdout_negative_predictive_value)
  ensemble_gb_holdout_overfitting[i] <- ensemble_gb_holdout_accuracy[i] / ensemble_gb_train_accuracy[i]
  ensemble_gb_holdout_overfitting_mean <- mean(ensemble_gb_holdout_overfitting)
  ensemble_gb_holdout_overfitting_range <- range(ensemble_gb_holdout_overfitting)
  ensemble_gb_holdout_overfitting_sd <- sd(ensemble_gb_holdout_overfitting)

  ensemble_gb_table <- ensemble_gb_test_table + ensemble_gb_validation_table
  ensemble_gb_table_total <- ensemble_gb_table_total + ensemble_gb_table

  ensemble_gb_end <- Sys.time()
  ensemble_gb_duration[i] <- ensemble_gb_end - ensemble_gb_start
  ensemble_gb_duration_mean <- mean(ensemble_gb_duration)
  ensemble_gb_duration_sd <- sd(ensemble_gb_duration)


  #### Ensemble Using ensemble_pls ####
  ensemble_pls_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_pls_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "PLSModel")
  }
  if(set_seed == "N"){
    ensemble_pls_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "PLSModel")
  }
  ensemble_pls_train_pred <- stats::predict(ensemble_pls_train_fit, ensemble_train, type = "response")
  ensemble_pls_train_predictions <- plogis(as.numeric(ensemble_pls_train_pred))
  ensemble_pls_train_predictions_binomial <- rbinom(n = length(ensemble_pls_train_predictions), size = 1, prob = ensemble_pls_train_predictions)
  ensemble_pls_train_table <- table(ensemble_pls_train_predictions_binomial, ensemble_y_train)
  ensemble_pls_train_true_positive_rate[i] <- ensemble_pls_train_table[2, 2] / sum(ensemble_pls_train_table[2, 2] + ensemble_pls_train_table[1, 2])
  ensemble_pls_train_true_positive_rate_mean <- mean(ensemble_pls_train_true_positive_rate)
  ensemble_pls_train_true_negative_rate[i] <- ensemble_pls_train_table[1, 1] / sum(ensemble_pls_train_table[1, 1] + ensemble_pls_train_table[2, 1])
  ensemble_pls_train_true_negative_rate_mean <- mean(ensemble_pls_train_true_negative_rate)
  ensemble_pls_train_false_positive_rate[i] <- ensemble_pls_train_table[2, 1] / sum(ensemble_pls_train_table[2, 1] + ensemble_pls_train_table[1, 1])
  ensemble_pls_train_false_positive_rate_mean <- mean(ensemble_pls_train_false_positive_rate)
  ensemble_pls_train_false_negative_rate[i] <- ensemble_pls_train_table[1, 2] / sum(ensemble_pls_train_table[1, 2] + ensemble_pls_train_table[2, 2])
  ensemble_pls_train_false_negative_rate_mean <- mean(ensemble_pls_train_false_negative_rate)
  ensemble_pls_train_accuracy[i] <- (ensemble_pls_train_table[1, 1] + ensemble_pls_train_table[2, 2]) / sum(ensemble_pls_train_table)
  ensemble_pls_train_accuracy_mean <- mean(ensemble_pls_train_accuracy)
  ensemble_pls_train_F1_score[i] <- 2 * (ensemble_pls_train_table[2, 2]) / sum(2 * ensemble_pls_train_table[2, 2] + ensemble_pls_train_table[1, 2] + ensemble_pls_train_table[2, 1])
  ensemble_pls_train_F1_score_mean <- mean(ensemble_pls_train_F1_score)
  ensemble_pls_train_positive_predictive_value[i] <- ensemble_pls_train_table[2, 2] / sum(ensemble_pls_train_table[2, 2] + ensemble_pls_train_table[2, 1])
  ensemble_pls_train_positive_predictive_value_mean <- mean(ensemble_pls_train_positive_predictive_value)
  ensemble_pls_train_negative_predictive_value[i] <- ensemble_pls_train_table[1, 1] / sum(ensemble_pls_train_table[1, 1] + ensemble_pls_train_table[1, 2])
  ensemble_pls_train_negative_predictive_value_mean <- mean(ensemble_pls_train_negative_predictive_value)

  ensemble_pls_test_pred <- stats::predict(ensemble_pls_train_fit, ensemble_test, type = "response")
  ensemble_pls_test_predictions <- plogis(as.numeric(ensemble_pls_test_pred))
  ensemble_pls_test_predictions_binomial <- rbinom(n = length(ensemble_pls_test_predictions), size = 1, prob = ensemble_pls_test_predictions)
  ensemble_pls_test_table <- table(ensemble_pls_test_predictions_binomial, ensemble_y_test)
  ensemble_pls_test_true_positive_rate[i] <- ensemble_pls_test_table[2, 2] / sum(ensemble_pls_test_table[2, 2] + ensemble_pls_test_table[1, 2])
  ensemble_pls_test_true_positive_rate_mean <- mean(ensemble_pls_test_true_positive_rate)
  ensemble_pls_test_true_negative_rate[i] <- ensemble_pls_test_table[1, 1] / sum(ensemble_pls_test_table[1, 1] + ensemble_pls_test_table[2, 1])
  ensemble_pls_test_true_negative_rate_mean <- mean(ensemble_pls_test_true_negative_rate)
  ensemble_pls_test_false_positive_rate[i] <- ensemble_pls_test_table[2, 1] / sum(ensemble_pls_test_table[2, 1] + ensemble_pls_test_table[1, 1])
  ensemble_pls_test_false_positive_rate_mean <- mean(ensemble_pls_test_false_positive_rate)
  ensemble_pls_test_false_negative_rate[i] <- ensemble_pls_test_table[1, 2] / sum(ensemble_pls_test_table[1, 2] + ensemble_pls_test_table[2, 2])
  ensemble_pls_test_false_negative_rate_mean <- mean(ensemble_pls_test_false_negative_rate)
  ensemble_pls_test_accuracy[i] <- (ensemble_pls_test_table[1, 1] + ensemble_pls_test_table[2, 2]) / sum(ensemble_pls_test_table)
  ensemble_pls_test_accuracy_mean <- mean(ensemble_pls_test_accuracy)
  ensemble_pls_test_F1_score[i] <- 2 * (ensemble_pls_test_table[2, 2]) / sum(2 * ensemble_pls_test_table[2, 2] + ensemble_pls_test_table[1, 2] + ensemble_pls_test_table[2, 1])
  ensemble_pls_test_F1_score_mean <- mean(ensemble_pls_test_F1_score)
  ensemble_pls_test_positive_predictive_value[i] <- ensemble_pls_test_table[2, 2] / sum(ensemble_pls_test_table[2, 2] + ensemble_pls_test_table[2, 1])
  ensemble_pls_test_positive_predictive_value_mean <- mean(ensemble_pls_test_positive_predictive_value)
  ensemble_pls_test_negative_predictive_value[i] <- ensemble_pls_test_table[1, 1] / sum(ensemble_pls_test_table[1, 1] + ensemble_pls_test_table[1, 2])
  ensemble_pls_test_negative_predictive_value_mean <- mean(ensemble_pls_test_negative_predictive_value)

  ensemble_pls_validation_pred <- stats::predict(ensemble_pls_train_fit, ensemble_validation, type = "response")
  ensemble_pls_validation_predictions <- plogis(as.numeric(ensemble_pls_validation_pred))
  ensemble_pls_validation_predictions_binomial <- rbinom(n = length(ensemble_pls_validation_predictions), size = 1, prob = ensemble_pls_validation_predictions)
  ensemble_pls_validation_table <- table(ensemble_pls_validation_predictions_binomial, ensemble_y_validation)
  ensemble_pls_validation_true_positive_rate[i] <- ensemble_pls_validation_table[2, 2] / sum(ensemble_pls_validation_table[2, 2] + ensemble_pls_validation_table[1, 2])
  ensemble_pls_validation_true_positive_rate_mean <- mean(ensemble_pls_validation_true_positive_rate)
  ensemble_pls_validation_true_negative_rate[i] <- ensemble_pls_validation_table[1, 1] / sum(ensemble_pls_validation_table[1, 1] + ensemble_pls_validation_table[2, 1])
  ensemble_pls_validation_true_negative_rate_mean <- mean(ensemble_pls_validation_true_negative_rate)
  ensemble_pls_validation_false_positive_rate[i] <- ensemble_pls_validation_table[2, 1] / sum(ensemble_pls_validation_table[2, 1] + ensemble_pls_validation_table[1, 1])
  ensemble_pls_validation_false_positive_rate_mean <- mean(ensemble_pls_validation_false_positive_rate)
  ensemble_pls_validation_false_negative_rate[i] <- ensemble_pls_validation_table[1, 2] / sum(ensemble_pls_validation_table[1, 2] + ensemble_pls_validation_table[2, 2])
  ensemble_pls_validation_false_negative_rate_mean <- mean(ensemble_pls_validation_false_negative_rate)
  ensemble_pls_validation_accuracy[i] <- (ensemble_pls_validation_table[1, 1] + ensemble_pls_validation_table[2, 2]) / sum(ensemble_pls_validation_table)
  ensemble_pls_validation_accuracy_mean <- mean(ensemble_pls_validation_accuracy)
  ensemble_pls_validation_F1_score[i] <- 2 * (ensemble_pls_validation_table[2, 2]) / sum(2 * ensemble_pls_validation_table[2, 2] + ensemble_pls_validation_table[1, 2] + ensemble_pls_validation_table[2, 1])
  ensemble_pls_validation_F1_score_mean <- mean(ensemble_pls_validation_F1_score)
  ensemble_pls_validation_positive_predictive_value[i] <- ensemble_pls_validation_table[2, 2] / sum(ensemble_pls_validation_table[2, 2] + ensemble_pls_validation_table[2, 1])
  ensemble_pls_validation_positive_predictive_value_mean <- mean(ensemble_pls_validation_positive_predictive_value)
  ensemble_pls_validation_negative_predictive_value[i] <- ensemble_pls_validation_table[1, 1] / sum(ensemble_pls_validation_table[1, 1] + ensemble_pls_validation_table[1, 2])
  ensemble_pls_validation_negative_predictive_value_mean <- mean(ensemble_pls_validation_negative_predictive_value)

  ensemble_pls_holdout_true_positive_rate[i] <- (ensemble_pls_test_true_positive_rate[i] + ensemble_pls_validation_true_positive_rate[i]) / 2
  ensemble_pls_holdout_true_positive_rate_mean <- mean(ensemble_pls_holdout_true_positive_rate)
  ensemble_pls_holdout_true_negative_rate[i] <- (ensemble_pls_test_true_negative_rate[i] + ensemble_pls_validation_true_negative_rate[i]) / 2
  ensemble_pls_holdout_true_negative_rate_mean <- mean(ensemble_pls_holdout_true_negative_rate)
  ensemble_pls_holdout_false_positive_rate[i] <- (ensemble_pls_test_false_positive_rate[i] + ensemble_pls_validation_false_positive_rate[i]) / 2
  ensemble_pls_holdout_false_positive_rate_mean <- mean(ensemble_pls_holdout_false_positive_rate)
  ensemble_pls_holdout_false_negative_rate[i] <- (ensemble_pls_test_false_negative_rate[i] + ensemble_pls_validation_false_negative_rate[i]) / 2
  ensemble_pls_holdout_false_negative_rate_mean <- mean(ensemble_pls_holdout_false_negative_rate)
  ensemble_pls_holdout_accuracy[i] <- (ensemble_pls_test_accuracy[i] + ensemble_pls_validation_accuracy[i]) / 2
  ensemble_pls_holdout_accuracy_mean <- mean(ensemble_pls_holdout_accuracy)
  ensemble_pls_holdout_accuracy_sd <- sd(ensemble_pls_holdout_accuracy)
  ensemble_pls_holdout_F1_score[i] <- (ensemble_pls_test_F1_score[i] + ensemble_pls_validation_F1_score[i]) / 2
  ensemble_pls_holdout_F1_score_mean <- mean(ensemble_pls_holdout_F1_score)
  ensemble_pls_holdout_positive_predictive_value[i] <- (ensemble_pls_test_positive_predictive_value[i] + ensemble_pls_validation_positive_predictive_value[i]) / 2
  ensemble_pls_holdout_positive_predictive_value_mean <- mean(ensemble_pls_holdout_positive_predictive_value)
  ensemble_pls_holdout_negative_predictive_value[i] <- (ensemble_pls_test_negative_predictive_value[i] + ensemble_pls_validation_negative_predictive_value[i]) / 2
  ensemble_pls_holdout_negative_predictive_value_mean <- mean(ensemble_pls_holdout_negative_predictive_value)
  ensemble_pls_holdout_overfitting[i] <- ensemble_pls_holdout_accuracy[i] / ensemble_pls_train_accuracy[i]
  ensemble_pls_holdout_overfitting_mean <- mean(ensemble_pls_holdout_overfitting)
  ensemble_pls_holdout_overfitting_range <- range(ensemble_pls_holdout_overfitting)
  ensemble_pls_holdout_overfitting_sd <- sd(ensemble_pls_holdout_overfitting)

  ensemble_pls_table <- ensemble_pls_test_table + ensemble_pls_validation_table
  ensemble_pls_table_total <- ensemble_pls_table_total + ensemble_pls_table

  ensemble_pls_end <- Sys.time()
  ensemble_pls_duration[i] <- ensemble_pls_end - ensemble_pls_start
  ensemble_pls_duration_mean <- mean(ensemble_pls_duration)
  ensemble_pls_duration_sd <- sd(ensemble_pls_duration)

  #### Ensemble Using ensemble_pda ####
  ensemble_pda_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_pda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "PDAModel")
  }
  if(set_seed == "N"){
    ensemble_pda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "PDAModel")
  }
  ensemble_pda_train_pred <- stats::predict(ensemble_pda_train_fit, ensemble_train, type = "response")
  ensemble_pda_train_predictions <- plogis(as.numeric(ensemble_pda_train_pred))
  ensemble_pda_train_predictions_binomial <- rbinom(n = length(ensemble_pda_train_predictions), size = 1, prob = ensemble_pda_train_predictions)
  ensemble_pda_train_table <- table(ensemble_pda_train_predictions_binomial, ensemble_y_train)
  ensemble_pda_train_true_positive_rate[i] <- ensemble_pda_train_table[2, 2] / sum(ensemble_pda_train_table[2, 2] + ensemble_pda_train_table[1, 2])
  ensemble_pda_train_true_positive_rate_mean <- mean(ensemble_pda_train_true_positive_rate)
  ensemble_pda_train_true_negative_rate[i] <- ensemble_pda_train_table[1, 1] / sum(ensemble_pda_train_table[1, 1] + ensemble_pda_train_table[2, 1])
  ensemble_pda_train_true_negative_rate_mean <- mean(ensemble_pda_train_true_negative_rate)
  ensemble_pda_train_false_positive_rate[i] <- ensemble_pda_train_table[2, 1] / sum(ensemble_pda_train_table[2, 1] + ensemble_pda_train_table[1, 1])
  ensemble_pda_train_false_positive_rate_mean <- mean(ensemble_pda_train_false_positive_rate)
  ensemble_pda_train_false_negative_rate[i] <- ensemble_pda_train_table[1, 2] / sum(ensemble_pda_train_table[1, 2] + ensemble_pda_train_table[2, 2])
  ensemble_pda_train_false_negative_rate_mean <- mean(ensemble_pda_train_false_negative_rate)
  ensemble_pda_train_accuracy[i] <- (ensemble_pda_train_table[1, 1] + ensemble_pda_train_table[2, 2]) / sum(ensemble_pda_train_table)
  ensemble_pda_train_accuracy_mean <- mean(ensemble_pda_train_accuracy)
  ensemble_pda_train_F1_score[i] <- 2 * (ensemble_pda_train_table[2, 2]) / sum(2 * ensemble_pda_train_table[2, 2] + ensemble_pda_train_table[1, 2] + ensemble_pda_train_table[2, 1])
  ensemble_pda_train_F1_score_mean <- mean(ensemble_pda_train_F1_score)
  ensemble_pda_train_positive_predictive_value[i] <- ensemble_pda_train_table[2, 2] / sum(ensemble_pda_train_table[2, 2] + ensemble_pda_train_table[2, 1])
  ensemble_pda_train_positive_predictive_value_mean <- mean(ensemble_pda_train_positive_predictive_value)
  ensemble_pda_train_negative_predictive_value[i] <- ensemble_pda_train_table[1, 1] / sum(ensemble_pda_train_table[1, 1] + ensemble_pda_train_table[1, 2])
  ensemble_pda_train_negative_predictive_value_mean <- mean(ensemble_pda_train_negative_predictive_value)

  ensemble_pda_test_pred <- stats::predict(ensemble_pda_train_fit, ensemble_test, type = "response")
  ensemble_pda_test_predictions <- plogis(as.numeric(ensemble_pda_test_pred))
  ensemble_pda_test_predictions_binomial <- rbinom(n = length(ensemble_pda_test_predictions), size = 1, prob = ensemble_pda_test_predictions)
  ensemble_pda_test_table <- table(ensemble_pda_test_predictions_binomial, ensemble_y_test)
  ensemble_pda_test_true_positive_rate[i] <- ensemble_pda_test_table[2, 2] / sum(ensemble_pda_test_table[2, 2] + ensemble_pda_test_table[1, 2])
  ensemble_pda_test_true_positive_rate_mean <- mean(ensemble_pda_test_true_positive_rate)
  ensemble_pda_test_true_negative_rate[i] <- ensemble_pda_test_table[1, 1] / sum(ensemble_pda_test_table[1, 1] + ensemble_pda_test_table[2, 1])
  ensemble_pda_test_true_negative_rate_mean <- mean(ensemble_pda_test_true_negative_rate)
  ensemble_pda_test_false_positive_rate[i] <- ensemble_pda_test_table[2, 1] / sum(ensemble_pda_test_table[2, 1] + ensemble_pda_test_table[1, 1])
  ensemble_pda_test_false_positive_rate_mean <- mean(ensemble_pda_test_false_positive_rate)
  ensemble_pda_test_false_negative_rate[i] <- ensemble_pda_test_table[1, 2] / sum(ensemble_pda_test_table[1, 2] + ensemble_pda_test_table[2, 2])
  ensemble_pda_test_false_negative_rate_mean <- mean(ensemble_pda_test_false_negative_rate)
  ensemble_pda_test_accuracy[i] <- (ensemble_pda_test_table[1, 1] + ensemble_pda_test_table[2, 2]) / sum(ensemble_pda_test_table)
  ensemble_pda_test_accuracy_mean <- mean(ensemble_pda_test_accuracy)
  ensemble_pda_test_F1_score[i] <- 2 * (ensemble_pda_test_table[2, 2]) / sum(2 * ensemble_pda_test_table[2, 2] + ensemble_pda_test_table[1, 2] + ensemble_pda_test_table[2, 1])
  ensemble_pda_test_F1_score_mean <- mean(ensemble_pda_test_F1_score)
  ensemble_pda_test_positive_predictive_value[i] <- ensemble_pda_test_table[2, 2] / sum(ensemble_pda_test_table[2, 2] + ensemble_pda_test_table[2, 1])
  ensemble_pda_test_positive_predictive_value_mean <- mean(ensemble_pda_test_positive_predictive_value)
  ensemble_pda_test_negative_predictive_value[i] <- ensemble_pda_test_table[1, 1] / sum(ensemble_pda_test_table[1, 1] + ensemble_pda_test_table[1, 2])
  ensemble_pda_test_negative_predictive_value_mean <- mean(ensemble_pda_test_negative_predictive_value)

  ensemble_pda_validation_pred <- stats::predict(ensemble_pda_train_fit, ensemble_validation, type = "response")
  ensemble_pda_validation_predictions <- plogis(as.numeric(ensemble_pda_validation_pred))
  ensemble_pda_validation_predictions_binomial <- rbinom(n = length(ensemble_pda_validation_predictions), size = 1, prob = ensemble_pda_validation_predictions)
  ensemble_pda_validation_table <- table(ensemble_pda_validation_predictions_binomial, ensemble_y_validation)
  ensemble_pda_validation_true_positive_rate[i] <- ensemble_pda_validation_table[2, 2] / sum(ensemble_pda_validation_table[2, 2] + ensemble_pda_validation_table[1, 2])
  ensemble_pda_validation_true_positive_rate_mean <- mean(ensemble_pda_validation_true_positive_rate)
  ensemble_pda_validation_true_negative_rate[i] <- ensemble_pda_validation_table[1, 1] / sum(ensemble_pda_validation_table[1, 1] + ensemble_pda_validation_table[2, 1])
  ensemble_pda_validation_true_negative_rate_mean <- mean(ensemble_pda_validation_true_negative_rate)
  ensemble_pda_validation_false_positive_rate[i] <- ensemble_pda_validation_table[2, 1] / sum(ensemble_pda_validation_table[2, 1] + ensemble_pda_validation_table[1, 1])
  ensemble_pda_validation_false_positive_rate_mean <- mean(ensemble_pda_validation_false_positive_rate)
  ensemble_pda_validation_false_negative_rate[i] <- ensemble_pda_validation_table[1, 2] / sum(ensemble_pda_validation_table[1, 2] + ensemble_pda_validation_table[2, 2])
  ensemble_pda_validation_false_negative_rate_mean <- mean(ensemble_pda_validation_false_negative_rate)
  ensemble_pda_validation_accuracy[i] <- (ensemble_pda_validation_table[1, 1] + ensemble_pda_validation_table[2, 2]) / sum(ensemble_pda_validation_table)
  ensemble_pda_validation_accuracy_mean <- mean(ensemble_pda_validation_accuracy)
  ensemble_pda_validation_F1_score[i] <- 2 * (ensemble_pda_validation_table[2, 2]) / sum(2 * ensemble_pda_validation_table[2, 2] + ensemble_pda_validation_table[1, 2] + ensemble_pda_validation_table[2, 1])
  ensemble_pda_validation_F1_score_mean <- mean(ensemble_pda_validation_F1_score)
  ensemble_pda_validation_positive_predictive_value[i] <- ensemble_pda_validation_table[2, 2] / sum(ensemble_pda_validation_table[2, 2] + ensemble_pda_validation_table[2, 1])
  ensemble_pda_validation_positive_predictive_value_mean <- mean(ensemble_pda_validation_positive_predictive_value)
  ensemble_pda_validation_negative_predictive_value[i] <- ensemble_pda_validation_table[1, 1] / sum(ensemble_pda_validation_table[1, 1] + ensemble_pda_validation_table[1, 2])
  ensemble_pda_validation_negative_predictive_value_mean <- mean(ensemble_pda_validation_negative_predictive_value)

  ensemble_pda_holdout_true_positive_rate[i] <- (ensemble_pda_test_true_positive_rate[i] + ensemble_pda_validation_true_positive_rate[i]) / 2
  ensemble_pda_holdout_true_positive_rate_mean <- mean(ensemble_pda_holdout_true_positive_rate)
  ensemble_pda_holdout_true_negative_rate[i] <- (ensemble_pda_test_true_negative_rate[i] + ensemble_pda_validation_true_negative_rate[i]) / 2
  ensemble_pda_holdout_true_negative_rate_mean <- mean(ensemble_pda_holdout_true_negative_rate)
  ensemble_pda_holdout_false_positive_rate[i] <- (ensemble_pda_test_false_positive_rate[i] + ensemble_pda_validation_false_positive_rate[i]) / 2
  ensemble_pda_holdout_false_positive_rate_mean <- mean(ensemble_pda_holdout_false_positive_rate)
  ensemble_pda_holdout_false_negative_rate[i] <- (ensemble_pda_test_false_negative_rate[i] + ensemble_pda_validation_false_negative_rate[i]) / 2
  ensemble_pda_holdout_false_negative_rate_mean <- mean(ensemble_pda_holdout_false_negative_rate)
  ensemble_pda_holdout_accuracy[i] <- (ensemble_pda_test_accuracy[i] + ensemble_pda_validation_accuracy[i]) / 2
  ensemble_pda_holdout_accuracy_mean <- mean(ensemble_pda_holdout_accuracy)
  ensemble_pda_holdout_accuracy_sd <- sd(ensemble_pda_holdout_accuracy)
  ensemble_pda_holdout_F1_score[i] <- (ensemble_pda_test_F1_score[i] + ensemble_pda_validation_F1_score[i]) / 2
  ensemble_pda_holdout_F1_score_mean <- mean(ensemble_pda_holdout_F1_score)
  ensemble_pda_holdout_positive_predictive_value[i] <- (ensemble_pda_test_positive_predictive_value[i] + ensemble_pda_validation_positive_predictive_value[i]) / 2
  ensemble_pda_holdout_positive_predictive_value_mean <- mean(ensemble_pda_holdout_positive_predictive_value)
  ensemble_pda_holdout_negative_predictive_value[i] <- (ensemble_pda_test_negative_predictive_value[i] + ensemble_pda_validation_negative_predictive_value[i]) / 2
  ensemble_pda_holdout_negative_predictive_value_mean <- mean(ensemble_pda_holdout_negative_predictive_value)
  ensemble_pda_holdout_overfitting[i] <- ensemble_pda_holdout_accuracy[i] / ensemble_pda_train_accuracy[i]
  ensemble_pda_holdout_overfitting_mean <- mean(ensemble_pda_holdout_overfitting)
  ensemble_pda_holdout_overfitting_range <- range(ensemble_pda_holdout_overfitting)
  ensemble_pda_holdout_overfitting_sd <- sd(ensemble_pda_holdout_overfitting)

  ensemble_pda_table <- ensemble_pda_test_table + ensemble_pda_validation_table
  ensemble_pda_table_total <- ensemble_pda_table_total + ensemble_pda_table

  ensemble_pda_end <- Sys.time()
  ensemble_pda_duration[i] <- ensemble_pda_end - ensemble_pda_start
  ensemble_pda_duration_mean <- mean(ensemble_pda_duration)
  ensemble_pda_duration_sd <- sd(ensemble_pda_duration)


  #### Ensemble Using RPart ####
  ensemble_rpart_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_rpart_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "RPartModel")
  }
  if(set_seed == "N"){
    ensemble_rpart_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "RPartModel")
  }
  ensemble_rpart_train_pred <- stats::predict(ensemble_rpart_train_fit, ensemble_train, type = "response")
  ensemble_rpart_train_predictions <- plogis(as.numeric(ensemble_rpart_train_pred))
  ensemble_rpart_train_predictions_binomial <- rbinom(n = length(ensemble_rpart_train_predictions), size = 1, prob = ensemble_rpart_train_predictions)
  ensemble_rpart_train_table <- table(ensemble_rpart_train_predictions_binomial, ensemble_y_train)
  ensemble_rpart_train_true_positive_rate[i] <- ensemble_rpart_train_table[2, 2] / sum(ensemble_rpart_train_table[2, 2] + ensemble_rpart_train_table[1, 2])
  ensemble_rpart_train_true_positive_rate_mean <- mean(ensemble_rpart_train_true_positive_rate)
  ensemble_rpart_train_true_negative_rate[i] <- ensemble_rpart_train_table[1, 1] / sum(ensemble_rpart_train_table[1, 1] + ensemble_rpart_train_table[2, 1])
  ensemble_rpart_train_true_negative_rate_mean <- mean(ensemble_rpart_train_true_negative_rate)
  ensemble_rpart_train_false_positive_rate[i] <- ensemble_rpart_train_table[2, 1] / sum(ensemble_rpart_train_table[2, 1] + ensemble_rpart_train_table[1, 1])
  ensemble_rpart_train_false_positive_rate_mean <- mean(ensemble_rpart_train_false_positive_rate)
  ensemble_rpart_train_false_negative_rate[i] <- ensemble_rpart_train_table[1, 2] / sum(ensemble_rpart_train_table[1, 2] + ensemble_rpart_train_table[2, 2])
  ensemble_rpart_train_false_negative_rate_mean <- mean(ensemble_rpart_train_false_negative_rate)
  ensemble_rpart_train_accuracy[i] <- (ensemble_rpart_train_table[1, 1] + ensemble_rpart_train_table[2, 2]) / sum(ensemble_rpart_train_table)
  ensemble_rpart_train_accuracy_mean <- mean(ensemble_rpart_train_accuracy)
  ensemble_rpart_train_F1_score[i] <- 2 * (ensemble_rpart_train_table[2, 2]) / sum(2 * ensemble_rpart_train_table[2, 2] + ensemble_rpart_train_table[1, 2] + ensemble_rpart_train_table[2, 1])
  ensemble_rpart_train_F1_score_mean <- mean(ensemble_rpart_train_F1_score)
  ensemble_rpart_train_positive_predictive_value[i] <- ensemble_rpart_train_table[2, 2] / sum(ensemble_rpart_train_table[2, 2] + ensemble_rpart_train_table[2, 1])
  ensemble_rpart_train_positive_predictive_value_mean <- mean(ensemble_rpart_train_positive_predictive_value)
  ensemble_rpart_train_negative_predictive_value[i] <- ensemble_rpart_train_table[1, 1] / sum(ensemble_rpart_train_table[1, 1] + ensemble_rpart_train_table[1, 2])
  ensemble_rpart_train_negative_predictive_value_mean <- mean(ensemble_rpart_train_negative_predictive_value)

  ensemble_rpart_test_pred <- stats::predict(ensemble_rpart_train_fit, ensemble_test, type = "response")
  ensemble_rpart_test_predictions <- plogis(as.numeric(ensemble_rpart_test_pred))
  ensemble_rpart_test_predictions_binomial <- rbinom(n = length(ensemble_rpart_test_predictions), size = 1, prob = ensemble_rpart_test_predictions)
  ensemble_rpart_test_table <- table(ensemble_rpart_test_predictions_binomial, ensemble_y_test)
  ensemble_rpart_test_true_positive_rate[i] <- ensemble_rpart_test_table[2, 2] / sum(ensemble_rpart_test_table[2, 2] + ensemble_rpart_test_table[1, 2])
  ensemble_rpart_test_true_positive_rate_mean <- mean(ensemble_rpart_test_true_positive_rate)
  ensemble_rpart_test_true_negative_rate[i] <- ensemble_rpart_test_table[1, 1] / sum(ensemble_rpart_test_table[1, 1] + ensemble_rpart_test_table[2, 1])
  ensemble_rpart_test_true_negative_rate_mean <- mean(ensemble_rpart_test_true_negative_rate)
  ensemble_rpart_test_false_positive_rate[i] <- ensemble_rpart_test_table[2, 1] / sum(ensemble_rpart_test_table[2, 1] + ensemble_rpart_test_table[1, 1])
  ensemble_rpart_test_false_positive_rate_mean <- mean(ensemble_rpart_test_false_positive_rate)
  ensemble_rpart_test_false_negative_rate[i] <- ensemble_rpart_test_table[1, 2] / sum(ensemble_rpart_test_table[1, 2] + ensemble_rpart_test_table[2, 2])
  ensemble_rpart_test_false_negative_rate_mean <- mean(ensemble_rpart_test_false_negative_rate)
  ensemble_rpart_test_accuracy[i] <- (ensemble_rpart_test_table[1, 1] + ensemble_rpart_test_table[2, 2]) / sum(ensemble_rpart_test_table)
  ensemble_rpart_test_accuracy_mean <- mean(ensemble_rpart_test_accuracy)
  ensemble_rpart_test_F1_score[i] <- 2 * (ensemble_rpart_test_table[2, 2]) / sum(2 * ensemble_rpart_test_table[2, 2] + ensemble_rpart_test_table[1, 2] + ensemble_rpart_test_table[2, 1])
  ensemble_rpart_test_F1_score_mean <- mean(ensemble_rpart_test_F1_score)
  ensemble_rpart_test_positive_predictive_value[i] <- ensemble_rpart_test_table[2, 2] / sum(ensemble_rpart_test_table[2, 2] + ensemble_rpart_test_table[2, 1])
  ensemble_rpart_test_positive_predictive_value_mean <- mean(ensemble_rpart_test_positive_predictive_value)
  ensemble_rpart_test_negative_predictive_value[i] <- ensemble_rpart_test_table[1, 1] / sum(ensemble_rpart_test_table[1, 1] + ensemble_rpart_test_table[1, 2])
  ensemble_rpart_test_negative_predictive_value_mean <- mean(ensemble_rpart_test_negative_predictive_value)

  ensemble_rpart_validation_pred <- stats::predict(ensemble_rpart_train_fit, ensemble_validation, type = "response")
  ensemble_rpart_validation_predictions <- plogis(as.numeric(ensemble_rpart_validation_pred))
  ensemble_rpart_validation_predictions_binomial <- rbinom(n = length(ensemble_rpart_validation_predictions), size = 1, prob = ensemble_rpart_validation_predictions)
  ensemble_rpart_validation_table <- table(ensemble_rpart_validation_predictions_binomial, ensemble_y_validation)
  ensemble_rpart_validation_true_positive_rate[i] <- ensemble_rpart_validation_table[2, 2] / sum(ensemble_rpart_validation_table[2, 2] + ensemble_rpart_validation_table[1, 2])
  ensemble_rpart_validation_true_positive_rate_mean <- mean(ensemble_rpart_validation_true_positive_rate)
  ensemble_rpart_validation_true_negative_rate[i] <- ensemble_rpart_validation_table[1, 1] / sum(ensemble_rpart_validation_table[1, 1] + ensemble_rpart_validation_table[2, 1])
  ensemble_rpart_validation_true_negative_rate_mean <- mean(ensemble_rpart_validation_true_negative_rate)
  ensemble_rpart_validation_false_positive_rate[i] <- ensemble_rpart_validation_table[2, 1] / sum(ensemble_rpart_validation_table[2, 1] + ensemble_rpart_validation_table[1, 1])
  ensemble_rpart_validation_false_positive_rate_mean <- mean(ensemble_rpart_validation_false_positive_rate)
  ensemble_rpart_validation_false_negative_rate[i] <- ensemble_rpart_validation_table[1, 2] / sum(ensemble_rpart_validation_table[1, 2] + ensemble_rpart_validation_table[2, 2])
  ensemble_rpart_validation_false_negative_rate_mean <- mean(ensemble_rpart_validation_false_negative_rate)
  ensemble_rpart_validation_accuracy[i] <- (ensemble_rpart_validation_table[1, 1] + ensemble_rpart_validation_table[2, 2]) / sum(ensemble_rpart_validation_table)
  ensemble_rpart_validation_accuracy_mean <- mean(ensemble_rpart_validation_accuracy)
  ensemble_rpart_validation_F1_score[i] <- 2 * (ensemble_rpart_validation_table[2, 2]) / sum(2 * ensemble_rpart_validation_table[2, 2] + ensemble_rpart_validation_table[1, 2] + ensemble_rpart_validation_table[2, 1])
  ensemble_rpart_validation_F1_score_mean <- mean(ensemble_rpart_validation_F1_score)
  ensemble_rpart_validation_positive_predictive_value[i] <- ensemble_rpart_validation_table[2, 2] / sum(ensemble_rpart_validation_table[2, 2] + ensemble_rpart_validation_table[2, 1])
  ensemble_rpart_validation_positive_predictive_value_mean <- mean(ensemble_rpart_validation_positive_predictive_value)
  ensemble_rpart_validation_negative_predictive_value[i] <- ensemble_rpart_validation_table[1, 1] / sum(ensemble_rpart_validation_table[1, 1] + ensemble_rpart_validation_table[1, 2])
  ensemble_rpart_validation_negative_predictive_value_mean <- mean(ensemble_rpart_validation_negative_predictive_value)

  ensemble_rpart_holdout_true_positive_rate[i] <- (ensemble_rpart_test_true_positive_rate[i] + ensemble_rpart_validation_true_positive_rate[i]) / 2
  ensemble_rpart_holdout_true_positive_rate_mean <- mean(ensemble_rpart_holdout_true_positive_rate)
  ensemble_rpart_holdout_true_negative_rate[i] <- (ensemble_rpart_test_true_negative_rate[i] + ensemble_rpart_validation_true_negative_rate[i]) / 2
  ensemble_rpart_holdout_true_negative_rate_mean <- mean(ensemble_rpart_holdout_true_negative_rate)
  ensemble_rpart_holdout_false_positive_rate[i] <- (ensemble_rpart_test_false_positive_rate[i] + ensemble_rpart_validation_false_positive_rate[i]) / 2
  ensemble_rpart_holdout_false_positive_rate_mean <- mean(ensemble_rpart_holdout_false_positive_rate)
  ensemble_rpart_holdout_false_negative_rate[i] <- (ensemble_rpart_test_false_negative_rate[i] + ensemble_rpart_validation_false_negative_rate[i]) / 2
  ensemble_rpart_holdout_false_negative_rate_mean <- mean(ensemble_rpart_holdout_false_negative_rate)
  ensemble_rpart_holdout_accuracy[i] <- (ensemble_rpart_test_accuracy[i] + ensemble_rpart_validation_accuracy[i]) / 2
  ensemble_rpart_holdout_accuracy_mean <- mean(ensemble_rpart_holdout_accuracy)
  ensemble_rpart_holdout_accuracy_sd <- sd(ensemble_rpart_holdout_accuracy)
  ensemble_rpart_holdout_F1_score[i] <- (ensemble_rpart_test_F1_score[i] + ensemble_rpart_validation_F1_score[i]) / 2
  ensemble_rpart_holdout_F1_score_mean <- mean(ensemble_rpart_holdout_F1_score)
  ensemble_rpart_holdout_positive_predictive_value[i] <- (ensemble_rpart_test_positive_predictive_value[i] + ensemble_rpart_validation_positive_predictive_value[i]) / 2
  ensemble_rpart_holdout_positive_predictive_value_mean <- mean(ensemble_rpart_holdout_positive_predictive_value)
  ensemble_rpart_holdout_negative_predictive_value[i] <- (ensemble_rpart_test_negative_predictive_value[i] + ensemble_rpart_validation_negative_predictive_value[i]) / 2
  ensemble_rpart_holdout_negative_predictive_value_mean <- mean(ensemble_rpart_holdout_negative_predictive_value)
  ensemble_rpart_holdout_overfitting[i] <- ensemble_rpart_holdout_accuracy[i] / ensemble_rpart_train_accuracy[i]
  ensemble_rpart_holdout_overfitting_mean <- mean(ensemble_rpart_holdout_overfitting)
  ensemble_rpart_holdout_overfitting_range <- range(ensemble_rpart_holdout_overfitting)
  ensemble_rpart_holdout_overfitting_sd <- sd(ensemble_rpart_holdout_overfitting)

  ensemble_rpart_table <- ensemble_rpart_test_table + ensemble_rpart_validation_table
  ensemble_rpart_table_total <- ensemble_rpart_table_total + ensemble_rpart_table

  ensemble_rpart_end <- Sys.time()
  ensemble_rpart_duration[i] <- ensemble_rpart_end - ensemble_rpart_start
  ensemble_rpart_duration_mean <- mean(ensemble_rpart_duration)
  ensemble_rpart_duration_sd <- sd(ensemble_rpart_duration)


  #### Ensemble Using Support Vector Machines ####
  ensemble_svm_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_svm_train_fit <- e1071::svm(as.factor(y) ~ ., data = ensemble_train, kernel = "radial", gamma = 1, cost = 1)
  }
  if(set_seed == "N"){
    ensemble_svm_train_fit <- e1071::svm(as.factor(y) ~ ., data = ensemble_train, kernel = "radial", gamma = 1, cost = 1)
  }
  ensemble_svm_train_pred <- stats::predict(ensemble_svm_train_fit, ensemble_train, type = "response")
  ensemble_svm_train_predictions <- plogis(as.numeric(ensemble_svm_train_pred))
  ensemble_svm_train_predictions_binomial <- rbinom(n = length(ensemble_svm_train_predictions), size = 1, prob = ensemble_svm_train_predictions)
  ensemble_svm_train_table <- table(ensemble_svm_train_predictions_binomial, ensemble_y_train)
  ensemble_svm_train_true_positive_rate[i] <- ensemble_svm_train_table[2, 2] / sum(ensemble_svm_train_table[2, 2] + ensemble_svm_train_table[1, 2])
  ensemble_svm_train_true_positive_rate_mean <- mean(ensemble_svm_train_true_positive_rate)
  ensemble_svm_train_true_negative_rate[i] <- ensemble_svm_train_table[1, 1] / sum(ensemble_svm_train_table[1, 1] + ensemble_svm_train_table[2, 1])
  ensemble_svm_train_true_negative_rate_mean <- mean(ensemble_svm_train_true_negative_rate)
  ensemble_svm_train_false_positive_rate[i] <- ensemble_svm_train_table[2, 1] / sum(ensemble_svm_train_table[2, 1] + ensemble_svm_train_table[1, 1])
  ensemble_svm_train_false_positive_rate_mean <- mean(ensemble_svm_train_false_positive_rate)
  ensemble_svm_train_false_negative_rate[i] <- ensemble_svm_train_table[1, 2] / sum(ensemble_svm_train_table[1, 2] + ensemble_svm_train_table[2, 2])
  ensemble_svm_train_false_negative_rate_mean <- mean(ensemble_svm_train_false_negative_rate)
  ensemble_svm_train_accuracy[i] <- (ensemble_svm_train_table[1, 1] + ensemble_svm_train_table[2, 2]) / sum(ensemble_svm_train_table)
  ensemble_svm_train_accuracy_mean <- mean(ensemble_svm_train_accuracy)
  ensemble_svm_train_F1_score[i] <- 2 * (ensemble_svm_train_table[2, 2]) / sum(2 * ensemble_svm_train_table[2, 2] + ensemble_svm_train_table[1, 2] + ensemble_svm_train_table[2, 1])
  ensemble_svm_train_F1_score_mean <- mean(ensemble_svm_train_F1_score)
  ensemble_svm_train_positive_predictive_value[i] <- ensemble_svm_train_table[2, 2] / sum(ensemble_svm_train_table[2, 2] + ensemble_svm_train_table[2, 1])
  ensemble_svm_train_positive_predictive_value_mean <- mean(ensemble_svm_train_positive_predictive_value)
  ensemble_svm_train_negative_predictive_value[i] <- ensemble_svm_train_table[1, 1] / sum(ensemble_svm_train_table[1, 1] + ensemble_svm_train_table[1, 2])
  ensemble_svm_train_negative_predictive_value_mean <- mean(ensemble_svm_train_negative_predictive_value)

  ensemble_svm_test_pred <- stats::predict(ensemble_svm_train_fit, ensemble_test, type = "response")
  ensemble_svm_test_predictions <- plogis(as.numeric(ensemble_svm_test_pred))
  ensemble_svm_test_predictions_binomial <- rbinom(n = length(ensemble_svm_test_predictions), size = 1, prob = ensemble_svm_test_predictions)
  ensemble_svm_test_table <- table(ensemble_svm_test_predictions_binomial, ensemble_y_test)
  ensemble_svm_test_true_positive_rate[i] <- ensemble_svm_test_table[2, 2] / sum(ensemble_svm_test_table[2, 2] + ensemble_svm_test_table[1, 2])
  ensemble_svm_test_true_positive_rate_mean <- mean(ensemble_svm_test_true_positive_rate)
  ensemble_svm_test_true_negative_rate[i] <- ensemble_svm_test_table[1, 1] / sum(ensemble_svm_test_table[1, 1] + ensemble_svm_test_table[2, 1])
  ensemble_svm_test_true_negative_rate_mean <- mean(ensemble_svm_test_true_negative_rate)
  ensemble_svm_test_false_positive_rate[i] <- ensemble_svm_test_table[2, 1] / sum(ensemble_svm_test_table[2, 1] + ensemble_svm_test_table[1, 1])
  ensemble_svm_test_false_positive_rate_mean <- mean(ensemble_svm_test_false_positive_rate)
  ensemble_svm_test_false_negative_rate[i] <- ensemble_svm_test_table[1, 2] / sum(ensemble_svm_test_table[1, 2] + ensemble_svm_test_table[2, 2])
  ensemble_svm_test_false_negative_rate_mean <- mean(ensemble_svm_test_false_negative_rate)
  ensemble_svm_test_accuracy[i] <- (ensemble_svm_test_table[1, 1] + ensemble_svm_test_table[2, 2]) / sum(ensemble_svm_test_table)
  ensemble_svm_test_accuracy_mean <- mean(ensemble_svm_test_accuracy)
  ensemble_svm_test_F1_score[i] <- 2 * (ensemble_svm_test_table[2, 2]) / sum(2 * ensemble_svm_test_table[2, 2] + ensemble_svm_test_table[1, 2] + ensemble_svm_test_table[2, 1])
  ensemble_svm_test_F1_score_mean <- mean(ensemble_svm_test_F1_score)
  ensemble_svm_test_positive_predictive_value[i] <- ensemble_svm_test_table[2, 2] / sum(ensemble_svm_test_table[2, 2] + ensemble_svm_test_table[2, 1])
  ensemble_svm_test_positive_predictive_value_mean <- mean(ensemble_svm_test_positive_predictive_value)
  ensemble_svm_test_negative_predictive_value[i] <- ensemble_svm_test_table[1, 1] / sum(ensemble_svm_test_table[1, 1] + ensemble_svm_test_table[1, 2])
  ensemble_svm_test_negative_predictive_value_mean <- mean(ensemble_svm_test_negative_predictive_value)

  ensemble_svm_validation_pred <- stats::predict(ensemble_svm_train_fit, ensemble_validation, type = "response")
  ensemble_svm_validation_predictions <- plogis(as.numeric(ensemble_svm_validation_pred))
  ensemble_svm_validation_predictions_binomial <- rbinom(n = length(ensemble_svm_validation_predictions), size = 1, prob = ensemble_svm_validation_predictions)
  ensemble_svm_validation_table <- table(ensemble_svm_validation_predictions_binomial, ensemble_y_validation)
  ensemble_svm_validation_true_positive_rate[i] <- ensemble_svm_validation_table[2, 2] / sum(ensemble_svm_validation_table[2, 2] + ensemble_svm_validation_table[1, 2])
  ensemble_svm_validation_true_positive_rate_mean <- mean(ensemble_svm_validation_true_positive_rate)
  ensemble_svm_validation_true_negative_rate[i] <- ensemble_svm_validation_table[1, 1] / sum(ensemble_svm_validation_table[1, 1] + ensemble_svm_validation_table[2, 1])
  ensemble_svm_validation_true_negative_rate_mean <- mean(ensemble_svm_validation_true_negative_rate)
  ensemble_svm_validation_false_positive_rate[i] <- ensemble_svm_validation_table[2, 1] / sum(ensemble_svm_validation_table[2, 1] + ensemble_svm_validation_table[1, 1])
  ensemble_svm_validation_false_positive_rate_mean <- mean(ensemble_svm_validation_false_positive_rate)
  ensemble_svm_validation_false_negative_rate[i] <- ensemble_svm_validation_table[1, 2] / sum(ensemble_svm_validation_table[1, 2] + ensemble_svm_validation_table[2, 2])
  ensemble_svm_validation_false_negative_rate_mean <- mean(ensemble_svm_validation_false_negative_rate)
  ensemble_svm_validation_accuracy[i] <- (ensemble_svm_validation_table[1, 1] + ensemble_svm_validation_table[2, 2]) / sum(ensemble_svm_validation_table)
  ensemble_svm_validation_accuracy_mean <- mean(ensemble_svm_validation_accuracy)
  ensemble_svm_validation_F1_score[i] <- 2 * (ensemble_svm_validation_table[2, 2]) / sum(2 * ensemble_svm_validation_table[2, 2] + ensemble_svm_validation_table[1, 2] + ensemble_svm_validation_table[2, 1])
  ensemble_svm_validation_F1_score_mean <- mean(ensemble_svm_validation_F1_score)
  ensemble_svm_validation_positive_predictive_value[i] <- ensemble_svm_validation_table[2, 2] / sum(ensemble_svm_validation_table[2, 2] + ensemble_svm_validation_table[2, 1])
  ensemble_svm_validation_positive_predictive_value_mean <- mean(ensemble_svm_validation_positive_predictive_value)
  ensemble_svm_validation_negative_predictive_value[i] <- ensemble_svm_validation_table[1, 1] / sum(ensemble_svm_validation_table[1, 1] + ensemble_svm_validation_table[1, 2])
  ensemble_svm_validation_negative_predictive_value_mean <- mean(ensemble_svm_validation_negative_predictive_value)

  ensemble_svm_holdout_true_positive_rate[i] <- (ensemble_svm_test_true_positive_rate[i] + ensemble_svm_validation_true_positive_rate[i]) / 2
  ensemble_svm_holdout_true_positive_rate_mean <- mean(ensemble_svm_holdout_true_positive_rate)
  ensemble_svm_holdout_true_negative_rate[i] <- (ensemble_svm_test_true_negative_rate[i] + ensemble_svm_validation_true_negative_rate[i]) / 2
  ensemble_svm_holdout_true_negative_rate_mean <- mean(ensemble_svm_holdout_true_negative_rate)
  ensemble_svm_holdout_false_positive_rate[i] <- (ensemble_svm_test_false_positive_rate[i] + ensemble_svm_validation_false_positive_rate[i]) / 2
  ensemble_svm_holdout_false_positive_rate_mean <- mean(ensemble_svm_holdout_false_positive_rate)
  ensemble_svm_holdout_false_negative_rate[i] <- (ensemble_svm_test_false_negative_rate[i] + ensemble_svm_validation_false_negative_rate[i]) / 2
  ensemble_svm_holdout_false_negative_rate_mean <- mean(ensemble_svm_holdout_false_negative_rate)
  ensemble_svm_holdout_accuracy[i] <- (ensemble_svm_test_accuracy[i] + ensemble_svm_validation_accuracy[i]) / 2
  ensemble_svm_holdout_accuracy_mean <- mean(ensemble_svm_holdout_accuracy)
  ensemble_svm_holdout_accuracy_sd <- sd(ensemble_svm_holdout_accuracy)
  ensemble_svm_holdout_F1_score[i] <- (ensemble_svm_test_F1_score[i] + ensemble_svm_validation_F1_score[i]) / 2
  ensemble_svm_holdout_F1_score_mean <- mean(ensemble_svm_holdout_F1_score)
  ensemble_svm_holdout_positive_predictive_value[i] <- (ensemble_svm_test_positive_predictive_value[i] + ensemble_svm_validation_positive_predictive_value[i]) / 2
  ensemble_svm_holdout_positive_predictive_value_mean <- mean(ensemble_svm_holdout_positive_predictive_value)
  ensemble_svm_holdout_negative_predictive_value[i] <- (ensemble_svm_test_negative_predictive_value[i] + ensemble_svm_validation_negative_predictive_value[i]) / 2
  ensemble_svm_holdout_negative_predictive_value_mean <- mean(ensemble_svm_holdout_negative_predictive_value)
  ensemble_svm_holdout_overfitting[i] <- ensemble_svm_holdout_accuracy[i] / ensemble_svm_train_accuracy[i]
  ensemble_svm_holdout_overfitting_mean <- mean(ensemble_svm_holdout_overfitting)
  ensemble_svm_holdout_overfitting_range <- range(ensemble_svm_holdout_overfitting)
  ensemble_svm_holdout_overfitting_sd <- sd(ensemble_svm_holdout_overfitting)

  ensemble_svm_table <- ensemble_svm_test_table + ensemble_svm_validation_table
  ensemble_svm_table_total <- ensemble_svm_table_total + ensemble_svm_table

  ensemble_svm_end <- Sys.time()
  ensemble_svm_duration[i] <- ensemble_svm_end - ensemble_svm_start
  ensemble_svm_duration_mean <- mean(ensemble_svm_duration)
  ensemble_svm_duration_sd <- sd(ensemble_svm_duration)


  #### Ensemble Using Trees ####
  ensemble_tree_start <- Sys.time()

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_tree_train_fit <- tree::tree(ensemble_train$y ~ ., data = ensemble_train)
  }
  if(set_seed == "N"){
    ensemble_tree_train_fit <- tree::tree(ensemble_train$y ~ ., data = ensemble_train)
  }
  ensemble_tree_train_pred <- stats::predict(ensemble_tree_train_fit, ensemble_train, type = "vector")
  ensemble_tree_train_predictions <- plogis(as.numeric(ensemble_tree_train_pred))
  ensemble_tree_train_predictions_binomial <- rbinom(n = length(ensemble_tree_train_predictions), size = 1, prob = ensemble_tree_train_predictions)
  ensemble_tree_train_table <- table(ensemble_tree_train_predictions_binomial, ensemble_y_train)
  ensemble_tree_train_true_positive_rate[i] <- ensemble_tree_train_table[2, 2] / sum(ensemble_tree_train_table[2, 2] + ensemble_tree_train_table[1, 2])
  ensemble_tree_train_true_positive_rate_mean <- mean(ensemble_tree_train_true_positive_rate)
  ensemble_tree_train_true_negative_rate[i] <- ensemble_tree_train_table[1, 1] / sum(ensemble_tree_train_table[1, 1] + ensemble_tree_train_table[2, 1])
  ensemble_tree_train_true_negative_rate_mean <- mean(ensemble_tree_train_true_negative_rate)
  ensemble_tree_train_false_positive_rate[i] <- ensemble_tree_train_table[2, 1] / sum(ensemble_tree_train_table[2, 1] + ensemble_tree_train_table[1, 1])
  ensemble_tree_train_false_positive_rate_mean <- mean(ensemble_tree_train_false_positive_rate)
  ensemble_tree_train_false_negative_rate[i] <- ensemble_tree_train_table[1, 2] / sum(ensemble_tree_train_table[1, 2] + ensemble_tree_train_table[2, 2])
  ensemble_tree_train_false_negative_rate_mean <- mean(ensemble_tree_train_false_negative_rate)
  ensemble_tree_train_accuracy[i] <- (ensemble_tree_train_table[1, 1] + ensemble_tree_train_table[2, 2]) / sum(ensemble_tree_train_table)
  ensemble_tree_train_accuracy_mean <- mean(ensemble_tree_train_accuracy)
  ensemble_tree_train_F1_score[i] <- 2 * (ensemble_tree_train_table[2, 2]) / sum(2 * ensemble_tree_train_table[2, 2] + ensemble_tree_train_table[1, 2] + ensemble_tree_train_table[2, 1])
  ensemble_tree_train_F1_score_mean <- mean(ensemble_tree_train_F1_score)
  ensemble_tree_train_positive_predictive_value[i] <- ensemble_tree_train_table[2, 2] / sum(ensemble_tree_train_table[2, 2] + ensemble_tree_train_table[2, 1])
  ensemble_tree_train_positive_predictive_value_mean <- mean(ensemble_tree_train_positive_predictive_value)
  ensemble_tree_train_negative_predictive_value[i] <- ensemble_tree_train_table[1, 1] / sum(ensemble_tree_train_table[1, 1] + ensemble_tree_train_table[1, 2])
  ensemble_tree_train_negative_predictive_value_mean <- mean(ensemble_tree_train_negative_predictive_value)

  ensemble_tree_test_pred <- stats::predict(ensemble_tree_train_fit, ensemble_test, type = "vector")
  ensemble_tree_test_predictions <- plogis(as.numeric(ensemble_tree_test_pred))
  ensemble_tree_test_predictions_binomial <- rbinom(n = length(ensemble_tree_test_predictions), size = 1, prob = ensemble_tree_test_predictions)
  ensemble_tree_test_table <- table(ensemble_tree_test_predictions_binomial, ensemble_y_test)
  ensemble_tree_test_true_positive_rate[i] <- ensemble_tree_test_table[2, 2] / sum(ensemble_tree_test_table[2, 2] + ensemble_tree_test_table[1, 2])
  ensemble_tree_test_true_positive_rate_mean <- mean(ensemble_tree_test_true_positive_rate)
  ensemble_tree_test_true_negative_rate[i] <- ensemble_tree_test_table[1, 1] / sum(ensemble_tree_test_table[1, 1] + ensemble_tree_test_table[2, 1])
  ensemble_tree_test_true_negative_rate_mean <- mean(ensemble_tree_test_true_negative_rate)
  ensemble_tree_test_false_positive_rate[i] <- ensemble_tree_test_table[2, 1] / sum(ensemble_tree_test_table[2, 1] + ensemble_tree_test_table[1, 1])
  ensemble_tree_test_false_positive_rate_mean <- mean(ensemble_tree_test_false_positive_rate)
  ensemble_tree_test_false_negative_rate[i] <- ensemble_tree_test_table[1, 2] / sum(ensemble_tree_test_table[1, 2] + ensemble_tree_test_table[2, 2])
  ensemble_tree_test_false_negative_rate_mean <- mean(ensemble_tree_test_false_negative_rate)
  ensemble_tree_test_accuracy[i] <- (ensemble_tree_test_table[1, 1] + ensemble_tree_test_table[2, 2]) / sum(ensemble_tree_test_table)
  ensemble_tree_test_accuracy_mean <- mean(ensemble_tree_test_accuracy)
  ensemble_tree_test_F1_score[i] <- 2 * (ensemble_tree_test_table[2, 2]) / sum(2 * ensemble_tree_test_table[2, 2] + ensemble_tree_test_table[1, 2] + ensemble_tree_test_table[2, 1])
  ensemble_tree_test_F1_score_mean <- mean(ensemble_tree_test_F1_score)
  ensemble_tree_test_positive_predictive_value[i] <- ensemble_tree_test_table[2, 2] / sum(ensemble_tree_test_table[2, 2] + ensemble_tree_test_table[2, 1])
  ensemble_tree_test_positive_predictive_value_mean <- mean(ensemble_tree_test_positive_predictive_value)
  ensemble_tree_test_negative_predictive_value[i] <- ensemble_tree_test_table[1, 1] / sum(ensemble_tree_test_table[1, 1] + ensemble_tree_test_table[1, 2])
  ensemble_tree_test_negative_predictive_value_mean <- mean(ensemble_tree_test_negative_predictive_value)

  ensemble_tree_validation_pred <- stats::predict(ensemble_tree_train_fit, ensemble_validation, type = "vector")
  ensemble_tree_validation_predictions <- plogis(as.numeric(ensemble_tree_validation_pred))
  ensemble_tree_validation_predictions_binomial <- rbinom(n = length(ensemble_tree_validation_predictions), size = 1, prob = ensemble_tree_validation_predictions)
  ensemble_tree_validation_table <- table(ensemble_tree_validation_predictions_binomial, ensemble_y_validation)
  ensemble_tree_validation_true_positive_rate[i] <- ensemble_tree_validation_table[2, 2] / sum(ensemble_tree_validation_table[2, 2] + ensemble_tree_validation_table[1, 2])
  ensemble_tree_validation_true_positive_rate_mean <- mean(ensemble_tree_validation_true_positive_rate)
  ensemble_tree_validation_true_negative_rate[i] <- ensemble_tree_validation_table[1, 1] / sum(ensemble_tree_validation_table[1, 1] + ensemble_tree_validation_table[2, 1])
  ensemble_tree_validation_true_negative_rate_mean <- mean(ensemble_tree_validation_true_negative_rate)
  ensemble_tree_validation_false_positive_rate[i] <- ensemble_tree_validation_table[2, 1] / sum(ensemble_tree_validation_table[2, 1] + ensemble_tree_validation_table[1, 1])
  ensemble_tree_validation_false_positive_rate_mean <- mean(ensemble_tree_validation_false_positive_rate)
  ensemble_tree_validation_false_negative_rate[i] <- ensemble_tree_validation_table[1, 2] / sum(ensemble_tree_validation_table[1, 2] + ensemble_tree_validation_table[2, 2])
  ensemble_tree_validation_false_negative_rate_mean <- mean(ensemble_tree_validation_false_negative_rate)
  ensemble_tree_validation_accuracy[i] <- (ensemble_tree_validation_table[1, 1] + ensemble_tree_validation_table[2, 2]) / sum(ensemble_tree_validation_table)
  ensemble_tree_validation_accuracy_mean <- mean(ensemble_tree_validation_accuracy)
  ensemble_tree_validation_F1_score[i] <- 2 * (ensemble_tree_validation_table[2, 2]) / sum(2 * ensemble_tree_validation_table[2, 2] + ensemble_tree_validation_table[1, 2] + ensemble_tree_validation_table[2, 1])
  ensemble_tree_validation_F1_score_mean <- mean(ensemble_tree_validation_F1_score)
  ensemble_tree_validation_positive_predictive_value[i] <- ensemble_tree_validation_table[2, 2] / sum(ensemble_tree_validation_table[2, 2] + ensemble_tree_validation_table[2, 1])
  ensemble_tree_validation_positive_predictive_value_mean <- mean(ensemble_tree_validation_positive_predictive_value)
  ensemble_tree_validation_negative_predictive_value[i] <- ensemble_tree_validation_table[1, 1] / sum(ensemble_tree_validation_table[1, 1] + ensemble_tree_validation_table[1, 2])
  ensemble_tree_validation_negative_predictive_value_mean <- mean(ensemble_tree_validation_negative_predictive_value)

  ensemble_tree_holdout_true_positive_rate[i] <- (ensemble_tree_test_true_positive_rate[i] + ensemble_tree_validation_true_positive_rate[i]) / 2
  ensemble_tree_holdout_true_positive_rate_mean <- mean(ensemble_tree_holdout_true_positive_rate)
  ensemble_tree_holdout_true_negative_rate[i] <- (ensemble_tree_test_true_negative_rate[i] + ensemble_tree_validation_true_negative_rate[i]) / 2
  ensemble_tree_holdout_true_negative_rate_mean <- mean(ensemble_tree_holdout_true_negative_rate)
  ensemble_tree_holdout_false_positive_rate[i] <- (ensemble_tree_test_false_positive_rate[i] + ensemble_tree_validation_false_positive_rate[i]) / 2
  ensemble_tree_holdout_false_positive_rate_mean <- mean(ensemble_tree_holdout_false_positive_rate)
  ensemble_tree_holdout_false_negative_rate[i] <- (ensemble_tree_test_false_negative_rate[i] + ensemble_tree_validation_false_negative_rate[i]) / 2
  ensemble_tree_holdout_false_negative_rate_mean <- mean(ensemble_tree_holdout_false_negative_rate)
  ensemble_tree_holdout_accuracy[i] <- (ensemble_tree_test_accuracy[i] + ensemble_tree_validation_accuracy[i]) / 2
  ensemble_tree_holdout_accuracy_mean <- mean(ensemble_tree_holdout_accuracy)
  ensemble_tree_holdout_accuracy_sd <- sd(ensemble_tree_holdout_accuracy)
  ensemble_tree_holdout_F1_score[i] <- (ensemble_tree_test_F1_score[i] + ensemble_tree_validation_F1_score[i]) / 2
  ensemble_tree_holdout_F1_score_mean <- mean(ensemble_tree_holdout_F1_score)
  ensemble_tree_holdout_positive_predictive_value[i] <- (ensemble_tree_test_positive_predictive_value[i] + ensemble_tree_validation_positive_predictive_value[i]) / 2
  ensemble_tree_holdout_positive_predictive_value_mean <- mean(ensemble_tree_holdout_positive_predictive_value)
  ensemble_tree_holdout_negative_predictive_value[i] <- (ensemble_tree_test_negative_predictive_value[i] + ensemble_tree_validation_negative_predictive_value[i]) / 2
  ensemble_tree_holdout_negative_predictive_value_mean <- mean(ensemble_tree_holdout_negative_predictive_value)
  ensemble_tree_holdout_overfitting[i] <- ensemble_tree_holdout_accuracy[i] / ensemble_tree_train_accuracy[i]
  ensemble_tree_holdout_overfitting_mean <- mean(ensemble_tree_holdout_overfitting)
  ensemble_tree_holdout_overfitting_range <- range(ensemble_tree_holdout_overfitting)
  ensemble_tree_holdout_overfitting_sd <- sd(ensemble_tree_holdout_overfitting)

  ensemble_tree_table <- ensemble_tree_test_table + ensemble_tree_validation_table
  ensemble_tree_table_total <- ensemble_tree_table_total + ensemble_tree_table

  ensemble_tree_end <- Sys.time()
  ensemble_tree_duration[i] <- ensemble_tree_end - ensemble_tree_start
  ensemble_tree_duration_mean <- mean(ensemble_tree_duration)
  ensemble_tree_duration_sd <- sd(ensemble_tree_duration)


  #### Ensemble Using XGBoost ####
  ensemble_tree_start <- Sys.time()

  #### XGBoost ####
  ensemble_xgb_start <- Sys.time()

  xgb_start <- Sys.time()
  ensemble_train_x <- data.matrix(ensemble_train[, -ncol(ensemble_train)])
  ensemble_train_y <- ensemble_train[, ncol(ensemble_train)]

  # define predictor and response variables in test set
  ensemble_test_x <- data.matrix(ensemble_test[, -ncol(ensemble_test)])
  ensemble_test_y <- ensemble_test[, ncol(ensemble_test)]

  # define predictor and response variables in validation set
  ensemble_validation_x <- data.matrix(ensemble_validation[, -ncol(ensemble_validation)])
  ensemble_validation_y <- ensemble_validation[, ncol(ensemble_validation)]

  # define final train, test and validation sets
  ensemble_xgb_train <- xgboost::xgb.DMatrix(data = ensemble_train_x, label = ensemble_train_y)
  ensemble_xgb_test <- xgboost::xgb.DMatrix(data = ensemble_test_x, label = ensemble_test_y)
  ensemble_xgb_validation <- xgboost::xgb.DMatrix(data = ensemble_validation_x, label = ensemble_validation_y)

  # define watchlist
  ensemble_watchlist <- list(train = ensemble_xgb_train, validation = ensemble_xgb_validation)
  ensemble_watchlist_test <- list(train = ensemble_xgb_train, test = ensemble_xgb_test)
  ensemble_watchlist_validation <- list(train = ensemble_xgb_train, validation = ensemble_xgb_validation)

  if(set_seed == "Y"){
    set.seed(seed = seed)
    ensemble_xgb_model <- xgboost::xgb.train(data = ensemble_xgb_train, max.depth = 3, watchlist = ensemble_watchlist_test, nrounds = 70)

    ensemble_xgboost_min <- which.min(ensemble_xgb_model$evaluation_log$validation_rmse)
  }
  if(set_seed == "N"){
    ensemble_xgb_model <- xgboost::xgb.train(data = ensemble_xgb_train, max.depth = 3, watchlist = ensemble_watchlist_test, nrounds = 70)

    ensemble_xgboost_min <- which.min(ensemble_xgb_model$evaluation_log$validation_rmse)
  }
  ensemble_xgb_train_pred <- stats::predict(ensemble_xgb_model, ensemble_train_x, type = "response")
  ensemble_xgb_train_predictions <- plogis(as.numeric(ensemble_xgb_train_pred))
  ensemble_xgb_train_predictions_binomial <- rbinom(n = length(ensemble_xgb_train_predictions), size = 1, prob = ensemble_xgb_train_predictions)
  ensemble_xgb_train_table <- table(ensemble_xgb_train_predictions_binomial, ensemble_y_train)
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

  ensemble_xgb_test_pred <- stats::predict(ensemble_xgb_model, ensemble_test_x, type = "response")
  ensemble_xgb_test_predictions <- plogis(as.numeric(ensemble_xgb_test_pred))
  ensemble_xgb_test_predictions_binomial <- rbinom(n = length(ensemble_xgb_test_predictions), size = 1, prob = ensemble_xgb_test_predictions)
  ensemble_xgb_test_table <- table(ensemble_xgb_test_predictions_binomial, ensemble_y_test)
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

  ensemble_xgb_validation_pred <- stats::predict(ensemble_xgb_model, ensemble_validation_x, type = "response")
  ensemble_xgb_validation_predictions <- plogis(as.numeric(ensemble_xgb_validation_pred))
  ensemble_xgb_validation_predictions_binomial <- rbinom(n = length(ensemble_xgb_validation_predictions), size = 1, prob = ensemble_xgb_validation_predictions)
  ensemble_xgb_validation_table <- table(ensemble_xgb_validation_predictions_binomial, ensemble_y_validation)
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

  ensemble_xgb_table <- ensemble_xgb_test_table + ensemble_xgb_validation_table
  ensemble_xgb_table_total <- ensemble_xgb_table_total + ensemble_xgb_table

  ensemble_xgb_end <- Sys.time()
  ensemble_xgb_duration[i] <- ensemble_xgb_end - ensemble_xgb_start
  ensemble_xgb_duration_mean <- mean(ensemble_xgb_duration)
  ensemble_xgb_duration_sd <- sd(ensemble_xgb_duration)
}

##################################

## ROC Curves start here #########

##################################

cubist_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(cubist_test_predictions_binomial, cubist_validation_predictions_binomial)))
cubist_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(cubist_test_predictions_binomial, cubist_validation_predictions_binomial)) - 1)), 4)
cubist_ROC <- pROC::ggroc(cubist_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Cubist ", "(AUC = ", cubist_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

fda_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(fda_test_predictions_binomial, fda_validation_predictions_binomial)))
fda_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(fda_test_predictions_binomial, fda_validation_predictions_binomial)) - 1)), 4)
fda_ROC <- pROC::ggroc(fda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Flexible Discriminant Analysis ", "(AUC = ", fda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

gam_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(gam_test_predictions_binomial, gam_validation_predictions_binomial)))
gam_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(gam_test_predictions_binomial, gam_validation_predictions_binomial)) - 1)), 4)
gam_ROC <- pROC::ggroc(gam_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Generalized Additve Models ", "(AUC = ", gam_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

glm_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), c(glm_test_predictions_binomial, glm_validation_predictions_binomial))
glm_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(glm_test_predictions_binomial, glm_validation_predictions_binomial)) - 1)), 4)
glm_ROC <- pROC::ggroc(glm_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Generalized Linear Models ", "(AUC = ", glm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

linear_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(linear_test_predictions_binomial, linear_validation_predictions_binomial)))
linear_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(linear_test_pred, linear_validation_pred)))), 4)
linear_ROC <- pROC::ggroc(linear_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Linear Models ROC ", "(AUC = ", linear_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

lda_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(lda_test_pred$class, lda_validation_pred$class)))
lda_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(lda_test_pred$class, lda_validation_pred$class)) - 1)), 4)
lda_ROC <- pROC::ggroc(lda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Linear Discriminant Analysis ", "(AUC = ", lda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

pda_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(pda_test_predictions_binomial, pda_validation_predictions_binomial)))
pda_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(pda_test_predictions_binomial, pda_validation_predictions_binomial)) - 1)), 4)
pda_ROC <- pROC::ggroc(pda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Penalized Discriiminant Analysis ", "(AUC = ", pda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

qda_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(qda_test_pred$class, qda_validation_pred$class)))
qda_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(qda_test_pred$class, qda_validation_pred$class)) - 1)), 4)
qda_ROC <- pROC::ggroc(qda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Quadratic Discriminant Analysis ", "(AUC = ", qda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

rf_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(rf_test_pred, rf_validation_pred)))
rf_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(rf_test_pred, rf_validation_pred)) - 1)), 4)
rf_ROC <- pROC::ggroc(rf_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Quadratic Discriminant Analysis ", "(AUC = ", rf_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

svm_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(svm_test_predictions_binomial, svm_validation_predictions_binomial)))
svm_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(svm_test_predictions_binomial, svm_validation_predictions_binomial)) - 1)), 4)
svm_ROC <- pROC::ggroc(svm_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Support Vector Machines ", "(AUC = ", svm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

tree_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(tree_test_pred, tree_validation_pred)))
tree_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(tree_test_predictions_binomial, tree_validation_predictions_binomial)) - 1)), 4)
tree_ROC <- pROC::ggroc(tree_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Trees ", "(AUC = ", tree_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_bagging_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_bagging_test_predictions_binomial, ensemble_bagging_validation_predictions_binomial)))
ensemble_bagging_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_bagging_test_predictions_binomial, ensemble_bagging_validation_predictions_binomial)) - 1)), 4)
ensemble_bagging_ROC <- pROC::ggroc(ensemble_bagging_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble Bagging ", "(AUC = ", ensemble_bagging_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_C50_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_C50_test_predictions_binomial, ensemble_C50_validation_predictions_binomial)))
ensemble_C50_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_C50_test_predictions_binomial, ensemble_C50_validation_predictions_binomial)) - 1)), 4)
ensemble_C50_ROC <- pROC::ggroc(ensemble_C50_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble C50 ", "(AUC = ", ensemble_C50_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_gb_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_gb_test_predictions_binomial, ensemble_gb_validation_predictions_binomial)))
ensemble_gb_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_gb_test_predictions_binomial, ensemble_gb_validation_predictions_binomial)) - 1)), 4)
ensemble_gb_ROC <- pROC::ggroc(ensemble_gb_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble gb ", "(AUC = ", ensemble_gb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_pls_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_pls_test_predictions_binomial, ensemble_pls_validation_predictions_binomial)))
ensemble_pls_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_pls_test_predictions_binomial, ensemble_pls_validation_predictions_binomial)) - 1)), 4)
ensemble_pls_ROC <- pROC::ggroc(ensemble_pls_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble pls ", "(AUC = ", ensemble_pls_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_pda_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_pda_test_predictions_binomial, ensemble_pda_validation_predictions_binomial)))
ensemble_pda_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_pda_test_predictions_binomial, ensemble_pda_validation_predictions_binomial)) - 1)), 4)
ensemble_pda_ROC <- pROC::ggroc(ensemble_pda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble pda ", "(AUC = ", ensemble_pda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_rpart_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_rpart_test_predictions_binomial, ensemble_rpart_validation_predictions_binomial)))
ensemble_rpart_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_rpart_test_predictions_binomial, ensemble_rpart_validation_predictions_binomial)) - 1)), 4)
ensemble_rpart_ROC <- pROC::ggroc(ensemble_rpart_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble rpart ", "(AUC = ", ensemble_rpart_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_svm_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_svm_test_predictions_binomial, ensemble_svm_validation_predictions_binomial)))
ensemble_svm_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_svm_test_predictions_binomial, ensemble_svm_validation_predictions_binomial)) - 1)), 4)
ensemble_svm_ROC <- pROC::ggroc(ensemble_svm_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble svm ", "(AUC = ", ensemble_svm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_tree_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_tree_test_predictions_binomial, ensemble_tree_validation_predictions_binomial)))
ensemble_tree_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_tree_test_predictions_binomial, ensemble_tree_validation_predictions_binomial)) - 1)), 4)
ensemble_tree_ROC <- pROC::ggroc(ensemble_tree_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble tree ", "(AUC = ", ensemble_tree_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_xgb_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_xgb_test_predictions_binomial, ensemble_xgb_validation_predictions_binomial)))
ensemble_xgb_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_xgb_test_predictions_binomial, ensemble_xgb_validation_predictions_binomial)) - 1)), 4)
ensemble_xgb_ROC <- pROC::ggroc(ensemble_xgb_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble xgb ", "(AUC = ", ensemble_xgb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")


ROC_curves <- gridExtra::grid.arrange(cubist_ROC, fda_ROC, gam_ROC,
                                      glm_ROC, linear_ROC,
                                      lda_ROC,
                                      pda_ROC, qda_ROC,
                                      rf_ROC,
                                      svm_ROC, tree_ROC,
                                      ensemble_bagging_ROC,
                                      ensemble_C50_ROC,
                                      ensemble_gb_ROC,
                                      ensemble_pls_ROC,
                                      ensemble_pda_ROC,
                                      ensemble_rpart_ROC,
                                      ensemble_svm_ROC,
                                      ensemble_tree_ROC, ensemble_xgb_ROC,
                                      ncol = 4
)

ROC_curves <- ggplotify::as.ggplot(ROC_curves)

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("ROC_curves.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("ROC_curves.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("ROC_curves.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("ROC_curves.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("ROC_curves.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("ROC_curves.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


holdout_results <- data.frame(
  Model = c(
    "Cubist", "Flexible Discriminant Analysis",
    "Generalized Additive Models", "Generalized Linear Models", "Linear Model",
    "Linear Discrmininant Analysis",
    "Penalized Discriminant Analysis", "Quadratic Discriminant Analysis",
    "Random Forest", "Support Vector Machines", "Trees",
    "Ensemble Bagging", "Ensemble C50", "Ensemble Gradient Boosted",
    "Ensemble Partial Least Squares", "Ensemble Penalized Discrmininat Analysis",
    "Ensemble RPart", "Ensemble Support Vector Machines",
    "Ensemble Trees", "Ensemble XGBoost"
  ),
  "Accuracy" = round(c(
    cubist_holdout_accuracy_mean, fda_holdout_accuracy_mean,
    gam_holdout_accuracy_mean, glm_holdout_accuracy_mean, linear_holdout_accuracy_mean,
    lda_holdout_accuracy_mean,
    pda_holdout_accuracy_mean, qda_holdout_accuracy_mean,
    rf_holdout_accuracy_mean,
    svm_holdout_accuracy_mean, tree_holdout_accuracy_mean,
    ensemble_bagging_holdout_accuracy_mean, ensemble_C50_holdout_accuracy_mean,
    ensemble_gb_holdout_accuracy_mean, ensemble_pls_holdout_accuracy_mean, ensemble_pda_holdout_accuracy_mean,
    ensemble_rpart_holdout_accuracy_mean,
    ensemble_svm_holdout_accuracy_mean, ensemble_tree_holdout_accuracy_mean, ensemble_xgb_holdout_accuracy_mean
  ), 4),
  "Accuracy_sd" = round(c(
    cubist_holdout_accuracy_sd, fda_holdout_accuracy_sd,
    gam_holdout_accuracy_sd, glm_holdout_accuracy_sd, linear_holdout_accuracy_sd,
    lda_holdout_accuracy_sd,
    pda_holdout_accuracy_sd, qda_holdout_accuracy_sd,
    rf_holdout_accuracy_sd,
    svm_holdout_accuracy_sd, tree_holdout_accuracy_sd,
    ensemble_bagging_holdout_accuracy_sd, ensemble_C50_holdout_accuracy_sd,
    ensemble_gb_holdout_accuracy_sd, ensemble_pls_holdout_accuracy_sd, ensemble_pda_holdout_accuracy_sd,
    ensemble_rpart_holdout_accuracy_sd,
    ensemble_svm_holdout_accuracy_sd, ensemble_tree_holdout_accuracy_sd, ensemble_xgb_holdout_accuracy_sd
  ), 4),
  "True_Positive_Rate_aka_Sensitivity" = round(c(
    cubist_holdout_true_positive_rate_mean, fda_holdout_true_positive_rate_mean,
    gam_holdout_true_positive_rate_mean, glm_holdout_true_positive_rate_mean, linear_holdout_true_positive_rate_mean,
    lda_holdout_true_positive_rate_mean,
    pda_holdout_true_positive_rate_mean, qda_holdout_true_positive_rate_mean,
    rf_holdout_true_positive_rate_mean,
    svm_holdout_true_positive_rate_mean, tree_holdout_true_positive_rate_mean,
    ensemble_bagging_holdout_true_positive_rate_mean, ensemble_C50_holdout_true_positive_rate_mean,
    ensemble_gb_holdout_true_positive_rate_mean, ensemble_pls_holdout_true_positive_rate_mean, ensemble_pda_holdout_true_positive_rate_mean,
    ensemble_rpart_holdout_true_positive_rate_mean,
    ensemble_svm_holdout_true_positive_rate_mean, ensemble_tree_holdout_true_positive_rate_mean, ensemble_xgb_holdout_true_positive_rate_mean
  ), 4),
  "True_Negative_Rate_aka_Specificity" = round(c(
    cubist_holdout_true_negative_rate_mean, fda_holdout_true_negative_rate_mean,
    gam_holdout_true_negative_rate_mean, glm_holdout_true_negative_rate_mean, linear_holdout_true_negative_rate_mean,
    lda_holdout_true_negative_rate_mean,
    pda_holdout_true_negative_rate_mean, qda_holdout_true_negative_rate_mean,
    rf_holdout_true_negative_rate_mean,
    svm_holdout_true_negative_rate_mean, tree_holdout_true_negative_rate_mean,
    ensemble_bagging_holdout_true_negative_rate_mean, ensemble_C50_holdout_true_negative_rate_mean,
    ensemble_gb_holdout_true_negative_rate_mean, ensemble_pls_holdout_true_negative_rate_mean, ensemble_pda_holdout_true_negative_rate_mean,
    ensemble_rpart_holdout_true_negative_rate_mean,
    ensemble_svm_holdout_true_negative_rate_mean, ensemble_tree_holdout_true_negative_rate_mean, ensemble_xgb_holdout_true_negative_rate_mean
  ), 4),
  "False_Positive_Rate_aka_Type_I_Error" = round(c(
    cubist_holdout_false_positive_rate_mean, fda_holdout_false_positive_rate_mean,
    gam_holdout_false_positive_rate_mean, glm_holdout_false_positive_rate_mean, linear_holdout_false_positive_rate_mean,
    lda_holdout_false_positive_rate_mean,
    pda_holdout_false_positive_rate_mean, qda_holdout_false_positive_rate_mean,
    rf_holdout_false_positive_rate_mean,
    svm_holdout_false_positive_rate_mean, tree_holdout_false_positive_rate_mean,
    ensemble_bagging_holdout_false_positive_rate_mean, ensemble_C50_holdout_false_positive_rate_mean,
    ensemble_gb_holdout_false_positive_rate_mean, ensemble_pls_holdout_false_positive_rate_mean, ensemble_pda_holdout_false_positive_rate_mean,
    ensemble_rpart_holdout_false_positive_rate_mean,
    ensemble_svm_holdout_false_positive_rate_mean, ensemble_tree_holdout_false_positive_rate_mean, ensemble_xgb_holdout_false_positive_rate_mean
  ), 4),
  "False_Negative_Rate_aka_Type_II_Error" = round(c(
    cubist_holdout_false_negative_rate_mean, fda_holdout_false_negative_rate_mean,
    gam_holdout_false_negative_rate_mean, glm_holdout_false_negative_rate_mean, linear_holdout_false_negative_rate_mean,
    lda_holdout_false_negative_rate_mean,
    pda_holdout_false_negative_rate_mean, qda_holdout_false_negative_rate_mean,
    rf_holdout_false_negative_rate_mean,
    svm_holdout_false_negative_rate_mean, tree_holdout_false_negative_rate_mean,
    ensemble_bagging_holdout_false_negative_rate_mean, ensemble_C50_holdout_false_negative_rate_mean,
    ensemble_gb_holdout_false_negative_rate_mean, ensemble_pls_holdout_false_negative_rate_mean, ensemble_pda_holdout_false_negative_rate_mean,
    ensemble_rpart_holdout_false_negative_rate_mean,
    ensemble_svm_holdout_false_negative_rate_mean, ensemble_tree_holdout_false_negative_rate_mean, ensemble_xgb_holdout_false_negative_rate_mean
  ), 4),
  "Positive_Predictive_Value_aka_Precision" = round(c(
    cubist_holdout_positive_predictive_value_mean, fda_holdout_positive_predictive_value_mean,
    gam_holdout_positive_predictive_value_mean, glm_holdout_positive_predictive_value_mean, linear_holdout_positive_predictive_value_mean,
    lda_holdout_positive_predictive_value_mean,
    pda_holdout_positive_predictive_value_mean, qda_holdout_positive_predictive_value_mean,
    rf_holdout_positive_predictive_value_mean,
    svm_holdout_positive_predictive_value_mean, tree_holdout_positive_predictive_value_mean,
    ensemble_bagging_holdout_positive_predictive_value_mean, ensemble_C50_holdout_positive_predictive_value_mean,
    ensemble_gb_holdout_positive_predictive_value_mean, ensemble_pls_holdout_positive_predictive_value_mean, ensemble_pda_holdout_positive_predictive_value_mean,
    ensemble_rpart_holdout_positive_predictive_value_mean,
    ensemble_svm_holdout_positive_predictive_value_mean, ensemble_tree_holdout_positive_predictive_value_mean, ensemble_xgb_holdout_positive_predictive_value_mean
  ), 4),
  "Negative_Predictive_Value" = round(c(
    cubist_holdout_negative_predictive_value_mean, fda_holdout_negative_predictive_value_mean,
    gam_holdout_negative_predictive_value_mean, glm_holdout_negative_predictive_value_mean, linear_holdout_negative_predictive_value_mean,
    lda_holdout_negative_predictive_value_mean,
    pda_holdout_negative_predictive_value_mean, qda_holdout_negative_predictive_value_mean,
    rf_holdout_negative_predictive_value_mean,
    svm_holdout_negative_predictive_value_mean, tree_holdout_negative_predictive_value_mean,
    ensemble_bagging_holdout_negative_predictive_value_mean, ensemble_C50_holdout_negative_predictive_value_mean,
    ensemble_gb_holdout_negative_predictive_value_mean, ensemble_pls_holdout_negative_predictive_value_mean, ensemble_pda_holdout_negative_predictive_value_mean,
    ensemble_rpart_holdout_negative_predictive_value_mean,
    ensemble_svm_holdout_negative_predictive_value_mean, ensemble_tree_holdout_negative_predictive_value_mean, ensemble_xgb_holdout_negative_predictive_value_mean
  ), 4),
  "F1_Score" = round(c(
    cubist_holdout_F1_score_mean, fda_holdout_F1_score_mean,
    gam_holdout_F1_score_mean, glm_holdout_F1_score_mean, linear_holdout_F1_score_mean,
    lda_holdout_F1_score_mean,
    pda_holdout_F1_score_mean, qda_holdout_F1_score_mean,
    pda_holdout_F1_score_mean,
    svm_holdout_F1_score_mean, tree_holdout_F1_score_mean,
    ensemble_bagging_holdout_F1_score_mean, ensemble_C50_holdout_F1_score_mean,
    ensemble_gb_holdout_F1_score_mean, ensemble_pls_holdout_F1_score_mean, ensemble_pda_holdout_F1_score_mean,
    ensemble_rpart_holdout_F1_score_mean,
    ensemble_svm_holdout_F1_score_mean, ensemble_tree_holdout_F1_score_mean, ensemble_xgb_holdout_F1_score_mean
  ), 4),
  "Area_Under_Curve" = c(
    cubist_auc, fda_auc,
    gam_auc, glm_auc, linear_auc,
    lda_auc,
    pda_auc, qda_auc,
    rf_auc,
    svm_auc, tree_auc,
    ensemble_bagging_auc, ensemble_C50_auc,
    ensemble_gb_auc, ensemble_pls_auc, ensemble_pda_auc,
    ensemble_rpart_auc,
    ensemble_svm_auc, ensemble_tree_auc, ensemble_xgb_auc
  ),
  "Overfitting_Mean" = round(c(
    cubist_holdout_overfitting_mean, fda_holdout_overfitting_mean,
    gam_holdout_overfitting_mean, glm_holdout_overfitting_mean, linear_holdout_overfitting_mean,
    lda_holdout_overfitting_mean,
    pda_holdout_overfitting_mean, qda_holdout_overfitting_mean,
    rf_holdout_overfitting_mean,
    svm_holdout_overfitting_mean, tree_holdout_overfitting_mean,
    ensemble_bagging_holdout_overfitting_mean, ensemble_C50_holdout_overfitting_mean,
    ensemble_gb_holdout_overfitting_mean, ensemble_pls_holdout_overfitting_mean, ensemble_pda_holdout_overfitting_mean,
    ensemble_rpart_holdout_overfitting_mean,
    ensemble_svm_holdout_overfitting_mean, ensemble_tree_holdout_overfitting_mean, ensemble_xgb_holdout_overfitting_mean
  ), 4),
  "Overfitting_sd" = round(c(
    cubist_holdout_overfitting_sd, fda_holdout_overfitting_sd,
    gam_holdout_overfitting_sd, glm_holdout_overfitting_sd, linear_holdout_overfitting_sd,
    lda_holdout_overfitting_sd,
    pda_holdout_overfitting_sd, qda_holdout_overfitting_sd,
    rf_holdout_overfitting_sd,
    svm_holdout_overfitting_sd, tree_holdout_overfitting_sd,
    ensemble_bagging_holdout_overfitting_sd, ensemble_C50_holdout_overfitting_sd,
    ensemble_gb_holdout_overfitting_sd, ensemble_pls_holdout_overfitting_sd, ensemble_pda_holdout_overfitting_sd,
    ensemble_rpart_holdout_overfitting_sd,
    ensemble_svm_holdout_overfitting_sd, ensemble_tree_holdout_overfitting_sd, ensemble_xgb_holdout_overfitting_sd
  ), 4),
  "Duration" = round(c(
    cubist_duration_mean, fda_duration_mean,
    gam_duration_mean, glm_duration_mean, linear_duration_mean,
    lda_duration_mean,
    pda_duration_mean, qda_duration_mean,
    rf_duration_mean,
    svm_duration_mean, tree_duration_mean,
    ensemble_bagging_duration_mean, ensemble_C50_duration_mean,
    ensemble_gb_duration_mean, ensemble_pls_duration_mean, ensemble_pda_duration_mean,
    ensemble_rpart_duration_mean,
    ensemble_svm_duration_mean, ensemble_tree_duration_mean, ensemble_xgb_duration_mean
  ), 4),
  "Duration_sd" = round(c(
    cubist_duration_sd, fda_duration_sd,
    gam_duration_sd, glm_duration_sd, linear_duration_sd,
    lda_duration_sd,
    pda_duration_sd, qda_duration_sd,
    rf_duration_sd,
    svm_duration_sd, tree_duration_sd,
    ensemble_bagging_duration_sd, ensemble_C50_duration_sd,
    ensemble_gb_duration_sd, ensemble_pls_duration_sd, ensemble_pda_duration_sd,
    ensemble_rpart_duration_sd,
    ensemble_svm_duration_sd, ensemble_tree_duration_sd, ensemble_xgb_duration_sd
  ), 4)
)

holdout_results <- holdout_results %>% dplyr::arrange(dplyr::desc(Accuracy))

holdout_results_final <- reactable::reactable(holdout_results,
                                              searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                              striped = TRUE, highlight = TRUE, resizable = TRUE
) %>%
  reactablefmtr::add_title("Mean of Holdout results")


accuracy_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_accuracy, fda_holdout_accuracy, gam_holdout_accuracy,
    glm_holdout_accuracy,
    linear_holdout_accuracy, lda_holdout_accuracy,
    pda_holdout_accuracy,
    qda_holdout_accuracy,
    rf_holdout_accuracy,
    tree_holdout_accuracy, svm_holdout_accuracy,
    ensemble_bagging_holdout_accuracy,
    ensemble_C50_holdout_accuracy, ensemble_gb_holdout_accuracy,
    ensemble_pls_holdout_accuracy,
    ensemble_pda_holdout_accuracy,
    ensemble_rpart_holdout_accuracy, ensemble_svm_holdout_accuracy,
    ensemble_tree_holdout_accuracy, ensemble_xgb_holdout_accuracy
  ),
  "mean" = rep(c(
    cubist_holdout_accuracy_mean, fda_holdout_accuracy_mean, gam_holdout_accuracy_mean,
    glm_holdout_accuracy_mean,
    linear_holdout_accuracy_mean, lda_holdout_accuracy_mean,
    pda_holdout_accuracy_mean,
    qda_holdout_accuracy_mean,
    rf_holdout_accuracy_mean,
    svm_holdout_accuracy_mean,
    tree_holdout_accuracy_mean,
    ensemble_bagging_holdout_accuracy_mean,
    ensemble_C50_holdout_accuracy_mean, ensemble_gb_holdout_accuracy_mean,
    ensemble_pls_holdout_accuracy_mean,
    ensemble_pda_holdout_accuracy_mean,
    ensemble_rpart_holdout_accuracy_mean, ensemble_svm_holdout_accuracy_mean,
    ensemble_tree_holdout_accuracy_mean, ensemble_xgb_holdout_accuracy_mean
  ), each = numresamples)
)

accuracy_plot <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4 , scales = "fixed") +
  ggplot2::ggtitle("Accuracy by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Accuracy by model fixed scales, higher is better \n The black horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

accuracy_plot2 <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("Accuracy by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Accuracy by model free scales, higher is better \n The black horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

total_accuracy_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples),
    rep("Linear", numresamples), rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples),
    rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "train" = c(
    cubist_train_accuracy, fda_train_accuracy, gam_train_accuracy,
    glm_train_accuracy,
    linear_train_accuracy, lda_train_accuracy,
    pda_train_accuracy,
    qda_train_accuracy,
    rf_train_accuracy,
    svm_train_accuracy,
    tree_train_accuracy,
    ensemble_bagging_train_accuracy,
    ensemble_C50_train_accuracy, ensemble_gb_train_accuracy,
    ensemble_pls_train_accuracy,
    ensemble_pda_train_accuracy,
    ensemble_rpart_train_accuracy, ensemble_svm_train_accuracy,
    ensemble_tree_train_accuracy, ensemble_xgb_train_accuracy
  ),
  "holdout" = c(
    cubist_holdout_accuracy, fda_holdout_accuracy, gam_holdout_accuracy,
    glm_holdout_accuracy,
    linear_holdout_accuracy, lda_holdout_accuracy,
    pda_holdout_accuracy,
    qda_holdout_accuracy,
    rf_holdout_accuracy,
    svm_holdout_accuracy,
    tree_holdout_accuracy,
    ensemble_bagging_holdout_accuracy,
    ensemble_C50_holdout_accuracy, ensemble_gb_holdout_accuracy,
    ensemble_pls_holdout_accuracy,
    ensemble_pda_holdout_accuracy,
    ensemble_rpart_holdout_accuracy, ensemble_svm_holdout_accuracy,
    ensemble_tree_holdout_accuracy, ensemble_xgb_holdout_accuracy
  )
)

total_plot <- ggplot2::ggplot(data = total_accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("Accuracy data including train and holdout results, by model and resample. The best possible result is 1.0") +
  ggplot2::labs(y = "Accuracy, the best possible result is 1.0") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("train", "holdout"),
    values = c(
      "train" = "red", "holdout" = "black"
    )
  )

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("total_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("total_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("total_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("total_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("total_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("total_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

total_plot2 <- ggplot2::ggplot(data = total_accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("Accuracy data including train and holdout results, by model and resample. The best possible result is 1.0") +
  ggplot2::labs(y = "Accuracy, the best possible result is 1.0") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("train", "holdout"),
    values = c(
      "train" = "red", "holdout" = "black"
    )
  )

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("total_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("total_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("total_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("total_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("total_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("total_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

true_positive_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_true_positive_rate, fda_holdout_true_positive_rate, gam_holdout_true_positive_rate,
    glm_holdout_true_positive_rate,
    linear_holdout_true_positive_rate, lda_holdout_true_positive_rate,
    pda_holdout_true_positive_rate,
    qda_holdout_true_positive_rate,
    rf_holdout_true_positive_rate,
    svm_holdout_true_positive_rate,
    tree_holdout_true_positive_rate,
    ensemble_bagging_holdout_true_positive_rate,
    ensemble_C50_holdout_true_positive_rate, ensemble_gb_holdout_true_positive_rate,
    ensemble_pls_holdout_true_positive_rate,
    ensemble_pda_holdout_true_positive_rate,
    ensemble_rpart_holdout_true_positive_rate, ensemble_svm_holdout_true_positive_rate,
    ensemble_tree_holdout_true_positive_rate, ensemble_xgb_holdout_true_positive_rate
  ),
  "mean" = rep(c(
    cubist_holdout_true_positive_rate_mean, fda_holdout_true_positive_rate_mean, gam_holdout_true_positive_rate_mean,
    glm_holdout_true_positive_rate_mean,
    linear_holdout_true_positive_rate_mean, lda_holdout_true_positive_rate_mean,
    pda_holdout_true_positive_rate_mean,
    qda_holdout_true_positive_rate_mean,
    rf_holdout_true_positive_rate_mean,
    svm_holdout_true_positive_rate_mean,
    tree_holdout_true_positive_rate_mean,
    ensemble_bagging_holdout_true_positive_rate_mean,
    ensemble_C50_holdout_true_positive_rate_mean, ensemble_gb_holdout_true_positive_rate_mean,
    ensemble_pls_holdout_true_positive_rate_mean,
    ensemble_pda_holdout_true_positive_rate_mean,
    ensemble_rpart_holdout_true_positive_rate_mean, ensemble_svm_holdout_true_positive_rate_mean,
    ensemble_tree_holdout_true_positive_rate_mean, ensemble_xgb_holdout_true_positive_rate_mean
  ), each = numresamples)
)

true_positive_rate_plot <- ggplot2::ggplot(data = true_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("True Positive Rate by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True Positive Rate by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_positive_rate_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_positive_rate_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_positive_rate_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_positive_rate_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_positive_rate_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_positive_rate_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

true_positive_rate_plot2 <- ggplot2::ggplot(data = true_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("True positive rate by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True positive rate by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_positive_rate_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_positive_rate_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_positive_rate_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_positive_rate_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_positive_rate_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_positive_rate_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

true_negative_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_true_negative_rate, fda_holdout_true_negative_rate, gam_holdout_true_negative_rate,
    glm_holdout_true_negative_rate,
    linear_holdout_true_negative_rate, lda_holdout_true_negative_rate,
    pda_holdout_true_negative_rate,
    qda_holdout_true_negative_rate,
    rf_holdout_true_negative_rate,
    svm_holdout_true_negative_rate,
    tree_holdout_true_negative_rate,
    ensemble_bagging_holdout_true_negative_rate,
    ensemble_C50_holdout_true_negative_rate, ensemble_gb_holdout_true_negative_rate,
    ensemble_pls_holdout_true_negative_rate,
    ensemble_pda_holdout_true_negative_rate,
    ensemble_rpart_holdout_true_negative_rate, ensemble_svm_holdout_true_negative_rate,
    ensemble_tree_holdout_true_negative_rate, ensemble_xgb_holdout_true_negative_rate
  ),
  "mean" = rep(c(
    cubist_holdout_true_negative_rate_mean, fda_holdout_true_negative_rate_mean, gam_holdout_true_negative_rate_mean,
    glm_holdout_true_negative_rate_mean,
    linear_holdout_true_negative_rate_mean, lda_holdout_true_negative_rate_mean,
    pda_holdout_true_negative_rate_mean,
    qda_holdout_true_negative_rate_mean,
    rf_holdout_true_negative_rate_mean,
    svm_holdout_true_negative_rate_mean,
    tree_holdout_true_negative_rate_mean,
    ensemble_bagging_holdout_true_negative_rate_mean,
    ensemble_C50_holdout_true_negative_rate_mean, ensemble_gb_holdout_true_negative_rate_mean,
    ensemble_pls_holdout_true_negative_rate_mean,
    ensemble_pda_holdout_true_negative_rate_mean,
    ensemble_rpart_holdout_true_negative_rate_mean, ensemble_svm_holdout_true_negative_rate_mean,
    ensemble_tree_holdout_true_negative_rate_mean, ensemble_xgb_holdout_true_negative_rate_mean
  ), each = numresamples)
)

true_negative_rate_plot <- ggplot2::ggplot(data = true_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("True negative rate by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True negative rate by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_negative_rate_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_negative_rate_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_negative_rate_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_negative_rate_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_negative_rate_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_negative_rate_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

true_negative_rate_plot2 <- ggplot2::ggplot(data = true_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("True negative rate by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "True negative rate by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("true_negative_rate_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("true_negative_rate_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("true_negative_rate_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("true_negative_rate_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("true_negative_rate_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("true_negative_rate_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

false_positive_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_false_positive_rate, fda_holdout_false_positive_rate, gam_holdout_false_positive_rate,
    glm_holdout_false_positive_rate,
    linear_holdout_false_positive_rate, lda_holdout_false_positive_rate,
    pda_holdout_false_positive_rate,
    qda_holdout_false_positive_rate,
    rf_holdout_false_negative_rate,
    svm_holdout_false_positive_rate,
    tree_holdout_false_positive_rate,
    ensemble_bagging_holdout_false_positive_rate,
    ensemble_C50_holdout_false_positive_rate, ensemble_gb_holdout_false_positive_rate,
    ensemble_pls_holdout_false_positive_rate,
    ensemble_pda_holdout_false_positive_rate,
    ensemble_rpart_holdout_false_positive_rate, ensemble_svm_holdout_false_positive_rate,
    ensemble_tree_holdout_false_positive_rate, ensemble_xgb_holdout_false_positive_rate
  ),
  "mean" = rep(c(
    cubist_holdout_false_positive_rate_mean, fda_holdout_false_positive_rate_mean, gam_holdout_false_positive_rate_mean,
    glm_holdout_false_positive_rate_mean,
    linear_holdout_false_positive_rate_mean, lda_holdout_false_positive_rate_mean,
    pda_holdout_false_positive_rate_mean,
    qda_holdout_false_positive_rate_mean,
    rf_holdout_false_positive_rate_mean,
    svm_holdout_false_positive_rate_mean,
    tree_holdout_false_positive_rate_mean,
    ensemble_bagging_holdout_false_positive_rate_mean,
    ensemble_C50_holdout_false_positive_rate_mean, ensemble_gb_holdout_false_positive_rate_mean,
    ensemble_pls_holdout_false_positive_rate_mean,
    ensemble_pda_holdout_false_positive_rate_mean,
    ensemble_rpart_holdout_false_positive_rate_mean, ensemble_svm_holdout_false_positive_rate_mean,
    ensemble_tree_holdout_false_positive_rate_mean, ensemble_xgb_holdout_false_positive_rate_mean
  ), each = numresamples)
)

false_positive_rate_plot <- ggplot2::ggplot(data = false_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("False positive rate by model fixed scales, lower is better. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "False positive rate by model fixed scales, lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_positive_rate_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_positive_rate_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_positive_rate_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_positive_rate_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_positive_rate_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_positive_rate_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

false_positive_rate_plot2 <- ggplot2::ggplot(data = false_positive_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("False positive rate by model free scales, lower is better. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "False positive rate by model free scales, lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_positive_rate_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_positive_rate_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_positive_rate_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_positive_rate_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_positive_rate_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_positive_rate_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}


false_negative_rate_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_false_negative_rate, fda_holdout_false_negative_rate, gam_holdout_false_negative_rate,
    glm_holdout_false_negative_rate,
    linear_holdout_false_negative_rate, lda_holdout_false_negative_rate,
    pda_holdout_false_negative_rate,
    qda_holdout_false_negative_rate,
    rf_holdout_false_negative_rate,
    svm_holdout_false_negative_rate,
    tree_holdout_false_negative_rate,
    ensemble_bagging_holdout_false_negative_rate,
    ensemble_C50_holdout_false_negative_rate, ensemble_gb_holdout_false_negative_rate,
    ensemble_pls_holdout_false_negative_rate,
    ensemble_pda_holdout_false_negative_rate,
    ensemble_rpart_holdout_false_negative_rate, ensemble_svm_holdout_false_negative_rate,
    ensemble_tree_holdout_false_negative_rate, ensemble_xgb_holdout_false_negative_rate
  ),
  "mean" = rep(c(
    cubist_holdout_false_negative_rate_mean, fda_holdout_false_negative_rate_mean, gam_holdout_false_negative_rate_mean,
    glm_holdout_false_negative_rate_mean,
    linear_holdout_false_negative_rate_mean, lda_holdout_false_negative_rate_mean,
    pda_holdout_false_negative_rate_mean,
    qda_holdout_false_negative_rate_mean,
    rf_holdout_false_negative_rate_mean,
    svm_holdout_false_negative_rate_mean,
    tree_holdout_false_negative_rate_mean,
    ensemble_bagging_holdout_false_negative_rate_mean,
    ensemble_C50_holdout_false_negative_rate_mean, ensemble_gb_holdout_false_negative_rate_mean,
    ensemble_pls_holdout_false_negative_rate_mean,
    ensemble_pda_holdout_false_negative_rate_mean,
    ensemble_rpart_holdout_false_negative_rate_mean, ensemble_svm_holdout_false_negative_rate_mean,
    ensemble_tree_holdout_false_negative_rate_mean, ensemble_xgb_holdout_false_negative_rate_mean
  ), each = numresamples)
)

false_negative_rate_plot <- ggplot2::ggplot(data = false_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("False negative rate by model fixed scales, lower is better. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "False negative rate by model fixed scales, lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_negative_rate_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_negative_rate_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_negative_rate_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_negative_rate_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_negative_rate_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_negative_rate_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

false_negative_rate_plot2 <- ggplot2::ggplot(data = false_negative_rate_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 0, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("False negative rate by model free scales, lower is better. \n The black horizontal line is the mean of the results, the red horizontal line is 0.") +
  ggplot2::labs(y = "False negative rate by model free scales, lower is better \n The horizontal line is the mean of the results, the red line is 0.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("false_negative_rate_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("false_negative_rate_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("false_negative_rate_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("false_negative_rate_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("false_negative_rate_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("false_negative_rate_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

F1_score_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_F1_score, fda_holdout_F1_score, gam_holdout_F1_score,
    glm_holdout_F1_score,
    linear_holdout_F1_score, lda_holdout_F1_score,
    pda_holdout_F1_score,
    qda_holdout_F1_score,
    rf_holdout_F1_score,
    svm_holdout_F1_score,
    tree_holdout_F1_score,
    ensemble_bagging_holdout_F1_score,
    ensemble_C50_holdout_F1_score, ensemble_gb_holdout_F1_score,
    ensemble_pls_holdout_F1_score,
    ensemble_pda_holdout_F1_score,
    ensemble_rpart_holdout_F1_score, ensemble_svm_holdout_F1_score,
    ensemble_tree_holdout_F1_score, ensemble_xgb_holdout_F1_score
  ),
  "mean" = rep(c(
    cubist_holdout_F1_score_mean, fda_holdout_F1_score_mean, gam_holdout_F1_score_mean,
    glm_holdout_F1_score_mean,
    linear_holdout_F1_score_mean, lda_holdout_F1_score_mean,
    pda_holdout_F1_score_mean,
    qda_holdout_F1_score_mean,
    rf_holdout_F1_score_mean,
    svm_holdout_F1_score_mean,
    tree_holdout_F1_score_mean,
    ensemble_bagging_holdout_F1_score_mean,
    ensemble_C50_holdout_F1_score_mean, ensemble_gb_holdout_F1_score_mean,
    ensemble_pls_holdout_F1_score_mean,
    ensemble_pda_holdout_F1_score_mean,
    ensemble_rpart_holdout_F1_score_mean, ensemble_svm_holdout_F1_score_mean,
    ensemble_tree_holdout_F1_score_mean, ensemble_xgb_holdout_F1_score_mean
  ), each = numresamples)
)

F1_score_plot <- ggplot2::ggplot(data = F1_score_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("F1 score by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "F1 score by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("F1_score_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("F1_score_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("F1_score_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("F1_score_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("F1_score_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("F1_score_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

F1_score_plot2 <- ggplot2::ggplot(data = F1_score_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("F1 score by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "F1 score by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("F1_score_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("F1_score_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("F1_score_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("F1_score_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("F1_score_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("F1_score_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

positive_predictive_value_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_positive_predictive_value, fda_holdout_positive_predictive_value, gam_holdout_positive_predictive_value,
    glm_holdout_positive_predictive_value,
    linear_holdout_positive_predictive_value, lda_holdout_positive_predictive_value,
    pda_holdout_positive_predictive_value,
    qda_holdout_positive_predictive_value,
    rf_holdout_positive_predictive_value,
    svm_holdout_positive_predictive_value,
    tree_holdout_positive_predictive_value,
    ensemble_bagging_holdout_positive_predictive_value,
    ensemble_C50_holdout_positive_predictive_value, ensemble_gb_holdout_positive_predictive_value,
    ensemble_pls_holdout_positive_predictive_value,
    ensemble_pda_holdout_positive_predictive_value,
    ensemble_rpart_holdout_positive_predictive_value, ensemble_svm_holdout_positive_predictive_value,
    ensemble_tree_holdout_positive_predictive_value, ensemble_xgb_holdout_positive_predictive_value
  ),
  "mean" = rep(c(
    cubist_holdout_positive_predictive_value_mean, fda_holdout_positive_predictive_value_mean, gam_holdout_positive_predictive_value_mean,
    glm_holdout_positive_predictive_value_mean,
    linear_holdout_positive_predictive_value_mean, lda_holdout_positive_predictive_value_mean,
    pda_holdout_positive_predictive_value_mean,
    qda_holdout_positive_predictive_value_mean,
    rf_holdout_positive_predictive_value_mean,
    svm_holdout_positive_predictive_value_mean,
    tree_holdout_positive_predictive_value_mean,
    ensemble_bagging_holdout_positive_predictive_value_mean,
    ensemble_C50_holdout_positive_predictive_value_mean, ensemble_gb_holdout_positive_predictive_value_mean,
    ensemble_pls_holdout_positive_predictive_value_mean,
    ensemble_pda_holdout_positive_predictive_value_mean,
    ensemble_rpart_holdout_positive_predictive_value_mean, ensemble_svm_holdout_positive_predictive_value_mean,
    ensemble_tree_holdout_positive_predictive_value_mean, ensemble_xgb_holdout_positive_predictive_value_mean
  ), each = numresamples)
)

positive_predictive_value_plot <- ggplot2::ggplot(data = positive_predictive_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("Positive predictive value by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Positive predictive value by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("positive_predictive_value_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("positive_predictive_value_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("positive_predictive_value_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("positive_predictive_value_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("positive_predictive_value_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("positive_predictive_value_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

positive_predictive_value_plot2 <- ggplot2::ggplot(data = positive_predictive_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("Positive predictive value by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Positive predictive value by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("positive_predictive_value_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("positive_predictive_value_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("positive_predictive_value_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("positive_predictive_value_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("positive_predictive_value_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("positive_predictive_value_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

negative_predictive_value_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_negative_predictive_value, fda_holdout_negative_predictive_value, gam_holdout_negative_predictive_value,
    glm_holdout_negative_predictive_value,
    linear_holdout_negative_predictive_value, lda_holdout_negative_predictive_value,
    pda_holdout_negative_predictive_value,
    qda_holdout_negative_predictive_value,
    rf_holdout_negative_predictive_value,
    svm_holdout_negative_predictive_value,
    tree_holdout_negative_predictive_value,
    ensemble_bagging_holdout_negative_predictive_value,
    ensemble_C50_holdout_negative_predictive_value, ensemble_gb_holdout_negative_predictive_value,
    ensemble_pls_holdout_negative_predictive_value,
    ensemble_pda_holdout_negative_predictive_value,
    ensemble_rpart_holdout_negative_predictive_value, ensemble_svm_holdout_negative_predictive_value,
    ensemble_tree_holdout_negative_predictive_value, ensemble_xgb_holdout_negative_predictive_value
  ),
  "mean" = rep(c(
    cubist_holdout_negative_predictive_value_mean, fda_holdout_negative_predictive_value_mean, gam_holdout_negative_predictive_value_mean,
    glm_holdout_negative_predictive_value_mean,
    linear_holdout_negative_predictive_value_mean, lda_holdout_negative_predictive_value_mean,
    pda_holdout_negative_predictive_value_mean,
    qda_holdout_negative_predictive_value_mean,
    rf_holdout_negative_predictive_value_mean,
    svm_holdout_negative_predictive_value_mean,
    tree_holdout_negative_predictive_value_mean,
    ensemble_bagging_holdout_negative_predictive_value_mean,
    ensemble_C50_holdout_negative_predictive_value_mean, ensemble_gb_holdout_negative_predictive_value_mean,
    ensemble_pls_holdout_negative_predictive_value_mean,
    ensemble_pda_holdout_negative_predictive_value_mean,
    ensemble_rpart_holdout_negative_predictive_value_mean, ensemble_svm_holdout_negative_predictive_value_mean,
    ensemble_tree_holdout_negative_predictive_value_mean, ensemble_xgb_holdout_negative_predictive_value_mean
  ), each = numresamples)
)

negative_predictive_value_plot <- ggplot2::ggplot(data = negative_predictive_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("Negative predictive value by model fixed scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Negative predictive value by model fixed scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("negative_predictive_value_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("negative_predictive_value_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("negative_predictive_value_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("negative_predictive_value_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("negative_predictive_value_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("negative_predictive_value_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

negative_predictive_value_plot2 <- ggplot2::ggplot(data = negative_predictive_value_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("Negative predictive value by model free scales, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Negative predictive value by model free scales, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("negative_predictive_value_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("negative_predictive_value_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("negative_predictive_value_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("negative_predictive_value_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("negative_predictive_value_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("negative_predictive_value_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

overfitting_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples),
    rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples),
    rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    cubist_holdout_overfitting, fda_holdout_overfitting, gam_holdout_overfitting,
    glm_holdout_overfitting,
    linear_holdout_overfitting, lda_holdout_overfitting,
    pda_holdout_overfitting,
    qda_holdout_overfitting,
    rf_holdout_overfitting,
    svm_holdout_overfitting,
    tree_holdout_overfitting,
    ensemble_bagging_holdout_overfitting,
    ensemble_C50_holdout_overfitting, ensemble_gb_holdout_overfitting,
    ensemble_pls_holdout_overfitting,
    ensemble_pda_holdout_overfitting,
    ensemble_rpart_holdout_overfitting, ensemble_svm_holdout_overfitting,
    ensemble_tree_holdout_overfitting, ensemble_xgb_holdout_overfitting
  )
)

overfitting_plot <- ggplot2::ggplot(data = overfitting_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "fixed") +
  ggplot2::ggtitle("Overfitting plot fixed scales\nOverfitting value by model, closer to one is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Overfititng value fixed scales, closer to one is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("overfitting_plot.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("overfitting_plot.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("overfitting_plot.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("overfitting_plot.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("overfitting_plot.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("overfitting_plot.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

overfitting_plot2 <- ggplot2::ggplot(data = overfitting_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 4, scales = "free") +
  ggplot2::ggtitle("Overfitting plot free scales\nOverfitting value by model, closer to one is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Overfititng value free scales, closer to one is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("overfitting_plot2.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("overfitting_plot2.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("overfitting_plot2.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("overfitting_plot2.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("overfitting_plot2.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("overfitting_plot2.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

accuracy_barchart <- ggplot2::ggplot(holdout_results, aes(x = reorder(Model, dplyr::desc(Accuracy)), y = Accuracy)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Accuracy", title = "Model accuracy, closer to one is better") +
  ggplot2::geom_text(aes(label = Accuracy), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(holdout_results$Accuracy) + 1) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Accuracy - Accuracy_sd, ymax = Accuracy + Accuracy_sd))

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("accuracy_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("accuracy_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("accuracy_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("accuracy_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("accuracy_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("accuracy_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

overfitting_barchart <- ggplot2::ggplot(holdout_results, aes(x = reorder(Model, dplyr::desc(Overfitting_Mean)), y = Overfitting_Mean)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Over or Under Fitting Mean", title = "Over or Under Fitting, closer to 1 is better") +
  ggplot2::geom_text(aes(label = Overfitting_Mean), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(holdout_results$Overfitting_Mean) +1) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Overfitting_Mean - Overfitting_sd, ymax = Overfitting_Mean + Overfitting_sd))

if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("overfitting_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("overfitting_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("overfitting_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("overfitting_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("overfitting_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("overfitting_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

duration_barchart <- ggplot2::ggplot(holdout_results, aes(x = reorder(Model, Duration), y = Duration)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Duration", title = "Duration, shorter is better") +
  ggplot2::geom_text(aes(label = Duration), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::geom_errorbar(aes(x = Model, ymin = Duration - Duration_sd, ymax = Duration + Duration_sd))
if(save_all_plots == "Y" && device == "eps"){
  ggplot2::ggsave("duration_barchart.eps", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "jpeg"){
  ggplot2::ggsave("duration_barchart.jpeg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "pdf"){
  ggplot2::ggsave("duration_barchart.pdf", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "png"){
  ggplot2::ggsave("duration_barchart.png", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "svg"){
  ggplot2::ggsave("duration_barchart.svg", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}
if(save_all_plots == "Y" && device == "tiff"){
  ggplot2::ggsave("duration_barchart.tiff", width = width, path = tempdir1, height = height, units = units, scale = scale, device = device, dpi = dpi)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("cubist_train_fit", fileext = ".RDS")
  saveRDS(cubist_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("fda_train_fit", fileext = ".RDS")
  saveRDS(fda_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("gam_train_fit", fileext = ".RDS")
  saveRDS(gam_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("glm_train_fit", fileext = ".RDS")
  saveRDS(glm_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("linear_train_fit", fileext = ".RDS")
  saveRDS(linear_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("lda_train_fit", fileext = ".RDS")
  saveRDS(lda_train_fit, fil)
}


if (save_all_trained_models == "Y") {
  fil <- tempfile("pda_train_fit", fileext = ".RDS")
  saveRDS(pda_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("qda_train_fit", fileext = ".RDS")
  saveRDS(qda_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("rf_train_fit", fileext = ".RDS")
  saveRDS(qda_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("svm_train_fit", fileext = ".RDS")
  saveRDS(svm_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("tree_train_fit", fileext = ".RDS")
  saveRDS(tree_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_bagging_train_fit", fileext = ".RDS")
  saveRDS(ensemble_bagging_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_C50_train_fit", fileext = ".RDS")
  saveRDS(ensemble_C50_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_gb_train_fit", fileext = ".RDS")
  saveRDS(ensemble_gb_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_pls_train_fit", fileext = ".RDS")
  saveRDS(ensemble_pls_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_pda_train_fit", fileext = ".RDS")
  saveRDS(ensemble_pda_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_rpart_train_fit", fileext = ".RDS")
  saveRDS(ensemble_rpart_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_svm_train_fit", fileext = ".RDS")
  saveRDS(ensemble_svm_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_tree_train_fit", fileext = ".RDS")
  saveRDS(ensemble_tree_train_fit, fil)
}

if (save_all_trained_models == "Y") {
  fil <- tempfile("ensembe_xgb_train_fit", fileext = ".RDS")
  saveRDS(ensemble_xgb_model, fil)
}

if (do_you_have_new_data == "Y") {
  Cubist <- predict(object = cubist_train_fit, newdata = newdata)
  Flexible_Discriminant_Analysis <- as.numeric(predict(object = fda_train_fit, newdata = newdata)) - 1
  Generalized_Linear_Models <- as.numeric(predict(object = glm_train_fit, newdata = newdata)) - 1
  Generalized_Linear_Models <- ifelse(Generalized_Linear_Models > 0, 1, 0)
  Generalized_Additive_Models <- as.numeric(predict(object = gam_train_fit, newdata = newdata)) - 1
  Generalized_Additive_Models <- ifelse(Generalized_Additive_Models > 0, 1, 0)
  Linear <- as.numeric(predict(object = linear_train_fit, newdata = newdata)) - 1
  Penalized_Discriminant_Analysis <- as.numeric(predict(object = pda_train_fit, newdata = newdata)) - 1
  Quadratic_Discriminant_Analysis <- as.numeric(predict(object = qda_train_fit, newdata = newdata)$class) - 1
  Random_Forest_Analysis <- as.numeric(predict(object = rf_train_fit, newdata = newdata)) - 1
  Support_Vector_Machines <- as.numeric(predict(object = svm_train_fit, newdata = newdata)) - 1
  Trees <- predict(object = tree_train_fit, newdata = newdata)

  new_ensemble <- data.frame(
    Cubist,
    Flexible_Discriminant_Analysis,
    Generalized_Linear_Models,
    Linear,
    Penalized_Discriminant_Analysis,
    Quadratic_Discriminant_Analysis,
    Random_Forest_Analysis,
    Support_Vector_Machines,
    Trees
  )

  # new_ensemble_row_numbers <- as.numeric(row.names(newdata))
  new_ensemble$y <- as.factor(newdata$y)
  thing <- colnames(ensemble1)
  new_ensemble %>% select(dplyr::all_of(thing))

  new_ensemble_bagging <- predict(object = ensemble_bagging_train_fit, newdata = new_ensemble)
  new_ensemble_C50 <- predict(object = ensemble_C50_train_fit, newdata = new_ensemble)
  new_ensemble_gb <- predict(object = ensemble_gb_train_fit, newdata = new_ensemble)
  new_ensemble_pls <- predict(object = ensemble_pls_train_fit, newdata = new_ensemble)
  new_ensemble_pda <- predict(object = ensemble_pda_train_fit, newdata = new_ensemble)
  new_ensemble_RPart <- predict(object = ensemble_rpart_train_fit, newdata = new_ensemble)
  new_ensemble_svm <- predict(object = ensemble_svm_train_fit, newdata = new_ensemble)
  new_ensemble_trees <- predict(object = ensemble_tree_train_fit, newdata = new_ensemble)

  new_data_results <- data.frame(
    "True_Value" = newdata$y,
    "Cubist" = Cubist,
    "Flexible_Discriminant_Analysis" = Flexible_Discriminant_Analysis,
    "Generalized_Linear_Models" = Generalized_Linear_Models,
    "Generalized_Additive_Models" = Generalized_Additive_Models,
    "Linear" = Linear,
    "Quadratic_Disrmininat_Analysis" = Quadratic_Discriminant_Analysis,
    "Random_Forest" = Random_Forest_Analysis,
    "Penalized_Discriminant_Analysis" = Penalized_Discriminant_Analysis,
    "Trees" = Trees,
    "Ensemble_Bagging" = new_ensemble_bagging,
    "Ensemble_C50" = new_ensemble_C50,
    "Ensemble_Gradient_Boosted" = new_ensemble_gb,
    "Ensemble_Partial_Least_Squares" = new_ensemble_pls,
    "Ensemble_Penalized_Discrmininat_Analysis" = new_ensemble_pda,
    "Ensemble_RPart" = new_ensemble_RPart,
    "Ensemble_Support_Vector_Machines" = new_ensemble_svm,
    "Ensemble_Trees" = new_ensemble_trees
  )

  new_data_results <- t(new_data_results)

  new_data_results <- reactable::reactable(new_data_results,
                                           searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                                           striped = TRUE, highlight = TRUE, resizable = TRUE
  ) %>%
    reactablefmtr::add_title("New data results")

  summary_tables <- list(
    "Cubist" = cubist_table_total, "Fixture Discrmininant Analysis" = fda_table_total, "Generalized Additive Methods" = gam_table_total,
    "Generalized Linear Models" = glm_table_total, "Linear" = linear_table_total, "Linear Discrmininant Analysis" = lda_table_total,
    "Penalized Discrminant Analysis" = pda_table_total,
    "Quadratic Discrmininant Analysis" = qda_table_total,
    "Random Forest Analysis" = rf_table_total,
    "Support Vector Machines" = svm_table_total, "Trees" = tree_table_total,
    "Ensemble Bagging" = ensemble_bagging_table_total, "Ensemble C50" = ensemble_C50_table_total,
    "Ensemble Gradient Boosted" = ensemble_gb_table_total, "Ensemble Partial Least Squares" = ensemble_pls_table_total,
    "Ensemble Penalized Discrmininant Analysis" = ensemble_pda_table_total,
    "Ensemble RPart" = ensemble_rpart_table_total, "Ensemble Support Vector Machines" = ensemble_svm_table_total,
    "Ensemble Trees" = ensemble_tree_table_total, "Ensemble XGBoost" = ensemble_xgb_table_total
  )

  return(list(
    "Head_of_data" = head_df, "Summary_tables" = summary_tables, "accuracy_plot" = accuracy_plot, "total_plot" = total_plot, "overfitting_plot" = overfitting_plot,
    "accuracy_barchart" = accuracy_barchart, "Duration_barchart" = duration_barchart, "Overfitting_barchart" = overfitting_barchart, "ROC_curves" = ROC_curves,
    "Boxplots" = boxplots, "Barchart" = barchart, "Correlation_table" = correlation_table,
    "Ensemble Correlation" = ensemble_correlation, "Ensemble_head" = head_ensemble, "New_data_results" = new_data_results,
    "Data_Summary" = data_summary, "Holdout_results" = holdout_results_final, "Data_dictionary" = str(df),
    "How_to_handle_strings" = how_to_handle_strings, "Train_amount" = train_amount, "Test_amount" = test_amount, "Validation_amount" = validation_amount
  ))
}

summary_tables <- list(
  "Cubist" = cubist_table_total, "Fixture Discrmininant Analysis" = fda_table_total, "Generalized Additive Methods" = gam_table_total,
  "Generalized Linear Models" = glm_table_total, "Linear" = linear_table_total, "Linear Discrmininant Analysis" = lda_table_total,
  "Penalized Discrminant Analysis" = pda_table_total,
  "Quadratic Discrmininant Analysis" = qda_table_total,
  "Support Vector Machines" = svm_table_total, "Trees" = tree_table_total,
  "Ensemble Bagging" = ensemble_bagging_table_total, "Ensemble C50" = ensemble_C50_table_total,
  "Ensemble Gradient Boosted" = ensemble_gb_table_total, "Ensemble Partial Least Squares" = ensemble_pls_table_total,
  "Ensemble Penalized Discrmininant Analysis" = ensemble_pda_table_total,
  "Ensemble RPart" = ensemble_rpart_table_total, "Ensemble Support Vector Machines" = ensemble_svm_table_total,
  "Ensemble Trees" = ensemble_tree_table_total, "Ensemble XGBoost" = ensemble_xgb_table_total
)

return(list(
  "Head_of_data" = head_df, "Summary_tables" = summary_tables, "accuracy_plot" = accuracy_plot, "total_plot" = total_plot, "accuracy_barchart" = accuracy_barchart,
  "overfitting_plot" = overfitting_plot, "Duration_barchart" = duration_barchart, "Overfitting_barchart" = overfitting_barchart, "ROC_curves" = ROC_curves,
  "Boxplots" = boxplots, "Barchart" = barchart, "Correlation_table" = correlation_table, 'VIF_results' = VIF_results,
  'True_positive_rate' = true_positive_rate_plot, 'True_positive_rate2' = true_positive_rate_plot2,
  'True_negative_rate' = true_negative_rate_plot, 'True_negative_rate2' = true_negative_rate_plot2,
  'False_positive_rate' = false_positive_rate_plot, 'False_positive_rate2' = false_positive_rate_plot2,
  'False_negative_rate' = false_negative_rate_plot, 'False_negative_rate2' = false_negative_rate_plot2,
  'F1_score' = F1_score_plot, 'F1_score2' = F1_score_plot2,
  'Positive_predictive_value' = positive_predictive_value_plot, 'Positive_predictive_value2' = positive_predictive_value_plot2,
  'Negative_predictive_value' = negative_predictive_value_plot, 'Negative_predictive_value2' = negative_predictive_value_plot2,
  "Ensemble Correlation" = ensemble_correlation, "Ensemble_head" = head_ensemble,
  "Data_Summary" = data_summary, "Holdout_results" = holdout_results_final,
  "How_to_handle_strings" = how_to_handle_strings, "Train_amount" = train_amount, "Test_amount" = test_amount, "Validation_amount" = validation_amount
)
)
}
