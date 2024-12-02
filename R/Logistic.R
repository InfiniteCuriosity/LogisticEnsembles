#' logistic—function to perform logistic analysis and return the results to the user.

#' @param data data can be a CSV file or within an R package, such as MASS::Pima.te
#' @param colnum the column number with the logistic data
#' @param numresamples the number of resamples
#' @param save_all_trained_models "Y" or "N". Places all the trained models in the Environment
#' @param how_to_handle_strings 0: No strings, 1: Factor values
#' @param do_you_have_new_data "Y" or "N". If "Y", then you will be asked for the new data
#' @param remove_ensemble_correlations_greater_than Enter a number to remove correlations in the ensembles
#' @param use_parallel "Y" or "N" for parallel processing
#' @param train_amount set the amount for the training data
#' @param test_amount set the amount for the testing data
#' @param validation_amount Set the amount for the validation data

#' @return a real number
#' @export Logistic Automatically builds 36 logistic models (23 individual models and 13 ensembles of models)
#'
#' @examples
#' options(rgl.useNULL=TRUE) #needed if there are issues with X11 with this example
#' Logistic(data = SAHeart,
#'   colnum = 10,
#'   numresamples = 2,
#'   how_to_handle_strings = 1,
#'   do_you_have_new_data = "N",
#'   save_all_trained_models = "N",
#'   remove_ensemble_correlations_greater_than = 1.00,
#'   use_parallel = "N",
#'   train_amount = 0.60,
#'   test_amount = 0.20,
#'   validation_amount = 0.20)
#'
#'
#' options(rgl.useNULL=TRUE) #needed if there are issues with X11 with this example
#' Logistic(data = Lebron,
#'   colnum = 6,
#'   numresamples = 10,
#'   save_all_trained_models = "Y",
#'   how_to_handle_strings = 0,
#'   do_you_have_new_data = "N",
#'   remove_ensemble_correlations_greater_than = 1.00,
#'   use_parallel = "Y",
#'   train_amount = 0.50,
#'   test_amount = 0.25,
#'   validation_amount = 0.25)
#'
#'
#' options(rgl.useNULL=TRUE) #needed if there are issues with X11 with this example
#' Logistic(data = Diabetes,
#'    colnum = ncol(Diabetes),
#'    numresamples = 10,
#'    save_all_trained_models = "N",
#'    how_to_handle_strings = 1,
#'    do_you_have_new_data = "N",
#'    remove_ensemble_correlations_greater_than = 1.00,
#'    use_parallel = "Y",
#'    train_amount = 0.60,
#'    test_amount = 0.20,
#'    validation_amount = 0.20)
#'
#' @importFrom adabag bagging
#' @importFrom arm bayesglm
#' @importFrom brnn brnn
#' @importFrom C50 C5.0
#' @importFrom corrplot corrplot
#' @importFrom Cubist cubist
#' @importFrom doParallel registerDoParallel
#' @importFrom dplyr across count mutate relocate select %>%
#' @importFrom e1071 naiveBayes svm
#' @importFrom gam gam
#' @importFrom gbm gbm
#' @importFrom ggplot2 geom_boxplot geom_histogram ggplot facet_wrap labs theme_bw labs aes
#' @importFrom graphics hist pairs panel.smooth par rect
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
#' @importFrom reactable reactable
#' @importFrom reactablefmtr add_title
#' @importFrom readr cols
#' @importFrom rpart rpart
#' @importFrom scales percent
#' @importFrom stats binomial cor predict reorder
#' @importFrom tidyr gather pivot_longer
#' @importFrom tree tree
#' @importFrom utils head read.csv str
#' @importFrom xgboost xgb.DMatrix xgb.train


Logistic <- function(data, colnum, numresamples, save_all_trained_models = c("Y", "N"), how_to_handle_strings = c("0", "1"),
                     do_you_have_new_data = c("Y", "N"), remove_ensemble_correlations_greater_than, use_parallel = c("Y", "N"),
                     train_amount, test_amount, validation_amount) {

use_parallel <- 0
no_cores <- 0

if (use_parallel == "Y") {
  cl <- parallel::makeCluster(no_cores, type = "FORK")
  doParallel::registerDoParallel(cl)
}

colnames(data)[colnum] <- "y"

df <- data %>% dplyr::relocate(y, .after = dplyr::last_col()) # Moves the target column to the last column on the right

df <- df[sample(1:nrow(df)), ] # randomizes the rows

head_df <- reactable::reactable(head(df),
                     searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                     striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Head of the data frame")

if (how_to_handle_strings == 1) {
  df <- dplyr::mutate_if(df, is.character, as.factor)
  df <- dplyr::mutate_if(df, is.factor, as.numeric)
}

if (do_you_have_new_data == "Y") {
  newdata <- readline("What is the URL for the new data? ")
  newdata <- read.csv(newdata)
  colnames(newdata)[colnum] <- "y"
  newdata <- newdata %>% dplyr::relocate(y, .after = dplyr::last_col()) # Moves the target column to the last column on the right
}

#### Initialize values to 0, in alphabetical order ####

adaboost_train_true_positive_rate <- 0
adaboost_train_true_negative_rate <- 0
adaboost_train_false_positive_rate <- 0
adaboost_train_false_negative_rate <- 0
adaboost_train_accuracy <- 0
adaboost_train_F1_score <- 0
adaboost_test_true_positive_rate <- 0
adaboost_test_true_negative_rate <- 0
adaboost_test_false_positive_rate <- 0
adaboost_test_false_negative_rate <- 0
adaboost_test_accuracy <- 0
adaboost_test_F1_score <- 0
adaboost_validation_true_positive_rate <- 0
adaboost_validation_true_negative_rate <- 0
adaboost_validation_false_positive_rate <- 0
adaboost_validation_false_negative_rate <- 0
adaboost_validation_accuracy <- 0
adaboost_validation_F1_score <- 0
adaboost_holdout_true_positive_rate <- 0
adaboost_holdout_true_negative_rate <- 0
adaboost_holdout_false_positive_rate <- 0
adaboost_holdout_false_negative_rate <- 0
adaboost_holdout_accuracy <- 0
adaboost_holdout_F1_score <- 0
adaboost_duration <- 0
adaboost_train_positive_predictive_value <- 0
adaboost_train_negative_predictive_value <- 0
adaboost_test_positive_predictive_value <- 0
adaboost_test_negative_predictive_value <- 0
adaboost_validation_positive_predictive_value <- 0
adaboost_validation_negative_predictive_value <- 0
adaboost_holdout_positive_predictive_value <- 0
adaboost_holdout_negative_predictive_value <- 0
adaboost_holdout_overfitting <- 0
adaboost_holdout_overfitting_mean <- 0
adaboost_table_total <- 0

bagging_train_true_positive_rate <- 0
bagging_train_true_negative_rate <- 0
bagging_train_false_positive_rate <- 0
bagging_train_false_negative_rate <- 0
bagging_train_accuracy <- 0
bagging_train_F1_score <- 0
bagging_test_true_positive_rate <- 0
bagging_test_true_negative_rate <- 0
bagging_test_false_positive_rate <- 0
bagging_test_false_negative_rate <- 0
bagging_test_accuracy <- 0
bagging_test_F1_score <- 0
bagging_validation_true_positive_rate <- 0
bagging_validation_true_negative_rate <- 0
bagging_validation_false_positive_rate <- 0
bagging_validation_false_negative_rate <- 0
bagging_validation_accuracy <- 0
bagging_validation_F1_score <- 0
bagging_holdout_true_positive_rate <- 0
bagging_holdout_true_negative_rate <- 0
bagging_holdout_false_positive_rate <- 0
bagging_holdout_false_negative_rate <- 0
bagging_holdout_accuracy <- 0
bagging_holdout_F1_score <- 0
bagging_duration <- 0
bagging_train_positive_predictive_value <- 0
bagging_train_negative_predictive_value <- 0
bagging_test_positive_predictive_value <- 0
bagging_test_negative_predictive_value <- 0
bagging_validation_positive_predictive_value <- 0
bagging_validation_negative_predictive_value <- 0
bagging_holdout_positive_predictive_value <- 0
bagging_holdout_negative_predictive_value <- 0
bagging_holdout_overfitting <- 0
bagging_holdout_overfitting_mean <- 0
bagging_table_total <- 0

bag_rf_train_true_positive_rate <- 0
bag_rf_train_true_negative_rate <- 0
bag_rf_train_false_positive_rate <- 0
bag_rf_train_false_negative_rate <- 0
bag_rf_train_accuracy <- 0
bag_rf_train_F1_score <- 0
bag_rf_test_true_positive_rate <- 0
bag_rf_test_true_negative_rate <- 0
bag_rf_test_false_positive_rate <- 0
bag_rf_test_false_negative_rate <- 0
bag_rf_test_accuracy <- 0
bag_rf_test_F1_score <- 0
bag_rf_validation_true_positive_rate <- 0
bag_rf_validation_true_negative_rate <- 0
bag_rf_validation_false_positive_rate <- 0
bag_rf_validation_false_negative_rate <- 0
bag_rf_validation_accuracy <- 0
bag_rf_validation_F1_score <- 0
bag_rf_holdout_true_positive_rate <- 0
bag_rf_holdout_true_negative_rate <- 0
bag_rf_holdout_false_positive_rate <- 0
bag_rf_holdout_false_negative_rate <- 0
bag_rf_holdout_accuracy <- 0
bag_rf_holdout_F1_score <- 0
bag_rf_duration <- 0
bag_rf_train_positive_predictive_value <- 0
bag_rf_train_negative_predictive_value <- 0
bag_rf_test_positive_predictive_value <- 0
bag_rf_test_negative_predictive_value <- 0
bag_rf_validation_positive_predictive_value <- 0
bag_rf_validation_negative_predictive_value <- 0
bag_rf_holdout_positive_predictive_value <- 0
bag_rf_holdout_negative_predictive_value <- 0
bag_rf_holdout_overfitting <- 0
bag_rf_holdout_overfitting_mean <- 0
bag_rf_table_total <- 0

bayesglm_train_true_positive_rate <- 0
bayesglm_train_true_negative_rate <- 0
bayesglm_train_false_positive_rate <- 0
bayesglm_train_false_negative_rate <- 0
bayesglm_train_accuracy <- 0
bayesglm_train_F1_score <- 0
bayesglm_test_true_positive_rate <- 0
bayesglm_test_true_negative_rate <- 0
bayesglm_test_false_positive_rate <- 0
bayesglm_test_false_negative_rate <- 0
bayesglm_test_accuracy <- 0
bayesglm_test_F1_score <- 0
bayesglm_validation_true_positive_rate <- 0
bayesglm_validation_true_negative_rate <- 0
bayesglm_validation_false_positive_rate <- 0
bayesglm_validation_false_negative_rate <- 0
bayesglm_validation_accuracy <- 0
bayesglm_validation_F1_score <- 0
bayesglm_holdout_true_positive_rate <- 0
bayesglm_holdout_true_negative_rate <- 0
bayesglm_holdout_false_positive_rate <- 0
bayesglm_holdout_false_negative_rate <- 0
bayesglm_holdout_accuracy <- 0
bayesglm_holdout_F1_score <- 0
bayesglm_duration <- 0
bayesglm_train_positive_predictive_value <- 0
bayesglm_train_negative_predictive_value <- 0
bayesglm_test_positive_predictive_value <- 0
bayesglm_test_negative_predictive_value <- 0
bayesglm_validation_positive_predictive_value <- 0
bayesglm_validation_negative_predictive_value <- 0
bayesglm_holdout_positive_predictive_value <- 0
bayesglm_holdout_negative_predictive_value <- 0
bayesglm_holdout_overfitting <- 0
bayesglm_holdout_overfitting_mean <- 0
bayesglm_table_total <- 0

bayesrnn_train_true_positive_rate <- 0
bayesrnn_train_true_negative_rate <- 0
bayesrnn_train_false_positive_rate <- 0
bayesrnn_train_false_negative_rate <- 0
bayesrnn_train_accuracy <- 0
bayesrnn_train_F1_score <- 0
bayesrnn_test_true_positive_rate <- 0
bayesrnn_test_true_negative_rate <- 0
bayesrnn_test_false_positive_rate <- 0
bayesrnn_test_false_negative_rate <- 0
bayesrnn_test_accuracy <- 0
bayesrnn_test_F1_score <- 0
bayesrnn_validation_true_positive_rate <- 0
bayesrnn_validation_true_negative_rate <- 0
bayesrnn_validation_false_positive_rate <- 0
bayesrnn_validation_false_negative_rate <- 0
bayesrnn_validation_accuracy <- 0
bayesrnn_validation_F1_score <- 0
bayesrnn_holdout_true_positive_rate <- 0
bayesrnn_holdout_true_negative_rate <- 0
bayesrnn_holdout_false_positive_rate <- 0
bayesrnn_holdout_false_negative_rate <- 0
bayesrnn_holdout_accuracy <- 0
bayesrnn_holdout_F1_score <- 0
bayesrnn_duration <- 0
bayesrnn_train_positive_predictive_value <- 0
bayesrnn_train_negative_predictive_value <- 0
bayesrnn_test_positive_predictive_value <- 0
bayesrnn_test_negative_predictive_value <- 0
bayesrnn_validation_positive_predictive_value <- 0
bayesrnn_validation_negative_predictive_value <- 0
bayesrnn_holdout_positive_predictive_value <- 0
bayesrnn_holdout_negative_predictive_value <- 0
bayesrnn_holdout_overfitting <- 0
bayesrnn_holdout_overfitting_mean <- 0
bayesrnn_table_total <- 0

C50_train_true_positive_rate <- 0
C50_train_true_negative_rate <- 0
C50_train_false_positive_rate <- 0
C50_train_false_negative_rate <- 0
C50_train_accuracy <- 0
C50_train_F1_score <- 0
C50_test_true_positive_rate <- 0
C50_test_true_negative_rate <- 0
C50_test_false_positive_rate <- 0
C50_test_false_negative_rate <- 0
C50_test_accuracy <- 0
C50_test_F1_score <- 0
C50_validation_true_positive_rate <- 0
C50_validation_true_negative_rate <- 0
C50_validation_false_positive_rate <- 0
C50_validation_false_negative_rate <- 0
C50_validation_accuracy <- 0
C50_validation_F1_score <- 0
C50_holdout_true_positive_rate <- 0
C50_holdout_true_negative_rate <- 0
C50_holdout_false_positive_rate <- 0
C50_holdout_false_negative_rate <- 0
C50_holdout_accuracy <- 0
C50_holdout_F1_score <- 0
C50_duration <- 0
C50_train_positive_predictive_value <- 0
C50_train_negative_predictive_value <- 0
C50_test_positive_predictive_value <- 0
C50_test_negative_predictive_value <- 0
C50_validation_positive_predictive_value <- 0
C50_validation_negative_predictive_value <- 0
C50_holdout_positive_predictive_value <- 0
C50_holdout_negative_predictive_value <- 0
C50_holdout_overfitting <- 0
C50_holdout_overfitting_mean <- 0
C50_table_total <- 0

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

mda_train_true_positive_rate <- 0
mda_train_true_negative_rate <- 0
mda_train_false_positive_rate <- 0
mda_train_false_negative_rate <- 0
mda_train_accuracy <- 0
mda_train_F1_score <- 0
mda_test_true_positive_rate <- 0
mda_test_true_negative_rate <- 0
mda_test_false_positive_rate <- 0
mda_test_false_negative_rate <- 0
mda_test_accuracy <- 0
mda_test_F1_score <- 0
mda_validation_true_positive_rate <- 0
mda_validation_true_negative_rate <- 0
mda_validation_false_positive_rate <- 0
mda_validation_false_negative_rate <- 0
mda_validation_accuracy <- 0
mda_validation_F1_score <- 0
mda_holdout_true_positive_rate <- 0
mda_holdout_true_negative_rate <- 0
mda_holdout_false_positive_rate <- 0
mda_holdout_false_negative_rate <- 0
mda_holdout_accuracy <- 0
mda_holdout_F1_score <- 0
mda_duration <- 0
mda_train_positive_predictive_value <- 0
mda_train_negative_predictive_value <- 0
mda_test_positive_predictive_value <- 0
mda_test_negative_predictive_value <- 0
mda_validation_positive_predictive_value <- 0
mda_validation_negative_predictive_value <- 0
mda_holdout_positive_predictive_value <- 0
mda_holdout_negative_predictive_value <- 0
mda_holdout_overfitting <- 0
mda_holdout_overfitting_mean <- 0
mda_table_total <- 0

n_bayes_train_true_positive_rate <- 0
n_bayes_train_true_negative_rate <- 0
n_bayes_train_false_positive_rate <- 0
n_bayes_train_false_negative_rate <- 0
n_bayes_train_accuracy <- 0
n_bayes_train_F1_score <- 0
n_bayes_test_true_positive_rate <- 0
n_bayes_test_true_negative_rate <- 0
n_bayes_test_false_positive_rate <- 0
n_bayes_test_false_negative_rate <- 0
n_bayes_test_accuracy <- 0
n_bayes_test_F1_score <- 0
n_bayes_validation_true_positive_rate <- 0
n_bayes_validation_true_negative_rate <- 0
n_bayes_validation_false_positive_rate <- 0
n_bayes_validation_false_negative_rate <- 0
n_bayes_validation_accuracy <- 0
n_bayes_validation_F1_score <- 0
n_bayes_holdout_true_positive_rate <- 0
n_bayes_holdout_true_negative_rate <- 0
n_bayes_holdout_false_positive_rate <- 0
n_bayes_holdout_false_negative_rate <- 0
n_bayes_holdout_accuracy <- 0
n_bayes_holdout_F1_score <- 0
n_bayes_duration <- 0
n_bayes_train_positive_predictive_value <- 0
n_bayes_train_negative_predictive_value <- 0
n_bayes_test_positive_predictive_value <- 0
n_bayes_test_negative_predictive_value <- 0
n_bayes_validation_positive_predictive_value <- 0
n_bayes_validation_negative_predictive_value <- 0
n_bayes_holdout_positive_predictive_value <- 0
n_bayes_holdout_negative_predictive_value <- 0
n_bayes_holdout_overfitting <- 0
n_bayes_holdout_overfitting_mean <- 0
n_bayes_table_total <- 0

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

ranger_train_true_positive_rate <- 0
ranger_train_true_negative_rate <- 0
ranger_train_false_positive_rate <- 0
ranger_train_false_negative_rate <- 0
ranger_train_accuracy <- 0
ranger_train_F1_score <- 0
ranger_test_true_positive_rate <- 0
ranger_test_true_negative_rate <- 0
ranger_test_false_positive_rate <- 0
ranger_test_false_negative_rate <- 0
ranger_test_accuracy <- 0
ranger_test_F1_score <- 0
ranger_validation_true_positive_rate <- 0
ranger_validation_true_negative_rate <- 0
ranger_validation_false_positive_rate <- 0
ranger_validation_false_negative_rate <- 0
ranger_validation_accuracy <- 0
ranger_validation_F1_score <- 0
ranger_holdout_true_positive_rate <- 0
ranger_holdout_true_negative_rate <- 0
ranger_holdout_false_positive_rate <- 0
ranger_holdout_false_negative_rate <- 0
ranger_holdout_accuracy <- 0
ranger_holdout_F1_score <- 0
ranger_duration <- 0
ranger_train_positive_predictive_value <- 0
ranger_train_negative_predictive_value <- 0
ranger_test_positive_predictive_value <- 0
ranger_test_negative_predictive_value <- 0
ranger_validation_positive_predictive_value <- 0
ranger_validation_negative_predictive_value <- 0
ranger_holdout_positive_predictive_value <- 0
ranger_holdout_negative_predictive_value <- 0
ranger_holdout_overfitting <- 0
ranger_holdout_overfitting_mean <- 0
ranger_table_total <- 0

rpart_train_true_positive_rate <- 0
rpart_train_true_negative_rate <- 0
rpart_train_false_positive_rate <- 0
rpart_train_false_negative_rate <- 0
rpart_train_accuracy <- 0
rpart_train_F1_score <- 0
rpart_test_true_positive_rate <- 0
rpart_test_true_negative_rate <- 0
rpart_test_false_positive_rate <- 0
rpart_test_false_negative_rate <- 0
rpart_test_accuracy <- 0
rpart_test_F1_score <- 0
rpart_validation_true_positive_rate <- 0
rpart_validation_true_negative_rate <- 0
rpart_validation_false_positive_rate <- 0
rpart_validation_false_negative_rate <- 0
rpart_validation_accuracy <- 0
rpart_validation_F1_score <- 0
rpart_holdout_true_positive_rate <- 0
rpart_holdout_true_negative_rate <- 0
rpart_holdout_false_positive_rate <- 0
rpart_holdout_false_negative_rate <- 0
rpart_holdout_accuracy <- 0
rpart_holdout_F1_score <- 0
rpart_duration <- 0
rpart_train_positive_predictive_value <- 0
rpart_train_negative_predictive_value <- 0
rpart_test_positive_predictive_value <- 0
rpart_test_negative_predictive_value <- 0
rpart_validation_positive_predictive_value <- 0
rpart_validation_negative_predictive_value <- 0
rpart_holdout_positive_predictive_value <- 0
rpart_holdout_negative_predictive_value <- 0
rpart_holdout_overfitting <- 0
rpart_holdout_overfitting_mean <- 0
rpart_table_total <- 0

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

xgb_train_true_positive_rate <- 0
xgb_train_true_negative_rate <- 0
xgb_train_false_positive_rate <- 0
xgb_train_false_negative_rate <- 0
xgb_train_accuracy <- 0
xgb_train_F1_score <- 0
xgb_test_true_positive_rate <- 0
xgb_test_true_negative_rate <- 0
xgb_test_false_positive_rate <- 0
xgb_test_false_negative_rate <- 0
xgb_test_accuracy <- 0
xgb_test_F1_score <- 0
xgb_validation_true_positive_rate <- 0
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
xgb_holdout_overfitting <- 0
xgb_holdout_overfitting_mean <- 0
xgb_table_total <- 0

ensemble_adaboost_train_true_positive_rate <- 0
ensemble_adaboost_train_true_negative_rate <- 0
ensemble_adaboost_train_false_positive_rate <- 0
ensemble_adaboost_train_false_negative_rate <- 0
ensemble_adaboost_train_accuracy <- 0
ensemble_adaboost_train_F1_score <- 0
ensemble_adaboost_test_true_positive_rate <- 0
ensemble_adaboost_test_true_negative_rate <- 0
ensemble_adaboost_test_false_positive_rate <- 0
ensemble_adaboost_test_false_negative_rate <- 0
ensemble_adaboost_test_accuracy <- 0
ensemble_adaboost_test_F1_score <- 0
ensemble_adaboost_validation_true_positive_rate <- 0
ensemble_adaboost_validation_true_negative_rate <- 0
ensemble_adaboost_validation_false_positive_rate <- 0
ensemble_adaboost_validation_false_negative_rate <- 0
ensemble_adaboost_validation_accuracy <- 0
ensemble_adaboost_validation_F1_score <- 0
ensemble_adaboost_holdout_true_positive_rate <- 0
ensemble_adaboost_holdout_true_negative_rate <- 0
ensemble_adaboost_holdout_false_positive_rate <- 0
ensemble_adaboost_holdout_false_negative_rate <- 0
ensemble_adaboost_holdout_accuracy <- 0
ensemble_adaboost_holdout_F1_score <- 0
ensemble_adaboost_duration <- 0
ensemble_adaboost_train_positive_predictive_value <- 0
ensemble_adaboost_train_negative_predictive_value <- 0
ensemble_adaboost_test_positive_predictive_value <- 0
ensemble_adaboost_test_negative_predictive_value <- 0
ensemble_adaboost_validation_positive_predictive_value <- 0
ensemble_adaboost_validation_negative_predictive_value <- 0
ensemble_adaboost_holdout_positive_predictive_value <- 0
ensemble_adaboost_holdout_negative_predictive_value <- 0
ensemble_adaboost_holdout_overfitting <- 0
ensemble_adaboost_holdout_overfitting_mean <- 0
ensemble_adaboost_table_total <- 0

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

ensemble_rf_train_true_positive_rate <- 0
ensemble_rf_train_true_negative_rate <- 0
ensemble_rf_train_false_positive_rate <- 0
ensemble_rf_train_false_negative_rate <- 0
ensemble_rf_train_accuracy <- 0
ensemble_rf_train_F1_score <- 0
ensemble_rf_test_true_positive_rate <- 0
ensemble_rf_test_true_negative_rate <- 0
ensemble_rf_test_false_positive_rate <- 0
ensemble_rf_test_false_negative_rate <- 0
ensemble_rf_test_accuracy <- 0
ensemble_rf_test_F1_score <- 0
ensemble_rf_validation_true_positive_rate <- 0
ensemble_rf_validation_true_negative_rate <- 0
ensemble_rf_validation_false_positive_rate <- 0
ensemble_rf_validation_false_negative_rate <- 0
ensemble_rf_validation_accuracy <- 0
ensemble_rf_validation_F1_score <- 0
ensemble_rf_holdout_true_positive_rate <- 0
ensemble_rf_holdout_true_negative_rate <- 0
ensemble_rf_holdout_false_positive_rate <- 0
ensemble_rf_holdout_false_negative_rate <- 0
ensemble_rf_holdout_accuracy <- 0
ensemble_rf_holdout_F1_score <- 0
ensemble_rf_duration <- 0
ensemble_rf_train_positive_predictive_value <- 0
ensemble_rf_train_negative_predictive_value <- 0
ensemble_rf_test_positive_predictive_value <- 0
ensemble_rf_test_negative_predictive_value <- 0
ensemble_rf_validation_positive_predictive_value <- 0
ensemble_rf_validation_negative_predictive_value <- 0
ensemble_rf_holdout_positive_predictive_value <- 0
ensemble_rf_holdout_negative_predictive_value <- 0
ensemble_rf_holdout_overfitting <- 0
ensemble_rf_holdout_overfitting_mean <- 0
ensemble_rf_table_total <- 0

ensemble_ranger_train_true_positive_rate <- 0
ensemble_ranger_train_true_negative_rate <- 0
ensemble_ranger_train_false_positive_rate <- 0
ensemble_ranger_train_false_negative_rate <- 0
ensemble_ranger_train_accuracy <- 0
ensemble_ranger_train_F1_score <- 0
ensemble_ranger_test_true_positive_rate <- 0
ensemble_ranger_test_true_negative_rate <- 0
ensemble_ranger_test_false_positive_rate <- 0
ensemble_ranger_test_false_negative_rate <- 0
ensemble_ranger_test_accuracy <- 0
ensemble_ranger_test_F1_score <- 0
ensemble_ranger_validation_true_positive_rate <- 0
ensemble_ranger_validation_true_negative_rate <- 0
ensemble_ranger_validation_false_positive_rate <- 0
ensemble_ranger_validation_false_negative_rate <- 0
ensemble_ranger_validation_accuracy <- 0
ensemble_ranger_validation_F1_score <- 0
ensemble_ranger_holdout_true_positive_rate <- 0
ensemble_ranger_holdout_true_negative_rate <- 0
ensemble_ranger_holdout_false_positive_rate <- 0
ensemble_ranger_holdout_false_negative_rate <- 0
ensemble_ranger_holdout_accuracy <- 0
ensemble_ranger_holdout_F1_score <- 0
ensemble_ranger_duration <- 0
ensemble_ranger_train_positive_predictive_value <- 0
ensemble_ranger_train_negative_predictive_value <- 0
ensemble_ranger_test_positive_predictive_value <- 0
ensemble_ranger_test_negative_predictive_value <- 0
ensemble_ranger_validation_positive_predictive_value <- 0
ensemble_ranger_validation_negative_predictive_value <- 0
ensemble_ranger_holdout_positive_predictive_value <- 0
ensemble_ranger_holdout_negative_predictive_value <- 0
ensemble_ranger_holdout_overfitting <- 0
ensemble_ranger_holdout_overfitting_mean <- 0
ensemble_ranger_table_total <- 0

ensemble_rda_train_true_positive_rate <- 0
ensemble_rda_train_true_negative_rate <- 0
ensemble_rda_train_false_positive_rate <- 0
ensemble_rda_train_false_negative_rate <- 0
ensemble_rda_train_accuracy <- 0
ensemble_rda_train_F1_score <- 0
ensemble_rda_test_true_positive_rate <- 0
ensemble_rda_test_true_negative_rate <- 0
ensemble_rda_test_false_positive_rate <- 0
ensemble_rda_test_false_negative_rate <- 0
ensemble_rda_test_accuracy <- 0
ensemble_rda_test_F1_score <- 0
ensemble_rda_validation_true_positive_rate <- 0
ensemble_rda_validation_true_negative_rate <- 0
ensemble_rda_validation_false_positive_rate <- 0
ensemble_rda_validation_false_negative_rate <- 0
ensemble_rda_validation_accuracy <- 0
ensemble_rda_validation_F1_score <- 0
ensemble_rda_holdout_true_positive_rate <- 0
ensemble_rda_holdout_true_negative_rate <- 0
ensemble_rda_holdout_false_positive_rate <- 0
ensemble_rda_holdout_false_negative_rate <- 0
ensemble_rda_holdout_accuracy <- 0
ensemble_rda_holdout_F1_score <- 0
ensemble_rda_duration <- 0
ensemble_rda_train_positive_predictive_value <- 0
ensemble_rda_train_negative_predictive_value <- 0
ensemble_rda_test_positive_predictive_value <- 0
ensemble_rda_test_negative_predictive_value <- 0
ensemble_rda_validation_positive_predictive_value <- 0
ensemble_rda_validation_negative_predictive_value <- 0
ensemble_rda_holdout_positive_predictive_value <- 0
ensemble_rda_holdout_negative_predictive_value <- 0
ensemble_rda_holdout_overfitting <- 0
ensemble_rda_holdout_overfitting_mean <- 0
ensemble_rda_table_total <- 0

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
  ggplot2::facet_wrap(~ name, scales = "free") +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.1, 0.25)))

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

#### Print correlation matrix of numeric data ####
correlation_table <- reactable::reactable(round(cor(df), 4),
                           searchable = TRUE, pagination = FALSE, wrap = TRUE, rownames = TRUE, fullWidth = TRUE, filterable = TRUE, bordered = TRUE,
                           striped = TRUE, highlight = TRUE, resizable = TRUE
)%>%
  reactablefmtr::add_title("Correlation of the data")

#### Pariwise scatter plot ####

panel.hist <- function(x, ...) {
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
display_pairs <- pairs(df,
                       panel = panel.smooth, main = "Pairwise scatter plots and histograms of the numerical data",
                       lower.panel = panel.smooth, diag.panel = panel.hist
)

#### Boxplots of the numeric data ####
boxplots <- df1 %>%
  tidyr::gather(key = "var", value = "value") %>%
  ggplot2::ggplot(aes(x = "", y = value)) +
  ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  ggplot2::facet_wrap(~var, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Boxplots of the numeric data")
# Thanks to https://rstudio-pubs-static_s3_amazonaws_com/388596_e21196f1adf04e0ea7cd68edd9eba966_html

# Break into train, test and validation sets

for (i in 1:numresamples) {
  print(noquote(""))
  print(paste0("Resampling number ", i, " of ", numresamples, sep = ','))
  print(noquote(""))
  df <- df[sample(1:nrow(df)), ] # randomize the rows again!

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


  #### Adaboost ####
  adaboost_start <- Sys.time()

  adaboost_train_fit <- MachineShop::fit(formula = as.factor(y) ~ ., data = train01, model = "AdaBoostModel")

  adaboost_train_pred <- stats::predict(adaboost_train_fit, train01, type = "prob")
  adaboost_train_predictions <- ifelse(adaboost_train_pred > 0.5, 1, 0)
  adaboost_train_table <- table(adaboost_train_predictions, y_train)
  adaboost_train_true_positive_rate[i] <- adaboost_train_table[2, 2] / sum(adaboost_train_table[2, 2] + adaboost_train_table[1, 2])
  adaboost_train_true_positive_rate_mean <- mean(adaboost_train_true_positive_rate)
  adaboost_train_true_negative_rate[i] <- adaboost_train_table[1, 1] / sum(adaboost_train_table[1, 1] + adaboost_train_table[2, 1])
  adaboost_train_true_negative_rate_mean <- mean(adaboost_train_true_negative_rate)
  adaboost_train_false_positive_rate[i] <- adaboost_train_table[2, 1] / sum(adaboost_train_table[2, 1] + adaboost_train_table[1, 1])
  adaboost_train_false_positive_rate_mean <- mean(adaboost_train_false_positive_rate)
  adaboost_train_false_negative_rate[i] <- adaboost_train_table[1, 2] / sum(adaboost_train_table[1, 2] + adaboost_train_table[2, 2])
  adaboost_train_false_negative_rate_mean <- mean(adaboost_train_false_negative_rate)
  adaboost_train_accuracy[i] <- (adaboost_train_table[1, 1] + adaboost_train_table[2, 2]) / sum(adaboost_train_table)
  adaboost_train_accuracy_mean <- mean(adaboost_train_accuracy)
  adaboost_train_F1_score[i] <- 2 * (adaboost_train_table[2, 2]) / sum(2 * adaboost_train_table[2, 2] + adaboost_train_table[1, 2] + adaboost_train_table[2, 1])
  adaboost_train_F1_score_mean <- mean(adaboost_train_F1_score)
  adaboost_train_positive_predictive_value[i] <- adaboost_train_table[2, 2] / sum(adaboost_train_table[2, 2] + adaboost_train_table[2, 1])
  adaboost_train_positive_predictive_value_mean <- mean(adaboost_train_positive_predictive_value)
  adaboost_train_negative_predictive_value[i] <- adaboost_train_table[1, 1] / sum(adaboost_train_table[1, 1] + adaboost_train_table[1, 2])
  adaboost_train_negative_predictive_value_mean <- mean(adaboost_train_negative_predictive_value)

  adaboost_test_pred <- stats::predict(adaboost_train_fit, test01, type = "prob")
  adaboost_test_predictions <- ifelse(adaboost_test_pred > 0.5, 1, 0)
  adaboost_test_table <- table(adaboost_test_predictions, y_test)
  adaboost_test_true_positive_rate[i] <- adaboost_test_table[2, 2] / sum(adaboost_test_table[2, 2] + adaboost_test_table[1, 2])
  adaboost_test_true_positive_rate_mean <- mean(adaboost_test_true_positive_rate)
  adaboost_test_true_negative_rate[i] <- adaboost_test_table[1, 1] / sum(adaboost_test_table[1, 1] + adaboost_test_table[2, 1])
  adaboost_test_true_negative_rate_mean <- mean(adaboost_test_true_negative_rate)
  adaboost_test_false_positive_rate[i] <- adaboost_test_table[2, 1] / sum(adaboost_test_table[2, 1] + adaboost_test_table[1, 1])
  adaboost_test_false_positive_rate_mean <- mean(adaboost_test_false_positive_rate)
  adaboost_test_false_negative_rate[i] <- adaboost_test_table[1, 2] / sum(adaboost_test_table[1, 2] + adaboost_test_table[2, 2])
  adaboost_test_false_negative_rate_mean <- mean(adaboost_test_false_negative_rate)
  adaboost_test_accuracy[i] <- (adaboost_test_table[1, 1] + adaboost_test_table[2, 2]) / sum(adaboost_test_table)
  adaboost_test_accuracy_mean <- mean(adaboost_test_accuracy)
  adaboost_test_F1_score[i] <- 2 * (adaboost_test_table[2, 2]) / sum(2 * adaboost_test_table[2, 2] + adaboost_test_table[1, 2] + adaboost_test_table[2, 1])
  adaboost_test_F1_score_mean <- mean(adaboost_test_F1_score)
  adaboost_test_positive_predictive_value[i] <- adaboost_test_table[2, 2] / sum(adaboost_test_table[2, 2] + adaboost_test_table[2, 1])
  adaboost_test_positive_predictive_value_mean <- mean(adaboost_test_positive_predictive_value)
  adaboost_test_negative_predictive_value[i] <- adaboost_test_table[1, 1] / sum(adaboost_test_table[1, 1] + adaboost_test_table[1, 2])
  adaboost_test_negative_predictive_value_mean <- mean(adaboost_test_negative_predictive_value)

  adaboost_validation_pred <- stats::predict(adaboost_train_fit, validation01, type = "prob")
  adaboost_validation_predictions <- ifelse(adaboost_validation_pred > 0.5, 1, 0)
  adaboost_validation_table <- table(adaboost_validation_predictions, y_validation)
  adaboost_validation_true_positive_rate[i] <- adaboost_validation_table[2, 2] / sum(adaboost_validation_table[2, 2] + adaboost_validation_table[1, 2])
  adaboost_validation_true_positive_rate_mean <- mean(adaboost_validation_true_positive_rate)
  adaboost_validation_true_negative_rate[i] <- adaboost_validation_table[1, 1] / sum(adaboost_validation_table[1, 1] + adaboost_validation_table[2, 1])
  adaboost_validation_true_negative_rate_mean <- mean(adaboost_validation_true_negative_rate)
  adaboost_validation_false_positive_rate[i] <- adaboost_validation_table[2, 1] / sum(adaboost_validation_table[2, 1] + adaboost_validation_table[1, 1])
  adaboost_validation_false_positive_rate_mean <- mean(adaboost_validation_false_positive_rate)
  adaboost_validation_false_negative_rate[i] <- adaboost_validation_table[1, 2] / sum(adaboost_validation_table[1, 2] + adaboost_validation_table[2, 2])
  adaboost_validation_false_negative_rate_mean <- mean(adaboost_validation_false_negative_rate)
  adaboost_validation_accuracy[i] <- (adaboost_validation_table[1, 1] + adaboost_validation_table[2, 2]) / sum(adaboost_validation_table)
  adaboost_validation_accuracy_mean <- mean(adaboost_validation_accuracy)
  adaboost_validation_F1_score[i] <- 2 * (adaboost_validation_table[2, 2]) / sum(2 * adaboost_validation_table[2, 2] + adaboost_validation_table[1, 2] + adaboost_validation_table[2, 1])
  adaboost_validation_F1_score_mean <- mean(adaboost_validation_F1_score)
  adaboost_validation_positive_predictive_value[i] <- adaboost_validation_table[2, 2] / sum(adaboost_validation_table[2, 2] + adaboost_validation_table[2, 1])
  adaboost_validation_positive_predictive_value_mean <- mean(adaboost_validation_positive_predictive_value)
  adaboost_validation_negative_predictive_value[i] <- adaboost_validation_table[1, 1] / sum(adaboost_validation_table[1, 1] + adaboost_validation_table[1, 2])
  adaboost_validation_negative_predictive_value_mean <- mean(adaboost_validation_negative_predictive_value)

  adaboost_holdout_true_positive_rate[i] <- (adaboost_test_true_positive_rate[i] + adaboost_validation_true_positive_rate[i]) / 2
  adaboost_holdout_true_positive_rate_mean <- mean(adaboost_holdout_true_positive_rate)
  adaboost_holdout_true_negative_rate[i] <- (adaboost_test_true_negative_rate[i] + adaboost_validation_true_negative_rate[i]) / 2
  adaboost_holdout_true_negative_rate_mean <- mean(adaboost_holdout_true_negative_rate)
  adaboost_holdout_false_positive_rate[i] <- (adaboost_test_false_positive_rate[i] + adaboost_validation_false_positive_rate[i]) / 2
  adaboost_holdout_false_positive_rate_mean <- mean(adaboost_holdout_false_positive_rate)
  adaboost_holdout_false_negative_rate[i] <- (adaboost_test_false_negative_rate[i] + adaboost_validation_false_negative_rate[i]) / 2
  adaboost_holdout_false_negative_rate_mean <- mean(adaboost_holdout_false_negative_rate)
  adaboost_holdout_accuracy[i] <- (adaboost_test_accuracy[i] + adaboost_validation_accuracy[i]) / 2
  adaboost_holdout_accuracy_mean <- mean(adaboost_holdout_accuracy)
  adaboost_holdout_F1_score[i] <- (adaboost_test_F1_score[i] + adaboost_validation_F1_score[i]) / 2
  adaboost_holdout_F1_score_mean <- mean(adaboost_holdout_F1_score)
  adaboost_holdout_positive_predictive_value[i] <- (adaboost_test_positive_predictive_value[i] + adaboost_validation_positive_predictive_value[i]) / 2
  adaboost_holdout_positive_predictive_value_mean <- mean(adaboost_holdout_positive_predictive_value)
  adaboost_holdout_negative_predictive_value[i] <- (adaboost_test_negative_predictive_value[i] + adaboost_validation_negative_predictive_value[i]) / 2
  adaboost_holdout_negative_predictive_value_mean <- mean(adaboost_holdout_negative_predictive_value)
  adaboost_holdout_overfitting[i] <- adaboost_holdout_accuracy[i] / adaboost_train_accuracy[i]
  adaboost_holdout_overfitting_mean <- mean(adaboost_holdout_overfitting)
  adaboost_holdout_overfitting_range <- range(adaboost_holdout_overfitting)

  adaboost_table <- adaboost_test_table + adaboost_validation_table
  adaboost_table_total <- adaboost_table_total + adaboost_table

  adaboost_end <- Sys.time()
  adaboost_duration[i] <- adaboost_end - adaboost_start
  adaboost_duration_mean <- mean(adaboost_duration)


  #### Bagged Random Forest ####
  bag_rf_start <- Sys.time()

  bag_rf_train_fit <- randomForest(as.factor(y_train) ~ ., data = train01, mtry = ncol(train), importance = TRUE)

  bag_rf_train_pred <- stats::predict(bag_rf_train_fit, train01, type = "prob")
  bag_rf_train_predictions <- ifelse(bag_rf_train_pred > 0.5, 1, 0)
  bag_rf_train_table <- table(bag_rf_train_predictions[, 2], y_train)
  bag_rf_train_true_positive_rate[i] <- bag_rf_train_table[2, 2] / sum(bag_rf_train_table[2, 2] + bag_rf_train_table[1, 2])
  bag_rf_train_true_positive_rate_mean <- mean(bag_rf_train_true_positive_rate)
  bag_rf_train_true_negative_rate[i] <- bag_rf_train_table[1, 1] / sum(bag_rf_train_table[1, 1] + bag_rf_train_table[2, 1])
  bag_rf_train_true_negative_rate_mean <- mean(bag_rf_train_true_negative_rate)
  bag_rf_train_false_positive_rate[i] <- bag_rf_train_table[2, 1] / sum(bag_rf_train_table[2, 1] + bag_rf_train_table[1, 1])
  bag_rf_train_false_positive_rate_mean <- mean(bag_rf_train_false_positive_rate)
  bag_rf_train_false_negative_rate[i] <- bag_rf_train_table[1, 2] / sum(bag_rf_train_table[1, 2] + bag_rf_train_table[2, 2])
  bag_rf_train_false_negative_rate_mean <- mean(bag_rf_train_false_negative_rate)
  bag_rf_train_accuracy[i] <- (bag_rf_train_table[1, 1] + bag_rf_train_table[2, 2]) / sum(bag_rf_train_table)
  bag_rf_train_accuracy_mean <- mean(bag_rf_train_accuracy)
  bag_rf_train_F1_score[i] <- 2 * (bag_rf_train_table[2, 2]) / sum(2 * bag_rf_train_table[2, 2] + bag_rf_train_table[1, 2] + bag_rf_train_table[2, 1])
  bag_rf_train_F1_score_mean <- mean(bag_rf_train_F1_score)
  bag_rf_train_positive_predictive_value[i] <- bag_rf_train_table[2, 2] / sum(bag_rf_train_table[2, 2] + bag_rf_train_table[2, 1])
  bag_rf_train_positive_predictive_value_mean <- mean(bag_rf_train_positive_predictive_value)
  bag_rf_train_negative_predictive_value[i] <- bag_rf_train_table[1, 1] / sum(bag_rf_train_table[1, 1] + bag_rf_train_table[1, 2])
  bag_rf_train_negative_predictive_value_mean <- mean(bag_rf_train_negative_predictive_value)

  bag_rf_test_pred <- stats::predict(bag_rf_train_fit, test01, type = "prob")
  bag_rf_test_predictions <- ifelse(bag_rf_test_pred > 0.5, 1, 0)
  bag_rf_test_table <- table(bag_rf_test_predictions[, 2], y_test)
  bag_rf_test_true_positive_rate[i] <- bag_rf_test_table[2, 2] / sum(bag_rf_test_table[2, 2] + bag_rf_test_table[1, 2])
  bag_rf_test_true_positive_rate_mean <- mean(bag_rf_test_true_positive_rate)
  bag_rf_test_true_negative_rate[i] <- bag_rf_test_table[1, 1] / sum(bag_rf_test_table[1, 1] + bag_rf_test_table[2, 1])
  bag_rf_test_true_negative_rate_mean <- mean(bag_rf_test_true_negative_rate)
  bag_rf_test_false_positive_rate[i] <- bag_rf_test_table[2, 1] / sum(bag_rf_test_table[2, 1] + bag_rf_test_table[1, 1])
  bag_rf_test_false_positive_rate_mean <- mean(bag_rf_test_false_positive_rate)
  bag_rf_test_false_negative_rate[i] <- bag_rf_test_table[1, 2] / sum(bag_rf_test_table[1, 2] + bag_rf_test_table[2, 2])
  bag_rf_test_false_negative_rate_mean <- mean(bag_rf_test_false_negative_rate)
  bag_rf_test_accuracy[i] <- (bag_rf_test_table[1, 1] + bag_rf_test_table[2, 2]) / sum(bag_rf_test_table)
  bag_rf_test_accuracy_mean <- mean(bag_rf_test_accuracy)
  bag_rf_test_F1_score[i] <- 2 * (bag_rf_test_table[2, 2]) / sum(2 * bag_rf_test_table[2, 2] + bag_rf_test_table[1, 2] + bag_rf_test_table[2, 1])
  bag_rf_test_F1_score_mean <- mean(bag_rf_test_F1_score)
  bag_rf_test_positive_predictive_value[i] <- bag_rf_test_table[2, 2] / sum(bag_rf_test_table[2, 2] + bag_rf_test_table[2, 1])
  bag_rf_test_positive_predictive_value_mean <- mean(bag_rf_test_positive_predictive_value)
  bag_rf_test_negative_predictive_value[i] <- bag_rf_test_table[1, 1] / sum(bag_rf_test_table[1, 1] + bag_rf_test_table[1, 2])
  bag_rf_test_negative_predictive_value_mean <- mean(bag_rf_test_negative_predictive_value)

  bag_rf_validation_pred <- stats::predict(bag_rf_train_fit, validation01, type = "prob")
  bag_rf_validation_predictions <- ifelse(bag_rf_validation_pred > 0.5, 1, 0)
  bag_rf_validation_table <- table(bag_rf_validation_predictions[, 2], y_validation)
  bag_rf_validation_true_positive_rate[i] <- bag_rf_validation_table[2, 2] / sum(bag_rf_validation_table[2, 2] + bag_rf_validation_table[1, 2])
  bag_rf_validation_true_positive_rate_mean <- mean(bag_rf_validation_true_positive_rate)
  bag_rf_validation_true_negative_rate[i] <- bag_rf_validation_table[1, 1] / sum(bag_rf_validation_table[1, 1] + bag_rf_validation_table[2, 1])
  bag_rf_validation_true_negative_rate_mean <- mean(bag_rf_validation_true_negative_rate)
  bag_rf_validation_false_positive_rate[i] <- bag_rf_validation_table[2, 1] / sum(bag_rf_validation_table[2, 1] + bag_rf_validation_table[1, 1])
  bag_rf_validation_false_positive_rate_mean <- mean(bag_rf_validation_false_positive_rate)
  bag_rf_validation_false_negative_rate[i] <- bag_rf_validation_table[1, 2] / sum(bag_rf_validation_table[1, 2] + bag_rf_validation_table[2, 2])
  bag_rf_validation_false_negative_rate_mean <- mean(bag_rf_validation_false_negative_rate)
  bag_rf_validation_accuracy[i] <- (bag_rf_validation_table[1, 1] + bag_rf_validation_table[2, 2]) / sum(bag_rf_validation_table)
  bag_rf_validation_accuracy_mean <- mean(bag_rf_validation_accuracy)
  bag_rf_validation_F1_score[i] <- 2 * (bag_rf_validation_table[2, 2]) / sum(2 * bag_rf_validation_table[2, 2] + bag_rf_validation_table[1, 2] + bag_rf_validation_table[2, 1])
  bag_rf_validation_F1_score_mean <- mean(bag_rf_validation_F1_score)
  bag_rf_validation_positive_predictive_value[i] <- bag_rf_validation_table[2, 2] / sum(bag_rf_validation_table[2, 2] + bag_rf_validation_table[2, 1])
  bag_rf_validation_positive_predictive_value_mean <- mean(bag_rf_validation_positive_predictive_value)
  bag_rf_validation_negative_predictive_value[i] <- bag_rf_validation_table[1, 1] / sum(bag_rf_validation_table[1, 1] + bag_rf_validation_table[1, 2])
  bag_rf_validation_negative_predictive_value_mean <- mean(bag_rf_validation_negative_predictive_value)

  bag_rf_holdout_true_positive_rate[i] <- (bag_rf_test_true_positive_rate[i] + bag_rf_validation_true_positive_rate[i]) / 2
  bag_rf_holdout_true_positive_rate_mean <- mean(bag_rf_holdout_true_positive_rate)
  bag_rf_holdout_true_negative_rate[i] <- (bag_rf_test_true_negative_rate[i] + bag_rf_validation_true_negative_rate[i]) / 2
  bag_rf_holdout_true_negative_rate_mean <- mean(bag_rf_holdout_true_negative_rate)
  bag_rf_holdout_false_positive_rate[i] <- (bag_rf_test_false_positive_rate[i] + bag_rf_validation_false_positive_rate[i]) / 2
  bag_rf_holdout_false_positive_rate_mean <- mean(bag_rf_holdout_false_positive_rate)
  bag_rf_holdout_false_negative_rate[i] <- (bag_rf_test_false_negative_rate[i] + bag_rf_validation_false_negative_rate[i]) / 2
  bag_rf_holdout_false_negative_rate_mean <- mean(bag_rf_holdout_false_negative_rate)
  bag_rf_holdout_accuracy[i] <- (bag_rf_test_accuracy[i] + bag_rf_validation_accuracy[i]) / 2
  bag_rf_holdout_accuracy_mean <- mean(bag_rf_holdout_accuracy)
  bag_rf_holdout_F1_score[i] <- (bag_rf_test_F1_score[i] + bag_rf_validation_F1_score[i]) / 2
  bag_rf_holdout_F1_score_mean <- mean(bag_rf_holdout_F1_score)
  bag_rf_holdout_positive_predictive_value[i] <- (bag_rf_test_positive_predictive_value[i] + bag_rf_validation_positive_predictive_value[i]) / 2
  bag_rf_holdout_positive_predictive_value_mean <- mean(bag_rf_holdout_positive_predictive_value)
  bag_rf_holdout_negative_predictive_value[i] <- (bag_rf_test_negative_predictive_value[i] + bag_rf_validation_negative_predictive_value[i]) / 2
  bag_rf_holdout_negative_predictive_value_mean <- mean(bag_rf_holdout_negative_predictive_value)
  bag_rf_holdout_overfitting[i] <- bag_rf_holdout_accuracy[i] / bag_rf_train_accuracy[i]
  bag_rf_holdout_overfitting_mean <- mean(bag_rf_holdout_overfitting)
  bag_rf_holdout_overfitting_range <- range(bag_rf_holdout_overfitting)

  bag_rf_table <- bag_rf_test_table + bag_rf_validation_table
  bag_rf_table_total <-  bag_rf_table_total + bag_rf_table

  bag_rf_end <- Sys.time()
  bag_rf_duration[i] <- bag_rf_end - bag_rf_start
  bag_rf_duration_mean <- mean(bag_rf_duration)


  #### Bagging ####
  bagging_start <- Sys.time()

  bagging_train_fit <- ipred::bagging(as.factor(y) ~ ., data = train01)

  bagging_train_pred <- stats::predict(bagging_train_fit, train01, type = "prob")
  bagging_train_predictions <- ifelse(bagging_train_pred > 0.5, 1, 0)
  bagging_train_table <- table(bagging_train_predictions[, 2], y_train)
  bagging_train_true_positive_rate[i] <- bagging_train_table[2, 2] / sum(bagging_train_table[2, 2] + bagging_train_table[1, 2])
  bagging_train_true_positive_rate_mean <- mean(bagging_train_true_positive_rate)
  bagging_train_true_negative_rate[i] <- bagging_train_table[1, 1] / sum(bagging_train_table[1, 1] + bagging_train_table[2, 1])
  bagging_train_true_negative_rate_mean <- mean(bagging_train_true_negative_rate)
  bagging_train_false_positive_rate[i] <- bagging_train_table[2, 1] / sum(bagging_train_table[2, 1] + bagging_train_table[1, 1])
  bagging_train_false_positive_rate_mean <- mean(bagging_train_false_positive_rate)
  bagging_train_false_negative_rate[i] <- bagging_train_table[1, 2] / sum(bagging_train_table[1, 2] + bagging_train_table[2, 2])
  bagging_train_false_negative_rate_mean <- mean(bagging_train_false_negative_rate)
  bagging_train_accuracy[i] <- (bagging_train_table[1, 1] + bagging_train_table[2, 2]) / sum(bagging_train_table)
  bagging_train_accuracy_mean <- mean(bagging_train_accuracy)
  bagging_train_F1_score[i] <- 2 * (bagging_train_table[2, 2]) / sum(2 * bagging_train_table[2, 2] + bagging_train_table[1, 2] + bagging_train_table[2, 1])
  bagging_train_F1_score_mean <- mean(bagging_train_F1_score)
  bagging_train_positive_predictive_value[i] <- bagging_train_table[2, 2] / sum(bagging_train_table[2, 2] + bagging_train_table[2, 1])
  bagging_train_positive_predictive_value_mean <- mean(bagging_train_positive_predictive_value)
  bagging_train_negative_predictive_value[i] <- bagging_train_table[1, 1] / sum(bagging_train_table[1, 1] + bagging_train_table[1, 2])
  bagging_train_negative_predictive_value_mean <- mean(bagging_train_negative_predictive_value)

  bagging_test_pred <- stats::predict(bagging_train_fit, test01, type = "prob")
  bagging_test_predictions <- ifelse(bagging_test_pred > 0.5, 1, 0)
  bagging_test_table <- table(bagging_test_predictions[, 2], y_test)
  bagging_test_true_positive_rate[i] <- bagging_test_table[2, 2] / sum(bagging_test_table[2, 2] + bagging_test_table[1, 2])
  bagging_test_true_positive_rate_mean <- mean(bagging_test_true_positive_rate)
  bagging_test_true_negative_rate[i] <- bagging_test_table[1, 1] / sum(bagging_test_table[1, 1] + bagging_test_table[2, 1])
  bagging_test_true_negative_rate_mean <- mean(bagging_test_true_negative_rate)
  bagging_test_false_positive_rate[i] <- bagging_test_table[2, 1] / sum(bagging_test_table[2, 1] + bagging_test_table[1, 1])
  bagging_test_false_positive_rate_mean <- mean(bagging_test_false_positive_rate)
  bagging_test_false_negative_rate[i] <- bagging_test_table[1, 2] / sum(bagging_test_table[1, 2] + bagging_test_table[2, 2])
  bagging_test_false_negative_rate_mean <- mean(bagging_test_false_negative_rate)
  bagging_test_accuracy[i] <- (bagging_test_table[1, 1] + bagging_test_table[2, 2]) / sum(bagging_test_table)
  bagging_test_accuracy_mean <- mean(bagging_test_accuracy)
  bagging_test_F1_score[i] <- 2 * (bagging_test_table[2, 2]) / sum(2 * bagging_test_table[2, 2] + bagging_test_table[1, 2] + bagging_test_table[2, 1])
  bagging_test_F1_score_mean <- mean(bagging_test_F1_score)
  bagging_test_positive_predictive_value[i] <- bagging_test_table[2, 2] / sum(bagging_test_table[2, 2] + bagging_test_table[2, 1])
  bagging_test_positive_predictive_value_mean <- mean(bagging_test_positive_predictive_value)
  bagging_test_negative_predictive_value[i] <- bagging_test_table[1, 1] / sum(bagging_test_table[1, 1] + bagging_test_table[1, 2])
  bagging_test_negative_predictive_value_mean <- mean(bagging_test_negative_predictive_value)

  bagging_validation_pred <- stats::predict(bagging_train_fit, validation01, type = "prob")
  bagging_validation_predictions <- ifelse(bagging_validation_pred > 0.5, 1, 0)
  bagging_validation_table <- table(bagging_validation_predictions[, 2], y_validation)
  bagging_validation_true_positive_rate[i] <- bagging_validation_table[2, 2] / sum(bagging_validation_table[2, 2] + bagging_validation_table[1, 2])
  bagging_validation_true_positive_rate_mean <- mean(bagging_validation_true_positive_rate)
  bagging_validation_true_negative_rate[i] <- bagging_validation_table[1, 1] / sum(bagging_validation_table[1, 1] + bagging_validation_table[2, 1])
  bagging_validation_true_negative_rate_mean <- mean(bagging_validation_true_negative_rate)
  bagging_validation_false_positive_rate[i] <- bagging_validation_table[2, 1] / sum(bagging_validation_table[2, 1] + bagging_validation_table[1, 1])
  bagging_validation_false_positive_rate_mean <- mean(bagging_validation_false_positive_rate)
  bagging_validation_false_negative_rate[i] <- bagging_validation_table[1, 2] / sum(bagging_validation_table[1, 2] + bagging_validation_table[2, 2])
  bagging_validation_false_negative_rate_mean <- mean(bagging_validation_false_negative_rate)
  bagging_validation_accuracy[i] <- (bagging_validation_table[1, 1] + bagging_validation_table[2, 2]) / sum(bagging_validation_table)
  bagging_validation_accuracy_mean <- mean(bagging_validation_accuracy)
  bagging_validation_F1_score[i] <- 2 * (bagging_validation_table[2, 2]) / sum(2 * bagging_validation_table[2, 2] + bagging_validation_table[1, 2] + bagging_validation_table[2, 1])
  bagging_validation_F1_score_mean <- mean(bagging_validation_F1_score)
  bagging_validation_positive_predictive_value[i] <- bagging_validation_table[2, 2] / sum(bagging_validation_table[2, 2] + bagging_validation_table[2, 1])
  bagging_validation_positive_predictive_value_mean <- mean(bagging_validation_positive_predictive_value)
  bagging_validation_negative_predictive_value[i] <- bagging_validation_table[1, 1] / sum(bagging_validation_table[1, 1] + bagging_validation_table[1, 2])
  bagging_validation_negative_predictive_value_mean <- mean(bagging_validation_negative_predictive_value)

  bagging_holdout_true_positive_rate[i] <- (bagging_test_true_positive_rate[i] + bagging_validation_true_positive_rate[i]) / 2
  bagging_holdout_true_positive_rate_mean <- mean(bagging_holdout_true_positive_rate)
  bagging_holdout_true_negative_rate[i] <- (bagging_test_true_negative_rate[i] + bagging_validation_true_negative_rate[i]) / 2
  bagging_holdout_true_negative_rate_mean <- mean(bagging_holdout_true_negative_rate)
  bagging_holdout_false_positive_rate[i] <- (bagging_test_false_positive_rate[i] + bagging_validation_false_positive_rate[i]) / 2
  bagging_holdout_false_positive_rate_mean <- mean(bagging_holdout_false_positive_rate)
  bagging_holdout_false_negative_rate[i] <- (bagging_test_false_negative_rate[i] + bagging_validation_false_negative_rate[i]) / 2
  bagging_holdout_false_negative_rate_mean <- mean(bagging_holdout_false_negative_rate)
  bagging_holdout_accuracy[i] <- (bagging_test_accuracy[i] + bagging_validation_accuracy[i]) / 2
  bagging_holdout_accuracy_mean <- mean(bagging_holdout_accuracy)
  bagging_holdout_F1_score[i] <- (bagging_test_F1_score[i] + bagging_validation_F1_score[i]) / 2
  bagging_holdout_F1_score_mean <- mean(bagging_holdout_F1_score)
  bagging_holdout_positive_predictive_value[i] <- (bagging_test_positive_predictive_value[i] + bagging_validation_positive_predictive_value[i]) / 2
  bagging_holdout_positive_predictive_value_mean <- mean(bagging_holdout_positive_predictive_value)
  bagging_holdout_negative_predictive_value[i] <- (bagging_test_negative_predictive_value[i] + bagging_validation_negative_predictive_value[i]) / 2
  bagging_holdout_negative_predictive_value_mean <- mean(bagging_holdout_negative_predictive_value)
  bagging_holdout_overfitting[i] <- bagging_holdout_accuracy[i] / bagging_train_accuracy[i]
  bagging_holdout_overfitting_mean <- mean(bagging_holdout_overfitting)
  bagging_holdout_overfitting_range <- range(bagging_holdout_overfitting)

  bagging_table <- bagging_test_table + bagging_validation_table
  bagging_table_total <- bagging_table_total + bagging_table

  bagging_end <- Sys.time()
  bagging_duration[i] <- bagging_end - bagging_start
  bagging_duration_mean <- mean(bagging_duration)


  #### bayesglm ####
  bayesglm_start <- Sys.time()

  bayesglm_train_fit <- arm::bayesglm(y ~ ., data = train, family = binomial)

  bayesglm_train_pred <- stats::predict(bayesglm_train_fit, train01, type = "response")
  bayesglm_train_predictions <- ifelse(bayesglm_train_pred > 0.5, 1, 0)
  bayesglm_train_table <- table(bayesglm_train_predictions, y_train)
  bayesglm_train_true_positive_rate[i] <- bayesglm_train_table[2, 2] / sum(bayesglm_train_table[2, 2] + bayesglm_train_table[1, 2])
  bayesglm_train_true_positive_rate_mean <- mean(bayesglm_train_true_positive_rate)
  bayesglm_train_true_negative_rate[i] <- bayesglm_train_table[1, 1] / sum(bayesglm_train_table[1, 1] + bayesglm_train_table[2, 1])
  bayesglm_train_true_negative_rate_mean <- mean(bayesglm_train_true_negative_rate)
  bayesglm_train_false_positive_rate[i] <- bayesglm_train_table[2, 1] / sum(bayesglm_train_table[2, 1] + bayesglm_train_table[1, 1])
  bayesglm_train_false_positive_rate_mean <- mean(bayesglm_train_false_positive_rate)
  bayesglm_train_false_negative_rate[i] <- bayesglm_train_table[1, 2] / sum(bayesglm_train_table[1, 2] + bayesglm_train_table[2, 2])
  bayesglm_train_false_negative_rate_mean <- mean(bayesglm_train_false_negative_rate)
  bayesglm_train_accuracy[i] <- (bayesglm_train_table[1, 1] + bayesglm_train_table[2, 2]) / sum(bayesglm_train_table)
  bayesglm_train_accuracy_mean <- mean(bayesglm_train_accuracy)
  bayesglm_train_F1_score[i] <- 2 * (bayesglm_train_table[2, 2]) / sum(2 * bayesglm_train_table[2, 2] + bayesglm_train_table[1, 2] + bayesglm_train_table[2, 1])
  bayesglm_train_F1_score_mean <- mean(bayesglm_train_F1_score)
  bayesglm_train_positive_predictive_value[i] <- bayesglm_train_table[2, 2] / sum(bayesglm_train_table[2, 2] + bayesglm_train_table[2, 1])
  bayesglm_train_positive_predictive_value_mean <- mean(bayesglm_train_positive_predictive_value)
  bayesglm_train_negative_predictive_value[i] <- bayesglm_train_table[1, 1] / sum(bayesglm_train_table[1, 1] + bayesglm_train_table[1, 2])
  bayesglm_train_negative_predictive_value_mean <- mean(bayesglm_train_negative_predictive_value)

  bayesglm_test_pred <- stats::predict(bayesglm_train_fit, test01, type = "response")
  bayesglm_test_predictions <- ifelse(bayesglm_test_pred > 0.5, 1, 0)
  bayesglm_test_table <- table(bayesglm_test_predictions, y_test)
  bayesglm_test_true_positive_rate[i] <- bayesglm_test_table[2, 2] / sum(bayesglm_test_table[2, 2] + bayesglm_test_table[1, 2])
  bayesglm_test_true_positive_rate_mean <- mean(bayesglm_test_true_positive_rate)
  bayesglm_test_true_negative_rate[i] <- bayesglm_test_table[1, 1] / sum(bayesglm_test_table[1, 1] + bayesglm_test_table[2, 1])
  bayesglm_test_true_negative_rate_mean <- mean(bayesglm_test_true_negative_rate)
  bayesglm_test_false_positive_rate[i] <- bayesglm_test_table[2, 1] / sum(bayesglm_test_table[2, 1] + bayesglm_test_table[1, 1])
  bayesglm_test_false_positive_rate_mean <- mean(bayesglm_test_false_positive_rate)
  bayesglm_test_false_negative_rate[i] <- bayesglm_test_table[1, 2] / sum(bayesglm_test_table[1, 2] + bayesglm_test_table[2, 2])
  bayesglm_test_false_negative_rate_mean <- mean(bayesglm_test_false_negative_rate)
  bayesglm_test_accuracy[i] <- (bayesglm_test_table[1, 1] + bayesglm_test_table[2, 2]) / sum(bayesglm_test_table)
  bayesglm_test_accuracy_mean <- mean(bayesglm_test_accuracy)
  bayesglm_test_F1_score[i] <- 2 * (bayesglm_test_table[2, 2]) / sum(2 * bayesglm_test_table[2, 2] + bayesglm_test_table[1, 2] + bayesglm_test_table[2, 1])
  bayesglm_test_F1_score_mean <- mean(bayesglm_test_F1_score)
  bayesglm_test_positive_predictive_value[i] <- bayesglm_test_table[2, 2] / sum(bayesglm_test_table[2, 2] + bayesglm_test_table[2, 1])
  bayesglm_test_positive_predictive_value_mean <- mean(bayesglm_test_positive_predictive_value)
  bayesglm_test_negative_predictive_value[i] <- bayesglm_test_table[1, 1] / sum(bayesglm_test_table[1, 1] + bayesglm_test_table[1, 2])
  bayesglm_test_negative_predictive_value_mean <- mean(bayesglm_test_negative_predictive_value)

  bayesglm_validation_pred <- stats::predict(bayesglm_train_fit, validation01, type = "response")
  bayesglm_validation_predictions <- ifelse(bayesglm_validation_pred > 0.5, 1, 0)
  bayesglm_validation_table <- table(bayesglm_validation_predictions, y_validation)
  bayesglm_validation_true_positive_rate[i] <- bayesglm_validation_table[2, 2] / sum(bayesglm_validation_table[2, 2] + bayesglm_validation_table[1, 2])
  bayesglm_validation_true_positive_rate_mean <- mean(bayesglm_validation_true_positive_rate)
  bayesglm_validation_true_negative_rate[i] <- bayesglm_validation_table[1, 1] / sum(bayesglm_validation_table[1, 1] + bayesglm_validation_table[2, 1])
  bayesglm_validation_true_negative_rate_mean <- mean(bayesglm_validation_true_negative_rate)
  bayesglm_validation_false_positive_rate[i] <- bayesglm_validation_table[2, 1] / sum(bayesglm_validation_table[2, 1] + bayesglm_validation_table[1, 1])
  bayesglm_validation_false_positive_rate_mean <- mean(bayesglm_validation_false_positive_rate)
  bayesglm_validation_false_negative_rate[i] <- bayesglm_validation_table[1, 2] / sum(bayesglm_validation_table[1, 2] + bayesglm_validation_table[2, 2])
  bayesglm_validation_false_negative_rate_mean <- mean(bayesglm_validation_false_negative_rate)
  bayesglm_validation_accuracy[i] <- (bayesglm_validation_table[1, 1] + bayesglm_validation_table[2, 2]) / sum(bayesglm_validation_table)
  bayesglm_validation_accuracy_mean <- mean(bayesglm_validation_accuracy)
  bayesglm_validation_F1_score[i] <- 2 * (bayesglm_validation_table[2, 2]) / sum(2 * bayesglm_validation_table[2, 2] + bayesglm_validation_table[1, 2] + bayesglm_validation_table[2, 1])
  bayesglm_validation_F1_score_mean <- mean(bayesglm_validation_F1_score)
  bayesglm_validation_positive_predictive_value[i] <- bayesglm_validation_table[2, 2] / sum(bayesglm_validation_table[2, 2] + bayesglm_validation_table[2, 1])
  bayesglm_validation_positive_predictive_value_mean <- mean(bayesglm_validation_positive_predictive_value)
  bayesglm_validation_negative_predictive_value[i] <- bayesglm_validation_table[1, 1] / sum(bayesglm_validation_table[1, 1] + bayesglm_validation_table[1, 2])
  bayesglm_validation_negative_predictive_value_mean <- mean(bayesglm_validation_negative_predictive_value)

  bayesglm_holdout_true_positive_rate[i] <- (bayesglm_test_true_positive_rate[i] + bayesglm_validation_true_positive_rate[i]) / 2
  bayesglm_holdout_true_positive_rate_mean <- mean(bayesglm_holdout_true_positive_rate)
  bayesglm_holdout_true_negative_rate[i] <- (bayesglm_test_true_negative_rate[i] + bayesglm_validation_true_negative_rate[i]) / 2
  bayesglm_holdout_true_negative_rate_mean <- mean(bayesglm_holdout_true_negative_rate)
  bayesglm_holdout_false_positive_rate[i] <- (bayesglm_test_false_positive_rate[i] + bayesglm_validation_false_positive_rate[i]) / 2
  bayesglm_holdout_false_positive_rate_mean <- mean(bayesglm_holdout_false_positive_rate)
  bayesglm_holdout_false_negative_rate[i] <- (bayesglm_test_false_negative_rate[i] + bayesglm_validation_false_negative_rate[i]) / 2
  bayesglm_holdout_false_negative_rate_mean <- mean(bayesglm_holdout_false_negative_rate)
  bayesglm_holdout_accuracy[i] <- (bayesglm_test_accuracy[i] + bayesglm_validation_accuracy[i]) / 2
  bayesglm_holdout_accuracy_mean <- mean(bayesglm_holdout_accuracy)
  bayesglm_holdout_F1_score[i] <- (bayesglm_test_F1_score[i] + bayesglm_validation_F1_score[i]) / 2
  bayesglm_holdout_F1_score_mean <- mean(bayesglm_holdout_F1_score)
  bayesglm_holdout_positive_predictive_value[i] <- (bayesglm_test_positive_predictive_value[i] + bayesglm_validation_positive_predictive_value[i]) / 2
  bayesglm_holdout_positive_predictive_value_mean <- mean(bayesglm_holdout_positive_predictive_value)
  bayesglm_holdout_negative_predictive_value[i] <- (bayesglm_test_negative_predictive_value[i] + bayesglm_validation_negative_predictive_value[i]) / 2
  bayesglm_holdout_negative_predictive_value_mean <- mean(bayesglm_holdout_negative_predictive_value)
  bayesglm_holdout_overfitting[i] <- bayesglm_holdout_accuracy[i] / bayesglm_train_accuracy[i]
  bayesglm_holdout_overfitting_mean <- mean(bayesglm_holdout_overfitting)
  bayesglm_holdout_overfitting_range <- range(bayesglm_holdout_overfitting)

  bayesglm_table <- bayesglm_test_table + bayesglm_validation_table
  bayesglm_table_total <- bayesglm_table_total + bayesglm_table

  bayesglm_end <- Sys.time()
  bayesglm_duration[i] <- bayesglm_end - bayesglm_start
  bayesglm_duration_mean <- mean(bayesglm_duration)


  #### bayesrnn ####
  bayesrnn_start <- Sys.time()

  bayesrnn_train_fit <- brnn::brnn(x = as.matrix(train), y = as.vector(train$y), neurons = 10, verbose = TRUE)

  bayesrnn_train_pred <- stats::predict(bayesrnn_train_fit, train01, type = "prob")
  bayesrnn_train_predictions <- ifelse(bayesrnn_train_pred > 0.5, 1, 0)
  bayesrnn_train_table <- table(bayesrnn_train_predictions, y_train)
  bayesrnn_train_true_positive_rate[i] <- bayesrnn_train_table[2, 2] / sum(bayesrnn_train_table[2, 2] + bayesrnn_train_table[1, 2])
  bayesrnn_train_true_positive_rate_mean <- mean(bayesrnn_train_true_positive_rate)
  bayesrnn_train_true_negative_rate[i] <- bayesrnn_train_table[1, 1] / sum(bayesrnn_train_table[1, 1] + bayesrnn_train_table[2, 1])
  bayesrnn_train_true_negative_rate_mean <- mean(bayesrnn_train_true_negative_rate)
  bayesrnn_train_false_positive_rate[i] <- bayesrnn_train_table[2, 1] / sum(bayesrnn_train_table[2, 1] + bayesrnn_train_table[1, 1])
  bayesrnn_train_false_positive_rate_mean <- mean(bayesrnn_train_false_positive_rate)
  bayesrnn_train_false_negative_rate[i] <- bayesrnn_train_table[1, 2] / sum(bayesrnn_train_table[1, 2] + bayesrnn_train_table[2, 2])
  bayesrnn_train_false_negative_rate_mean <- mean(bayesrnn_train_false_negative_rate)
  bayesrnn_train_accuracy[i] <- (bayesrnn_train_table[1, 1] + bayesrnn_train_table[2, 2]) / sum(bayesrnn_train_table)
  bayesrnn_train_accuracy_mean <- mean(bayesrnn_train_accuracy)
  bayesrnn_train_F1_score[i] <- 2 * (bayesrnn_train_table[2, 2]) / sum(2 * bayesrnn_train_table[2, 2] + bayesrnn_train_table[1, 2] + bayesrnn_train_table[2, 1])
  bayesrnn_train_F1_score_mean <- mean(bayesrnn_train_F1_score)
  bayesrnn_train_positive_predictive_value[i] <- bayesrnn_train_table[2, 2] / sum(bayesrnn_train_table[2, 2] + bayesrnn_train_table[2, 1])
  bayesrnn_train_positive_predictive_value_mean <- mean(bayesrnn_train_positive_predictive_value)
  bayesrnn_train_negative_predictive_value[i] <- bayesrnn_train_table[1, 1] / sum(bayesrnn_train_table[1, 1] + bayesrnn_train_table[1, 2])
  bayesrnn_train_negative_predictive_value_mean <- mean(bayesrnn_train_negative_predictive_value)

  bayesrnn_test_pred <- stats::predict(bayesrnn_train_fit, test01, type = "prob")
  bayesrnn_test_predictions <- ifelse(bayesrnn_test_pred > 0.5, 1, 0)
  bayesrnn_test_table <- table(bayesrnn_test_predictions, y_test)
  bayesrnn_test_true_positive_rate[i] <- bayesrnn_test_table[2, 2] / sum(bayesrnn_test_table[2, 2] + bayesrnn_test_table[1, 2])
  bayesrnn_test_true_positive_rate_mean <- mean(bayesrnn_test_true_positive_rate)
  bayesrnn_test_true_negative_rate[i] <- bayesrnn_test_table[1, 1] / sum(bayesrnn_test_table[1, 1] + bayesrnn_test_table[2, 1])
  bayesrnn_test_true_negative_rate_mean <- mean(bayesrnn_test_true_negative_rate)
  bayesrnn_test_false_positive_rate[i] <- bayesrnn_test_table[2, 1] / sum(bayesrnn_test_table[2, 1] + bayesrnn_test_table[1, 1])
  bayesrnn_test_false_positive_rate_mean <- mean(bayesrnn_test_false_positive_rate)
  bayesrnn_test_false_negative_rate[i] <- bayesrnn_test_table[1, 2] / sum(bayesrnn_test_table[1, 2] + bayesrnn_test_table[2, 2])
  bayesrnn_test_false_negative_rate_mean <- mean(bayesrnn_test_false_negative_rate)
  bayesrnn_test_accuracy[i] <- (bayesrnn_test_table[1, 1] + bayesrnn_test_table[2, 2]) / sum(bayesrnn_test_table)
  bayesrnn_test_accuracy_mean <- mean(bayesrnn_test_accuracy)
  bayesrnn_test_F1_score[i] <- 2 * (bayesrnn_test_table[2, 2]) / sum(2 * bayesrnn_test_table[2, 2] + bayesrnn_test_table[1, 2] + bayesrnn_test_table[2, 1])
  bayesrnn_test_F1_score_mean <- mean(bayesrnn_test_F1_score)
  bayesrnn_test_positive_predictive_value[i] <- bayesrnn_test_table[2, 2] / sum(bayesrnn_test_table[2, 2] + bayesrnn_test_table[2, 1])
  bayesrnn_test_positive_predictive_value_mean <- mean(bayesrnn_test_positive_predictive_value)
  bayesrnn_test_negative_predictive_value[i] <- bayesrnn_test_table[1, 1] / sum(bayesrnn_test_table[1, 1] + bayesrnn_test_table[1, 2])
  bayesrnn_test_negative_predictive_value_mean <- mean(bayesrnn_test_negative_predictive_value)

  bayesrnn_validation_pred <- stats::predict(bayesrnn_train_fit, validation01, type = "prob")
  bayesrnn_validation_predictions <- ifelse(bayesrnn_validation_pred > 0.5, 1, 0)
  bayesrnn_validation_table <- table(bayesrnn_validation_predictions, y_validation)
  bayesrnn_validation_true_positive_rate[i] <- bayesrnn_validation_table[2, 2] / sum(bayesrnn_validation_table[2, 2] + bayesrnn_validation_table[1, 2])
  bayesrnn_validation_true_positive_rate_mean <- mean(bayesrnn_validation_true_positive_rate)
  bayesrnn_validation_true_negative_rate[i] <- bayesrnn_validation_table[1, 1] / sum(bayesrnn_validation_table[1, 1] + bayesrnn_validation_table[2, 1])
  bayesrnn_validation_true_negative_rate_mean <- mean(bayesrnn_validation_true_negative_rate)
  bayesrnn_validation_false_positive_rate[i] <- bayesrnn_validation_table[2, 1] / sum(bayesrnn_validation_table[2, 1] + bayesrnn_validation_table[1, 1])
  bayesrnn_validation_false_positive_rate_mean <- mean(bayesrnn_validation_false_positive_rate)
  bayesrnn_validation_false_negative_rate[i] <- bayesrnn_validation_table[1, 2] / sum(bayesrnn_validation_table[1, 2] + bayesrnn_validation_table[2, 2])
  bayesrnn_validation_false_negative_rate_mean <- mean(bayesrnn_validation_false_negative_rate)
  bayesrnn_validation_accuracy[i] <- (bayesrnn_validation_table[1, 1] + bayesrnn_validation_table[2, 2]) / sum(bayesrnn_validation_table)
  bayesrnn_validation_accuracy_mean <- mean(bayesrnn_validation_accuracy)
  bayesrnn_validation_F1_score[i] <- 2 * (bayesrnn_validation_table[2, 2]) / sum(2 * bayesrnn_validation_table[2, 2] + bayesrnn_validation_table[1, 2] + bayesrnn_validation_table[2, 1])
  bayesrnn_validation_F1_score_mean <- mean(bayesrnn_validation_F1_score)
  bayesrnn_validation_positive_predictive_value[i] <- bayesrnn_validation_table[2, 2] / sum(bayesrnn_validation_table[2, 2] + bayesrnn_validation_table[2, 1])
  bayesrnn_validation_positive_predictive_value_mean <- mean(bayesrnn_validation_positive_predictive_value)
  bayesrnn_validation_negative_predictive_value[i] <- bayesrnn_validation_table[1, 1] / sum(bayesrnn_validation_table[1, 1] + bayesrnn_validation_table[1, 2])
  bayesrnn_validation_negative_predictive_value_mean <- mean(bayesrnn_validation_negative_predictive_value)

  bayesrnn_holdout_true_positive_rate[i] <- (bayesrnn_test_true_positive_rate[i] + bayesrnn_validation_true_positive_rate[i]) / 2
  bayesrnn_holdout_true_positive_rate_mean <- mean(bayesrnn_holdout_true_positive_rate)
  bayesrnn_holdout_true_negative_rate[i] <- (bayesrnn_test_true_negative_rate[i] + bayesrnn_validation_true_negative_rate[i]) / 2
  bayesrnn_holdout_true_negative_rate_mean <- mean(bayesrnn_holdout_true_negative_rate)
  bayesrnn_holdout_false_positive_rate[i] <- (bayesrnn_test_false_positive_rate[i] + bayesrnn_validation_false_positive_rate[i]) / 2
  bayesrnn_holdout_false_positive_rate_mean <- mean(bayesrnn_holdout_false_positive_rate)
  bayesrnn_holdout_false_negative_rate[i] <- (bayesrnn_test_false_negative_rate[i] + bayesrnn_validation_false_negative_rate[i]) / 2
  bayesrnn_holdout_false_negative_rate_mean <- mean(bayesrnn_holdout_false_negative_rate)
  bayesrnn_holdout_accuracy[i] <- (bayesrnn_test_accuracy[i] + bayesrnn_validation_accuracy[i]) / 2
  bayesrnn_holdout_accuracy_mean <- mean(bayesrnn_holdout_accuracy)
  bayesrnn_holdout_F1_score[i] <- (bayesrnn_test_F1_score[i] + bayesrnn_validation_F1_score[i]) / 2
  bayesrnn_holdout_F1_score_mean <- mean(bayesrnn_holdout_F1_score)
  bayesrnn_holdout_positive_predictive_value[i] <- (bayesrnn_test_positive_predictive_value[i] + bayesrnn_validation_positive_predictive_value[i]) / 2
  bayesrnn_holdout_positive_predictive_value_mean <- mean(bayesrnn_holdout_positive_predictive_value)
  bayesrnn_holdout_negative_predictive_value[i] <- (bayesrnn_test_negative_predictive_value[i] + bayesrnn_validation_negative_predictive_value[i]) / 2
  bayesrnn_holdout_negative_predictive_value_mean <- mean(bayesrnn_holdout_negative_predictive_value)
  bayesrnn_holdout_overfitting[i] <- bayesrnn_holdout_accuracy[i] / bayesrnn_train_accuracy[i]
  bayesrnn_holdout_overfitting_mean <- mean(bayesrnn_holdout_overfitting)
  bayesrnn_holdout_overfitting_range <- range(bayesrnn_holdout_overfitting)

  bayesrnn_table <- bayesrnn_test_table + bayesrnn_validation_table
  bayesrnn_table_total <- bayesrnn_table_total + bayesrnn_table

  bayesrnn_end <- Sys.time()
  bayesrnn_duration[i] <- bayesrnn_end - bayesrnn_start
  bayesrnn_duration_mean <- mean(bayesrnn_duration)


  #### C50 ####
  C50_start <- Sys.time()

  C50_train_fit <- C50::C5.0(as.factor(y_train) ~ ., data = train)

  C50_train_pred <- stats::predict(C50_train_fit, train01, type = "prob")
  C50_train_predictions <- ifelse(C50_train_pred[, 2] > 0.5, 1, 0)
  C50_train_table <- table(C50_train_predictions, y_train)
  C50_train_true_positive_rate[i] <- C50_train_table[2, 2] / sum(C50_train_table[2, 2] + C50_train_table[1, 2])
  C50_train_true_positive_rate_mean <- mean(C50_train_true_positive_rate)
  C50_train_true_negative_rate[i] <- C50_train_table[1, 1] / sum(C50_train_table[1, 1] + C50_train_table[2, 1])
  C50_train_true_negative_rate_mean <- mean(C50_train_true_negative_rate)
  C50_train_false_positive_rate[i] <- C50_train_table[2, 1] / sum(C50_train_table[2, 1] + C50_train_table[1, 1])
  C50_train_false_positive_rate_mean <- mean(C50_train_false_positive_rate)
  C50_train_false_negative_rate[i] <- C50_train_table[1, 2] / sum(C50_train_table[1, 2] + C50_train_table[2, 2])
  C50_train_false_negative_rate_mean <- mean(C50_train_false_negative_rate)
  C50_train_accuracy[i] <- (C50_train_table[1, 1] + C50_train_table[2, 2]) / sum(C50_train_table)
  C50_train_accuracy_mean <- mean(C50_train_accuracy)
  C50_train_F1_score[i] <- 2 * (C50_train_table[2, 2]) / sum(2 * C50_train_table[2, 2] + C50_train_table[1, 2] + C50_train_table[2, 1])
  C50_train_F1_score_mean <- mean(C50_train_F1_score)
  C50_train_positive_predictive_value[i] <- C50_train_table[2, 2] / sum(C50_train_table[2, 2] + C50_train_table[2, 1])
  C50_train_positive_predictive_value_mean <- mean(C50_train_positive_predictive_value)
  C50_train_negative_predictive_value[i] <- C50_train_table[1, 1] / sum(C50_train_table[1, 1] + C50_train_table[1, 2])
  C50_train_negative_predictive_value_mean <- mean(C50_train_negative_predictive_value)

  C50_test_pred <- stats::predict(C50_train_fit, test01, type = "prob")
  C50_test_predictions <- ifelse(C50_test_pred[, 2] > 0.5, 1, 0)
  C50_test_table <- table(C50_test_predictions, y_test)
  C50_test_true_positive_rate[i] <- C50_test_table[2, 2] / sum(C50_test_table[2, 2] + C50_test_table[1, 2])
  C50_test_true_positive_rate_mean <- mean(C50_test_true_positive_rate)
  C50_test_true_negative_rate[i] <- C50_test_table[1, 1] / sum(C50_test_table[1, 1] + C50_test_table[2, 1])
  C50_test_true_negative_rate_mean <- mean(C50_test_true_negative_rate)
  C50_test_false_positive_rate[i] <- C50_test_table[2, 1] / sum(C50_test_table[2, 1] + C50_test_table[1, 1])
  C50_test_false_positive_rate_mean <- mean(C50_test_false_positive_rate)
  C50_test_false_negative_rate[i] <- C50_test_table[1, 2] / sum(C50_test_table[1, 2] + C50_test_table[2, 2])
  C50_test_false_negative_rate_mean <- mean(C50_test_false_negative_rate)
  C50_test_accuracy[i] <- (C50_test_table[1, 1] + C50_test_table[2, 2]) / sum(C50_test_table)
  C50_test_accuracy_mean <- mean(C50_test_accuracy)
  C50_test_F1_score[i] <- 2 * (C50_test_table[2, 2]) / sum(2 * C50_test_table[2, 2] + C50_test_table[1, 2] + C50_test_table[2, 1])
  C50_test_F1_score_mean <- mean(C50_test_F1_score)
  C50_test_positive_predictive_value[i] <- C50_test_table[2, 2] / sum(C50_test_table[2, 2] + C50_test_table[2, 1])
  C50_test_positive_predictive_value_mean <- mean(C50_test_positive_predictive_value)
  C50_test_negative_predictive_value[i] <- C50_test_table[1, 1] / sum(C50_test_table[1, 1] + C50_test_table[1, 2])
  C50_test_negative_predictive_value_mean <- mean(C50_test_negative_predictive_value)

  C50_validation_pred <- stats::predict(C50_train_fit, validation01, type = "prob")
  C50_validation_predictions <- ifelse(C50_validation_pred[, 2] > 0.5, 1, 0)
  C50_validation_table <- table(C50_validation_predictions, y_validation)
  C50_validation_true_positive_rate[i] <- C50_validation_table[2, 2] / sum(C50_validation_table[2, 2] + C50_validation_table[1, 2])
  C50_validation_true_positive_rate_mean <- mean(C50_validation_true_positive_rate)
  C50_validation_true_negative_rate[i] <- C50_validation_table[1, 1] / sum(C50_validation_table[1, 1] + C50_validation_table[2, 1])
  C50_validation_true_negative_rate_mean <- mean(C50_validation_true_negative_rate)
  C50_validation_false_positive_rate[i] <- C50_validation_table[2, 1] / sum(C50_validation_table[2, 1] + C50_validation_table[1, 1])
  C50_validation_false_positive_rate_mean <- mean(C50_validation_false_positive_rate)
  C50_validation_false_negative_rate[i] <- C50_validation_table[1, 2] / sum(C50_validation_table[1, 2] + C50_validation_table[2, 2])
  C50_validation_false_negative_rate_mean <- mean(C50_validation_false_negative_rate)
  C50_validation_accuracy[i] <- (C50_validation_table[1, 1] + C50_validation_table[2, 2]) / sum(C50_validation_table)
  C50_validation_accuracy_mean <- mean(C50_validation_accuracy)
  C50_validation_F1_score[i] <- 2 * (C50_validation_table[2, 2]) / sum(2 * C50_validation_table[2, 2] + C50_validation_table[1, 2] + C50_validation_table[2, 1])
  C50_validation_F1_score_mean <- mean(C50_validation_F1_score)
  C50_validation_positive_predictive_value[i] <- C50_validation_table[2, 2] / sum(C50_validation_table[2, 2] + C50_validation_table[2, 1])
  C50_validation_positive_predictive_value_mean <- mean(C50_validation_positive_predictive_value)
  C50_validation_negative_predictive_value[i] <- C50_validation_table[1, 1] / sum(C50_validation_table[1, 1] + C50_validation_table[1, 2])
  C50_validation_negative_predictive_value_mean <- mean(C50_validation_negative_predictive_value)

  C50_holdout_true_positive_rate[i] <- (C50_test_true_positive_rate[i] + C50_validation_true_positive_rate[i]) / 2
  C50_holdout_true_positive_rate_mean <- mean(C50_holdout_true_positive_rate)
  C50_holdout_true_negative_rate[i] <- (C50_test_true_negative_rate[i] + C50_validation_true_negative_rate[i]) / 2
  C50_holdout_true_negative_rate_mean <- mean(C50_holdout_true_negative_rate)
  C50_holdout_false_positive_rate[i] <- (C50_test_false_positive_rate[i] + C50_validation_false_positive_rate[i]) / 2
  C50_holdout_false_positive_rate_mean <- mean(C50_holdout_false_positive_rate)
  C50_holdout_false_negative_rate[i] <- (C50_test_false_negative_rate[i] + C50_validation_false_negative_rate[i]) / 2
  C50_holdout_false_negative_rate_mean <- mean(C50_holdout_false_negative_rate)
  C50_holdout_accuracy[i] <- (C50_test_accuracy[i] + C50_validation_accuracy[i]) / 2
  C50_holdout_accuracy_mean <- mean(C50_holdout_accuracy)
  C50_holdout_F1_score[i] <- (C50_test_F1_score[i] + C50_validation_F1_score[i]) / 2
  C50_holdout_F1_score_mean <- mean(C50_holdout_F1_score)
  C50_holdout_positive_predictive_value[i] <- (C50_test_positive_predictive_value[i] + C50_validation_positive_predictive_value[i]) / 2
  C50_holdout_positive_predictive_value_mean <- mean(C50_holdout_positive_predictive_value)
  C50_holdout_negative_predictive_value[i] <- (C50_test_negative_predictive_value[i] + C50_validation_negative_predictive_value[i]) / 2
  C50_holdout_negative_predictive_value_mean <- mean(C50_holdout_negative_predictive_value)
  C50_holdout_overfitting[i] <- C50_holdout_accuracy[i] / C50_train_accuracy[i]
  C50_holdout_overfitting_mean <- mean(C50_holdout_overfitting)
  C50_holdout_overfitting_range <- range(C50_holdout_overfitting)

  C50_table <- C50_test_table + C50_validation_table
  C50_table_total <- C50_table_total + C50_table

  C50_end <- Sys.time()
  C50_duration[i] <- C50_end - C50_start
  C50_duration_mean <- mean(C50_duration)


  #### cubist ####
  cubist_start <- Sys.time()

  cubist_train_fit <- Cubist::cubist(x = as.data.frame(train), y = train$y)

  cubist_train_pred <- stats::predict(cubist_train_fit, train01, type = "prob")
  cubist_train_table <- table(cubist_train_pred, y_train)
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

  cubist_test_pred <- stats::predict(cubist_train_fit, test01, type = "prob")
  cubist_test_table <- table(cubist_test_pred, y_test)
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

  cubist_validation_pred <- stats::predict(cubist_train_fit, validation01, type = "prob")
  cubist_validation_table <- table(cubist_validation_pred, y_validation)
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
  cubist_holdout_F1_score[i] <- (cubist_test_F1_score[i] + cubist_validation_F1_score[i]) / 2
  cubist_holdout_F1_score_mean <- mean(cubist_holdout_F1_score)
  cubist_holdout_positive_predictive_value[i] <- (cubist_test_positive_predictive_value[i] + cubist_validation_positive_predictive_value[i]) / 2
  cubist_holdout_positive_predictive_value_mean <- mean(cubist_holdout_positive_predictive_value)
  cubist_holdout_negative_predictive_value[i] <- (cubist_test_negative_predictive_value[i] + cubist_validation_negative_predictive_value[i]) / 2
  cubist_holdout_negative_predictive_value_mean <- mean(cubist_holdout_negative_predictive_value)
  cubist_holdout_overfitting[i] <- cubist_holdout_accuracy[i] / cubist_train_accuracy[i]
  cubist_holdout_overfitting_mean <- mean(cubist_holdout_overfitting)
  cubist_holdout_overfitting_range <- range(cubist_holdout_overfitting)

  cubist_table <- cubist_test_table + cubist_validation_table
  cubist_table_total <- cubist_table_total + cubist_table

  cubist_end <- Sys.time()
  cubist_duration[i] <- cubist_end - cubist_start
  cubist_duration_mean <- mean(cubist_duration)


  #### Flexible discriminant analysis ####
  fda_start <- Sys.time()

  fda_train_fit <- mda::fda(as.factor(y_train) ~ ., data = train)

  fda_train_pred <- stats::predict(fda_train_fit, train01, type = "posterior")
  fda_train_predictions <- ifelse(fda_train_pred[, 2] > 0.5, 1, 0)
  fda_train_table <- table(fda_train_predictions, y_train)
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

  fda_test_pred <- stats::predict(fda_train_fit, test01, type = "posterior")
  fda_test_predictions <- ifelse(fda_test_pred[, 2] > 0.5, 1, 0)
  fda_test_table <- table(fda_test_predictions, y_test)
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

  fda_validation_pred <- stats::predict(fda_train_fit, validation01, type = "posterior")
  fda_validation_predictions <- ifelse(fda_validation_pred[, 2] > 0.5, 1, 0)
  fda_validation_table <- table(fda_validation_predictions, y_validation)
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
  fda_holdout_F1_score[i] <- (fda_test_F1_score[i] + fda_validation_F1_score[i]) / 2
  fda_holdout_F1_score_mean <- mean(fda_holdout_F1_score)
  fda_holdout_positive_predictive_value[i] <- (fda_test_positive_predictive_value[i] + fda_validation_positive_predictive_value[i]) / 2
  fda_holdout_positive_predictive_value_mean <- mean(fda_holdout_positive_predictive_value)
  fda_holdout_negative_predictive_value[i] <- (fda_test_negative_predictive_value[i] + fda_validation_negative_predictive_value[i]) / 2
  fda_holdout_negative_predictive_value_mean <- mean(fda_holdout_negative_predictive_value)
  fda_holdout_overfitting[i] <- fda_holdout_accuracy[i] / fda_train_accuracy[i]
  fda_holdout_overfitting_mean <- mean(fda_holdout_overfitting)
  fda_holdout_overfitting_range <- range(fda_holdout_overfitting)

  fda_table <- fda_test_table + fda_validation_table
  fda_table_total <- fda_table_total + fda_table

  fda_end <- Sys.time()
  fda_duration[i] <- fda_end - fda_start
  fda_duration_mean <- mean(fda_duration)

  #### Generalized Additive Models ####
  gam_start <- Sys.time()

  gam_train_fit <- gam::gam(y ~ ., data = train)

  gam_train_pred <- stats::predict(gam_train_fit, train01, type = "response")
  gam_train_predictions <- ifelse(gam_train_pred > 0.5, 1, 0)
  gam_train_table <- table(gam_train_predictions, y_train)
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
  gam_test_predictions <- ifelse(gam_test_pred > 0.5, 1, 0)
  gam_test_table <- table(gam_test_predictions, y_test)
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
  gam_validation_predictions <- ifelse(gam_validation_pred > 0.5, 1, 0)
  gam_validation_table <- table(gam_validation_predictions, y_validation)
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
  gam_holdout_F1_score[i] <- (gam_test_F1_score[i] + gam_validation_F1_score[i]) / 2
  gam_holdout_F1_score_mean <- mean(gam_holdout_F1_score)
  gam_holdout_positive_predictive_value[i] <- (gam_test_positive_predictive_value[i] + gam_validation_positive_predictive_value[i]) / 2
  gam_holdout_positive_predictive_value_mean <- mean(gam_holdout_positive_predictive_value)
  gam_holdout_negative_predictive_value[i] <- (gam_test_negative_predictive_value[i] + gam_validation_negative_predictive_value[i]) / 2
  gam_holdout_negative_predictive_value_mean <- mean(gam_holdout_negative_predictive_value)
  gam_holdout_overfitting[i] <- gam_holdout_accuracy[i] / gam_train_accuracy[i]
  gam_holdout_overfitting_mean <- mean(gam_holdout_overfitting)
  gam_holdout_overfitting_range <- range(gam_holdout_overfitting)

  gam_table <- gam_test_table + gam_validation_table
  gam_table_total <- gam_table_total + gam_table

  gam_end <- Sys.time()
  gam_duration[i] <- gam_end - gam_start
  gam_duration_mean <- mean(gam_duration)


  #### Gradient Boosted ####
  gb_start <- Sys.time()

  gb_train_fit <- gbm::gbm(train$y ~ ., data = train, distribution = "gaussian", n.trees = 100, shrinkage = 0.1, interaction.depth = 10)

  gb_train_pred <- stats::predict(gb_train_fit, train01, type = "response")
  gb_train_predictions <- ifelse(gb_train_pred > 0.5, 1, 0)
  gb_train_table <- table(gb_train_predictions, y_train)
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
  gb_test_predictions <- ifelse(gb_test_pred > 0.5, 1, 0)
  gb_test_table <- table(gb_test_predictions, y_test)
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
  gb_validation_predictions <- ifelse(gb_validation_pred > 0.5, 1, 0)
  gb_validation_table <- table(gb_validation_predictions, y_validation)
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
  gb_holdout_F1_score[i] <- (gb_test_F1_score[i] + gb_validation_F1_score[i]) / 2
  gb_holdout_F1_score_mean <- mean(gb_holdout_F1_score)
  gb_holdout_positive_predictive_value[i] <- (gb_test_positive_predictive_value[i] + gb_validation_positive_predictive_value[i]) / 2
  gb_holdout_positive_predictive_value_mean <- mean(gb_holdout_positive_predictive_value)
  gb_holdout_negative_predictive_value[i] <- (gb_test_negative_predictive_value[i] + gb_validation_negative_predictive_value[i]) / 2
  gb_holdout_negative_predictive_value_mean <- mean(gb_holdout_negative_predictive_value)
  gb_holdout_overfitting[i] <- gb_holdout_accuracy[i] / gb_train_accuracy[i]
  gb_holdout_overfitting_mean <- mean(gb_holdout_overfitting)
  gb_holdout_overfitting_range <- range(gb_holdout_overfitting)

  gb_table <- gb_test_table + gb_validation_table
  gb_table_total <-  gb_table_total + gb_table

  gb_end <- Sys.time()
  gb_duration[i] <- gb_end - gb_start
  gb_duration_mean <- mean(gb_duration)


  #### Generalized Linear Models ####
  glm_start <- Sys.time()

  glm_train_fit <- stats::glm(y ~ ., data = train, family = binomial(link = "logit"))

  glm_train_pred <- stats::predict(glm_train_fit, train01, type = "response")
  glm_train_predictions <- ifelse(glm_train_pred > 0.5, 1, 0)
  glm_train_table <- table(glm_train_predictions, y_train)
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
  glm_test_predictions <- ifelse(glm_test_pred > 0.5, 1, 0)
  glm_test_table <- table(glm_test_predictions, y_test)
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
  glm_validation_predictions <- ifelse(glm_validation_pred > 0.5, 1, 0)
  glm_validation_table <- table(glm_validation_predictions, y_validation)
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
  glm_holdout_F1_score[i] <- (glm_test_F1_score[i] + glm_validation_F1_score[i]) / 2
  glm_holdout_F1_score_mean <- mean(glm_holdout_F1_score)
  glm_holdout_positive_predictive_value[i] <- (glm_test_positive_predictive_value[i] + glm_validation_positive_predictive_value[i]) / 2
  glm_holdout_positive_predictive_value_mean <- mean(glm_holdout_positive_predictive_value)
  glm_holdout_negative_predictive_value[i] <- (glm_test_negative_predictive_value[i] + glm_validation_negative_predictive_value[i]) / 2
  glm_holdout_negative_predictive_value_mean <- mean(glm_holdout_negative_predictive_value)
  glm_holdout_overfitting[i] <- glm_holdout_accuracy[i] / glm_train_accuracy[i]
  glm_holdout_overfitting_mean <- mean(glm_holdout_overfitting)
  glm_holdout_overfitting_range <- range(glm_holdout_overfitting)

  glm_table <- glm_test_table + glm_validation_table
  glm_table_total <- glm_table_total + glm_table

  glm_end <- Sys.time()
  glm_duration[i] <- glm_end - glm_start
  glm_duration_mean <- mean(glm_duration)


  #### Linear ####
  linear_start <- Sys.time()

  linear_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "LMModel")

  linear_train_pred <- stats::predict(linear_train_fit, train01, type = "response")
  linear_train_table <- table(linear_train_pred, y_train)
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
  linear_test_table <- table(linear_test_pred, y_test)
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
  linear_validation_table <- table(linear_validation_pred, y_validation)
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

  linear_table <- linear_test_table + linear_validation_table
  linear_table_total <- linear_table_total + linear_table

  linear_end <- Sys.time()
  linear_duration[i] <- linear_end - linear_start
  linear_duration_mean <- mean(linear_duration)

  #### Linear Discriminant Analysis ####
  lda_start <- Sys.time()

  lda_train_fit <- MASS::lda(as.factor(y) ~ ., data = train01, model = "LMModel")

  lda_train_pred <- stats::predict(lda_train_fit, train01, type = "response")
  lda_train_table <- table(lda_train_pred$class, y_train)
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
  lda_test_table <- table(lda_test_pred$class, y_test)
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
  lda_validation_table <- table(lda_validation_pred$class, y_validation)
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
  lda_holdout_F1_score[i] <- (lda_test_F1_score[i] + lda_validation_F1_score[i]) / 2
  lda_holdout_F1_score_mean <- mean(lda_holdout_F1_score)
  lda_holdout_positive_predictive_value[i] <- (lda_test_positive_predictive_value[i] + lda_validation_positive_predictive_value[i]) / 2
  lda_holdout_positive_predictive_value_mean <- mean(lda_holdout_positive_predictive_value)
  lda_holdout_negative_predictive_value[i] <- (lda_test_negative_predictive_value[i] + lda_validation_negative_predictive_value[i]) / 2
  lda_holdout_negative_predictive_value_mean <- mean(lda_holdout_negative_predictive_value)
  lda_holdout_overfitting[i] <- lda_holdout_accuracy[i] / lda_train_accuracy[i]
  lda_holdout_overfitting_mean <- mean(lda_holdout_overfitting)
  lda_holdout_overfitting_range <- range(lda_holdout_overfitting)

  lda_table <- lda_test_table + lda_validation_table
  lda_table_total <- lda_table_total + lda_table

  lda_end <- Sys.time()
  lda_duration[i] <- lda_end - lda_start
  lda_duration_mean <- mean(lda_duration)


  #### Mixed Discriminant analysis ####
  mda_start <- Sys.time()

  mda_train_fit <- mda::mda(formula = as.factor(y) ~ ., data = train01)

  mda_train_pred <- stats::predict(mda_train_fit, train01, type = "posterior")
  mda_train_predictions <- ifelse(mda_train_pred > 0.50, 1, 0)[, 2]
  mda_train_table <- table(mda_train_predictions, y_train)
  mda_train_true_positive_rate[i] <- mda_train_table[2, 2] / sum(mda_train_table[2, 2] + mda_train_table[1, 2])
  mda_train_true_positive_rate_mean <- mean(mda_train_true_positive_rate)
  mda_train_true_negative_rate[i] <- mda_train_table[1, 1] / sum(mda_train_table[1, 1] + mda_train_table[2, 1])
  mda_train_true_negative_rate_mean <- mean(mda_train_true_negative_rate)
  mda_train_false_positive_rate[i] <- mda_train_table[2, 1] / sum(mda_train_table[2, 1] + mda_train_table[1, 1])
  mda_train_false_positive_rate_mean <- mean(mda_train_false_positive_rate)
  mda_train_false_negative_rate[i] <- mda_train_table[1, 2] / sum(mda_train_table[1, 2] + mda_train_table[2, 2])
  mda_train_false_negative_rate_mean <- mean(mda_train_false_negative_rate)
  mda_train_accuracy[i] <- (mda_train_table[1, 1] + mda_train_table[2, 2]) / sum(mda_train_table)
  mda_train_accuracy_mean <- mean(mda_train_accuracy)
  mda_train_F1_score[i] <- 2 * (mda_train_table[2, 2]) / sum(2 * mda_train_table[2, 2] + mda_train_table[1, 2] + mda_train_table[2, 1])
  mda_train_F1_score_mean <- mean(mda_train_F1_score)
  mda_train_positive_predictive_value[i] <- mda_train_table[2, 2] / sum(mda_train_table[2, 2] + mda_train_table[2, 1])
  mda_train_positive_predictive_value_mean <- mean(mda_train_positive_predictive_value)
  mda_train_negative_predictive_value[i] <- mda_train_table[1, 1] / sum(mda_train_table[1, 1] + mda_train_table[1, 2])
  mda_train_negative_predictive_value_mean <- mean(mda_train_negative_predictive_value)

  mda_test_pred <- stats::predict(mda_train_fit, test01, type = "posterior")
  mda_test_predictions <- ifelse(mda_test_pred > 0.50, 1, 0)[, 2]
  mda_test_table <- table(mda_test_predictions, y_test)
  mda_test_true_positive_rate[i] <- mda_test_table[2, 2] / sum(mda_test_table[2, 2] + mda_test_table[1, 2])
  mda_test_true_positive_rate_mean <- mean(mda_test_true_positive_rate)
  mda_test_true_negative_rate[i] <- mda_test_table[1, 1] / sum(mda_test_table[1, 1] + mda_test_table[2, 1])
  mda_test_true_negative_rate_mean <- mean(mda_test_true_negative_rate)
  mda_test_false_positive_rate[i] <- mda_test_table[2, 1] / sum(mda_test_table[2, 1] + mda_test_table[1, 1])
  mda_test_false_positive_rate_mean <- mean(mda_test_false_positive_rate)
  mda_test_false_negative_rate[i] <- mda_test_table[1, 2] / sum(mda_test_table[1, 2] + mda_test_table[2, 2])
  mda_test_false_negative_rate_mean <- mean(mda_test_false_negative_rate)
  mda_test_accuracy[i] <- (mda_test_table[1, 1] + mda_test_table[2, 2]) / sum(mda_test_table)
  mda_test_accuracy_mean <- mean(mda_test_accuracy)
  mda_test_F1_score[i] <- 2 * (mda_test_table[2, 2]) / sum(2 * mda_test_table[2, 2] + mda_test_table[1, 2] + mda_test_table[2, 1])
  mda_test_F1_score_mean <- mean(mda_test_F1_score)
  mda_test_positive_predictive_value[i] <- mda_test_table[2, 2] / sum(mda_test_table[2, 2] + mda_test_table[2, 1])
  mda_test_positive_predictive_value_mean <- mean(mda_test_positive_predictive_value)
  mda_test_negative_predictive_value[i] <- mda_test_table[1, 1] / sum(mda_test_table[1, 1] + mda_test_table[1, 2])
  mda_test_negative_predictive_value_mean <- mean(mda_test_negative_predictive_value)

  mda_validation_pred <- stats::predict(mda_train_fit, validation01, type = "posterior")
  mda_validation_predictions <- ifelse(mda_validation_pred > 0.50, 1, 0)[, 2]
  mda_validation_table <- table(mda_validation_predictions, y_validation)
  mda_validation_true_positive_rate[i] <- mda_validation_table[2, 2] / sum(mda_validation_table[2, 2] + mda_validation_table[1, 2])
  mda_validation_true_positive_rate_mean <- mean(mda_validation_true_positive_rate)
  mda_validation_true_negative_rate[i] <- mda_validation_table[1, 1] / sum(mda_validation_table[1, 1] + mda_validation_table[2, 1])
  mda_validation_true_negative_rate_mean <- mean(mda_validation_true_negative_rate)
  mda_validation_false_positive_rate[i] <- mda_validation_table[2, 1] / sum(mda_validation_table[2, 1] + mda_validation_table[1, 1])
  mda_validation_false_positive_rate_mean <- mean(mda_validation_false_positive_rate)
  mda_validation_false_negative_rate[i] <- mda_validation_table[1, 2] / sum(mda_validation_table[1, 2] + mda_validation_table[2, 2])
  mda_validation_false_negative_rate_mean <- mean(mda_validation_false_negative_rate)
  mda_validation_accuracy[i] <- (mda_validation_table[1, 1] + mda_validation_table[2, 2]) / sum(mda_validation_table)
  mda_validation_accuracy_mean <- mean(mda_validation_accuracy)
  mda_validation_F1_score[i] <- 2 * (mda_validation_table[2, 2]) / sum(2 * mda_validation_table[2, 2] + mda_validation_table[1, 2] + mda_validation_table[2, 1])
  mda_validation_F1_score_mean <- mean(mda_validation_F1_score)
  mda_validation_positive_predictive_value[i] <- mda_validation_table[2, 2] / sum(mda_validation_table[2, 2] + mda_validation_table[2, 1])
  mda_validation_positive_predictive_value_mean <- mean(mda_validation_positive_predictive_value)
  mda_validation_negative_predictive_value[i] <- mda_validation_table[1, 1] / sum(mda_validation_table[1, 1] + mda_validation_table[1, 2])
  mda_validation_negative_predictive_value_mean <- mean(mda_validation_negative_predictive_value)

  mda_holdout_true_positive_rate[i] <- (mda_test_true_positive_rate[i] + mda_validation_true_positive_rate[i]) / 2
  mda_holdout_true_positive_rate_mean <- mean(mda_holdout_true_positive_rate)
  mda_holdout_true_negative_rate[i] <- (mda_test_true_negative_rate[i] + mda_validation_true_negative_rate[i]) / 2
  mda_holdout_true_negative_rate_mean <- mean(mda_holdout_true_negative_rate)
  mda_holdout_false_positive_rate[i] <- (mda_test_false_positive_rate[i] + mda_validation_false_positive_rate[i]) / 2
  mda_holdout_false_positive_rate_mean <- mean(mda_holdout_false_positive_rate)
  mda_holdout_false_negative_rate[i] <- (mda_test_false_negative_rate[i] + mda_validation_false_negative_rate[i]) / 2
  mda_holdout_false_negative_rate_mean <- mean(mda_holdout_false_negative_rate)
  mda_holdout_accuracy[i] <- (mda_test_accuracy[i] + mda_validation_accuracy[i]) / 2
  mda_holdout_accuracy_mean <- mean(mda_holdout_accuracy)
  mda_holdout_F1_score[i] <- (mda_test_F1_score[i] + mda_validation_F1_score[i]) / 2
  mda_holdout_F1_score_mean <- mean(mda_holdout_F1_score)
  mda_holdout_positive_predictive_value[i] <- (mda_test_positive_predictive_value[i] + mda_validation_positive_predictive_value[i]) / 2
  mda_holdout_positive_predictive_value_mean <- mean(mda_holdout_positive_predictive_value)
  mda_holdout_negative_predictive_value[i] <- (mda_test_negative_predictive_value[i] + mda_validation_negative_predictive_value[i]) / 2
  mda_holdout_negative_predictive_value_mean <- mean(mda_holdout_negative_predictive_value)
  mda_holdout_overfitting[i] <- mda_holdout_accuracy[i] / mda_train_accuracy[i]
  mda_holdout_overfitting_mean <- mean(mda_holdout_overfitting)
  mda_holdout_overfitting_range <- range(mda_holdout_overfitting)

  mda_table <- mda_test_table + mda_validation_table
  mda_table_total <- mda_table_total + mda_table

  mda_end <- Sys.time()
  mda_duration[i] <- mda_end - mda_start
  mda_duration_mean <- mean(mda_duration)


  #### Naive Bayes ####
  n_bayes_start <- Sys.time()

  n_bayes_train_fit <- e1071::naiveBayes(as.factor(y_train) ~ ., data = train)

  n_bayes_train_pred <- stats::predict(n_bayes_train_fit, train01, type = "raw")
  n_bayes_train_predictions <- ifelse(n_bayes_train_pred > 0.50, 1, 0)[, 2]
  n_bayes_train_table <- table(n_bayes_train_predictions, y_train)
  n_bayes_train_true_positive_rate[i] <- n_bayes_train_table[2, 2] / sum(n_bayes_train_table[2, 2] + n_bayes_train_table[1, 2])
  n_bayes_train_true_positive_rate_mean <- mean(n_bayes_train_true_positive_rate)
  n_bayes_train_true_negative_rate[i] <- n_bayes_train_table[1, 1] / sum(n_bayes_train_table[1, 1] + n_bayes_train_table[2, 1])
  n_bayes_train_true_negative_rate_mean <- mean(n_bayes_train_true_negative_rate)
  n_bayes_train_false_positive_rate[i] <- n_bayes_train_table[2, 1] / sum(n_bayes_train_table[2, 1] + n_bayes_train_table[1, 1])
  n_bayes_train_false_positive_rate_mean <- mean(n_bayes_train_false_positive_rate)
  n_bayes_train_false_negative_rate[i] <- n_bayes_train_table[1, 2] / sum(n_bayes_train_table[1, 2] + n_bayes_train_table[2, 2])
  n_bayes_train_false_negative_rate_mean <- mean(n_bayes_train_false_negative_rate)
  n_bayes_train_accuracy[i] <- (n_bayes_train_table[1, 1] + n_bayes_train_table[2, 2]) / sum(n_bayes_train_table)
  n_bayes_train_accuracy_mean <- mean(n_bayes_train_accuracy)
  n_bayes_train_F1_score[i] <- 2 * (n_bayes_train_table[2, 2]) / sum(2 * n_bayes_train_table[2, 2] + n_bayes_train_table[1, 2] + n_bayes_train_table[2, 1])
  n_bayes_train_F1_score_mean <- mean(n_bayes_train_F1_score)
  n_bayes_train_positive_predictive_value[i] <- n_bayes_train_table[2, 2] / sum(n_bayes_train_table[2, 2] + n_bayes_train_table[2, 1])
  n_bayes_train_positive_predictive_value_mean <- mean(n_bayes_train_positive_predictive_value)
  n_bayes_train_negative_predictive_value[i] <- n_bayes_train_table[1, 1] / sum(n_bayes_train_table[1, 1] + n_bayes_train_table[1, 2])
  n_bayes_train_negative_predictive_value_mean <- mean(n_bayes_train_negative_predictive_value)

  n_bayes_test_pred <- stats::predict(n_bayes_train_fit, test01, type = "raw")
  n_bayes_test_predictions <- ifelse(n_bayes_test_pred > 0.50, 1, 0)[, 2]
  n_bayes_test_table <- table(n_bayes_test_predictions, y_test)
  n_bayes_test_true_positive_rate[i] <- n_bayes_test_table[2, 2] / sum(n_bayes_test_table[2, 2] + n_bayes_test_table[1, 2])
  n_bayes_test_true_positive_rate_mean <- mean(n_bayes_test_true_positive_rate)
  n_bayes_test_true_negative_rate[i] <- n_bayes_test_table[1, 1] / sum(n_bayes_test_table[1, 1] + n_bayes_test_table[2, 1])
  n_bayes_test_true_negative_rate_mean <- mean(n_bayes_test_true_negative_rate)
  n_bayes_test_false_positive_rate[i] <- n_bayes_test_table[2, 1] / sum(n_bayes_test_table[2, 1] + n_bayes_test_table[1, 1])
  n_bayes_test_false_positive_rate_mean <- mean(n_bayes_test_false_positive_rate)
  n_bayes_test_false_negative_rate[i] <- n_bayes_test_table[1, 2] / sum(n_bayes_test_table[1, 2] + n_bayes_test_table[2, 2])
  n_bayes_test_false_negative_rate_mean <- mean(n_bayes_test_false_negative_rate)
  n_bayes_test_accuracy[i] <- (n_bayes_test_table[1, 1] + n_bayes_test_table[2, 2]) / sum(n_bayes_test_table)
  n_bayes_test_accuracy_mean <- mean(n_bayes_test_accuracy)
  n_bayes_test_F1_score[i] <- 2 * (n_bayes_test_table[2, 2]) / sum(2 * n_bayes_test_table[2, 2] + n_bayes_test_table[1, 2] + n_bayes_test_table[2, 1])
  n_bayes_test_F1_score_mean <- mean(n_bayes_test_F1_score)
  n_bayes_test_positive_predictive_value[i] <- n_bayes_test_table[2, 2] / sum(n_bayes_test_table[2, 2] + n_bayes_test_table[2, 1])
  n_bayes_test_positive_predictive_value_mean <- mean(n_bayes_test_positive_predictive_value)
  n_bayes_test_negative_predictive_value[i] <- n_bayes_test_table[1, 1] / sum(n_bayes_test_table[1, 1] + n_bayes_test_table[1, 2])
  n_bayes_test_negative_predictive_value_mean <- mean(n_bayes_test_negative_predictive_value)

  n_bayes_validation_pred <- stats::predict(n_bayes_train_fit, validation01, type = "raw")
  n_bayes_validation_predictions <- ifelse(n_bayes_validation_pred > 0.50, 1, 0)[, 2]
  n_bayes_validation_table <- table(n_bayes_validation_predictions, y_validation)
  n_bayes_validation_true_positive_rate[i] <- n_bayes_validation_table[2, 2] / sum(n_bayes_validation_table[2, 2] + n_bayes_validation_table[1, 2])
  n_bayes_validation_true_positive_rate_mean <- mean(n_bayes_validation_true_positive_rate)
  n_bayes_validation_true_negative_rate[i] <- n_bayes_validation_table[1, 1] / sum(n_bayes_validation_table[1, 1] + n_bayes_validation_table[2, 1])
  n_bayes_validation_true_negative_rate_mean <- mean(n_bayes_validation_true_negative_rate)
  n_bayes_validation_false_positive_rate[i] <- n_bayes_validation_table[2, 1] / sum(n_bayes_validation_table[2, 1] + n_bayes_validation_table[1, 1])
  n_bayes_validation_false_positive_rate_mean <- mean(n_bayes_validation_false_positive_rate)
  n_bayes_validation_false_negative_rate[i] <- n_bayes_validation_table[1, 2] / sum(n_bayes_validation_table[1, 2] + n_bayes_validation_table[2, 2])
  n_bayes_validation_false_negative_rate_mean <- mean(n_bayes_validation_false_negative_rate)
  n_bayes_validation_accuracy[i] <- (n_bayes_validation_table[1, 1] + n_bayes_validation_table[2, 2]) / sum(n_bayes_validation_table)
  n_bayes_validation_accuracy_mean <- mean(n_bayes_validation_accuracy)
  n_bayes_validation_F1_score[i] <- 2 * (n_bayes_validation_table[2, 2]) / sum(2 * n_bayes_validation_table[2, 2] + n_bayes_validation_table[1, 2] + n_bayes_validation_table[2, 1])
  n_bayes_validation_F1_score_mean <- mean(n_bayes_validation_F1_score)
  n_bayes_validation_positive_predictive_value[i] <- n_bayes_validation_table[2, 2] / sum(n_bayes_validation_table[2, 2] + n_bayes_validation_table[2, 1])
  n_bayes_validation_positive_predictive_value_mean <- mean(n_bayes_validation_positive_predictive_value)
  n_bayes_validation_negative_predictive_value[i] <- n_bayes_validation_table[1, 1] / sum(n_bayes_validation_table[1, 1] + n_bayes_validation_table[1, 2])
  n_bayes_validation_negative_predictive_value_mean <- mean(n_bayes_validation_negative_predictive_value)

  n_bayes_holdout_true_positive_rate[i] <- (n_bayes_test_true_positive_rate[i] + n_bayes_validation_true_positive_rate[i]) / 2
  n_bayes_holdout_true_positive_rate_mean <- mean(n_bayes_holdout_true_positive_rate)
  n_bayes_holdout_true_negative_rate[i] <- (n_bayes_test_true_negative_rate[i] + n_bayes_validation_true_negative_rate[i]) / 2
  n_bayes_holdout_true_negative_rate_mean <- mean(n_bayes_holdout_true_negative_rate)
  n_bayes_holdout_false_positive_rate[i] <- (n_bayes_test_false_positive_rate[i] + n_bayes_validation_false_positive_rate[i]) / 2
  n_bayes_holdout_false_positive_rate_mean <- mean(n_bayes_holdout_false_positive_rate)
  n_bayes_holdout_false_negative_rate[i] <- (n_bayes_test_false_negative_rate[i] + n_bayes_validation_false_negative_rate[i]) / 2
  n_bayes_holdout_false_negative_rate_mean <- mean(n_bayes_holdout_false_negative_rate)
  n_bayes_holdout_accuracy[i] <- (n_bayes_test_accuracy[i] + n_bayes_validation_accuracy[i]) / 2
  n_bayes_holdout_accuracy_mean <- mean(n_bayes_holdout_accuracy)
  n_bayes_holdout_F1_score[i] <- (n_bayes_test_F1_score[i] + n_bayes_validation_F1_score[i]) / 2
  n_bayes_holdout_F1_score_mean <- mean(n_bayes_holdout_F1_score)
  n_bayes_holdout_positive_predictive_value[i] <- (n_bayes_test_positive_predictive_value[i] + n_bayes_validation_positive_predictive_value[i]) / 2
  n_bayes_holdout_positive_predictive_value_mean <- mean(n_bayes_holdout_positive_predictive_value)
  n_bayes_holdout_negative_predictive_value[i] <- (n_bayes_test_negative_predictive_value[i] + n_bayes_validation_negative_predictive_value[i]) / 2
  n_bayes_holdout_negative_predictive_value_mean <- mean(n_bayes_holdout_negative_predictive_value)
  n_bayes_holdout_overfitting[i] <- n_bayes_holdout_accuracy[i] / n_bayes_train_accuracy[i]
  n_bayes_holdout_overfitting_mean <- mean(n_bayes_holdout_overfitting)
  n_bayes_holdout_overfitting_range <- range(n_bayes_holdout_overfitting)

  n_bayes_table <- n_bayes_test_table + n_bayes_validation_table
  n_bayes_table_total <- n_bayes_table_total + n_bayes_table

  n_bayes_end <- Sys.time()
  n_bayes_duration[i] <- n_bayes_end - n_bayes_start
  n_bayes_duration_mean <- mean(n_bayes_duration)


  #### Penalized Discriminant Analysis ####
  pda_start <- Sys.time()

  pda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "PDAModel")

  pda_train_pred <- stats::predict(pda_train_fit, train01, type = "response")
  pda_train_table <- table(pda_train_pred, y_train)
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
  pda_test_table <- table(pda_test_pred, y_test)
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
  pda_validation_table <- table(pda_validation_pred, y_validation)
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
  pda_holdout_F1_score[i] <- (pda_test_F1_score[i] + pda_validation_F1_score[i]) / 2
  pda_holdout_F1_score_mean <- mean(pda_holdout_F1_score)
  pda_holdout_positive_predictive_value[i] <- (pda_test_positive_predictive_value[i] + pda_validation_positive_predictive_value[i]) / 2
  pda_holdout_positive_predictive_value_mean <- mean(pda_holdout_positive_predictive_value)
  pda_holdout_negative_predictive_value[i] <- (pda_test_negative_predictive_value[i] + pda_validation_negative_predictive_value[i]) / 2
  pda_holdout_negative_predictive_value_mean <- mean(pda_holdout_negative_predictive_value)
  pda_holdout_overfitting[i] <- pda_holdout_accuracy[i] / pda_train_accuracy[i]
  pda_holdout_overfitting_mean <- mean(pda_holdout_overfitting)
  pda_holdout_overfitting_range <- range(pda_holdout_overfitting)

  pda_table <- pda_test_table + pda_validation_table
  pda_table_total <- pda_table_total + pda_table

  pda_end <- Sys.time()
  pda_duration[i] <- pda_end - pda_start
  pda_duration_mean <- mean(pda_duration)


  #### Quadratic Discriminant Analysis ####
  qda_start <- Sys.time()

  qda_train_fit <- MASS::qda(as.factor(y) ~ ., data = train01)

  qda_train_pred <- stats::predict(qda_train_fit, train01, type = "response")
  qda_train_table <- table(qda_train_pred$class, y_train)
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
  qda_test_table <- table(qda_test_pred$class, y_test)
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
  qda_validation_table <- table(qda_validation_pred$class, y_validation)
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
  qda_holdout_F1_score[i] <- (qda_test_F1_score[i] + qda_validation_F1_score[i]) / 2
  qda_holdout_F1_score_mean <- mean(qda_holdout_F1_score)
  qda_holdout_positive_predictive_value[i] <- (qda_test_positive_predictive_value[i] + qda_validation_positive_predictive_value[i]) / 2
  qda_holdout_positive_predictive_value_mean <- mean(qda_holdout_positive_predictive_value)
  qda_holdout_negative_predictive_value[i] <- (qda_test_negative_predictive_value[i] + qda_validation_negative_predictive_value[i]) / 2
  qda_holdout_negative_predictive_value_mean <- mean(qda_holdout_negative_predictive_value)
  qda_holdout_overfitting[i] <- qda_holdout_accuracy[i] / qda_train_accuracy[i]
  qda_holdout_overfitting_mean <- mean(qda_holdout_overfitting)
  qda_holdout_overfitting_range <- range(qda_holdout_overfitting)

  qda_table <- qda_test_table + qda_validation_table
  qda_table_total <- qda_table_total + qda_table

  qda_end <- Sys.time()
  qda_duration[i] <- qda_end - qda_start
  qda_duration_mean <- mean(qda_duration)


  #### Random Forest ####
  rf_start <- Sys.time()

  rf_train_fit <- randomForest(x = train, y = as.factor(y_train), data = df)

  rf_train_pred <- stats::predict(rf_train_fit, train01, type = "prob")
  rf_train_probabilities <- ifelse(rf_train_pred > 0.50, 1, 0)[, 2]
  rf_train_table <- table(rf_train_probabilities, y_train)
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

  rf_test_pred <- stats::predict(rf_train_fit, test01, type = "prob")
  rf_test_probabilities <- ifelse(rf_test_pred > 0.50, 1, 0)[, 2]
  rf_test_table <- table(rf_test_probabilities, y_test)
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

  rf_validation_pred <- stats::predict(rf_train_fit, validation01, type = "prob")
  rf_validation_probabilities <- ifelse(rf_validation_pred > 0.50, 1, 0)[, 2]
  rf_validation_table <- table(rf_validation_probabilities, y_validation)
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
  rf_holdout_F1_score[i] <- (rf_test_F1_score[i] + rf_validation_F1_score[i]) / 2
  rf_holdout_F1_score_mean <- mean(rf_holdout_F1_score)
  rf_holdout_positive_predictive_value[i] <- (rf_test_positive_predictive_value[i] + rf_validation_positive_predictive_value[i]) / 2
  rf_holdout_positive_predictive_value_mean <- mean(rf_holdout_positive_predictive_value)
  rf_holdout_negative_predictive_value[i] <- (rf_test_negative_predictive_value[i] + rf_validation_negative_predictive_value[i]) / 2
  rf_holdout_negative_predictive_value_mean <- mean(rf_holdout_negative_predictive_value)
  rf_holdout_overfitting[i] <- rf_holdout_accuracy[i] / rf_train_accuracy[i]
  rf_holdout_overfitting_mean <- mean(rf_holdout_overfitting)
  rf_holdout_overfitting_range <- range(rf_holdout_overfitting)

  rf_table <- rf_test_table + rf_validation_table
  rf_table_total <- rf_table_total + rf_table

  rf_end <- Sys.time()
  rf_duration[i] <- rf_end - rf_start
  rf_duration_mean <- mean(rf_duration)


  #### Ranger ####
  ranger_start <- Sys.time()

  ranger_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "RangerModel")

  ranger_train_pred <- stats::predict(ranger_train_fit, train01, type = "prob")
  ranger_train_probabilities <- ifelse(ranger_train_pred > 0.5, 1, 0)
  ranger_train_table <- table(ranger_train_probabilities, y_train)
  ranger_train_true_positive_rate[i] <- ranger_train_table[2, 2] / sum(ranger_train_table[2, 2] + ranger_train_table[1, 2])
  ranger_train_true_positive_rate_mean <- mean(ranger_train_true_positive_rate)
  ranger_train_true_negative_rate[i] <- ranger_train_table[1, 1] / sum(ranger_train_table[1, 1] + ranger_train_table[2, 1])
  ranger_train_true_negative_rate_mean <- mean(ranger_train_true_negative_rate)
  ranger_train_false_positive_rate[i] <- ranger_train_table[2, 1] / sum(ranger_train_table[2, 1] + ranger_train_table[1, 1])
  ranger_train_false_positive_rate_mean <- mean(ranger_train_false_positive_rate)
  ranger_train_false_negative_rate[i] <- ranger_train_table[1, 2] / sum(ranger_train_table[1, 2] + ranger_train_table[2, 2])
  ranger_train_false_negative_rate_mean <- mean(ranger_train_false_negative_rate)
  ranger_train_accuracy[i] <- (ranger_train_table[1, 1] + ranger_train_table[2, 2]) / sum(ranger_train_table)
  ranger_train_accuracy_mean <- mean(ranger_train_accuracy)
  ranger_train_F1_score[i] <- 2 * (ranger_train_table[2, 2]) / sum(2 * ranger_train_table[2, 2] + ranger_train_table[1, 2] + ranger_train_table[2, 1])
  ranger_train_F1_score_mean <- mean(ranger_train_F1_score)
  ranger_train_positive_predictive_value[i] <- ranger_train_table[2, 2] / sum(ranger_train_table[2, 2] + ranger_train_table[2, 1])
  ranger_train_positive_predictive_value_mean <- mean(ranger_train_positive_predictive_value)
  ranger_train_negative_predictive_value[i] <- ranger_train_table[1, 1] / sum(ranger_train_table[1, 1] + ranger_train_table[1, 2])
  ranger_train_negative_predictive_value_mean <- mean(ranger_train_negative_predictive_value)

  ranger_test_pred <- stats::predict(ranger_train_fit, test01, type = "prob")
  ranger_test_probabilities <- ifelse(ranger_test_pred > 0.5, 1, 0)
  ranger_test_table <- table(ranger_test_probabilities, y_test)
  ranger_test_true_positive_rate[i] <- ranger_test_table[2, 2] / sum(ranger_test_table[2, 2] + ranger_test_table[1, 2])
  ranger_test_true_positive_rate_mean <- mean(ranger_test_true_positive_rate)
  ranger_test_true_negative_rate[i] <- ranger_test_table[1, 1] / sum(ranger_test_table[1, 1] + ranger_test_table[2, 1])
  ranger_test_true_negative_rate_mean <- mean(ranger_test_true_negative_rate)
  ranger_test_false_positive_rate[i] <- ranger_test_table[2, 1] / sum(ranger_test_table[2, 1] + ranger_test_table[1, 1])
  ranger_test_false_positive_rate_mean <- mean(ranger_test_false_positive_rate)
  ranger_test_false_negative_rate[i] <- ranger_test_table[1, 2] / sum(ranger_test_table[1, 2] + ranger_test_table[2, 2])
  ranger_test_false_negative_rate_mean <- mean(ranger_test_false_negative_rate)
  ranger_test_accuracy[i] <- (ranger_test_table[1, 1] + ranger_test_table[2, 2]) / sum(ranger_test_table)
  ranger_test_accuracy_mean <- mean(ranger_test_accuracy)
  ranger_test_F1_score[i] <- 2 * (ranger_test_table[2, 2]) / sum(2 * ranger_test_table[2, 2] + ranger_test_table[1, 2] + ranger_test_table[2, 1])
  ranger_test_F1_score_mean <- mean(ranger_test_F1_score)
  ranger_test_positive_predictive_value[i] <- ranger_test_table[2, 2] / sum(ranger_test_table[2, 2] + ranger_test_table[2, 1])
  ranger_test_positive_predictive_value_mean <- mean(ranger_test_positive_predictive_value)
  ranger_test_negative_predictive_value[i] <- ranger_test_table[1, 1] / sum(ranger_test_table[1, 1] + ranger_test_table[1, 2])
  ranger_test_negative_predictive_value_mean <- mean(ranger_test_negative_predictive_value)

  ranger_validation_pred <- stats::predict(ranger_train_fit, validation01, type = "prob")
  ranger_validation_probabilities <- ifelse(ranger_validation_pred > 0.5, 1, 0)
  ranger_validation_table <- table(ranger_validation_probabilities, y_validation)
  ranger_validation_true_positive_rate[i] <- ranger_validation_table[2, 2] / sum(ranger_validation_table[2, 2] + ranger_validation_table[1, 2])
  ranger_validation_true_positive_rate_mean <- mean(ranger_validation_true_positive_rate)
  ranger_validation_true_negative_rate[i] <- ranger_validation_table[1, 1] / sum(ranger_validation_table[1, 1] + ranger_validation_table[2, 1])
  ranger_validation_true_negative_rate_mean <- mean(ranger_validation_true_negative_rate)
  ranger_validation_false_positive_rate[i] <- ranger_validation_table[2, 1] / sum(ranger_validation_table[2, 1] + ranger_validation_table[1, 1])
  ranger_validation_false_positive_rate_mean <- mean(ranger_validation_false_positive_rate)
  ranger_validation_false_negative_rate[i] <- ranger_validation_table[1, 2] / sum(ranger_validation_table[1, 2] + ranger_validation_table[2, 2])
  ranger_validation_false_negative_rate_mean <- mean(ranger_validation_false_negative_rate)
  ranger_validation_accuracy[i] <- (ranger_validation_table[1, 1] + ranger_validation_table[2, 2]) / sum(ranger_validation_table)
  ranger_validation_accuracy_mean <- mean(ranger_validation_accuracy)
  ranger_validation_F1_score[i] <- 2 * (ranger_validation_table[2, 2]) / sum(2 * ranger_validation_table[2, 2] + ranger_validation_table[1, 2] + ranger_validation_table[2, 1])
  ranger_validation_F1_score_mean <- mean(ranger_validation_F1_score)
  ranger_validation_positive_predictive_value[i] <- ranger_validation_table[2, 2] / sum(ranger_validation_table[2, 2] + ranger_validation_table[2, 1])
  ranger_validation_positive_predictive_value_mean <- mean(ranger_validation_positive_predictive_value)
  ranger_validation_negative_predictive_value[i] <- ranger_validation_table[1, 1] / sum(ranger_validation_table[1, 1] + ranger_validation_table[1, 2])
  ranger_validation_negative_predictive_value_mean <- mean(ranger_validation_negative_predictive_value)

  ranger_holdout_true_positive_rate[i] <- (ranger_test_true_positive_rate[i] + ranger_validation_true_positive_rate[i]) / 2
  ranger_holdout_true_positive_rate_mean <- mean(ranger_holdout_true_positive_rate)
  ranger_holdout_true_negative_rate[i] <- (ranger_test_true_negative_rate[i] + ranger_validation_true_negative_rate[i]) / 2
  ranger_holdout_true_negative_rate_mean <- mean(ranger_holdout_true_negative_rate)
  ranger_holdout_false_positive_rate[i] <- (ranger_test_false_positive_rate[i] + ranger_validation_false_positive_rate[i]) / 2
  ranger_holdout_false_positive_rate_mean <- mean(ranger_holdout_false_positive_rate)
  ranger_holdout_false_negative_rate[i] <- (ranger_test_false_negative_rate[i] + ranger_validation_false_negative_rate[i]) / 2
  ranger_holdout_false_negative_rate_mean <- mean(ranger_holdout_false_negative_rate)
  ranger_holdout_accuracy[i] <- (ranger_test_accuracy[i] + ranger_validation_accuracy[i]) / 2
  ranger_holdout_accuracy_mean <- mean(ranger_holdout_accuracy)
  ranger_holdout_F1_score[i] <- (ranger_test_F1_score[i] + ranger_validation_F1_score[i]) / 2
  ranger_holdout_F1_score_mean <- mean(ranger_holdout_F1_score)
  ranger_holdout_positive_predictive_value[i] <- (ranger_test_positive_predictive_value[i] + ranger_validation_positive_predictive_value[i]) / 2
  ranger_holdout_positive_predictive_value_mean <- mean(ranger_holdout_positive_predictive_value)
  ranger_holdout_negative_predictive_value[i] <- (ranger_test_negative_predictive_value[i] + ranger_validation_negative_predictive_value[i]) / 2
  ranger_holdout_negative_predictive_value_mean <- mean(ranger_holdout_negative_predictive_value)
  ranger_holdout_overfitting[i] <- ranger_holdout_accuracy[i] / ranger_train_accuracy[i]
  ranger_holdout_overfitting_mean <- mean(ranger_holdout_overfitting)
  ranger_holdout_overfitting_range <- range(ranger_holdout_overfitting)

  ranger_table <- ranger_test_table + ranger_validation_table
  ranger_table_total <- ranger_table_total + ranger_table

  ranger_end <- Sys.time()
  ranger_duration[i] <- ranger_end - ranger_start
  ranger_duration_mean <- mean(ranger_duration)


  #### Rpart ####
  rpart_start <- Sys.time()

  rpart_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = train01, model = "RPartModel")

  rpart_train_pred <- stats::predict(rpart_train_fit, train01, type = "prob")
  rpart_train_probabilities <- ifelse(rpart_train_pred > 0.5, 1, 0)
  rpart_train_table <- table(rpart_train_probabilities, y_train)
  rpart_train_true_positive_rate[i] <- rpart_train_table[2, 2] / sum(rpart_train_table[2, 2] + rpart_train_table[1, 2])
  rpart_train_true_positive_rate_mean <- mean(rpart_train_true_positive_rate)
  rpart_train_true_negative_rate[i] <- rpart_train_table[1, 1] / sum(rpart_train_table[1, 1] + rpart_train_table[2, 1])
  rpart_train_true_negative_rate_mean <- mean(rpart_train_true_negative_rate)
  rpart_train_false_positive_rate[i] <- rpart_train_table[2, 1] / sum(rpart_train_table[2, 1] + rpart_train_table[1, 1])
  rpart_train_false_positive_rate_mean <- mean(rpart_train_false_positive_rate)
  rpart_train_false_negative_rate[i] <- rpart_train_table[1, 2] / sum(rpart_train_table[1, 2] + rpart_train_table[2, 2])
  rpart_train_false_negative_rate_mean <- mean(rpart_train_false_negative_rate)
  rpart_train_accuracy[i] <- (rpart_train_table[1, 1] + rpart_train_table[2, 2]) / sum(rpart_train_table)
  rpart_train_accuracy_mean <- mean(rpart_train_accuracy)
  rpart_train_F1_score[i] <- 2 * (rpart_train_table[2, 2]) / sum(2 * rpart_train_table[2, 2] + rpart_train_table[1, 2] + rpart_train_table[2, 1])
  rpart_train_F1_score_mean <- mean(rpart_train_F1_score)
  rpart_train_positive_predictive_value[i] <- rpart_train_table[2, 2] / sum(rpart_train_table[2, 2] + rpart_train_table[2, 1])
  rpart_train_positive_predictive_value_mean <- mean(rpart_train_positive_predictive_value)
  rpart_train_negative_predictive_value[i] <- rpart_train_table[1, 1] / sum(rpart_train_table[1, 1] + rpart_train_table[1, 2])
  rpart_train_negative_predictive_value_mean <- mean(rpart_train_negative_predictive_value)

  rpart_test_pred <- stats::predict(rpart_train_fit, test01, type = "prob")
  rpart_test_probabilities <- ifelse(rpart_test_pred > 0.5, 1, 0)
  rpart_test_table <- table(rpart_test_probabilities, y_test)
  rpart_test_true_positive_rate[i] <- rpart_test_table[2, 2] / sum(rpart_test_table[2, 2] + rpart_test_table[1, 2])
  rpart_test_true_positive_rate_mean <- mean(rpart_test_true_positive_rate)
  rpart_test_true_negative_rate[i] <- rpart_test_table[1, 1] / sum(rpart_test_table[1, 1] + rpart_test_table[2, 1])
  rpart_test_true_negative_rate_mean <- mean(rpart_test_true_negative_rate)
  rpart_test_false_positive_rate[i] <- rpart_test_table[2, 1] / sum(rpart_test_table[2, 1] + rpart_test_table[1, 1])
  rpart_test_false_positive_rate_mean <- mean(rpart_test_false_positive_rate)
  rpart_test_false_negative_rate[i] <- rpart_test_table[1, 2] / sum(rpart_test_table[1, 2] + rpart_test_table[2, 2])
  rpart_test_false_negative_rate_mean <- mean(rpart_test_false_negative_rate)
  rpart_test_accuracy[i] <- (rpart_test_table[1, 1] + rpart_test_table[2, 2]) / sum(rpart_test_table)
  rpart_test_accuracy_mean <- mean(rpart_test_accuracy)
  rpart_test_F1_score[i] <- 2 * (rpart_test_table[2, 2]) / sum(2 * rpart_test_table[2, 2] + rpart_test_table[1, 2] + rpart_test_table[2, 1])
  rpart_test_F1_score_mean <- mean(rpart_test_F1_score)
  rpart_test_positive_predictive_value[i] <- rpart_test_table[2, 2] / sum(rpart_test_table[2, 2] + rpart_test_table[2, 1])
  rpart_test_positive_predictive_value_mean <- mean(rpart_test_positive_predictive_value)
  rpart_test_negative_predictive_value[i] <- rpart_test_table[1, 1] / sum(rpart_test_table[1, 1] + rpart_test_table[1, 2])
  rpart_test_negative_predictive_value_mean <- mean(rpart_test_negative_predictive_value)

  rpart_validation_pred <- stats::predict(rpart_train_fit, validation01, type = "prob")
  rpart_validation_probabilities <- ifelse(rpart_validation_pred > 0.5, 1, 0)
  rpart_validation_table <- table(rpart_validation_probabilities, y_validation)
  rpart_validation_true_positive_rate[i] <- rpart_validation_table[2, 2] / sum(rpart_validation_table[2, 2] + rpart_validation_table[1, 2])
  rpart_validation_true_positive_rate_mean <- mean(rpart_validation_true_positive_rate)
  rpart_validation_true_negative_rate[i] <- rpart_validation_table[1, 1] / sum(rpart_validation_table[1, 1] + rpart_validation_table[2, 1])
  rpart_validation_true_negative_rate_mean <- mean(rpart_validation_true_negative_rate)
  rpart_validation_false_positive_rate[i] <- rpart_validation_table[2, 1] / sum(rpart_validation_table[2, 1] + rpart_validation_table[1, 1])
  rpart_validation_false_positive_rate_mean <- mean(rpart_validation_false_positive_rate)
  rpart_validation_false_negative_rate[i] <- rpart_validation_table[1, 2] / sum(rpart_validation_table[1, 2] + rpart_validation_table[2, 2])
  rpart_validation_false_negative_rate_mean <- mean(rpart_validation_false_negative_rate)
  rpart_validation_accuracy[i] <- (rpart_validation_table[1, 1] + rpart_validation_table[2, 2]) / sum(rpart_validation_table)
  rpart_validation_accuracy_mean <- mean(rpart_validation_accuracy)
  rpart_validation_F1_score[i] <- 2 * (rpart_validation_table[2, 2]) / sum(2 * rpart_validation_table[2, 2] + rpart_validation_table[1, 2] + rpart_validation_table[2, 1])
  rpart_validation_F1_score_mean <- mean(rpart_validation_F1_score)
  rpart_validation_positive_predictive_value[i] <- rpart_validation_table[2, 2] / sum(rpart_validation_table[2, 2] + rpart_validation_table[2, 1])
  rpart_validation_positive_predictive_value_mean <- mean(rpart_validation_positive_predictive_value)
  rpart_validation_negative_predictive_value[i] <- rpart_validation_table[1, 1] / sum(rpart_validation_table[1, 1] + rpart_validation_table[1, 2])
  rpart_validation_negative_predictive_value_mean <- mean(rpart_validation_negative_predictive_value)

  rpart_holdout_true_positive_rate[i] <- (rpart_test_true_positive_rate[i] + rpart_validation_true_positive_rate[i]) / 2
  rpart_holdout_true_positive_rate_mean <- mean(rpart_holdout_true_positive_rate)
  rpart_holdout_true_negative_rate[i] <- (rpart_test_true_negative_rate[i] + rpart_validation_true_negative_rate[i]) / 2
  rpart_holdout_true_negative_rate_mean <- mean(rpart_holdout_true_negative_rate)
  rpart_holdout_false_positive_rate[i] <- (rpart_test_false_positive_rate[i] + rpart_validation_false_positive_rate[i]) / 2
  rpart_holdout_false_positive_rate_mean <- mean(rpart_holdout_false_positive_rate)
  rpart_holdout_false_negative_rate[i] <- (rpart_test_false_negative_rate[i] + rpart_validation_false_negative_rate[i]) / 2
  rpart_holdout_false_negative_rate_mean <- mean(rpart_holdout_false_negative_rate)
  rpart_holdout_accuracy[i] <- (rpart_test_accuracy[i] + rpart_validation_accuracy[i]) / 2
  rpart_holdout_accuracy_mean <- mean(rpart_holdout_accuracy)
  rpart_holdout_F1_score[i] <- (rpart_test_F1_score[i] + rpart_validation_F1_score[i]) / 2
  rpart_holdout_F1_score_mean <- mean(rpart_holdout_F1_score)
  rpart_holdout_positive_predictive_value[i] <- (rpart_test_positive_predictive_value[i] + rpart_validation_positive_predictive_value[i]) / 2
  rpart_holdout_positive_predictive_value_mean <- mean(rpart_holdout_positive_predictive_value)
  rpart_holdout_negative_predictive_value[i] <- (rpart_test_negative_predictive_value[i] + rpart_validation_negative_predictive_value[i]) / 2
  rpart_holdout_negative_predictive_value_mean <- mean(rpart_holdout_negative_predictive_value)
  rpart_holdout_overfitting[i] <- rpart_holdout_accuracy[i] / rpart_train_accuracy[i]
  rpart_holdout_overfitting_mean <- mean(rpart_holdout_overfitting)
  rpart_holdout_overfitting_range <- range(rpart_holdout_overfitting)

  rpart_table <- rpart_test_table + rpart_validation_table
  rpart_table_total <- rpart_table_total + rpart_table

  rpart_end <- Sys.time()
  rpart_duration[i] <- rpart_end - rpart_start
  rpart_duration_mean <- mean(rpart_duration)


  #### Support Vector Machines (SVM) ####
  svm_start <- Sys.time()

  svm_train_fit <- e1071::svm(as.factor(y) ~ ., data = train01)

  svm_train_pred <- stats::predict(svm_train_fit, train01, type = "prob")
  svm_train_table <- table(svm_train_pred, y_train)
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

  svm_test_pred <- stats::predict(svm_train_fit, test01, type = "prob")
  svm_test_table <- table(svm_test_pred, y_test)
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

  svm_validation_pred <- stats::predict(svm_train_fit, validation01, type = "prob")
  svm_validation_table <- table(svm_validation_pred, y_validation)
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
  svm_holdout_F1_score[i] <- (svm_test_F1_score[i] + svm_validation_F1_score[i]) / 2
  svm_holdout_F1_score_mean <- mean(svm_holdout_F1_score)
  svm_holdout_positive_predictive_value[i] <- (svm_test_positive_predictive_value[i] + svm_validation_positive_predictive_value[i]) / 2
  svm_holdout_positive_predictive_value_mean <- mean(svm_holdout_positive_predictive_value)
  svm_holdout_negative_predictive_value[i] <- (svm_test_negative_predictive_value[i] + svm_validation_negative_predictive_value[i]) / 2
  svm_holdout_negative_predictive_value_mean <- mean(svm_holdout_negative_predictive_value)
  svm_holdout_overfitting[i] <- svm_holdout_accuracy[i] / svm_train_accuracy[i]
  svm_holdout_overfitting_mean <- mean(svm_holdout_overfitting)
  svm_holdout_overfitting_range <- range(svm_holdout_overfitting)

  svm_table <- svm_test_table + svm_validation_table
  svm_table_total <- svm_table_total + svm_table

  svm_end <- Sys.time()
  svm_duration[i] <- svm_end - svm_start
  svm_duration_mean <- mean(svm_duration)


  #### tree ####
  tree_start <- Sys.time()

  tree_train_fit <- tree::tree(y ~ ., data = train)

  tree_train_pred <- stats::predict(tree_train_fit, train01, type = "vector")
  tree_train_predictions <- ifelse(tree_train_pred > 0.5, 1, 0)
  tree_train_table <- table(tree_train_predictions, y_train)
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
  tree_test_predictions <- ifelse(tree_test_pred > 0.5, 1, 0)
  tree_test_table <- table(tree_test_predictions, y_test)
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
  tree_validation_predictions <- ifelse(tree_validation_pred > 0.5, 1, 0)
  tree_validation_table <- table(tree_validation_predictions, y_validation)
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
  tree_holdout_F1_score[i] <- (tree_test_F1_score[i] + tree_validation_F1_score[i]) / 2
  tree_holdout_F1_score_mean <- mean(tree_holdout_F1_score)
  tree_holdout_positive_predictive_value[i] <- (tree_test_positive_predictive_value[i] + tree_validation_positive_predictive_value[i]) / 2
  tree_holdout_positive_predictive_value_mean <- mean(tree_holdout_positive_predictive_value)
  tree_holdout_negative_predictive_value[i] <- (tree_test_negative_predictive_value[i] + tree_validation_negative_predictive_value[i]) / 2
  tree_holdout_negative_predictive_value_mean <- mean(tree_holdout_negative_predictive_value)
  tree_holdout_overfitting[i] <- tree_holdout_accuracy[i] / tree_train_accuracy[i]
  tree_holdout_overfitting_mean <- mean(tree_holdout_overfitting)
  tree_holdout_overfitting_range <- range(tree_holdout_overfitting)

  tree_table <- tree_test_table + tree_validation_table
  tree_table_total <- tree_table_total + tree_table

  tree_end <- Sys.time()
  tree_duration[i] <- tree_end - tree_start
  tree_duration_mean <- mean(tree_duration)


  #### XGBoost ####
  xgb_start <- Sys.time()

  xgb_start <- Sys.time()
  train_x <- data.matrix(train[, -ncol(train)])
  train_y <- train[, ncol(train)]

  # define predictor and response variables in test set
  test_x <- data.matrix(test[, -ncol(test)])
  test_y <- test[, ncol(test)]

  # define predictor and response variables in validation set
  validation_x <- data.matrix(validation[, -ncol(validation)])
  validation_y <- validation[, ncol(validation)]

  # define final train, test and validation sets
  xgb_train <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
  xgb_test <- xgboost::xgb.DMatrix(data = test_x, label = test_y)
  xgb_validation <- xgboost::xgb.DMatrix(data = validation_x, label = validation_y)

  # define watchlist
  watchlist <- list(train = xgb_train, validation = xgb_validation)
  watchlist_test <- list(train = xgb_train, test = xgb_test)
  watchlist_validation <- list(train = xgb_train, validation = xgb_validation)

  xgb_model <- xgboost::xgb.train(data = xgb_train, max.depth = 3, watchlist = watchlist_test, nrounds = 70)

  xgb_min <- which.min(xgb_model$evaluation_log$validation_rmse)

  xgb_train_pred <- stats::predict(object = xgb_model, newdata = train_x, type = "prob")
  xgb_train_predictions <- ifelse(xgb_train_pred > 0.5, 1, 0)
  xgb_train_table <- table(xgb_train_predictions, y_train)
  xgb_train_true_positive_rate[i] <- xgb_train_table[2, 2] / sum(xgb_train_table[2, 2] + xgb_train_table[1, 2])
  xgb_train_true_positive_rate_mean <- mean(xgb_train_true_positive_rate)
  xgb_train_true_negative_rate[i] <- xgb_train_table[1, 1] / sum(xgb_train_table[1, 1] + xgb_train_table[2, 1])
  xgb_train_true_negative_rate_mean <- mean(xgb_train_true_negative_rate)
  xgb_train_false_positive_rate[i] <- xgb_train_table[2, 1] / sum(xgb_train_table[2, 1] + xgb_train_table[1, 1])
  xgb_train_false_positive_rate_mean <- mean(xgb_train_false_positive_rate)
  xgb_train_false_negative_rate[i] <- xgb_train_table[1, 2] / sum(xgb_train_table[1, 2] + xgb_train_table[2, 2])
  xgb_train_false_negative_rate_mean <- mean(xgb_train_false_negative_rate)
  xgb_train_accuracy[i] <- (xgb_train_table[1, 1] + xgb_train_table[2, 2]) / sum(xgb_train_table)
  xgb_train_accuracy_mean <- mean(xgb_train_accuracy)
  xgb_train_F1_score[i] <- 2 * (xgb_train_table[2, 2]) / sum(2 * xgb_train_table[2, 2] + xgb_train_table[1, 2] + xgb_train_table[2, 1])
  xgb_train_F1_score_mean <- mean(xgb_train_F1_score)
  xgb_train_positive_predictive_value[i] <- xgb_train_table[2, 2] / sum(xgb_train_table[2, 2] + xgb_train_table[2, 1])
  xgb_train_positive_predictive_value_mean <- mean(xgb_train_positive_predictive_value)
  xgb_train_negative_predictive_value[i] <- xgb_train_table[1, 1] / sum(xgb_train_table[1, 1] + xgb_train_table[1, 2])
  xgb_train_negative_predictive_value_mean <- mean(xgb_train_negative_predictive_value)

  xgb_test_pred <- stats::predict(object = xgb_model, newdata = test_x, type = "prob")
  xgb_test_predictions <- ifelse(xgb_test_pred > 0.5, 1, 0)
  xgb_test_table <- table(xgb_test_predictions, y_test)
  xgb_test_true_positive_rate[i] <- xgb_test_table[2, 2] / sum(xgb_test_table[2, 2] + xgb_test_table[1, 2])
  xgb_test_true_positive_rate_mean <- mean(xgb_test_true_positive_rate)
  xgb_test_true_negative_rate[i] <- xgb_test_table[1, 1] / sum(xgb_test_table[1, 1] + xgb_test_table[2, 1])
  xgb_test_true_negative_rate_mean <- mean(xgb_test_true_negative_rate)
  xgb_test_false_positive_rate[i] <- xgb_test_table[2, 1] / sum(xgb_test_table[2, 1] + xgb_test_table[1, 1])
  xgb_test_false_positive_rate_mean <- mean(xgb_test_false_positive_rate)
  xgb_test_false_negative_rate[i] <- xgb_test_table[1, 2] / sum(xgb_test_table[1, 2] + xgb_test_table[2, 2])
  xgb_test_false_negative_rate_mean <- mean(xgb_test_false_negative_rate)
  xgb_test_accuracy[i] <- (xgb_test_table[1, 1] + xgb_test_table[2, 2]) / sum(xgb_test_table)
  xgb_test_accuracy_mean <- mean(xgb_test_accuracy)
  xgb_test_F1_score[i] <- 2 * (xgb_test_table[2, 2]) / sum(2 * xgb_test_table[2, 2] + xgb_test_table[1, 2] + xgb_test_table[2, 1])
  xgb_test_F1_score_mean <- mean(xgb_test_F1_score)
  xgb_test_positive_predictive_value[i] <- xgb_test_table[2, 2] / sum(xgb_test_table[2, 2] + xgb_test_table[2, 1])
  xgb_test_positive_predictive_value_mean <- mean(xgb_test_positive_predictive_value)
  xgb_test_negative_predictive_value[i] <- xgb_test_table[1, 1] / sum(xgb_test_table[1, 1] + xgb_test_table[1, 2])
  xgb_test_negative_predictive_value_mean <- mean(xgb_test_negative_predictive_value)

  xgb_validation_pred <- stats::predict(object = xgb_model, newdata = validation_x, type = "prob")
  xgb_validation_predictions <- ifelse(xgb_validation_pred > 0.5, 1, 0)
  xgb_validation_table <- table(xgb_validation_predictions, y_validation)
  xgb_validation_true_positive_rate[i] <- xgb_validation_table[2, 2] / sum(xgb_validation_table[2, 2] + xgb_validation_table[1, 2])
  xgb_validation_true_positive_rate_mean <- mean(xgb_validation_true_positive_rate)
  xgb_validation_true_negative_rate[i] <- xgb_validation_table[1, 1] / sum(xgb_validation_table[1, 1] + xgb_validation_table[2, 1])
  xgb_validation_true_negative_rate_mean <- mean(xgb_validation_true_negative_rate)
  xgb_validation_false_positive_rate[i] <- xgb_validation_table[2, 1] / sum(xgb_validation_table[2, 1] + xgb_validation_table[1, 1])
  xgb_validation_false_positive_rate_mean <- mean(xgb_validation_false_positive_rate)
  xgb_validation_false_negative_rate[i] <- xgb_validation_table[1, 2] / sum(xgb_validation_table[1, 2] + xgb_validation_table[2, 2])
  xgb_validation_false_negative_rate_mean <- mean(xgb_validation_false_negative_rate)
  xgb_validation_accuracy[i] <- (xgb_validation_table[1, 1] + xgb_validation_table[2, 2]) / sum(xgb_validation_table)
  xgb_validation_accuracy_mean <- mean(xgb_validation_accuracy)
  xgb_validation_F1_score[i] <- 2 * (xgb_validation_table[2, 2]) / sum(2 * xgb_validation_table[2, 2] + xgb_validation_table[1, 2] + xgb_validation_table[2, 1])
  xgb_validation_F1_score_mean <- mean(xgb_validation_F1_score)
  xgb_validation_positive_predictive_value[i] <- xgb_validation_table[2, 2] / sum(xgb_validation_table[2, 2] + xgb_validation_table[2, 1])
  xgb_validation_positive_predictive_value_mean <- mean(xgb_validation_positive_predictive_value)
  xgb_validation_negative_predictive_value[i] <- xgb_validation_table[1, 1] / sum(xgb_validation_table[1, 1] + xgb_validation_table[1, 2])
  xgb_validation_negative_predictive_value_mean <- mean(xgb_validation_negative_predictive_value)

  xgb_holdout_true_positive_rate[i] <- (xgb_test_true_positive_rate[i] + xgb_validation_true_positive_rate[i]) / 2
  xgb_holdout_true_positive_rate_mean <- mean(xgb_holdout_true_positive_rate)
  xgb_holdout_true_negative_rate[i] <- (xgb_test_true_negative_rate[i] + xgb_validation_true_negative_rate[i]) / 2
  xgb_holdout_true_negative_rate_mean <- mean(xgb_holdout_true_negative_rate)
  xgb_holdout_false_positive_rate[i] <- (xgb_test_false_positive_rate[i] + xgb_validation_false_positive_rate[i]) / 2
  xgb_holdout_false_positive_rate_mean <- mean(xgb_holdout_false_positive_rate)
  xgb_holdout_false_negative_rate[i] <- (xgb_test_false_negative_rate[i] + xgb_validation_false_negative_rate[i]) / 2
  xgb_holdout_false_negative_rate_mean <- mean(xgb_holdout_false_negative_rate)
  xgb_holdout_accuracy[i] <- (xgb_test_accuracy[i] + xgb_validation_accuracy[i]) / 2
  xgb_holdout_accuracy_mean <- mean(xgb_holdout_accuracy)
  xgb_holdout_F1_score[i] <- (xgb_test_F1_score[i] + xgb_validation_F1_score[i]) / 2
  xgb_holdout_F1_score_mean <- mean(xgb_holdout_F1_score)
  xgb_holdout_positive_predictive_value[i] <- (xgb_test_positive_predictive_value[i] + xgb_validation_positive_predictive_value[i]) / 2
  xgb_holdout_positive_predictive_value_mean <- mean(xgb_holdout_positive_predictive_value)
  xgb_holdout_negative_predictive_value[i] <- (xgb_test_negative_predictive_value[i] + xgb_validation_negative_predictive_value[i]) / 2
  xgb_holdout_negative_predictive_value_mean <- mean(xgb_holdout_negative_predictive_value)
  xgb_holdout_overfitting[i] <- xgb_holdout_accuracy[i] / xgb_train_accuracy[i]
  xgb_holdout_overfitting_mean <- mean(xgb_holdout_overfitting)
  xgb_holdout_overfitting_range <- range(xgb_holdout_overfitting)

  xgb_table <- xgb_test_table + xgb_validation_table
  xgb_table_total <- xgb_table_total + xgb_table

  xgb_end <- Sys.time()
  xgb_duration[i] <- xgb_end - xgb_start
  xgb_duration_mean <- mean(xgb_duration)



  ##################################################################################################################

  ######################################## Ensembles start here ####################################################

  ##################################################################################################################

  ensemble1 <- data.frame(
    "ADA_boost" = c(adaboost_test_pred, adaboost_validation_pred),
    "Bagged_RF" = c(bag_rf_test_pred[, 2], bag_rf_validation_pred[, 2]),
    "Bagging" = c(bagging_test_pred[, 2], bagging_validation_pred[, 2]),
    "BayesGLM" = c(bayesglm_test_predictions, bayesglm_validation_predictions),
    "BayesRNN" = c(bayesrnn_test_predictions, bayesrnn_validation_predictions),
    "C50" = c(C50_test_predictions, C50_validation_predictions),
    "Cubist" = c(cubist_test_pred, cubist_validation_pred),
    "Flexible_Discriminant_Analysis" = c(fda_test_predictions, fda_validation_predictions),
    "Gradient_Boosted" = c(gb_test_predictions, gb_validation_predictions),
    "Generalized_Linear_Models" = c(glm_test_pred, glm_validation_pred),
    "Linear" = as.numeric(c(linear_test_pred, linear_validation_pred)) - 1,
    "Mixture_Discrmininant_Analysis" = c(mda_test_pred[, 2], mda_validation_pred[, 2]),
    "Naive_Bayes" = c(n_bayes_test_predictions, n_bayes_validation_predictions),
    "Penalized_Discriminant_Analysis" = as.numeric(c(pda_test_pred, pda_validation_pred)) - 1,
    "Quadratic_Discriminant_Analysis" = as.numeric(c(qda_test_pred$class, qda_validation_pred$class)) - 1,
    "Random_Forest" = c(rf_test_probabilities, rf_validation_probabilities),
    "Ranger" = c(ranger_test_pred, ranger_validation_pred),
    "RPart" = c(rpart_test_pred, rpart_validation_pred),
    "Support_Vector_Machines" = as.numeric(c(svm_test_pred, svm_validation_pred)) - 1,
    "Trees" = c(tree_test_predictions, tree_validation_predictions),
    "XGBoost" = c(xgb_test_predictions, xgb_validation_predictions)
  )

  ensemble_row_numbers <- as.numeric(c(row.names(test), row.names(validation)))
  ensemble1$y <- c(test$y, validation$y)

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

  ensemble_index <- sample(c(1:3), nrow(ensemble1), replace = TRUE, prob = c(train_amount, test_amount, validation_amount))
  ensemble_train <- ensemble1[ensemble_index == 1, ]
  ensemble_test <- ensemble1[ensemble_index == 2, ]
  ensemble_validation <- ensemble1[ensemble_index == 3, ]
  ensemble_y_train <- ensemble_train$y
  ensemble_y_test <- ensemble_test$y
  ensemble_y_validation <- ensemble_validation$y

  print(noquote(""))
  print("Working on the Ensembles section")
  print(noquote(""))

  #### Ensemble Using Adaboost ####
  ensemble_adaboost_start <- Sys.time()

  ensemble_adaboost_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "AdaBoostModel")

  ensemble_adaboost_train_pred <- stats::predict(ensemble_adaboost_train_fit, ensemble_train, type = "prob")
  ensemble_adaboost_train_probabilities <- ifelse(ensemble_adaboost_train_pred > 0.5, 1, 0)
  ensemble_adaboost_train_table <- table(ensemble_adaboost_train_probabilities, ensemble_y_train)
  ensemble_adaboost_train_true_positive_rate[i] <- ensemble_adaboost_train_table[2, 2] / sum(ensemble_adaboost_train_table[2, 2] + ensemble_adaboost_train_table[1, 2])
  ensemble_adaboost_train_true_positive_rate_mean <- mean(ensemble_adaboost_train_true_positive_rate)
  ensemble_adaboost_train_true_negative_rate[i] <- ensemble_adaboost_train_table[1, 1] / sum(ensemble_adaboost_train_table[1, 1] + ensemble_adaboost_train_table[2, 1])
  ensemble_adaboost_train_true_negative_rate_mean <- mean(ensemble_adaboost_train_true_negative_rate)
  ensemble_adaboost_train_false_positive_rate[i] <- ensemble_adaboost_train_table[2, 1] / sum(ensemble_adaboost_train_table[2, 1] + ensemble_adaboost_train_table[1, 1])
  ensemble_adaboost_train_false_positive_rate_mean <- mean(ensemble_adaboost_train_false_positive_rate)
  ensemble_adaboost_train_false_negative_rate[i] <- ensemble_adaboost_train_table[1, 2] / sum(ensemble_adaboost_train_table[1, 2] + ensemble_adaboost_train_table[2, 2])
  ensemble_adaboost_train_false_negative_rate_mean <- mean(ensemble_adaboost_train_false_negative_rate)
  ensemble_adaboost_train_accuracy[i] <- (ensemble_adaboost_train_table[1, 1] + ensemble_adaboost_train_table[2, 2]) / sum(ensemble_adaboost_train_table)
  ensemble_adaboost_train_accuracy_mean <- mean(ensemble_adaboost_train_accuracy)
  ensemble_adaboost_train_F1_score[i] <- 2 * (ensemble_adaboost_train_table[2, 2]) / sum(2 * ensemble_adaboost_train_table[2, 2] + ensemble_adaboost_train_table[1, 2] + ensemble_adaboost_train_table[2, 1])
  ensemble_adaboost_train_F1_score_mean <- mean(ensemble_adaboost_train_F1_score)
  ensemble_adaboost_train_positive_predictive_value[i] <- ensemble_adaboost_train_table[2, 2] / sum(ensemble_adaboost_train_table[2, 2] + ensemble_adaboost_train_table[2, 1])
  ensemble_adaboost_train_positive_predictive_value_mean <- mean(ensemble_adaboost_train_positive_predictive_value)
  ensemble_adaboost_train_negative_predictive_value[i] <- ensemble_adaboost_train_table[1, 1] / sum(ensemble_adaboost_train_table[1, 1] + ensemble_adaboost_train_table[1, 2])
  ensemble_adaboost_train_negative_predictive_value_mean <- mean(ensemble_adaboost_train_negative_predictive_value)

  ensemble_adaboost_test_pred <- stats::predict(ensemble_adaboost_train_fit, ensemble_test, type = "prob")
  ensemble_adaboost_test_probabilities <- ifelse(ensemble_adaboost_test_pred > 0.5, 1, 0)
  ensemble_adaboost_test_table <- table(ensemble_adaboost_test_probabilities, ensemble_y_test)
  ensemble_adaboost_test_true_positive_rate[i] <- ensemble_adaboost_test_table[2, 2] / sum(ensemble_adaboost_test_table[2, 2] + ensemble_adaboost_test_table[1, 2])
  ensemble_adaboost_test_true_positive_rate_mean <- mean(ensemble_adaboost_test_true_positive_rate)
  ensemble_adaboost_test_true_negative_rate[i] <- ensemble_adaboost_test_table[1, 1] / sum(ensemble_adaboost_test_table[1, 1] + ensemble_adaboost_test_table[2, 1])
  ensemble_adaboost_test_true_negative_rate_mean <- mean(ensemble_adaboost_test_true_negative_rate)
  ensemble_adaboost_test_false_positive_rate[i] <- ensemble_adaboost_test_table[2, 1] / sum(ensemble_adaboost_test_table[2, 1] + ensemble_adaboost_test_table[1, 1])
  ensemble_adaboost_test_false_positive_rate_mean <- mean(ensemble_adaboost_test_false_positive_rate)
  ensemble_adaboost_test_false_negative_rate[i] <- ensemble_adaboost_test_table[1, 2] / sum(ensemble_adaboost_test_table[1, 2] + ensemble_adaboost_test_table[2, 2])
  ensemble_adaboost_test_false_negative_rate_mean <- mean(ensemble_adaboost_test_false_negative_rate)
  ensemble_adaboost_test_accuracy[i] <- (ensemble_adaboost_test_table[1, 1] + ensemble_adaboost_test_table[2, 2]) / sum(ensemble_adaboost_test_table)
  ensemble_adaboost_test_accuracy_mean <- mean(ensemble_adaboost_test_accuracy)
  ensemble_adaboost_test_F1_score[i] <- 2 * (ensemble_adaboost_test_table[2, 2]) / sum(2 * ensemble_adaboost_test_table[2, 2] + ensemble_adaboost_test_table[1, 2] + ensemble_adaboost_test_table[2, 1])
  ensemble_adaboost_test_F1_score_mean <- mean(ensemble_adaboost_test_F1_score)
  ensemble_adaboost_test_positive_predictive_value[i] <- ensemble_adaboost_test_table[2, 2] / sum(ensemble_adaboost_test_table[2, 2] + ensemble_adaboost_test_table[2, 1])
  ensemble_adaboost_test_positive_predictive_value_mean <- mean(ensemble_adaboost_test_positive_predictive_value)
  ensemble_adaboost_test_negative_predictive_value[i] <- ensemble_adaboost_test_table[1, 1] / sum(ensemble_adaboost_test_table[1, 1] + ensemble_adaboost_test_table[1, 2])
  ensemble_adaboost_test_negative_predictive_value_mean <- mean(ensemble_adaboost_test_negative_predictive_value)

  ensemble_adaboost_validation_pred <- stats::predict(ensemble_adaboost_train_fit, ensemble_validation, type = "prob")
  ensemble_adaboost_validation_probabilities <- ifelse(ensemble_adaboost_validation_pred > 0.5, 1, 0)
  ensemble_adaboost_validation_table <- table(ensemble_adaboost_validation_probabilities, ensemble_y_validation)
  ensemble_adaboost_validation_true_positive_rate[i] <- ensemble_adaboost_validation_table[2, 2] / sum(ensemble_adaboost_validation_table[2, 2] + ensemble_adaboost_validation_table[1, 2])
  ensemble_adaboost_validation_true_positive_rate_mean <- mean(ensemble_adaboost_validation_true_positive_rate)
  ensemble_adaboost_validation_true_negative_rate[i] <- ensemble_adaboost_validation_table[1, 1] / sum(ensemble_adaboost_validation_table[1, 1] + ensemble_adaboost_validation_table[2, 1])
  ensemble_adaboost_validation_true_negative_rate_mean <- mean(ensemble_adaboost_validation_true_negative_rate)
  ensemble_adaboost_validation_false_positive_rate[i] <- ensemble_adaboost_validation_table[2, 1] / sum(ensemble_adaboost_validation_table[2, 1] + ensemble_adaboost_validation_table[1, 1])
  ensemble_adaboost_validation_false_positive_rate_mean <- mean(ensemble_adaboost_validation_false_positive_rate)
  ensemble_adaboost_validation_false_negative_rate[i] <- ensemble_adaboost_validation_table[1, 2] / sum(ensemble_adaboost_validation_table[1, 2] + ensemble_adaboost_validation_table[2, 2])
  ensemble_adaboost_validation_false_negative_rate_mean <- mean(ensemble_adaboost_validation_false_negative_rate)
  ensemble_adaboost_validation_accuracy[i] <- (ensemble_adaboost_validation_table[1, 1] + ensemble_adaboost_validation_table[2, 2]) / sum(ensemble_adaboost_validation_table)
  ensemble_adaboost_validation_accuracy_mean <- mean(ensemble_adaboost_validation_accuracy)
  ensemble_adaboost_validation_F1_score[i] <- 2 * (ensemble_adaboost_validation_table[2, 2]) / sum(2 * ensemble_adaboost_validation_table[2, 2] + ensemble_adaboost_validation_table[1, 2] + ensemble_adaboost_validation_table[2, 1])
  ensemble_adaboost_validation_F1_score_mean <- mean(ensemble_adaboost_validation_F1_score)
  ensemble_adaboost_validation_positive_predictive_value[i] <- ensemble_adaboost_validation_table[2, 2] / sum(ensemble_adaboost_validation_table[2, 2] + ensemble_adaboost_validation_table[2, 1])
  ensemble_adaboost_validation_positive_predictive_value_mean <- mean(ensemble_adaboost_validation_positive_predictive_value)
  ensemble_adaboost_validation_negative_predictive_value[i] <- ensemble_adaboost_validation_table[1, 1] / sum(ensemble_adaboost_validation_table[1, 1] + ensemble_adaboost_validation_table[1, 2])
  ensemble_adaboost_validation_negative_predictive_value_mean <- mean(ensemble_adaboost_validation_negative_predictive_value)

  ensemble_adaboost_holdout_true_positive_rate[i] <- (ensemble_adaboost_test_true_positive_rate[i] + ensemble_adaboost_validation_true_positive_rate[i]) / 2
  ensemble_adaboost_holdout_true_positive_rate_mean <- mean(ensemble_adaboost_holdout_true_positive_rate)
  ensemble_adaboost_holdout_true_negative_rate[i] <- (ensemble_adaboost_test_true_negative_rate[i] + ensemble_adaboost_validation_true_negative_rate[i]) / 2
  ensemble_adaboost_holdout_true_negative_rate_mean <- mean(ensemble_adaboost_holdout_true_negative_rate)
  ensemble_adaboost_holdout_false_positive_rate[i] <- (ensemble_adaboost_test_false_positive_rate[i] + ensemble_adaboost_validation_false_positive_rate[i]) / 2
  ensemble_adaboost_holdout_false_positive_rate_mean <- mean(ensemble_adaboost_holdout_false_positive_rate)
  ensemble_adaboost_holdout_false_negative_rate[i] <- (ensemble_adaboost_test_false_negative_rate[i] + ensemble_adaboost_validation_false_negative_rate[i]) / 2
  ensemble_adaboost_holdout_false_negative_rate_mean <- mean(ensemble_adaboost_holdout_false_negative_rate)
  ensemble_adaboost_holdout_accuracy[i] <- (ensemble_adaboost_test_accuracy[i] + ensemble_adaboost_validation_accuracy[i]) / 2
  ensemble_adaboost_holdout_accuracy_mean <- mean(ensemble_adaboost_holdout_accuracy)
  ensemble_adaboost_holdout_F1_score[i] <- (ensemble_adaboost_test_F1_score[i] + ensemble_adaboost_validation_F1_score[i]) / 2
  ensemble_adaboost_holdout_F1_score_mean <- mean(ensemble_adaboost_holdout_F1_score)
  ensemble_adaboost_holdout_positive_predictive_value[i] <- (ensemble_adaboost_test_positive_predictive_value[i] + ensemble_adaboost_validation_positive_predictive_value[i]) / 2
  ensemble_adaboost_holdout_positive_predictive_value_mean <- mean(ensemble_adaboost_holdout_positive_predictive_value)
  ensemble_adaboost_holdout_negative_predictive_value[i] <- (ensemble_adaboost_test_negative_predictive_value[i] + ensemble_adaboost_validation_negative_predictive_value[i]) / 2
  ensemble_adaboost_holdout_negative_predictive_value_mean <- mean(ensemble_adaboost_holdout_negative_predictive_value)
  ensemble_adaboost_holdout_overfitting[i] <- ensemble_adaboost_holdout_accuracy[i] / ensemble_adaboost_train_accuracy[i]
  ensemble_adaboost_holdout_overfitting_mean <- mean(ensemble_adaboost_holdout_overfitting)
  ensemble_adaboost_holdout_overfitting_range <- range(ensemble_adaboost_holdout_overfitting)

  ensemble_adaboost_table <- ensemble_adaboost_test_table + ensemble_adaboost_validation_table
  ensemble_adaboost_table_total <- ensemble_adaboost_table_total + ensemble_adaboost_table

  ensemble_adaboost_end <- Sys.time()
  ensemble_adaboost_duration[i] <- ensemble_adaboost_end - ensemble_adaboost_start
  ensemble_adaboost_duration_mean <- mean(ensemble_adaboost_duration)

  #### Ensemble Using Bagging ####
  ensemble_bagging_start <- Sys.time()

  ensemble_bagging_train_fit <- ipred::bagging(as.factor(y) ~ ., data = ensemble_train, coob = TRUE)


  ensemble_bagging_train_pred <- stats::predict(ensemble_bagging_train_fit, ensemble_train, type = "prob")
  ensemble_bagging_train_probabilities <- ifelse(ensemble_bagging_train_pred[, 2] > 0.5, 1, 0)
  ensemble_bagging_train_table <- table(ensemble_bagging_train_probabilities, ensemble_y_train)
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

  ensemble_bagging_test_pred <- stats::predict(ensemble_bagging_train_fit, ensemble_test, type = "prob")
  ensemble_bagging_test_probabilities <- ifelse(ensemble_bagging_test_pred[, 2] > 0.5, 1, 0)
  ensemble_bagging_test_table <- table(ensemble_bagging_test_probabilities, ensemble_y_test)
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

  ensemble_bagging_validation_pred <- stats::predict(ensemble_bagging_train_fit, ensemble_validation, type = "prob")
  ensemble_bagging_validation_probabilities <- ifelse(ensemble_bagging_validation_pred[, 2] > 0.5, 1, 0)
  ensemble_bagging_validation_table <- table(ensemble_bagging_validation_probabilities, ensemble_y_validation)
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
  ensemble_bagging_holdout_F1_score[i] <- (ensemble_bagging_test_F1_score[i] + ensemble_bagging_validation_F1_score[i]) / 2
  ensemble_bagging_holdout_F1_score_mean <- mean(ensemble_bagging_holdout_F1_score)
  ensemble_bagging_holdout_positive_predictive_value[i] <- (ensemble_bagging_test_positive_predictive_value[i] + ensemble_bagging_validation_positive_predictive_value[i]) / 2
  ensemble_bagging_holdout_positive_predictive_value_mean <- mean(ensemble_bagging_holdout_positive_predictive_value)
  ensemble_bagging_holdout_negative_predictive_value[i] <- (ensemble_bagging_test_negative_predictive_value[i] + ensemble_bagging_validation_negative_predictive_value[i]) / 2
  ensemble_bagging_holdout_negative_predictive_value_mean <- mean(ensemble_bagging_holdout_negative_predictive_value)
  ensemble_bagging_holdout_overfitting[i] <- ensemble_bagging_holdout_accuracy[i] / ensemble_bagging_train_accuracy[i]
  ensemble_bagging_holdout_overfitting_mean <- mean(ensemble_bagging_holdout_overfitting)
  ensemble_bagging_holdout_overfitting_range <- range(ensemble_bagging_holdout_overfitting)

  ensemble_bagging_table <- ensemble_bagging_test_table + ensemble_bagging_validation_table
  ensemble_bagging_table_total <- ensemble_bagging_table_total + ensemble_bagging_table

  ensemble_bagging_end <- Sys.time()
  ensemble_bagging_duration[i] <- ensemble_bagging_end - ensemble_bagging_start
  ensemble_bagging_duration_mean <- mean(ensemble_bagging_duration)


  #### Ensemble Using ensemble_C50 ####
  ensemble_C50_start <- Sys.time()

  ensemble_C50_train_fit <- C50::C5.0(as.factor(ensemble_y_train) ~ ., data = ensemble_train)

  ensemble_C50_train_pred <- stats::predict(ensemble_C50_train_fit, ensemble_train, type = "prob")
  ensemble_C50_train_probabilities <- ifelse(ensemble_C50_train_pred[, 2] > 0.5, 1, 0)
  ensemble_C50_train_table <- table(ensemble_C50_train_probabilities, ensemble_y_train)
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

  ensemble_C50_test_pred <- stats::predict(ensemble_C50_train_fit, ensemble_test, type = "prob")
  ensemble_C50_test_probabilities <- ifelse(ensemble_C50_test_pred[, 2] > 0.5, 1, 0)
  ensemble_C50_test_table <- table(ensemble_C50_test_probabilities, ensemble_y_test)
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

  ensemble_C50_validation_pred <- stats::predict(ensemble_C50_train_fit, ensemble_validation, type = "prob")
  ensemble_C50_validation_probabilities <- ifelse(ensemble_C50_validation_pred[, 2] > 0.5, 1, 0)
  ensemble_C50_validation_table <- table(ensemble_C50_validation_probabilities, ensemble_y_validation)
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
  ensemble_C50_holdout_F1_score[i] <- (ensemble_C50_test_F1_score[i] + ensemble_C50_validation_F1_score[i]) / 2
  ensemble_C50_holdout_F1_score_mean <- mean(ensemble_C50_holdout_F1_score)
  ensemble_C50_holdout_positive_predictive_value[i] <- (ensemble_C50_test_positive_predictive_value[i] + ensemble_C50_validation_positive_predictive_value[i]) / 2
  ensemble_C50_holdout_positive_predictive_value_mean <- mean(ensemble_C50_holdout_positive_predictive_value)
  ensemble_C50_holdout_negative_predictive_value[i] <- (ensemble_C50_test_negative_predictive_value[i] + ensemble_C50_validation_negative_predictive_value[i]) / 2
  ensemble_C50_holdout_negative_predictive_value_mean <- mean(ensemble_C50_holdout_negative_predictive_value)
  ensemble_C50_holdout_overfitting[i] <- ensemble_C50_holdout_accuracy[i] / ensemble_C50_train_accuracy[i]
  ensemble_C50_holdout_overfitting_mean <- mean(ensemble_C50_holdout_overfitting)
  ensemble_C50_holdout_overfitting_range <- range(ensemble_C50_holdout_overfitting)

  ensemble_C50_table <- ensemble_C50_test_table + ensemble_C50_validation_table
  ensemble_C50_table_total <- ensemble_C50_table_total + ensemble_C50_table

  ensemble_C50_end <- Sys.time()
  ensemble_C50_duration[i] <- ensemble_C50_end - ensemble_C50_start
  ensemble_C50_duration_mean <- mean(ensemble_C50_duration)


  #### Ensemble Using Gradient Boosted ####
  ensemble_gb_start <- Sys.time()

  ensemble_gb_train_fit <- gbm::gbm(ensemble_train$y ~ ., data = ensemble_train, distribution = "gaussian", n.trees = 100, shrinkage = 0.1, interaction.depth = 10)

  ensemble_gb_train_pred <- stats::predict(ensemble_gb_train_fit, ensemble_train, type = "response")
  ensemble_gb_train_probabilities <- ifelse(ensemble_gb_train_pred > 0.5, 1, 0)
  ensemble_gb_train_table <- table(ensemble_gb_train_probabilities, ensemble_y_train)
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
  ensemble_gb_test_probabilities <- ifelse(ensemble_gb_test_pred > 0.5, 1, 0)
  ensemble_gb_test_table <- table(ensemble_gb_test_probabilities, ensemble_y_test)
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
  ensemble_gb_validation_probabilities <- ifelse(ensemble_gb_validation_pred > 0.5, 1, 0)
  ensemble_gb_validation_table <- table(ensemble_gb_validation_probabilities, ensemble_y_validation)
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
  ensemble_gb_holdout_F1_score[i] <- (ensemble_gb_test_F1_score[i] + ensemble_gb_validation_F1_score[i]) / 2
  ensemble_gb_holdout_F1_score_mean <- mean(ensemble_gb_holdout_F1_score)
  ensemble_gb_holdout_positive_predictive_value[i] <- (ensemble_gb_test_positive_predictive_value[i] + ensemble_gb_validation_positive_predictive_value[i]) / 2
  ensemble_gb_holdout_positive_predictive_value_mean <- mean(ensemble_gb_holdout_positive_predictive_value)
  ensemble_gb_holdout_negative_predictive_value[i] <- (ensemble_gb_test_negative_predictive_value[i] + ensemble_gb_validation_negative_predictive_value[i]) / 2
  ensemble_gb_holdout_negative_predictive_value_mean <- mean(ensemble_gb_holdout_negative_predictive_value)
  ensemble_gb_holdout_overfitting[i] <- ensemble_gb_holdout_accuracy[i] / ensemble_gb_train_accuracy[i]
  ensemble_gb_holdout_overfitting_mean <- mean(ensemble_gb_holdout_overfitting)
  ensemble_gb_holdout_overfitting_range <- range(ensemble_gb_holdout_overfitting)

  ensemble_gb_table <- ensemble_gb_test_table + ensemble_gb_validation_table
  ensemble_gb_table_total <- ensemble_gb_table_total + ensemble_gb_table

  ensemble_gb_end <- Sys.time()
  ensemble_gb_duration[i] <- ensemble_gb_end - ensemble_gb_start
  ensemble_gb_duration_mean <- mean(ensemble_gb_duration)


  #### Ensemble Using ensemble_pls ####
  ensemble_pls_start <- Sys.time()

  ensemble_pls_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "PLSModel")

  ensemble_pls_train_pred <- stats::predict(ensemble_pls_train_fit, ensemble_train, type = "response")
  ensemble_pls_train_table <- table(ensemble_pls_train_pred, ensemble_y_train)
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

  ensemble_pls_test_pred <- predict(ensemble_pls_train_fit, ensemble_test, type = "response")
  ensemble_pls_test_table <- table(ensemble_pls_test_pred, ensemble_y_test)
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
  ensemble_pls_validation_table <- table(ensemble_pls_validation_pred, ensemble_y_validation)
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
  ensemble_pls_holdout_F1_score[i] <- (ensemble_pls_test_F1_score[i] + ensemble_pls_validation_F1_score[i]) / 2
  ensemble_pls_holdout_F1_score_mean <- mean(ensemble_pls_holdout_F1_score)
  ensemble_pls_holdout_positive_predictive_value[i] <- (ensemble_pls_test_positive_predictive_value[i] + ensemble_pls_validation_positive_predictive_value[i]) / 2
  ensemble_pls_holdout_positive_predictive_value_mean <- mean(ensemble_pls_holdout_positive_predictive_value)
  ensemble_pls_holdout_negative_predictive_value[i] <- (ensemble_pls_test_negative_predictive_value[i] + ensemble_pls_validation_negative_predictive_value[i]) / 2
  ensemble_pls_holdout_negative_predictive_value_mean <- mean(ensemble_pls_holdout_negative_predictive_value)
  ensemble_pls_holdout_overfitting[i] <- ensemble_pls_holdout_accuracy[i] / ensemble_pls_train_accuracy[i]
  ensemble_pls_holdout_overfitting_mean <- mean(ensemble_pls_holdout_overfitting)
  ensemble_pls_holdout_overfitting_range <- range(ensemble_pls_holdout_overfitting)

  ensemble_pls_table <- ensemble_pls_test_table + ensemble_pls_validation_table
  ensemble_pls_table_total <- ensemble_pls_table_total + ensemble_pls_table

  ensemble_pls_end <- Sys.time()
  ensemble_pls_duration[i] <- ensemble_pls_end - ensemble_pls_start
  ensemble_pls_duration_mean <- mean(ensemble_pls_duration)


  #### Ensemble Using ensemble_pda ####
  ensemble_pda_start <- Sys.time()

  ensemble_pda_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "PDAModel")

  ensemble_pda_train_pred <- stats::predict(ensemble_pda_train_fit, ensemble_train, type = "response")
  ensemble_pda_train_table <- table(ensemble_pda_train_pred, ensemble_y_train)
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

  ensemble_pda_test_pred <- predict(ensemble_pda_train_fit, ensemble_test, type = "response")
  ensemble_pda_test_table <- table(ensemble_pda_test_pred, ensemble_y_test)
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
  ensemble_pda_validation_table <- table(ensemble_pda_validation_pred, ensemble_y_validation)
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
  ensemble_pda_holdout_F1_score[i] <- (ensemble_pda_test_F1_score[i] + ensemble_pda_validation_F1_score[i]) / 2
  ensemble_pda_holdout_F1_score_mean <- mean(ensemble_pda_holdout_F1_score)
  ensemble_pda_holdout_positive_predictive_value[i] <- (ensemble_pda_test_positive_predictive_value[i] + ensemble_pda_validation_positive_predictive_value[i]) / 2
  ensemble_pda_holdout_positive_predictive_value_mean <- mean(ensemble_pda_holdout_positive_predictive_value)
  ensemble_pda_holdout_negative_predictive_value[i] <- (ensemble_pda_test_negative_predictive_value[i] + ensemble_pda_validation_negative_predictive_value[i]) / 2
  ensemble_pda_holdout_negative_predictive_value_mean <- mean(ensemble_pda_holdout_negative_predictive_value)
  ensemble_pda_holdout_overfitting[i] <- ensemble_pda_holdout_accuracy[i] / ensemble_pda_train_accuracy[i]
  ensemble_pda_holdout_overfitting_mean <- mean(ensemble_pda_holdout_overfitting)
  ensemble_pda_holdout_overfitting_range <- range(ensemble_pda_holdout_overfitting)

  ensemble_pda_table <- ensemble_pda_test_table + ensemble_pda_validation_table
  ensemble_pda_table_total <- ensemble_pda_table_total + ensemble_pda_table

  ensemble_pda_end <- Sys.time()
  ensemble_pda_duration[i] <- ensemble_pda_end - ensemble_pda_start
  ensemble_pda_duration_mean <- mean(ensemble_pda_duration)


  #### Ensemble Using Random Forest ####
  ensemble_rf_start <- Sys.time()

  ensemble_rf_train_fit <- randomForest(x = ensemble_train, y = as.factor(ensemble_y_train), data = ensemble1)

  ensemble_rf_train_pred <- stats::predict(ensemble_rf_train_fit, ensemble_train, type = "prob")
  ensemble_rf_train_predictions <- ifelse(ensemble_rf_train_pred > 0.50, 1, 0)[, 2]
  ensemble_rf_train_table <- table(ensemble_rf_train_predictions, ensemble_y_train)
  ensemble_rf_train_true_positive_rate[i] <- ensemble_rf_train_table[2, 2] / sum(ensemble_rf_train_table[2, 2] + ensemble_rf_train_table[1, 2])
  ensemble_rf_train_true_positive_rate_mean <- mean(ensemble_rf_train_true_positive_rate)
  ensemble_rf_train_true_negative_rate[i] <- ensemble_rf_train_table[1, 1] / sum(ensemble_rf_train_table[1, 1] + ensemble_rf_train_table[2, 1])
  ensemble_rf_train_true_negative_rate_mean <- mean(ensemble_rf_train_true_negative_rate)
  ensemble_rf_train_false_positive_rate[i] <- ensemble_rf_train_table[2, 1] / sum(ensemble_rf_train_table[2, 1] + ensemble_rf_train_table[1, 1])
  ensemble_rf_train_false_positive_rate_mean <- mean(ensemble_rf_train_false_positive_rate)
  ensemble_rf_train_false_negative_rate[i] <- ensemble_rf_train_table[1, 2] / sum(ensemble_rf_train_table[1, 2] + ensemble_rf_train_table[2, 2])
  ensemble_rf_train_false_negative_rate_mean <- mean(ensemble_rf_train_false_negative_rate)
  ensemble_rf_train_accuracy[i] <- (ensemble_rf_train_table[1, 1] + ensemble_rf_train_table[2, 2]) / sum(ensemble_rf_train_table)
  ensemble_rf_train_accuracy_mean <- mean(ensemble_rf_train_accuracy)
  ensemble_rf_train_F1_score[i] <- 2 * (ensemble_rf_train_table[2, 2]) / sum(2 * ensemble_rf_train_table[2, 2] + ensemble_rf_train_table[1, 2] + ensemble_rf_train_table[2, 1])
  ensemble_rf_train_F1_score_mean <- mean(ensemble_rf_train_F1_score)
  ensemble_rf_train_positive_predictive_value[i] <- ensemble_rf_train_table[2, 2] / sum(ensemble_rf_train_table[2, 2] + ensemble_rf_train_table[2, 1])
  ensemble_rf_train_positive_predictive_value_mean <- mean(ensemble_rf_train_positive_predictive_value)
  ensemble_rf_train_negative_predictive_value[i] <- ensemble_rf_train_table[1, 1] / sum(ensemble_rf_train_table[1, 1] + ensemble_rf_train_table[1, 2])
  ensemble_rf_train_negative_predictive_value_mean <- mean(ensemble_rf_train_negative_predictive_value)

  ensemble_rf_test_pred <- stats::predict(ensemble_rf_train_fit, ensemble_test, type = "prob")
  ensemble_rf_test_predictions <- ifelse(ensemble_rf_test_pred > 0.50, 1, 0)[, 2]
  ensemble_rf_test_table <- table(ensemble_rf_test_predictions, ensemble_y_test)
  ensemble_rf_test_true_positive_rate[i] <- ensemble_rf_test_table[2, 2] / sum(ensemble_rf_test_table[2, 2] + ensemble_rf_test_table[1, 2])
  ensemble_rf_test_true_positive_rate_mean <- mean(ensemble_rf_test_true_positive_rate)
  ensemble_rf_test_true_negative_rate[i] <- ensemble_rf_test_table[1, 1] / sum(ensemble_rf_test_table[1, 1] + ensemble_rf_test_table[2, 1])
  ensemble_rf_test_true_negative_rate_mean <- mean(ensemble_rf_test_true_negative_rate)
  ensemble_rf_test_false_positive_rate[i] <- ensemble_rf_test_table[2, 1] / sum(ensemble_rf_test_table[2, 1] + ensemble_rf_test_table[1, 1])
  ensemble_rf_test_false_positive_rate_mean <- mean(ensemble_rf_test_false_positive_rate)
  ensemble_rf_test_false_negative_rate[i] <- ensemble_rf_test_table[1, 2] / sum(ensemble_rf_test_table[1, 2] + ensemble_rf_test_table[2, 2])
  ensemble_rf_test_false_negative_rate_mean <- mean(ensemble_rf_test_false_negative_rate)
  ensemble_rf_test_accuracy[i] <- (ensemble_rf_test_table[1, 1] + ensemble_rf_test_table[2, 2]) / sum(ensemble_rf_test_table)
  ensemble_rf_test_accuracy_mean <- mean(ensemble_rf_test_accuracy)
  ensemble_rf_test_F1_score[i] <- 2 * (ensemble_rf_test_table[2, 2]) / sum(2 * ensemble_rf_test_table[2, 2] + ensemble_rf_test_table[1, 2] + ensemble_rf_test_table[2, 1])
  ensemble_rf_test_F1_score_mean <- mean(ensemble_rf_test_F1_score)
  ensemble_rf_test_positive_predictive_value[i] <- ensemble_rf_test_table[2, 2] / sum(ensemble_rf_test_table[2, 2] + ensemble_rf_test_table[2, 1])
  ensemble_rf_test_positive_predictive_value_mean <- mean(ensemble_rf_test_positive_predictive_value)
  ensemble_rf_test_negative_predictive_value[i] <- ensemble_rf_test_table[1, 1] / sum(ensemble_rf_test_table[1, 1] + ensemble_rf_test_table[1, 2])
  ensemble_rf_test_negative_predictive_value_mean <- mean(ensemble_rf_test_negative_predictive_value)

  ensemble_rf_validation_pred <- stats::predict(ensemble_rf_train_fit, ensemble_validation, type = "prob")
  ensemble_rf_validation_probabilities <- ifelse(ensemble_rf_validation_pred > 0.5, 1, 0)[, 2]
  ensemble_rf_validation_table <- table(ensemble_rf_validation_probabilities, ensemble_y_validation)
  ensemble_rf_validation_true_positive_rate[i] <- ensemble_rf_validation_table[2, 2] / sum(ensemble_rf_validation_table[2, 2] + ensemble_rf_validation_table[1, 2])
  ensemble_rf_validation_true_positive_rate_mean <- mean(ensemble_rf_validation_true_positive_rate)
  ensemble_rf_validation_true_negative_rate[i] <- ensemble_rf_validation_table[1, 1] / sum(ensemble_rf_validation_table[1, 1] + ensemble_rf_validation_table[2, 1])
  ensemble_rf_validation_true_negative_rate_mean <- mean(ensemble_rf_validation_true_negative_rate)
  ensemble_rf_validation_false_positive_rate[i] <- ensemble_rf_validation_table[2, 1] / sum(ensemble_rf_validation_table[2, 1] + ensemble_rf_validation_table[1, 1])
  ensemble_rf_validation_false_positive_rate_mean <- mean(ensemble_rf_validation_false_positive_rate)
  ensemble_rf_validation_false_negative_rate[i] <- ensemble_rf_validation_table[1, 2] / sum(ensemble_rf_validation_table[1, 2] + ensemble_rf_validation_table[2, 2])
  ensemble_rf_validation_false_negative_rate_mean <- mean(ensemble_rf_validation_false_negative_rate)
  ensemble_rf_validation_accuracy[i] <- (ensemble_rf_validation_table[1, 1] + ensemble_rf_validation_table[2, 2]) / sum(ensemble_rf_validation_table)
  ensemble_rf_validation_accuracy_mean <- mean(ensemble_rf_validation_accuracy)
  ensemble_rf_validation_F1_score[i] <- 2 * (ensemble_rf_validation_table[2, 2]) / sum(2 * ensemble_rf_validation_table[2, 2] + ensemble_rf_validation_table[1, 2] + ensemble_rf_validation_table[2, 1])
  ensemble_rf_validation_F1_score_mean <- mean(ensemble_rf_validation_F1_score)
  ensemble_rf_validation_positive_predictive_value[i] <- ensemble_rf_validation_table[2, 2] / sum(ensemble_rf_validation_table[2, 2] + ensemble_rf_validation_table[2, 1])
  ensemble_rf_validation_positive_predictive_value_mean <- mean(ensemble_rf_validation_positive_predictive_value)
  ensemble_rf_validation_negative_predictive_value[i] <- ensemble_rf_validation_table[1, 1] / sum(ensemble_rf_validation_table[1, 1] + ensemble_rf_validation_table[1, 2])
  ensemble_rf_validation_negative_predictive_value_mean <- mean(ensemble_rf_validation_negative_predictive_value)

  ensemble_rf_holdout_true_positive_rate[i] <- (ensemble_rf_test_true_positive_rate[i] + ensemble_rf_validation_true_positive_rate[i]) / 2
  ensemble_rf_holdout_true_positive_rate_mean <- mean(ensemble_rf_holdout_true_positive_rate)
  ensemble_rf_holdout_true_negative_rate[i] <- (ensemble_rf_test_true_negative_rate[i] + ensemble_rf_validation_true_negative_rate[i]) / 2
  ensemble_rf_holdout_true_negative_rate_mean <- mean(ensemble_rf_holdout_true_negative_rate)
  ensemble_rf_holdout_false_positive_rate[i] <- (ensemble_rf_test_false_positive_rate[i] + ensemble_rf_validation_false_positive_rate[i]) / 2
  ensemble_rf_holdout_false_positive_rate_mean <- mean(ensemble_rf_holdout_false_positive_rate)
  ensemble_rf_holdout_false_negative_rate[i] <- (ensemble_rf_test_false_negative_rate[i] + ensemble_rf_validation_false_negative_rate[i]) / 2
  ensemble_rf_holdout_false_negative_rate_mean <- mean(ensemble_rf_holdout_false_negative_rate)
  ensemble_rf_holdout_accuracy[i] <- (ensemble_rf_test_accuracy[i] + ensemble_rf_validation_accuracy[i]) / 2
  ensemble_rf_holdout_accuracy_mean <- mean(ensemble_rf_holdout_accuracy)
  ensemble_rf_holdout_F1_score[i] <- (ensemble_rf_test_F1_score[i] + ensemble_rf_validation_F1_score[i]) / 2
  ensemble_rf_holdout_F1_score_mean <- mean(ensemble_rf_holdout_F1_score)
  ensemble_rf_holdout_positive_predictive_value[i] <- (ensemble_rf_test_positive_predictive_value[i] + ensemble_rf_validation_positive_predictive_value[i]) / 2
  ensemble_rf_holdout_positive_predictive_value_mean <- mean(ensemble_rf_holdout_positive_predictive_value)
  ensemble_rf_holdout_negative_predictive_value[i] <- (ensemble_rf_test_negative_predictive_value[i] + ensemble_rf_validation_negative_predictive_value[i]) / 2
  ensemble_rf_holdout_negative_predictive_value_mean <- mean(ensemble_rf_holdout_negative_predictive_value)
  ensemble_rf_holdout_overfitting[i] <- ensemble_rf_holdout_accuracy[i] / ensemble_rf_train_accuracy[i]
  ensemble_rf_holdout_overfitting_mean <- mean(ensemble_rf_holdout_overfitting)
  ensemble_rf_holdout_overfitting_range <- range(ensemble_rf_holdout_overfitting)

  ensemble_rf_table <- ensemble_rf_test_table + ensemble_rf_validation_table
  ensemble_rf_table_total <- ensemble_rf_table_total + ensemble_rf_table

  ensemble_rf_end <- Sys.time()
  ensemble_rf_duration[i] <- ensemble_rf_end - ensemble_rf_start
  ensemble_rf_duration_mean <- mean(ensemble_rf_duration)


  #### Ensemble Using Ranger ####
  ensemble_ranger_start <- Sys.time()

  ensemble_ranger_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "RangerModel")

  ensemble_ranger_train_pred <- stats::predict(ensemble_ranger_train_fit, ensemble_train, type = "response")
  ensemble_ranger_train_table <- table(ensemble_ranger_train_pred, ensemble_y_train)
  ensemble_ranger_train_true_positive_rate[i] <- ensemble_ranger_train_table[2, 2] / sum(ensemble_ranger_train_table[2, 2] + ensemble_ranger_train_table[1, 2])
  ensemble_ranger_train_true_positive_rate_mean <- mean(ensemble_ranger_train_true_positive_rate)
  ensemble_ranger_train_true_negative_rate[i] <- ensemble_ranger_train_table[1, 1] / sum(ensemble_ranger_train_table[1, 1] + ensemble_ranger_train_table[2, 1])
  ensemble_ranger_train_true_negative_rate_mean <- mean(ensemble_ranger_train_true_negative_rate)
  ensemble_ranger_train_false_positive_rate[i] <- ensemble_ranger_train_table[2, 1] / sum(ensemble_ranger_train_table[2, 1] + ensemble_ranger_train_table[1, 1])
  ensemble_ranger_train_false_positive_rate_mean <- mean(ensemble_ranger_train_false_positive_rate)
  ensemble_ranger_train_false_negative_rate[i] <- ensemble_ranger_train_table[1, 2] / sum(ensemble_ranger_train_table[1, 2] + ensemble_ranger_train_table[2, 2])
  ensemble_ranger_train_false_negative_rate_mean <- mean(ensemble_ranger_train_false_negative_rate)
  ensemble_ranger_train_accuracy[i] <- (ensemble_ranger_train_table[1, 1] + ensemble_ranger_train_table[2, 2]) / sum(ensemble_ranger_train_table)
  ensemble_ranger_train_accuracy_mean <- mean(ensemble_ranger_train_accuracy)
  ensemble_ranger_train_F1_score[i] <- 2 * (ensemble_ranger_train_table[2, 2]) / sum(2 * ensemble_ranger_train_table[2, 2] + ensemble_ranger_train_table[1, 2] + ensemble_ranger_train_table[2, 1])
  ensemble_ranger_train_F1_score_mean <- mean(ensemble_ranger_train_F1_score)
  ensemble_ranger_train_positive_predictive_value[i] <- ensemble_ranger_train_table[2, 2] / sum(ensemble_ranger_train_table[2, 2] + ensemble_ranger_train_table[2, 1])
  ensemble_ranger_train_positive_predictive_value_mean <- mean(ensemble_ranger_train_positive_predictive_value)
  ensemble_ranger_train_negative_predictive_value[i] <- ensemble_ranger_train_table[1, 1] / sum(ensemble_ranger_train_table[1, 1] + ensemble_ranger_train_table[1, 2])
  ensemble_ranger_train_negative_predictive_value_mean <- mean(ensemble_ranger_train_negative_predictive_value)

  ensemble_ranger_test_pred <- stats::predict(ensemble_ranger_train_fit, ensemble_test, type = "response")
  ensemble_ranger_test_table <- table(ensemble_ranger_test_pred, ensemble_y_test)
  ensemble_ranger_test_true_positive_rate[i] <- ensemble_ranger_test_table[2, 2] / sum(ensemble_ranger_test_table[2, 2] + ensemble_ranger_test_table[1, 2])
  ensemble_ranger_test_true_positive_rate_mean <- mean(ensemble_ranger_test_true_positive_rate)
  ensemble_ranger_test_true_negative_rate[i] <- ensemble_ranger_test_table[1, 1] / sum(ensemble_ranger_test_table[1, 1] + ensemble_ranger_test_table[2, 1])
  ensemble_ranger_test_true_negative_rate_mean <- mean(ensemble_ranger_test_true_negative_rate)
  ensemble_ranger_test_false_positive_rate[i] <- ensemble_ranger_test_table[2, 1] / sum(ensemble_ranger_test_table[2, 1] + ensemble_ranger_test_table[1, 1])
  ensemble_ranger_test_false_positive_rate_mean <- mean(ensemble_ranger_test_false_positive_rate)
  ensemble_ranger_test_false_negative_rate[i] <- ensemble_ranger_test_table[1, 2] / sum(ensemble_ranger_test_table[1, 2] + ensemble_ranger_test_table[2, 2])
  ensemble_ranger_test_false_negative_rate_mean <- mean(ensemble_ranger_test_false_negative_rate)
  ensemble_ranger_test_accuracy[i] <- (ensemble_ranger_test_table[1, 1] + ensemble_ranger_test_table[2, 2]) / sum(ensemble_ranger_test_table)
  ensemble_ranger_test_accuracy_mean <- mean(ensemble_ranger_test_accuracy)
  ensemble_ranger_test_F1_score[i] <- 2 * (ensemble_ranger_test_table[2, 2]) / sum(2 * ensemble_ranger_test_table[2, 2] + ensemble_ranger_test_table[1, 2] + ensemble_ranger_test_table[2, 1])
  ensemble_ranger_test_F1_score_mean <- mean(ensemble_ranger_test_F1_score)
  ensemble_ranger_test_positive_predictive_value[i] <- ensemble_ranger_test_table[2, 2] / sum(ensemble_ranger_test_table[2, 2] + ensemble_ranger_test_table[2, 1])
  ensemble_ranger_test_positive_predictive_value_mean <- mean(ensemble_ranger_test_positive_predictive_value)
  ensemble_ranger_test_negative_predictive_value[i] <- ensemble_ranger_test_table[1, 1] / sum(ensemble_ranger_test_table[1, 1] + ensemble_ranger_test_table[1, 2])
  ensemble_ranger_test_negative_predictive_value_mean <- mean(ensemble_ranger_test_negative_predictive_value)

  ensemble_ranger_validation_pred <- stats::predict(ensemble_ranger_train_fit, ensemble_validation, type = "response")
  ensemble_ranger_validation_table <- table(ensemble_ranger_validation_pred, ensemble_y_validation)
  ensemble_ranger_validation_true_positive_rate[i] <- ensemble_ranger_validation_table[2, 2] / sum(ensemble_ranger_validation_table[2, 2] + ensemble_ranger_validation_table[1, 2])
  ensemble_ranger_validation_true_positive_rate_mean <- mean(ensemble_ranger_validation_true_positive_rate)
  ensemble_ranger_validation_true_negative_rate[i] <- ensemble_ranger_validation_table[1, 1] / sum(ensemble_ranger_validation_table[1, 1] + ensemble_ranger_validation_table[2, 1])
  ensemble_ranger_validation_true_negative_rate_mean <- mean(ensemble_ranger_validation_true_negative_rate)
  ensemble_ranger_validation_false_positive_rate[i] <- ensemble_ranger_validation_table[2, 1] / sum(ensemble_ranger_validation_table[2, 1] + ensemble_ranger_validation_table[1, 1])
  ensemble_ranger_validation_false_positive_rate_mean <- mean(ensemble_ranger_validation_false_positive_rate)
  ensemble_ranger_validation_false_negative_rate[i] <- ensemble_ranger_validation_table[1, 2] / sum(ensemble_ranger_validation_table[1, 2] + ensemble_ranger_validation_table[2, 2])
  ensemble_ranger_validation_false_negative_rate_mean <- mean(ensemble_ranger_validation_false_negative_rate)
  ensemble_ranger_validation_accuracy[i] <- (ensemble_ranger_validation_table[1, 1] + ensemble_ranger_validation_table[2, 2]) / sum(ensemble_ranger_validation_table)
  ensemble_ranger_validation_accuracy_mean <- mean(ensemble_ranger_validation_accuracy)
  ensemble_ranger_validation_F1_score[i] <- 2 * (ensemble_ranger_validation_table[2, 2]) / sum(2 * ensemble_ranger_validation_table[2, 2] + ensemble_ranger_validation_table[1, 2] + ensemble_ranger_validation_table[2, 1])
  ensemble_ranger_validation_F1_score_mean <- mean(ensemble_ranger_validation_F1_score)
  ensemble_ranger_validation_positive_predictive_value[i] <- ensemble_ranger_validation_table[2, 2] / sum(ensemble_ranger_validation_table[2, 2] + ensemble_ranger_validation_table[2, 1])
  ensemble_ranger_validation_positive_predictive_value_mean <- mean(ensemble_ranger_validation_positive_predictive_value)
  ensemble_ranger_validation_negative_predictive_value[i] <- ensemble_ranger_validation_table[1, 1] / sum(ensemble_ranger_validation_table[1, 1] + ensemble_ranger_validation_table[1, 2])
  ensemble_ranger_validation_negative_predictive_value_mean <- mean(ensemble_ranger_validation_negative_predictive_value)

  ensemble_ranger_holdout_true_positive_rate[i] <- (ensemble_ranger_test_true_positive_rate[i] + ensemble_ranger_validation_true_positive_rate[i]) / 2
  ensemble_ranger_holdout_true_positive_rate_mean <- mean(ensemble_ranger_holdout_true_positive_rate)
  ensemble_ranger_holdout_true_negative_rate[i] <- (ensemble_ranger_test_true_negative_rate[i] + ensemble_ranger_validation_true_negative_rate[i]) / 2
  ensemble_ranger_holdout_true_negative_rate_mean <- mean(ensemble_ranger_holdout_true_negative_rate)
  ensemble_ranger_holdout_false_positive_rate[i] <- (ensemble_ranger_test_false_positive_rate[i] + ensemble_ranger_validation_false_positive_rate[i]) / 2
  ensemble_ranger_holdout_false_positive_rate_mean <- mean(ensemble_ranger_holdout_false_positive_rate)
  ensemble_ranger_holdout_false_negative_rate[i] <- (ensemble_ranger_test_false_negative_rate[i] + ensemble_ranger_validation_false_negative_rate[i]) / 2
  ensemble_ranger_holdout_false_negative_rate_mean <- mean(ensemble_ranger_holdout_false_negative_rate)
  ensemble_ranger_holdout_accuracy[i] <- (ensemble_ranger_test_accuracy[i] + ensemble_ranger_validation_accuracy[i]) / 2
  ensemble_ranger_holdout_accuracy_mean <- mean(ensemble_ranger_holdout_accuracy)
  ensemble_ranger_holdout_F1_score[i] <- (ensemble_ranger_test_F1_score[i] + ensemble_ranger_validation_F1_score[i]) / 2
  ensemble_ranger_holdout_F1_score_mean <- mean(ensemble_ranger_holdout_F1_score)
  ensemble_ranger_holdout_positive_predictive_value[i] <- (ensemble_ranger_test_positive_predictive_value[i] + ensemble_ranger_validation_positive_predictive_value[i]) / 2
  ensemble_ranger_holdout_positive_predictive_value_mean <- mean(ensemble_ranger_holdout_positive_predictive_value)
  ensemble_ranger_holdout_negative_predictive_value[i] <- (ensemble_ranger_test_negative_predictive_value[i] + ensemble_ranger_validation_negative_predictive_value[i]) / 2
  ensemble_ranger_holdout_negative_predictive_value_mean <- mean(ensemble_ranger_holdout_negative_predictive_value)
  ensemble_ranger_holdout_overfitting[i] <- ensemble_ranger_holdout_accuracy[i] / ensemble_ranger_train_accuracy[i]
  ensemble_ranger_holdout_overfitting_mean <- mean(ensemble_ranger_holdout_overfitting)
  ensemble_ranger_holdout_overfitting_range <- range(ensemble_ranger_holdout_overfitting)

  ensemble_ranger_table <- ensemble_ranger_test_table + ensemble_ranger_validation_table
  ensemble_ranger_table_total <- ensemble_ranger_table_total + ensemble_ranger_table

  ensemble_ranger_end <- Sys.time()
  ensemble_ranger_duration[i] <- ensemble_ranger_end - ensemble_ranger_start
  ensemble_ranger_duration_mean <- mean(ensemble_ranger_duration)


  #### Ensemble Using Regularized Discriminant Analysis ####
  ensemble_rda_start <- Sys.time()

  ensemble_rda_train_fit <- klaR::rda(as.factor(y) ~ ., data = ensemble_train)

  ensemble_rda_train_pred <- stats::predict(ensemble_rda_train_fit, ensemble_train, type = "response")
  ensemble_rda_train_table <- table(ensemble_rda_train_pred$class, ensemble_y_train)
  ensemble_rda_train_true_positive_rate[i] <- ensemble_rda_train_table[2, 2] / sum(ensemble_rda_train_table[2, 2] + ensemble_rda_train_table[1, 2])
  ensemble_rda_train_true_positive_rate_mean <- mean(ensemble_rda_train_true_positive_rate)
  ensemble_rda_train_true_negative_rate[i] <- ensemble_rda_train_table[1, 1] / sum(ensemble_rda_train_table[1, 1] + ensemble_rda_train_table[2, 1])
  ensemble_rda_train_true_negative_rate_mean <- mean(ensemble_rda_train_true_negative_rate)
  ensemble_rda_train_false_positive_rate[i] <- ensemble_rda_train_table[2, 1] / sum(ensemble_rda_train_table[2, 1] + ensemble_rda_train_table[1, 1])
  ensemble_rda_train_false_positive_rate_mean <- mean(ensemble_rda_train_false_positive_rate)
  ensemble_rda_train_false_negative_rate[i] <- ensemble_rda_train_table[1, 2] / sum(ensemble_rda_train_table[1, 2] + ensemble_rda_train_table[2, 2])
  ensemble_rda_train_false_negative_rate_mean <- mean(ensemble_rda_train_false_negative_rate)
  ensemble_rda_train_accuracy[i] <- (ensemble_rda_train_table[1, 1] + ensemble_rda_train_table[2, 2]) / sum(ensemble_rda_train_table)
  ensemble_rda_train_accuracy_mean <- mean(ensemble_rda_train_accuracy)
  ensemble_rda_train_F1_score[i] <- 2 * (ensemble_rda_train_table[2, 2]) / sum(2 * ensemble_rda_train_table[2, 2] + ensemble_rda_train_table[1, 2] + ensemble_rda_train_table[2, 1])
  ensemble_rda_train_F1_score_mean <- mean(ensemble_rda_train_F1_score)
  ensemble_rda_train_positive_predictive_value[i] <- ensemble_rda_train_table[2, 2] / sum(ensemble_rda_train_table[2, 2] + ensemble_rda_train_table[2, 1])
  ensemble_rda_train_positive_predictive_value_mean <- mean(ensemble_rda_train_positive_predictive_value)
  ensemble_rda_train_negative_predictive_value[i] <- ensemble_rda_train_table[1, 1] / sum(ensemble_rda_train_table[1, 1] + ensemble_rda_train_table[1, 2])
  ensemble_rda_train_negative_predictive_value_mean <- mean(ensemble_rda_train_negative_predictive_value)

  ensemble_rda_test_pred <- stats::predict(ensemble_rda_train_fit, ensemble_test, type = "response")
  ensemble_rda_test_table <- table(ensemble_rda_test_pred$class, ensemble_y_test)
  ensemble_rda_test_true_positive_rate[i] <- ensemble_rda_test_table[2, 2] / sum(ensemble_rda_test_table[2, 2] + ensemble_rda_test_table[1, 2])
  ensemble_rda_test_true_positive_rate_mean <- mean(ensemble_rda_test_true_positive_rate)
  ensemble_rda_test_true_negative_rate[i] <- ensemble_rda_test_table[1, 1] / sum(ensemble_rda_test_table[1, 1] + ensemble_rda_test_table[2, 1])
  ensemble_rda_test_true_negative_rate_mean <- mean(ensemble_rda_test_true_negative_rate)
  ensemble_rda_test_false_positive_rate[i] <- ensemble_rda_test_table[2, 1] / sum(ensemble_rda_test_table[2, 1] + ensemble_rda_test_table[1, 1])
  ensemble_rda_test_false_positive_rate_mean <- mean(ensemble_rda_test_false_positive_rate)
  ensemble_rda_test_false_negative_rate[i] <- ensemble_rda_test_table[1, 2] / sum(ensemble_rda_test_table[1, 2] + ensemble_rda_test_table[2, 2])
  ensemble_rda_test_false_negative_rate_mean <- mean(ensemble_rda_test_false_negative_rate)
  ensemble_rda_test_accuracy[i] <- (ensemble_rda_test_table[1, 1] + ensemble_rda_test_table[2, 2]) / sum(ensemble_rda_test_table)
  ensemble_rda_test_accuracy_mean <- mean(ensemble_rda_test_accuracy)
  ensemble_rda_test_F1_score[i] <- 2 * (ensemble_rda_test_table[2, 2]) / sum(2 * ensemble_rda_test_table[2, 2] + ensemble_rda_test_table[1, 2] + ensemble_rda_test_table[2, 1])
  ensemble_rda_test_F1_score_mean <- mean(ensemble_rda_test_F1_score)
  ensemble_rda_test_positive_predictive_value[i] <- ensemble_rda_test_table[2, 2] / sum(ensemble_rda_test_table[2, 2] + ensemble_rda_test_table[2, 1])
  ensemble_rda_test_positive_predictive_value_mean <- mean(ensemble_rda_test_positive_predictive_value)
  ensemble_rda_test_negative_predictive_value[i] <- ensemble_rda_test_table[1, 1] / sum(ensemble_rda_test_table[1, 1] + ensemble_rda_test_table[1, 2])
  ensemble_rda_test_negative_predictive_value_mean <- mean(ensemble_rda_test_negative_predictive_value)

  ensemble_rda_validation_pred <- stats::predict(ensemble_rda_train_fit, ensemble_validation, type = "response")
  ensemble_rda_validation_table <- table(ensemble_rda_validation_pred$class, ensemble_y_validation)
  ensemble_rda_validation_true_positive_rate[i] <- ensemble_rda_validation_table[2, 2] / sum(ensemble_rda_validation_table[2, 2] + ensemble_rda_validation_table[1, 2])
  ensemble_rda_validation_true_positive_rate_mean <- mean(ensemble_rda_validation_true_positive_rate)
  ensemble_rda_validation_true_negative_rate[i] <- ensemble_rda_validation_table[1, 1] / sum(ensemble_rda_validation_table[1, 1] + ensemble_rda_validation_table[2, 1])
  ensemble_rda_validation_true_negative_rate_mean <- mean(ensemble_rda_validation_true_negative_rate)
  ensemble_rda_validation_false_positive_rate[i] <- ensemble_rda_validation_table[2, 1] / sum(ensemble_rda_validation_table[2, 1] + ensemble_rda_validation_table[1, 1])
  ensemble_rda_validation_false_positive_rate_mean <- mean(ensemble_rda_validation_false_positive_rate)
  ensemble_rda_validation_false_negative_rate[i] <- ensemble_rda_validation_table[1, 2] / sum(ensemble_rda_validation_table[1, 2] + ensemble_rda_validation_table[2, 2])
  ensemble_rda_validation_false_negative_rate_mean <- mean(ensemble_rda_validation_false_negative_rate)
  ensemble_rda_validation_accuracy[i] <- (ensemble_rda_validation_table[1, 1] + ensemble_rda_validation_table[2, 2]) / sum(ensemble_rda_validation_table)
  ensemble_rda_validation_accuracy_mean <- mean(ensemble_rda_validation_accuracy)
  ensemble_rda_validation_F1_score[i] <- 2 * (ensemble_rda_validation_table[2, 2]) / sum(2 * ensemble_rda_validation_table[2, 2] + ensemble_rda_validation_table[1, 2] + ensemble_rda_validation_table[2, 1])
  ensemble_rda_validation_F1_score_mean <- mean(ensemble_rda_validation_F1_score)
  ensemble_rda_validation_positive_predictive_value[i] <- ensemble_rda_validation_table[2, 2] / sum(ensemble_rda_validation_table[2, 2] + ensemble_rda_validation_table[2, 1])
  ensemble_rda_validation_positive_predictive_value_mean <- mean(ensemble_rda_validation_positive_predictive_value)
  ensemble_rda_validation_negative_predictive_value[i] <- ensemble_rda_validation_table[1, 1] / sum(ensemble_rda_validation_table[1, 1] + ensemble_rda_validation_table[1, 2])
  ensemble_rda_validation_negative_predictive_value_mean <- mean(ensemble_rda_validation_negative_predictive_value)

  ensemble_rda_holdout_true_positive_rate[i] <- (ensemble_rda_test_true_positive_rate[i] + ensemble_rda_validation_true_positive_rate[i]) / 2
  ensemble_rda_holdout_true_positive_rate_mean <- mean(ensemble_rda_holdout_true_positive_rate)
  ensemble_rda_holdout_true_negative_rate[i] <- (ensemble_rda_test_true_negative_rate[i] + ensemble_rda_validation_true_negative_rate[i]) / 2
  ensemble_rda_holdout_true_negative_rate_mean <- mean(ensemble_rda_holdout_true_negative_rate)
  ensemble_rda_holdout_false_positive_rate[i] <- (ensemble_rda_test_false_positive_rate[i] + ensemble_rda_validation_false_positive_rate[i]) / 2
  ensemble_rda_holdout_false_positive_rate_mean <- mean(ensemble_rda_holdout_false_positive_rate)
  ensemble_rda_holdout_false_negative_rate[i] <- (ensemble_rda_test_false_negative_rate[i] + ensemble_rda_validation_false_negative_rate[i]) / 2
  ensemble_rda_holdout_false_negative_rate_mean <- mean(ensemble_rda_holdout_false_negative_rate)
  ensemble_rda_holdout_accuracy[i] <- (ensemble_rda_test_accuracy[i] + ensemble_rda_validation_accuracy[i]) / 2
  ensemble_rda_holdout_accuracy_mean <- mean(ensemble_rda_holdout_accuracy)
  ensemble_rda_holdout_F1_score[i] <- (ensemble_rda_test_F1_score[i] + ensemble_rda_validation_F1_score[i]) / 2
  ensemble_rda_holdout_F1_score_mean <- mean(ensemble_rda_holdout_F1_score)
  ensemble_rda_holdout_positive_predictive_value[i] <- (ensemble_rda_test_positive_predictive_value[i] + ensemble_rda_validation_positive_predictive_value[i]) / 2
  ensemble_rda_holdout_positive_predictive_value_mean <- mean(ensemble_rda_holdout_positive_predictive_value)
  ensemble_rda_holdout_negative_predictive_value[i] <- (ensemble_rda_test_negative_predictive_value[i] + ensemble_rda_validation_negative_predictive_value[i]) / 2
  ensemble_rda_holdout_negative_predictive_value_mean <- mean(ensemble_rda_holdout_negative_predictive_value)
  ensemble_rda_holdout_overfitting[i] <- ensemble_rda_holdout_accuracy[i] / ensemble_rda_train_accuracy[i]
  ensemble_rda_holdout_overfitting_mean <- mean(ensemble_rda_holdout_overfitting)
  ensemble_rda_holdout_overfitting_range <- range(ensemble_rda_holdout_overfitting)

  ensemble_rda_table <- ensemble_rda_test_table + ensemble_rda_validation_table
  ensemble_rda_table_total <- ensemble_rda_table_total + ensemble_rda_table

  ensemble_rda_end <- Sys.time()
  ensemble_rda_duration[i] <- ensemble_rda_end - ensemble_rda_start
  ensemble_rda_duration_mean <- mean(ensemble_rda_duration)


  #### Ensemble Using RPart ####
  ensemble_rpart_start <- Sys.time()

  ensemble_rpart_train_fit <- MachineShop::fit(as.factor(y) ~ ., data = ensemble_train, model = "RPartModel")

  ensemble_rpart_train_pred <- stats::predict(ensemble_rpart_train_fit, ensemble_train, type = "response")
  ensemble_rpart_train_table <- table(ensemble_rpart_train_pred, ensemble_y_train)
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
  ensemble_rpart_test_table <- table(ensemble_rpart_test_pred, ensemble_y_test)
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
  ensemble_rpart_validation_table <- table(ensemble_rpart_validation_pred, ensemble_y_validation)
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
  ensemble_rpart_holdout_F1_score[i] <- (ensemble_rpart_test_F1_score[i] + ensemble_rpart_validation_F1_score[i]) / 2
  ensemble_rpart_holdout_F1_score_mean <- mean(ensemble_rpart_holdout_F1_score)
  ensemble_rpart_holdout_positive_predictive_value[i] <- (ensemble_rpart_test_positive_predictive_value[i] + ensemble_rpart_validation_positive_predictive_value[i]) / 2
  ensemble_rpart_holdout_positive_predictive_value_mean <- mean(ensemble_rpart_holdout_positive_predictive_value)
  ensemble_rpart_holdout_negative_predictive_value[i] <- (ensemble_rpart_test_negative_predictive_value[i] + ensemble_rpart_validation_negative_predictive_value[i]) / 2
  ensemble_rpart_holdout_negative_predictive_value_mean <- mean(ensemble_rpart_holdout_negative_predictive_value)
  ensemble_rpart_holdout_overfitting[i] <- ensemble_rpart_holdout_accuracy[i] / ensemble_rpart_train_accuracy[i]
  ensemble_rpart_holdout_overfitting_mean <- mean(ensemble_rpart_holdout_overfitting)
  ensemble_rpart_holdout_overfitting_range <- range(ensemble_rpart_holdout_overfitting)

  ensemble_rpart_table <- ensemble_rpart_test_table + ensemble_rpart_validation_table
  ensemble_rpart_table_total <- ensemble_rpart_table_total + ensemble_rpart_table

  ensemble_rpart_end <- Sys.time()
  ensemble_rpart_duration[i] <- ensemble_rpart_end - ensemble_rpart_start
  ensemble_rpart_duration_mean <- mean(ensemble_rpart_duration)


  #### Ensemble Using Support Vector Machines ####
  ensemble_svm_start <- Sys.time()

  ensemble_svm_train_fit <- e1071::svm(as.factor(y) ~ ., data = ensemble_train, kernel = "radial", gamma = 1, cost = 1)

  ensemble_svm_train_pred <- stats::predict(ensemble_svm_train_fit, ensemble_train, type = "response")
  ensemble_svm_train_table <- table(ensemble_svm_train_pred, ensemble_y_train)
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
  ensemble_svm_test_table <- table(ensemble_svm_test_pred, ensemble_y_test)
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
  ensemble_svm_validation_table <- table(ensemble_svm_validation_pred, ensemble_y_validation)
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
  ensemble_svm_holdout_F1_score[i] <- (ensemble_svm_test_F1_score[i] + ensemble_svm_validation_F1_score[i]) / 2
  ensemble_svm_holdout_F1_score_mean <- mean(ensemble_svm_holdout_F1_score)
  ensemble_svm_holdout_positive_predictive_value[i] <- (ensemble_svm_test_positive_predictive_value[i] + ensemble_svm_validation_positive_predictive_value[i]) / 2
  ensemble_svm_holdout_positive_predictive_value_mean <- mean(ensemble_svm_holdout_positive_predictive_value)
  ensemble_svm_holdout_negative_predictive_value[i] <- (ensemble_svm_test_negative_predictive_value[i] + ensemble_svm_validation_negative_predictive_value[i]) / 2
  ensemble_svm_holdout_negative_predictive_value_mean <- mean(ensemble_svm_holdout_negative_predictive_value)
  ensemble_svm_holdout_overfitting[i] <- ensemble_svm_holdout_accuracy[i] / ensemble_svm_train_accuracy[i]
  ensemble_svm_holdout_overfitting_mean <- mean(ensemble_svm_holdout_overfitting)
  ensemble_svm_holdout_overfitting_range <- range(ensemble_svm_holdout_overfitting)

  ensemble_svm_table <- ensemble_svm_test_table + ensemble_svm_validation_table
  ensemble_svm_table_total <- ensemble_svm_table_total + ensemble_svm_table

  ensemble_svm_end <- Sys.time()
  ensemble_svm_duration[i] <- ensemble_svm_end - ensemble_svm_start
  ensemble_svm_duration_mean <- mean(ensemble_svm_duration)


  #### Ensemble Using Trees ####
  ensemble_tree_start <- Sys.time()

  ensemble_tree_train_fit <- tree::tree(ensemble_train$y ~ ., data = ensemble_train)

  ensemble_tree_train_pred <- stats::predict(ensemble_tree_train_fit, ensemble_train, type = "vector")
  ensemble_tree_train_probabilities <- ifelse(ensemble_tree_train_pred > 0.5, 1, 0)
  ensemble_tree_train_table <- table(ensemble_tree_train_probabilities, ensemble_y_train)
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
  ensemble_tree_test_probabilities <- ifelse(ensemble_tree_test_pred > 0.5, 1, 0)
  ensemble_tree_test_table <- table(ensemble_tree_test_probabilities, ensemble_y_test)
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
  ensemble_tree_validation_probabilities <- ifelse(ensemble_tree_validation_pred > 0.5, 1, 0)
  ensemble_tree_validation_table <- table(ensemble_tree_validation_probabilities, ensemble_y_validation)
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
  ensemble_tree_holdout_F1_score[i] <- (ensemble_tree_test_F1_score[i] + ensemble_tree_validation_F1_score[i]) / 2
  ensemble_tree_holdout_F1_score_mean <- mean(ensemble_tree_holdout_F1_score)
  ensemble_tree_holdout_positive_predictive_value[i] <- (ensemble_tree_test_positive_predictive_value[i] + ensemble_tree_validation_positive_predictive_value[i]) / 2
  ensemble_tree_holdout_positive_predictive_value_mean <- mean(ensemble_tree_holdout_positive_predictive_value)
  ensemble_tree_holdout_negative_predictive_value[i] <- (ensemble_tree_test_negative_predictive_value[i] + ensemble_tree_validation_negative_predictive_value[i]) / 2
  ensemble_tree_holdout_negative_predictive_value_mean <- mean(ensemble_tree_holdout_negative_predictive_value)
  ensemble_tree_holdout_overfitting[i] <- ensemble_tree_holdout_accuracy[i] / ensemble_tree_train_accuracy[i]
  ensemble_tree_holdout_overfitting_mean <- mean(ensemble_tree_holdout_overfitting)
  ensemble_tree_holdout_overfitting_range <- range(ensemble_tree_holdout_overfitting)

  ensemble_tree_table <- ensemble_tree_test_table + ensemble_tree_validation_table
  ensemble_tree_table_total <- ensemble_tree_table_total + ensemble_tree_table

  ensemble_tree_end <- Sys.time()
  ensemble_tree_duration[i] <- ensemble_tree_end - ensemble_tree_start
  ensemble_tree_duration_mean <- mean(ensemble_tree_duration)


  #### Ensemble Using Support Vector Machines ####
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

  ensemble_xgb_model <- xgboost::xgb.train(data = ensemble_xgb_train, max.depth = 3, watchlist = ensemble_watchlist_test, nrounds = 70)

  ensemble_xgboost_min <- which.min(ensemble_xgb_model$evaluation_log$validation_rmse)

  ensemble_xgb_train_pred <- predict(object = ensemble_xgb_model, newdata = ensemble_train_x, type = "response")
  ensemble_xgb_train_probabilities <- ifelse(ensemble_xgb_train_pred > 0.5, 1, 0)
  ensemble_xgb_train_table <- table(ensemble_xgb_train_probabilities, ensemble_y_train)
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

  ensemble_xgb_test_pred <- predict(object = ensemble_xgb_model, newdata = ensemble_test_x, type = "response")
  ensemble_xgb_test_probabilities <- ifelse(ensemble_xgb_test_pred > 0.5, 1, 0)
  ensemble_xgb_test_table <- table(ensemble_xgb_test_probabilities, ensemble_y_test)
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

  ensemble_xgb_validation_pred <- predict(object = ensemble_xgb_model, newdata = ensemble_validation_x, type = "response")
  ensemble_xgb_validation_probabilities <- ifelse(ensemble_xgb_validation_pred > 0.5, 1, 0)
  ensemble_xgb_validation_table <- table(ensemble_xgb_validation_probabilities, ensemble_y_validation)
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
  ensemble_xgb_holdout_F1_score[i] <- (ensemble_xgb_test_F1_score[i] + ensemble_xgb_validation_F1_score[i]) / 2
  ensemble_xgb_holdout_F1_score_mean <- mean(ensemble_xgb_holdout_F1_score)
  ensemble_xgb_holdout_positive_predictive_value[i] <- (ensemble_xgb_test_positive_predictive_value[i] + ensemble_xgb_validation_positive_predictive_value[i]) / 2
  ensemble_xgb_holdout_positive_predictive_value_mean <- mean(ensemble_xgb_holdout_positive_predictive_value)
  ensemble_xgb_holdout_negative_predictive_value[i] <- (ensemble_xgb_test_negative_predictive_value[i] + ensemble_xgb_validation_negative_predictive_value[i]) / 2
  ensemble_xgb_holdout_negative_predictive_value_mean <- mean(ensemble_xgb_holdout_negative_predictive_value)
  ensemble_xgb_holdout_overfitting[i] <- ensemble_xgb_holdout_accuracy[i] / ensemble_xgb_train_accuracy[i]
  ensemble_xgb_holdout_overfitting_mean <- mean(ensemble_xgb_holdout_overfitting)
  ensemble_xgb_holdout_overfitting_range <- range(ensemble_xgb_holdout_overfitting)

  ensemble_xgb_table <- ensemble_xgb_test_table + ensemble_xgb_validation_table
  ensemble_xgb_table_total <- ensemble_xgb_table_total + ensemble_xgb_table

  ensemble_xgb_end <- Sys.time()
  ensemble_xgb_duration[i] <- ensemble_xgb_end - ensemble_xgb_start
  ensemble_xgb_duration_mean <- mean(ensemble_xgb_duration)
}

##################################

## ROC Curves start here #########

##################################

adaboost_roc_obj <- pROC::roc(c(test01$y, validation01$y), c(adaboost_test_pred, adaboost_validation_pred))
adaboost_auc <- round(pROC::auc(c(test01$y, validation01$y), c(adaboost_test_pred, adaboost_validation_pred)), 4)
adaboost_ROC <- pROC::ggroc(adaboost_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Adaboost ", "(AUC = ", adaboost_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

bag_rf_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(bag_rf_test_predictions[, 2], bag_rf_validation_predictions[, 2])) - 1)
bag_rf_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(bag_rf_test_predictions[, 2], bag_rf_validation_predictions[, 2])) - 1)), 4)
bag_rf_ROC <- pROC::ggroc(bag_rf_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Bagged Random Forest ", "(AUC = ", bag_rf_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

bagging_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(bagging_test_predictions[, 2], bagging_validation_predictions[, 2])))
bagging_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(bagging_test_predictions[, 2], bagging_validation_predictions[, 2])) - 1)), 4)
bagging_ROC <- pROC::ggroc(bagging_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Bagging ", "(AUC = ", bagging_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

bayesglm_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(bayesglm_test_predictions, bayesglm_validation_predictions)))
bayesglm_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(bayesglm_test_predictions, bayesglm_validation_predictions)) - 1)), 4)
bayesglm_ROC <- pROC::ggroc(bayesglm_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("BayesGLM ", "(AUC = ", bayesglm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

bayesrnn_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(bayesrnn_test_predictions, bayesrnn_validation_predictions)))
bayesrnn_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(bayesrnn_test_predictions, bayesrnn_validation_predictions)) - 1)), 4)
bayesrnn_ROC <- pROC::ggroc(bayesrnn_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("BayesRNN ", "(AUC = ", bayesrnn_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

C50_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(C50_test_predictions, C50_validation_predictions)))
C50_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(C50_test_predictions, C50_validation_predictions)) - 1)), 4)
C50_ROC <- pROC::ggroc(C50_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("C50 ROC curve ", "(AUC = ", C50_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

cubist_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(cubist_test_pred, cubist_validation_pred)))
cubist_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(cubist_test_pred, cubist_validation_pred)) - 1)), 4)
cubist_ROC <- pROC::ggroc(cubist_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Cubist ", "(AUC = ", cubist_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

fda_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(fda_test_predictions, fda_validation_predictions)))
fda_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(fda_test_predictions, fda_validation_predictions)) - 1)), 4)
fda_ROC <- pROC::ggroc(fda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Flexible Discriminant Analysis ", "(AUC = ", fda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

gam_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(gam_test_predictions, gam_validation_predictions)))
gam_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(gam_test_predictions, gam_validation_predictions)) - 1)), 4)
gam_ROC <- pROC::ggroc(gam_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Generalized Additve Models ", "(AUC = ", gam_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

gb_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(gb_test_predictions, gb_validation_predictions)))
gb_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(gb_test_predictions, gb_validation_predictions)) - 1)), 4)
gb_ROC <- pROC::ggroc(gb_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Gradient Boosted ", "(AUC = ", gb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

glm_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), c(glm_test_pred, glm_validation_pred))
glm_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(glm_test_pred, glm_validation_pred)) - 1)), 4)
glm_ROC <- pROC::ggroc(glm_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Generalized Linear Models ", "(AUC = ", glm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

linear_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(linear_test_pred, linear_validation_pred)))
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

mda_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(mda_test_predictions, mda_validation_predictions)))
mda_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(mda_test_predictions, mda_validation_predictions)) - 1)), 4)
mda_ROC <- pROC::ggroc(mda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Mixture Discriminant Analysis ", "(AUC = ", mda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

n_bayes_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(n_bayes_test_predictions, n_bayes_validation_predictions)))
n_bayes_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(n_bayes_test_predictions, n_bayes_validation_predictions)) - 1)), 4)
n_bayes_ROC <- pROC::ggroc(n_bayes_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Naive Bayes ", "(AUC = ", n_bayes_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

pda_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(pda_test_pred, pda_validation_pred)))
pda_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(pda_test_pred, pda_validation_pred)) - 1)), 4)
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

rf_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(rf_test_probabilities, rf_validation_probabilities)))
rf_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(rf_test_probabilities, rf_validation_probabilities)) - 1)), 4)
rf_ROC <- pROC::ggroc(rf_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Random Forest ", "(AUC = ", rf_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ranger_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(ranger_test_pred, ranger_validation_pred)))
ranger_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(ranger_test_pred, ranger_validation_pred)) - 1)), 4)
ranger_ROC <- pROC::ggroc(ranger_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ranger ", "(AUC = ", ranger_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

rpart_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(rpart_test_pred, rpart_validation_pred)))
rpart_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(rpart_test_pred, rpart_validation_pred)) - 1)), 4)
rpart_ROC <- pROC::ggroc(rpart_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("RPart ", "(AUC = ", rpart_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

svm_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(svm_test_pred, svm_validation_pred)))
svm_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(svm_test_pred, svm_validation_pred)) - 1)), 4)
svm_ROC <- pROC::ggroc(svm_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Support Vector Machines ", "(AUC = ", svm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

tree_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(tree_test_pred, tree_validation_pred)))
tree_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(tree_test_pred, tree_validation_pred)) - 1)), 4)
tree_ROC <- pROC::ggroc(tree_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Trees ", "(AUC = ", tree_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

xgb_roc_obj <- pROC::roc(as.numeric(c(test01$y, validation01$y)), as.numeric(c(xgb_test_pred, xgb_validation_pred)))
xgb_auc <- round((pROC::auc(c(test01$y, validation01$y), as.numeric(c(xgb_test_pred, xgb_validation_pred)) - 1)), 4)
xgb_ROC <- pROC::ggroc(xgb_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("XGBoost ", "(AUC = ", xgb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")


ensemble_adaboost_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_adaboost_test_pred, ensemble_adaboost_validation_pred)))
ensemble_adaboost_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_adaboost_test_pred, ensemble_adaboost_validation_pred)) - 1)), 4)
ensemble_adaboost_ROC <- pROC::ggroc(ensemble_adaboost_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble Adaboostoost ", "(AUC = ", ensemble_adaboost_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_bagging_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_bagging_test_pred[, 2], ensemble_bagging_validation_pred[, 2])))
ensemble_bagging_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_bagging_test_pred[, 2], ensemble_bagging_validation_pred[, 2])) - 1)), 4)
ensemble_bagging_ROC <- pROC::ggroc(ensemble_bagging_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble Bagging ", "(AUC = ", ensemble_bagging_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_C50_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_C50_test_pred[, 2], ensemble_C50_validation_pred[, 2])))
ensemble_C50_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_C50_test_pred[, 2], ensemble_C50_validation_pred[, 2])) - 1)), 4)
ensemble_C50_ROC <- pROC::ggroc(ensemble_C50_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble_C50 ", "(AUC = ", ensemble_C50_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_gb_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_gb_test_pred, ensemble_gb_validation_pred)))
ensemble_gb_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_gb_test_pred, ensemble_gb_validation_pred)) - 1)), 4)
ensemble_gb_ROC <- pROC::ggroc(ensemble_gb_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble Gradient Boosted ", "(AUC = ", ensemble_gb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_pls_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_pls_test_pred, ensemble_pls_validation_pred)))
ensemble_pls_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_pls_test_pred, ensemble_pls_validation_pred)) - 1)), 4)
ensemble_pls_ROC <- pROC::ggroc(ensemble_pls_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble PLS ", "(AUC = ", ensemble_pls_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_pda_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_pda_test_pred, ensemble_pda_validation_pred)))
ensemble_pda_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_pda_test_pred, ensemble_pda_validation_pred)))), 4)
ensemble_pda_ROC <- pROC::ggroc(ensemble_pda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble PDA ", "(AUC = ", ensemble_pda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_rf_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_rf_test_pred[, 2], ensemble_rf_validation_pred[, 2])))
ensemble_rf_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_rf_test_pred[, 2], ensemble_rf_validation_pred[, 2])) - 1)), 4)
ensemble_rf_ROC <- pROC::ggroc(ensemble_rf_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble Random Forest ", "(AUC = ", ensemble_rf_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_ranger_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_ranger_test_pred, ensemble_ranger_validation_pred)))
ensemble_ranger_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_ranger_test_pred, ensemble_ranger_validation_pred)) - 1)), 4)
ensemble_ranger_ROC <- pROC::ggroc(ensemble_ranger_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble Ranger ", "(AUC = ", ensemble_ranger_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_rda_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_rda_test_pred$posterior[, 2], ensemble_rda_validation_pred$posterior[, 2])))
ensemble_rda_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_rda_test_pred$posterior[, 2], ensemble_rda_validation_pred$posterior[, 2])) - 1)), 4)
ensemble_rda_ROC <- pROC::ggroc(ensemble_rda_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble RDA ROC curve ", "(AUC = ", ensemble_rda_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_rpart_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_rpart_test_pred, ensemble_rpart_validation_pred)))
ensemble_rpart_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_rpart_test_pred, ensemble_rpart_validation_pred)))), 4)
ensemble_rpart_ROC <- pROC::ggroc(ensemble_rpart_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble Rpart ", "(AUC = ", ensemble_rpart_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_svm_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_svm_test_pred, ensemble_svm_validation_pred)))
ensemble_svm_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_svm_test_pred, ensemble_svm_validation_pred)))), 4)
ensemble_svm_ROC <- pROC::ggroc(ensemble_svm_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble SVM ", "(AUC = ", ensemble_svm_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_tree_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_tree_test_pred, ensemble_tree_validation_pred)))
ensemble_tree_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_tree_test_pred, ensemble_tree_validation_pred)) - 1)), 4)
ensemble_tree_ROC <- pROC::ggroc(ensemble_tree_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble Trees ", "(AUC = ", ensemble_tree_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")

ensemble_xgb_roc_obj <- pROC::roc(as.numeric(c(ensemble_test$y, ensemble_validation$y)), as.numeric(c(ensemble_xgb_test_pred, ensemble_xgb_validation_pred)))
ensemble_xgb_auc <- round((pROC::auc(c(ensemble_test$y, ensemble_validation$y), as.numeric(c(ensemble_xgb_test_pred, ensemble_xgb_validation_pred)) - 1)), 4)
ensemble_xgb_ROC <- pROC::ggroc(ensemble_xgb_roc_obj, color = "steelblue", size = 2) +
  ggplot2::ggtitle(paste0("Ensemble XGBoost ", "(AUC = ", ensemble_xgb_auc, ")")) +
  ggplot2::labs(x = "Specificity", y = "Sensitivity") +
  ggplot2::annotate("segment", x = 1, xend = 0, y = 0, yend = 1, color = "grey")


ROC_curves <- gridExtra::grid.arrange(adaboost_ROC, bag_rf_ROC, bagging_ROC,
                                      bayesglm_ROC, bayesrnn_ROC, C50_ROC,
                                      cubist_ROC, fda_ROC, gam_ROC,
                                      gb_ROC, glm_ROC, linear_ROC,
                                      lda_ROC, mda_ROC, n_bayes_ROC,
                                      pda_ROC, qda_ROC,
                                      rf_ROC, ranger_ROC,
                                      rpart_ROC, svm_ROC, tree_ROC, xgb_ROC,
                                      ensemble_adaboost_ROC, ensemble_bagging_ROC,
                                      ensemble_C50_ROC,
                                      ensemble_gb_ROC,
                                      ensemble_pls_ROC,
                                      ensemble_pda_ROC,
                                      ensemble_rf_ROC, ensemble_ranger_ROC,
                                      ensemble_rda_ROC, ensemble_rpart_ROC,
                                      ensemble_svm_ROC,
                                      ensemble_tree_ROC, ensemble_xgb_ROC,
                                      ncol = 6
)




holdout_results <- data.frame(
  Model = c(
    "ADA Boost", "Bagged Random Forest", "Bagging", "Bayes GLM", "Bayes RNN", "C50", "Cubist", "Flexible Discriminant Analysis",
    "Generalized Additive Models", "Gradient Boosted", "Generalized Linear Models", "Linear Model",
    "Linear Discrmininant Analysis", "Mixed Discriminant Analysis", "Naive Bayes",
    "Penalized Discriminant Analysis", "Quadratic Discriminant Analysis", "Random Forest",
    "Ranger", "RPart", "Support Vector Machines", "Trees", "XGBoost",
    "Ensemble ADA Boost", "Ensemble Bagging", "Ensemble C50", "Ensemble Gradient Boosted",
    "Ensemble Partial Least Squares", "Ensemble Penalized Discrmininat Analysis", "Ensemble Random Forest",
    "Ensemble Ranger", "Ensemble Regularized Discriminant Analysis", "Ensemble RPart", "Ensemble Support Vector Machines",
    "Ensemble Trees", "Ensemble XGBoost"
  ),
  "Accuracy" = round(c(
    adaboost_holdout_accuracy_mean, bag_rf_holdout_accuracy_mean, bagging_holdout_accuracy_mean, bayesglm_holdout_accuracy_mean,
    bayesrnn_holdout_accuracy_mean, C50_holdout_accuracy_mean, cubist_holdout_accuracy_mean, fda_holdout_accuracy_mean,
    gam_holdout_accuracy_mean, gb_holdout_accuracy_mean, glm_holdout_accuracy_mean, linear_holdout_accuracy_mean,
    lda_holdout_accuracy_mean, mda_holdout_accuracy_mean, n_bayes_holdout_accuracy_mean,
    pda_holdout_accuracy_mean, qda_holdout_accuracy_mean, rf_holdout_accuracy_mean, ranger_holdout_accuracy_mean,
    rpart_holdout_accuracy_mean, svm_holdout_accuracy_mean, tree_holdout_accuracy_mean, xgb_holdout_accuracy_mean,
    ensemble_adaboost_holdout_accuracy_mean, ensemble_bagging_holdout_accuracy_mean, ensemble_C50_holdout_accuracy_mean,
    ensemble_gb_holdout_accuracy_mean, ensemble_pls_holdout_accuracy_mean, ensemble_pda_holdout_accuracy_mean, ensemble_rf_holdout_accuracy_mean,
    ensemble_ranger_holdout_accuracy_mean, ensemble_rda_holdout_accuracy_mean, ensemble_rpart_holdout_accuracy_mean,
    ensemble_svm_holdout_accuracy_mean, ensemble_tree_holdout_accuracy_mean, ensemble_xgb_holdout_accuracy_mean
  ), 4),
  "True_Positive_Rate_aka_Sensitivity" = round(c(
    adaboost_holdout_true_positive_rate_mean, bag_rf_holdout_true_positive_rate_mean, bagging_holdout_true_positive_rate_mean, bayesglm_holdout_true_positive_rate_mean,
    bayesrnn_holdout_true_positive_rate_mean, C50_holdout_true_positive_rate_mean, cubist_holdout_true_positive_rate_mean, fda_holdout_true_positive_rate_mean,
    gam_holdout_true_positive_rate_mean, gb_holdout_true_positive_rate_mean, glm_holdout_true_positive_rate_mean, linear_holdout_true_positive_rate_mean,
    lda_holdout_true_positive_rate_mean, mda_holdout_true_positive_rate_mean, n_bayes_holdout_true_positive_rate_mean,
    pda_holdout_true_positive_rate_mean, qda_holdout_true_positive_rate_mean, rf_holdout_true_positive_rate_mean, ranger_holdout_true_positive_rate_mean,
    rpart_holdout_true_positive_rate_mean, svm_holdout_true_positive_rate_mean, tree_holdout_true_positive_rate_mean, xgb_holdout_true_positive_rate_mean,
    ensemble_adaboost_holdout_true_positive_rate_mean, ensemble_bagging_holdout_true_positive_rate_mean, ensemble_C50_holdout_true_positive_rate_mean,
    ensemble_gb_holdout_true_positive_rate_mean, ensemble_pls_holdout_true_positive_rate_mean, ensemble_pda_holdout_true_positive_rate_mean, ensemble_rf_holdout_true_positive_rate_mean,
    ensemble_ranger_holdout_true_positive_rate_mean, ensemble_rda_holdout_true_positive_rate_mean, ensemble_rpart_holdout_true_positive_rate_mean,
    ensemble_svm_holdout_true_positive_rate_mean, ensemble_tree_holdout_true_positive_rate_mean, ensemble_xgb_holdout_true_positive_rate_mean
  ), 4),
  "True_Negative_Rate_aka_Specificity" = round(c(
    adaboost_holdout_true_negative_rate_mean, bag_rf_holdout_true_negative_rate_mean, bagging_holdout_true_negative_rate_mean, bayesglm_holdout_true_negative_rate_mean,
    bayesrnn_holdout_true_negative_rate_mean, C50_holdout_true_negative_rate_mean, cubist_holdout_true_negative_rate_mean, fda_holdout_true_negative_rate_mean,
    gam_holdout_true_negative_rate_mean, gb_holdout_true_negative_rate_mean, glm_holdout_true_negative_rate_mean, linear_holdout_true_negative_rate_mean,
    lda_holdout_true_negative_rate_mean, mda_holdout_true_negative_rate_mean, n_bayes_holdout_true_negative_rate_mean,
    pda_holdout_true_negative_rate_mean, qda_holdout_true_negative_rate_mean, rf_holdout_true_negative_rate_mean, ranger_holdout_true_negative_rate_mean,
    rpart_holdout_true_negative_rate_mean, svm_holdout_true_negative_rate_mean, tree_holdout_true_negative_rate_mean, xgb_holdout_true_negative_rate_mean,
    ensemble_adaboost_holdout_true_negative_rate_mean, ensemble_bagging_holdout_true_negative_rate_mean, ensemble_C50_holdout_true_negative_rate_mean,
    ensemble_gb_holdout_true_negative_rate_mean, ensemble_pls_holdout_true_negative_rate_mean, ensemble_pda_holdout_true_negative_rate_mean, ensemble_rf_holdout_true_negative_rate_mean,
    ensemble_ranger_holdout_true_negative_rate_mean, ensemble_rda_holdout_true_negative_rate_mean, ensemble_rpart_holdout_true_negative_rate_mean,
    ensemble_svm_holdout_true_negative_rate_mean, ensemble_tree_holdout_true_negative_rate_mean, ensemble_xgb_holdout_true_negative_rate_mean
  ), 4),
  "False_Positive_Rate_aka_Type_I_Error" = round(c(
    adaboost_holdout_false_positive_rate_mean, bag_rf_holdout_false_positive_rate_mean, bagging_holdout_false_positive_rate_mean, bayesglm_holdout_false_positive_rate_mean,
    bayesrnn_holdout_false_positive_rate_mean, C50_holdout_false_positive_rate_mean, cubist_holdout_false_positive_rate_mean, fda_holdout_false_positive_rate_mean,
    gam_holdout_false_positive_rate_mean, gb_holdout_false_positive_rate_mean, glm_holdout_false_positive_rate_mean, linear_holdout_false_positive_rate_mean,
    lda_holdout_false_positive_rate_mean, mda_holdout_false_positive_rate_mean, n_bayes_holdout_false_positive_rate_mean,
    pda_holdout_false_positive_rate_mean, qda_holdout_false_positive_rate_mean, rf_holdout_false_positive_rate_mean, ranger_holdout_false_positive_rate_mean,
    rpart_holdout_false_positive_rate_mean, svm_holdout_false_positive_rate_mean, tree_holdout_false_positive_rate_mean, xgb_holdout_false_positive_rate_mean,
    ensemble_adaboost_holdout_false_positive_rate_mean, ensemble_bagging_holdout_false_positive_rate_mean, ensemble_C50_holdout_false_positive_rate_mean,
    ensemble_gb_holdout_false_positive_rate_mean, ensemble_pls_holdout_false_positive_rate_mean, ensemble_pda_holdout_false_positive_rate_mean, ensemble_rf_holdout_false_positive_rate_mean,
    ensemble_ranger_holdout_false_positive_rate_mean, ensemble_rda_holdout_false_positive_rate_mean, ensemble_rpart_holdout_false_positive_rate_mean,
    ensemble_svm_holdout_false_positive_rate_mean, ensemble_tree_holdout_false_positive_rate_mean, ensemble_xgb_holdout_false_positive_rate_mean
  ), 4),
  "False_Negative_Rate_aka_Type_II_Error" = round(c(
    adaboost_holdout_false_negative_rate_mean, bag_rf_holdout_false_negative_rate_mean, bagging_holdout_false_negative_rate_mean, bayesglm_holdout_false_negative_rate_mean,
    bayesrnn_holdout_false_negative_rate_mean, C50_holdout_false_negative_rate_mean, cubist_holdout_false_negative_rate_mean, fda_holdout_false_negative_rate_mean,
    gam_holdout_false_negative_rate_mean, gb_holdout_false_negative_rate_mean, glm_holdout_false_negative_rate_mean, linear_holdout_false_negative_rate_mean,
    lda_holdout_false_negative_rate_mean, mda_holdout_false_negative_rate_mean, n_bayes_holdout_false_negative_rate_mean,
    pda_holdout_false_negative_rate_mean, qda_holdout_false_negative_rate_mean, rf_holdout_false_negative_rate_mean, ranger_holdout_false_negative_rate_mean,
    rpart_holdout_false_negative_rate_mean, svm_holdout_false_negative_rate_mean, tree_holdout_false_negative_rate_mean, xgb_holdout_false_negative_rate_mean,
    ensemble_adaboost_holdout_false_negative_rate_mean, ensemble_bagging_holdout_false_negative_rate_mean, ensemble_C50_holdout_false_negative_rate_mean,
    ensemble_gb_holdout_false_negative_rate_mean, ensemble_pls_holdout_false_negative_rate_mean, ensemble_pda_holdout_false_negative_rate_mean, ensemble_rf_holdout_false_negative_rate_mean,
    ensemble_ranger_holdout_false_negative_rate_mean, ensemble_rda_holdout_false_negative_rate_mean, ensemble_rpart_holdout_false_negative_rate_mean,
    ensemble_svm_holdout_false_negative_rate_mean, ensemble_tree_holdout_false_negative_rate_mean, ensemble_xgb_holdout_false_negative_rate_mean
  ), 4),
  "Positive_Predictive_Value_ada_Precision" = round(c(
    adaboost_holdout_positive_predictive_value_mean, bag_rf_holdout_positive_predictive_value_mean, bagging_holdout_positive_predictive_value_mean, bayesglm_holdout_positive_predictive_value_mean,
    bayesrnn_holdout_positive_predictive_value_mean, C50_holdout_positive_predictive_value_mean, cubist_holdout_positive_predictive_value_mean, fda_holdout_positive_predictive_value_mean,
    gam_holdout_positive_predictive_value_mean, gb_holdout_positive_predictive_value_mean, glm_holdout_positive_predictive_value_mean, linear_holdout_positive_predictive_value_mean,
    lda_holdout_positive_predictive_value_mean, mda_holdout_positive_predictive_value_mean, n_bayes_holdout_positive_predictive_value_mean,
    pda_holdout_positive_predictive_value_mean, qda_holdout_positive_predictive_value_mean, rf_holdout_positive_predictive_value_mean, ranger_holdout_positive_predictive_value_mean,
    rpart_holdout_positive_predictive_value_mean, svm_holdout_positive_predictive_value_mean, tree_holdout_positive_predictive_value_mean, xgb_holdout_positive_predictive_value_mean,
    ensemble_adaboost_holdout_positive_predictive_value_mean, ensemble_bagging_holdout_positive_predictive_value_mean, ensemble_C50_holdout_positive_predictive_value_mean,
    ensemble_gb_holdout_positive_predictive_value_mean, ensemble_pls_holdout_positive_predictive_value_mean, ensemble_pda_holdout_positive_predictive_value_mean, ensemble_rf_holdout_positive_predictive_value_mean,
    ensemble_ranger_holdout_positive_predictive_value_mean, ensemble_rda_holdout_positive_predictive_value_mean, ensemble_rpart_holdout_positive_predictive_value_mean,
    ensemble_svm_holdout_positive_predictive_value_mean, ensemble_tree_holdout_positive_predictive_value_mean, ensemble_xgb_holdout_positive_predictive_value_mean
  ), 4),
  "Negative_Predictive_Value" = round(c(
    adaboost_holdout_negative_predictive_value_mean, bag_rf_holdout_negative_predictive_value_mean, bagging_holdout_negative_predictive_value_mean, bayesglm_holdout_negative_predictive_value_mean,
    bayesrnn_holdout_negative_predictive_value_mean, C50_holdout_negative_predictive_value_mean, cubist_holdout_negative_predictive_value_mean, fda_holdout_negative_predictive_value_mean,
    gam_holdout_negative_predictive_value_mean, gb_holdout_negative_predictive_value_mean, glm_holdout_negative_predictive_value_mean, linear_holdout_negative_predictive_value_mean,
    lda_holdout_negative_predictive_value_mean, mda_holdout_negative_predictive_value_mean, n_bayes_holdout_negative_predictive_value_mean,
    pda_holdout_negative_predictive_value_mean, qda_holdout_negative_predictive_value_mean, rf_holdout_negative_predictive_value_mean, ranger_holdout_negative_predictive_value_mean,
    rpart_holdout_negative_predictive_value_mean, svm_holdout_negative_predictive_value_mean, tree_holdout_negative_predictive_value_mean, xgb_holdout_negative_predictive_value_mean,
    ensemble_adaboost_holdout_negative_predictive_value_mean, ensemble_bagging_holdout_negative_predictive_value_mean, ensemble_C50_holdout_negative_predictive_value_mean,
    ensemble_gb_holdout_negative_predictive_value_mean, ensemble_pls_holdout_negative_predictive_value_mean, ensemble_pda_holdout_negative_predictive_value_mean, ensemble_rf_holdout_negative_predictive_value_mean,
    ensemble_ranger_holdout_negative_predictive_value_mean, ensemble_rda_holdout_negative_predictive_value_mean, ensemble_rpart_holdout_negative_predictive_value_mean,
    ensemble_svm_holdout_negative_predictive_value_mean, ensemble_tree_holdout_negative_predictive_value_mean, ensemble_xgb_holdout_negative_predictive_value_mean
  ), 4),
  "F1_Score" = round(c(
    adaboost_holdout_F1_score_mean, bag_rf_holdout_F1_score_mean, bagging_holdout_F1_score_mean, bayesglm_holdout_F1_score_mean,
    bayesrnn_holdout_F1_score_mean, C50_holdout_F1_score_mean, cubist_holdout_F1_score_mean, fda_holdout_F1_score_mean,
    gam_holdout_F1_score_mean, gb_holdout_F1_score_mean, glm_holdout_F1_score_mean, linear_holdout_F1_score_mean,
    lda_holdout_F1_score_mean, mda_holdout_F1_score_mean, n_bayes_holdout_F1_score_mean,
    pda_holdout_F1_score_mean, qda_holdout_F1_score_mean, rf_holdout_F1_score_mean, ranger_holdout_F1_score_mean,
    rpart_holdout_F1_score_mean, svm_holdout_F1_score_mean, tree_holdout_F1_score_mean, xgb_holdout_F1_score_mean,
    ensemble_adaboost_holdout_F1_score_mean, ensemble_bagging_holdout_F1_score_mean, ensemble_C50_holdout_F1_score_mean,
    ensemble_gb_holdout_F1_score_mean, ensemble_pls_holdout_F1_score_mean, ensemble_pda_holdout_F1_score_mean, ensemble_rf_holdout_F1_score_mean,
    ensemble_ranger_holdout_F1_score_mean, ensemble_rda_holdout_F1_score_mean, ensemble_rpart_holdout_F1_score_mean,
    ensemble_svm_holdout_F1_score_mean, ensemble_tree_holdout_F1_score_mean, ensemble_xgb_holdout_F1_score_mean
  ), 4),
  "Area_Under_Curve" = c(
    adaboost_auc, bag_rf_auc, bagging_auc, bayesglm_auc,
    bayesrnn_auc, C50_auc, cubist_auc, fda_auc,
    gam_auc, gb_auc, glm_auc, linear_auc,
    lda_auc, mda_auc, n_bayes_auc,
    pda_auc, qda_auc, rf_auc, ranger_auc,
    rpart_auc, svm_auc, tree_auc, xgb_auc,
    ensemble_adaboost_auc, ensemble_bagging_auc, ensemble_C50_auc,
    ensemble_gb_auc, ensemble_pls_auc, ensemble_pda_auc, ensemble_rf_auc,
    ensemble_ranger_auc, ensemble_rda_auc, ensemble_rpart_auc,
    ensemble_svm_auc, ensemble_tree_auc, ensemble_xgb_auc
  ),
  "Overfitting_Min" = round(c(
    adaboost_holdout_overfitting_range[1], bag_rf_holdout_overfitting_range[1], bagging_holdout_overfitting_range[1], bayesglm_holdout_overfitting_range[1],
    bayesrnn_holdout_overfitting_range[1], C50_holdout_overfitting_range[1], cubist_holdout_overfitting_range[1], fda_holdout_overfitting_range[1],
    gam_holdout_overfitting_range[1], gb_holdout_overfitting_range[1], glm_holdout_overfitting_range[1], linear_holdout_overfitting_range[1],
    lda_holdout_overfitting_range[1], mda_holdout_overfitting_range[1], n_bayes_holdout_overfitting_range[1],
    pda_holdout_overfitting_range[1], qda_holdout_overfitting_range[1], rf_holdout_overfitting_range[1], ranger_holdout_overfitting_range[1],
    rpart_holdout_overfitting_range[1], svm_holdout_overfitting_range[1], tree_holdout_overfitting_range[1], xgb_holdout_overfitting_range[1],
    ensemble_adaboost_holdout_overfitting_range[1], ensemble_bagging_holdout_overfitting_range[1], ensemble_C50_holdout_overfitting_range[1],
    ensemble_gb_holdout_overfitting_range[1], ensemble_pls_holdout_overfitting_range[1], ensemble_pda_holdout_overfitting_range[1], ensemble_rf_holdout_overfitting_range[1],
    ensemble_ranger_holdout_overfitting_range[1], ensemble_rda_holdout_overfitting_range[1], ensemble_rpart_holdout_overfitting_range[1],
    ensemble_svm_holdout_overfitting_range[1], ensemble_tree_holdout_overfitting_range[1], ensemble_xgb_holdout_overfitting_range[1]
  ), 4),
  "Overfitting_Mean" = round(c(
    adaboost_holdout_overfitting_mean, bag_rf_holdout_overfitting_mean, bagging_holdout_overfitting_mean, bayesglm_holdout_overfitting_mean,
    bayesrnn_holdout_overfitting_mean, C50_holdout_overfitting_mean, cubist_holdout_overfitting_mean, fda_holdout_overfitting_mean,
    gam_holdout_overfitting_mean, gb_holdout_overfitting_mean, glm_holdout_overfitting_mean, linear_holdout_overfitting_mean,
    lda_holdout_overfitting_mean, mda_holdout_overfitting_mean, n_bayes_holdout_overfitting_mean,
    pda_holdout_overfitting_mean, qda_holdout_overfitting_mean, rf_holdout_overfitting_mean, ranger_holdout_overfitting_mean,
    rpart_holdout_overfitting_mean, svm_holdout_overfitting_mean, tree_holdout_overfitting_mean, xgb_holdout_overfitting_mean,
    ensemble_adaboost_holdout_overfitting_mean, ensemble_bagging_holdout_overfitting_mean, ensemble_C50_holdout_overfitting_mean,
    ensemble_gb_holdout_overfitting_mean, ensemble_pls_holdout_overfitting_mean, ensemble_pda_holdout_overfitting_mean, ensemble_rf_holdout_overfitting_mean,
    ensemble_ranger_holdout_overfitting_mean, ensemble_rda_holdout_overfitting_mean, ensemble_rpart_holdout_overfitting_mean,
    ensemble_svm_holdout_overfitting_mean, ensemble_tree_holdout_overfitting_mean, ensemble_xgb_holdout_overfitting_mean
  ), 4),
  "Overfitting_Max" = round(c(
    adaboost_holdout_overfitting_range[2], bag_rf_holdout_overfitting_range[2], bagging_holdout_overfitting_range[2], bayesglm_holdout_overfitting_range[2],
    bayesrnn_holdout_overfitting_range[2], C50_holdout_overfitting_range[2], cubist_holdout_overfitting_range[2], fda_holdout_overfitting_range[2],
    gam_holdout_overfitting_range[2], gb_holdout_overfitting_range[2], glm_holdout_overfitting_range[2], linear_holdout_overfitting_range[2],
    lda_holdout_overfitting_range[2], mda_holdout_overfitting_range[2], n_bayes_holdout_overfitting_range[2],
    pda_holdout_overfitting_range[2], qda_holdout_overfitting_range[2], rf_holdout_overfitting_range[2], ranger_holdout_overfitting_range[2],
    rpart_holdout_overfitting_range[2], svm_holdout_overfitting_range[2], tree_holdout_overfitting_range[2], xgb_holdout_overfitting_range[2],
    ensemble_adaboost_holdout_overfitting_range[2], ensemble_bagging_holdout_overfitting_range[2], ensemble_C50_holdout_overfitting_range[2],
    ensemble_gb_holdout_overfitting_range[2], ensemble_pls_holdout_overfitting_range[2], ensemble_pda_holdout_overfitting_range[2], ensemble_rf_holdout_overfitting_range[2],
    ensemble_ranger_holdout_overfitting_range[2], ensemble_rda_holdout_overfitting_range[2], ensemble_rpart_holdout_overfitting_range[2],
    ensemble_svm_holdout_overfitting_range[2], ensemble_tree_holdout_overfitting_range[2], ensemble_xgb_holdout_overfitting_range[2]
  ), 4),
  "Duration" = round(c(
    adaboost_duration_mean, bag_rf_duration_mean, bagging_duration_mean, bayesglm_duration_mean,
    bayesrnn_duration_mean, C50_duration_mean, cubist_duration_mean, fda_duration_mean,
    gam_duration_mean, gb_duration_mean, glm_duration_mean, linear_duration_mean,
    lda_duration_mean, mda_duration_mean, n_bayes_duration_mean,
    pda_duration_mean, qda_duration_mean, rf_duration_mean, ranger_duration_mean,
    rpart_duration_mean, svm_duration_mean, tree_duration_mean, xgb_duration_mean,
    ensemble_adaboost_duration_mean, ensemble_bagging_duration_mean, ensemble_C50_duration_mean,
    ensemble_gb_duration_mean, ensemble_pls_duration_mean, ensemble_pda_duration_mean, ensemble_rf_duration_mean,
    ensemble_ranger_duration_mean, ensemble_rda_duration_mean, ensemble_rpart_duration_mean,
    ensemble_svm_duration_mean, ensemble_tree_duration_mean, ensemble_xgb_duration_mean
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
    rep("ADA Boost", numresamples), rep("Bagged Random Forest", numresamples), rep("Bagging", numresamples),
    rep("BayesGLM", numresamples), rep("BayesRNN", numresamples), rep("C50", numresamples),
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Gradient Boosted", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples), rep("Mixture Discriminant Analysis", numresamples), rep("Naive Bayes", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples), rep("XGBoost", numresamples),
    rep("Ensemble ADA Boost", numresamples), rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Ranger", numresamples), rep("Ensemble Regularized Discriminant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    adaboost_holdout_accuracy, bag_rf_holdout_accuracy, bagging_holdout_accuracy,
    bayesglm_holdout_accuracy, bayesrnn_holdout_accuracy, C50_holdout_accuracy,
    cubist_holdout_accuracy, fda_holdout_accuracy, gam_holdout_accuracy,
    glm_holdout_accuracy, gb_holdout_accuracy,
    linear_holdout_accuracy, lda_holdout_accuracy, mda_holdout_accuracy,
    n_bayes_holdout_accuracy, pda_holdout_accuracy,
    qda_holdout_accuracy, rf_holdout_accuracy, ranger_holdout_accuracy,
    rpart_holdout_accuracy, svm_holdout_accuracy,
    tree_holdout_accuracy, svm_holdout_accuracy,
    ensemble_adaboost_holdout_accuracy, ensemble_bagging_holdout_accuracy,
    ensemble_C50_holdout_accuracy, ensemble_gb_holdout_accuracy,
    ensemble_pls_holdout_accuracy,
    ensemble_pda_holdout_accuracy, ensemble_rf_holdout_accuracy,
    ensemble_ranger_holdout_accuracy, ensemble_rda_holdout_accuracy,
    ensemble_rpart_holdout_accuracy, ensemble_svm_holdout_accuracy,
    ensemble_tree_holdout_accuracy, ensemble_xgb_holdout_accuracy
  ),
  "mean" = rep(c(
    adaboost_holdout_accuracy_mean, bag_rf_holdout_accuracy_mean, bagging_holdout_accuracy_mean,
    bayesglm_holdout_accuracy_mean, bayesrnn_holdout_accuracy_mean, C50_holdout_accuracy_mean,
    cubist_holdout_accuracy_mean, fda_holdout_accuracy_mean, gam_holdout_accuracy_mean,
    glm_holdout_accuracy_mean, gb_holdout_accuracy_mean,
    linear_holdout_accuracy_mean, lda_holdout_accuracy_mean, mda_holdout_accuracy_mean,
    n_bayes_holdout_accuracy_mean, pda_holdout_accuracy_mean,
    qda_holdout_accuracy_mean, rf_holdout_accuracy_mean, ranger_holdout_accuracy_mean,
    rpart_holdout_accuracy_mean, svm_holdout_accuracy_mean,
    tree_holdout_accuracy_mean, svm_holdout_accuracy_mean,
    ensemble_adaboost_holdout_accuracy_mean, ensemble_bagging_holdout_accuracy_mean,
    ensemble_C50_holdout_accuracy_mean, ensemble_gb_holdout_accuracy_mean,
    ensemble_pls_holdout_accuracy_mean,
    ensemble_pda_holdout_accuracy_mean, ensemble_rf_holdout_accuracy_mean,
    ensemble_ranger_holdout_accuracy_mean, ensemble_rda_holdout_accuracy_mean,
    ensemble_rpart_holdout_accuracy_mean, ensemble_svm_holdout_accuracy_mean,
    ensemble_tree_holdout_accuracy_mean, ensemble_xgb_holdout_accuracy_mean
  ), each = numresamples)
)

accuracy_plot <- ggplot2::ggplot(data = accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = mean), linewidth = 1.25) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 6) +
  ggplot2::ggtitle("Accuracy by model, higher is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Accuracy by model, higher is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

total_accuracy_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("ADA Boost", numresamples), rep("Bagged Random Forest", numresamples), rep("Bagging", numresamples),
    rep("BayesGLM", numresamples), rep("BayesRNN", numresamples), rep("C50", numresamples),
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Gradient Boosted", numresamples),
    rep("Linear", numresamples), rep("Linear Discrmininant Analysis", numresamples), rep("Mixture Discriminant Analysis", numresamples),
    rep("Naive Bayes", numresamples), rep("Penalized Discrmininant Analysis", numresamples),
    rep("Quadratic Discrmininant Analysis", numresamples), rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples), rep("XGBoost", numresamples),
    rep("Ensemble ADA Boost", numresamples), rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Ranger", numresamples), rep("Ensemble Regularized Discriminant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "train" = c(
    adaboost_train_accuracy, bag_rf_train_accuracy, bagging_train_accuracy,
    bayesglm_train_accuracy, bayesrnn_train_accuracy, C50_train_accuracy,
    cubist_train_accuracy, fda_train_accuracy, gam_train_accuracy,
    glm_train_accuracy, gb_train_accuracy,
    linear_train_accuracy, lda_train_accuracy, mda_train_accuracy,
    n_bayes_train_accuracy, pda_train_accuracy,
    qda_train_accuracy, rf_train_accuracy, ranger_train_accuracy,
    rpart_train_accuracy, svm_train_accuracy,
    tree_train_accuracy, svm_train_accuracy,
    ensemble_adaboost_train_accuracy, ensemble_bagging_train_accuracy,
    ensemble_C50_train_accuracy, ensemble_gb_train_accuracy,
    ensemble_pls_train_accuracy,
    ensemble_pda_train_accuracy, ensemble_rf_train_accuracy,
    ensemble_ranger_train_accuracy, ensemble_rda_train_accuracy,
    ensemble_rpart_train_accuracy, ensemble_svm_train_accuracy,
    ensemble_tree_train_accuracy, ensemble_xgb_train_accuracy
  ),
  "holdout" = c(
    adaboost_holdout_accuracy, bag_rf_holdout_accuracy, bagging_holdout_accuracy,
    bayesglm_holdout_accuracy, bayesrnn_holdout_accuracy, C50_holdout_accuracy,
    cubist_holdout_accuracy, fda_holdout_accuracy, gam_holdout_accuracy,
    glm_holdout_accuracy, gb_holdout_accuracy,
    linear_holdout_accuracy, lda_holdout_accuracy, mda_holdout_accuracy,
    n_bayes_holdout_accuracy, pda_holdout_accuracy,
    qda_holdout_accuracy, rf_holdout_accuracy, ranger_holdout_accuracy,
    rpart_holdout_accuracy, svm_holdout_accuracy,
    tree_holdout_accuracy, svm_holdout_accuracy,
    ensemble_adaboost_holdout_accuracy, ensemble_bagging_holdout_accuracy,
    ensemble_C50_holdout_accuracy, ensemble_gb_holdout_accuracy,
    ensemble_pls_holdout_accuracy,
    ensemble_pda_holdout_accuracy, ensemble_rf_holdout_accuracy,
    ensemble_ranger_holdout_accuracy, ensemble_rda_holdout_accuracy,
    ensemble_rpart_holdout_accuracy, ensemble_svm_holdout_accuracy,
    ensemble_tree_holdout_accuracy, ensemble_xgb_holdout_accuracy
  )
)

total_plot <- ggplot2::ggplot(data = total_accuracy_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = train, color = "train")) +
  ggplot2::geom_point(mapping = aes(x = count, y = train)) +
  ggplot2::geom_line(mapping = aes(x = count, y = holdout, color = "holdout")) +
  ggplot2::geom_point(mapping = aes(x = count, y = holdout)) +
  ggplot2::facet_wrap(~model, ncol = 6) +
  ggplot2::ggtitle("Accuracy data including train and holdout results, by model and resample. The best result is 1.0") +
  ggplot2::labs(y = "Accuracy, the best result is 1.0") +
  ggplot2::scale_color_manual(
    name = "Total Results",
    breaks = c("train", "holdout"),
    values = c(
      "train" = "red", "holdout" = "black"
    )
  )

overfitting_data <- data.frame(
  "count" = 1:numresamples,
  "model" = c(
    rep("ADA Boost", numresamples), rep("Bagged Random Forest", numresamples), rep("Bagging", numresamples),
    rep("BayesGLM", numresamples), rep("BayesRNN", numresamples), rep("C50", numresamples),
    rep("Cubist", numresamples), rep("Flexible Discriminant Analysis", numresamples), rep("Generalized Additive Models", numresamples),
    rep("Generalized Linear Models", numresamples), rep("Gradient Boosted", numresamples), rep("Linear", numresamples),
    rep("Linear Discrmininant Analysis", numresamples), rep("Mixture Discriminant Analysis", numresamples), rep("Naive Bayes", numresamples),
    rep("Penalized Discrmininant Analysis", numresamples), rep("Quadratic Discrmininant Analysis", numresamples),
    rep("Random Forest", numresamples), rep("Ranger", numresamples),
    rep("RPart", numresamples), rep("Support Vector Machines", numresamples),
    rep("Trees", numresamples), rep("XGBoost", numresamples),
    rep("Ensemble ADA Boost", numresamples), rep("Ensemble Bagging", numresamples),
    rep("Ensemble C50", numresamples), rep("Ensemble Gradient Boosted", numresamples),
    rep("Ensemble Partial Least Squares", numresamples),
    rep("Ensemble Penalized Discrmininant Analysis", numresamples), rep("Ensemble Random Forest", numresamples),
    rep("Ensemble Ranger", numresamples), rep("Ensemble Regularized Discriminant Analysis", numresamples),
    rep("Ensemble RPart", numresamples), rep("Ensemble Support Vector Machines", numresamples),
    rep("Ensemble Trees", numresamples), rep("Ensemble XGBoost", numresamples)
  ),
  "data" = c(
    adaboost_holdout_overfitting, bag_rf_holdout_overfitting, bagging_holdout_overfitting,
    bayesglm_holdout_overfitting, bayesrnn_holdout_overfitting, C50_holdout_overfitting,
    cubist_holdout_overfitting, fda_holdout_overfitting, gam_holdout_overfitting,
    glm_holdout_overfitting, gb_holdout_overfitting,
    linear_holdout_overfitting, lda_holdout_overfitting, mda_holdout_overfitting,
    n_bayes_holdout_overfitting, pda_holdout_overfitting,
    qda_holdout_overfitting, rf_holdout_overfitting, ranger_holdout_overfitting,
    rpart_holdout_overfitting, svm_holdout_overfitting,
    tree_holdout_overfitting, svm_holdout_overfitting,
    ensemble_adaboost_holdout_overfitting, ensemble_bagging_holdout_overfitting,
    ensemble_C50_holdout_overfitting, ensemble_gb_holdout_overfitting,
    ensemble_pls_holdout_overfitting,
    ensemble_pda_holdout_overfitting, ensemble_rf_holdout_overfitting,
    ensemble_ranger_holdout_overfitting, ensemble_rda_holdout_overfitting,
    ensemble_rpart_holdout_overfitting, ensemble_svm_holdout_overfitting,
    ensemble_tree_holdout_overfitting, ensemble_xgb_holdout_overfitting
  )
)

overfitting_plot <- ggplot2::ggplot(data = overfitting_data, mapping = ggplot2::aes(x = count, y = data, color = model)) +
  ggplot2::geom_line(mapping = aes(x = count, y = data)) +
  ggplot2::geom_point(mapping = aes(x = count, y = data)) +
  ggplot2::geom_hline(aes(yintercept = 1, color = "red")) +
  ggplot2::facet_wrap(~model, ncol = 6) +
  ggplot2::ggtitle("Overfitting plot\nOverfitting value by model, closer to one is better. \n The black horizontal line is the mean of the results, the red horizontal line is 1.") +
  ggplot2::labs(y = "Overfititng value, closer to one is better \n The horizontal line is the mean of the results, the red line is 1.") +
  ggplot2::theme(legend.position = "none")

accuracy_barchart <- ggplot2::ggplot(holdout_results, aes(x = reorder(Model, dplyr::desc(Accuracy)), y = Accuracy)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Accuracy", title = "Model accuracy, closer to one is better") +
  ggplot2::geom_text(aes(label = Accuracy), vjust = -0.5, hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(holdout_results$Accuracy) + 1)

overfitting_barchart <- ggplot2::ggplot(holdout_results, aes(x = reorder(Model, dplyr::desc(Overfitting_Mean)), y = Overfitting_Mean)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Over or Under Fitting Mean", title = "Over or Under Fitting, closer to 1 is better") +
  ggplot2::geom_text(aes(label = Overfitting_Mean), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(holdout_results$Overfitting_Mean) +1)

duration_barchart <- ggplot2::ggplot(holdout_results, aes(x = reorder(Model, Duration), y = Duration)) +
  ggplot2::geom_col(width = 0.5)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 1, hjust=1)) +
  ggplot2::labs(x = "Model", y = "Duration", title = "Duration, shorter is better") +
  ggplot2::geom_text(aes(label = Duration), vjust = 0,hjust = -0.5, angle = 90) +
  ggplot2::ylim(0, max(holdout_results$Duration) + 1)

if (save_all_trained_models == "Y") {
  adaboost_train_fit <<- adaboost_train_fit
  bag_rf_train_fit <<- bag_rf_train_fit
  bagging_train_fit <<- bagging_train_fit
  bayesglm_train_fit <<- bayesglm_train_fit
  bayesrnn_train_fit <<- bayesrnn_train_fit
  C50_train_fit <<- C50_train_fit
  cubist_train_fit <<- cubist_train_fit
  fda_train_fit <<- fda_train_fit
  gam_train_fit <<- gam_train_fit
  gb_train_fit <<- gb_train_fit
  glm_train_fit <<- glm_train_fit
  linear_train_fit <<- linear_train_fit
  lda_train_fit <<- lda_train_fit
  mda_train_fit <<- mda_train_fit
  n_bayes_train_fit <<- n_bayes_train_fit
  pda_train_fit <<- pda_train_fit
  qda_train_fit <<- qda_train_fit
  rf_train_fit <<- rf_train_fit
  ranger_train_fit <<- ranger_train_fit
  rpart_train_fit <<- rpart_train_fit
  svm_train_fit <<- svm_train_fit
  tree_train_fit <<- tree_train_fit
  xgb_model <<- xgb_model
  ensemble_adaboost_train_fit <<- ensemble_adaboost_train_fit
  ensemble_bagging_train_fit <<- ensemble_bagging_train_fit
  ensemble_C50_train_fit <<- ensemble_C50_train_fit
  ensemble_gb_train_fit <<- ensemble_gb_train_fit
  ensemble_pls_train_fit <<- ensemble_pls_train_fit
  ensemble_pda_train_fit <<- ensemble_pda_train_fit
  ensemble_rf_train_fit <<- ensemble_rf_train_fit
  ensemble_ranger_train_fit <<- ensemble_ranger_train_fit
  ensemble_rda_train_fit <<- ensemble_rda_train_fit
  ensemble_rpart_train_fit <<- ensemble_rpart_train_fit
  ensemble_svm_train_fit <<- ensemble_svm_train_fit
  ensemble_tree_train_fit <<- ensemble_tree_train_fit
  ensemble_xgb_model <<- ensemble_xgb_model
}

if (do_you_have_new_data == "Y") {
  ADA_boost <- as.numeric(predict(object = adaboost_train_fit, newdata = newdata)) - 1
  Bagged_RF <- as.numeric(predict(object = bag_rf_train_fit, newdata = newdata)) - 1
  Bagging <- as.numeric(predict(object = bagging_train_fit, newdata = newdata)) - 1
  BayesGLM <- predict(object = bayesglm_train_fit, newdata = newdata)
  BayesGLM <- ifelse(BayesGLM > 0, 0, 1)
  BayesRNN <- as.numeric(predict(object = bayesrnn_train_fit, newdata = newdata))
  C50 <- as.numeric(predict(object = C50_train_fit, newdata = newdata)) - 1
  Cubist <- predict(object = cubist_train_fit, newdata = newdata)
  Flexible_Discriminant_Analysis <- as.numeric(predict(object = fda_train_fit, newdata = newdata)) - 1
  Generalized_Linear_Models <- as.numeric(predict(object = glm_train_fit, newdata = newdata)) - 1
  Generalized_Linear_Models <- ifelse(Generalized_Linear_Models > 0, 1, 0)
  Gradient_Boosted <- as.numeric(predict(object = gb_train_fit, newdata = newdata)) - 1
  Gradient_Boosted <- ifelse(Gradient_Boosted > 0, 1, 0)
  Generalized_Additive_Models <- as.numeric(predict(object = gam_train_fit, newdata = newdata)) - 1
  Generalized_Additive_Models <- ifelse(Generalized_Additive_Models > 0, 1, 0)
  Linear <- as.numeric(predict(object = linear_train_fit, newdata = newdata)) - 1
  Mixture_Discrmininant_Analysis <- as.numeric(predict(object = mda_train_fit, newdata = newdata)) - 1
  Naive_Bayes <- as.numeric(predict(object = n_bayes_train_fit, newdata = newdata)) - 1
  Penalized_Discriminant_Analysis <- as.numeric(predict(object = pda_train_fit, newdata = newdata)) - 1
  Quadratic_Discriminant_Analysis <- as.numeric(predict(object = qda_train_fit, newdata = newdata)$class) - 1
  Random_Forest <- as.numeric(predict(object = rf_train_fit, newdata = newdata)) - 1
  Ranger <- as.numeric(predict(object = ranger_train_fit, newdata = newdata)) - 1
  RPart <- as.numeric(predict(object = rpart_train_fit, newdata = newdata)) - 1
  Support_Vector_Machines <- as.numeric(predict(object = svm_train_fit, newdata = newdata)) - 1
  Trees <- predict(object = tree_train_fit, newdata = newdata)
  XGBoost <- predict(object = tree_train_fit, newdata = newdata) #### FIX THIS

  new_ensemble <- data.frame(
    ADA_boost,
    Bagged_RF,
    Bagging,
    BayesGLM,
    BayesRNN,
    C50,
    Cubist,
    Flexible_Discriminant_Analysis,
    Gradient_Boosted,
    Generalized_Linear_Models,
    Linear,
    Mixture_Discrmininant_Analysis,
    Naive_Bayes,
    Penalized_Discriminant_Analysis,
    Quadratic_Discriminant_Analysis,
    Random_Forest,
    Ranger,
    RPart,
    Support_Vector_Machines,
    Trees,
    XGBoost
  )

  # new_ensemble_row_numbers <- as.numeric(row.names(newdata))
  new_ensemble$y <- as.factor(newdata$y)
  thing <- colnames(ensemble1)
  new_ensemble %>% select(dplyr::all_of(thing))

  new_ensemble_adaboost <- predict(object = ensemble_adaboost_train_fit, newdata = new_ensemble)
  new_ensemble_bagging <- predict(object = ensemble_bagging_train_fit, newdata = new_ensemble)
  new_ensemble_C50 <- predict(object = ensemble_C50_train_fit, newdata = new_ensemble)
  new_ensemble_gb <- predict(object = ensemble_gb_train_fit, newdata = new_ensemble)
  new_ensemble_pls <- predict(object = ensemble_pls_train_fit, newdata = new_ensemble)
  new_ensemble_pda <- predict(object = ensemble_pda_train_fit, newdata = new_ensemble)
  new_ensemble_ranger <- predict(object = ensemble_ranger_train_fit, newdata = new_ensemble)
  new_ensemble_rda <- predict(object = ensemble_rda_train_fit, newdata = new_ensemble)$class
  new_ensemble_RPart <- predict(object = ensemble_rpart_train_fit, newdata = new_ensemble)
  new_ensemble_svm <- predict(object = ensemble_svm_train_fit, newdata = new_ensemble)
  new_ensemble_trees <- predict(object = ensemble_tree_train_fit, newdata = new_ensemble)

  new_data_results <- data.frame(
    "True_Value" = newdata$y,
    "ADA_Boost" = ADA_boost,
    "Bagged_Random_Forest" = Bagged_RF,
    "Bagging" = Bagging,
    "BayesGLM" = BayesGLM,
    "BayesRNN" = BayesRNN,
    "C50" = C50,
    "Cubist" = Cubist,
    "Flexible_Discriminant_Analysis" = Flexible_Discriminant_Analysis,
    "Generalized_Linear_Models" = Generalized_Linear_Models,
    "Gradient_Boosted" = Gradient_Boosted,
    "Generalized_Additive_Models" = Generalized_Additive_Models,
    "Linear" = Linear,
    "Mixture_Discrmininant_Analysis" = Mixture_Discrmininant_Analysis,
    "Naive_Bayes" = Naive_Bayes,
    "Quadratic_Disrmininat_Analysis" = Quadratic_Discriminant_Analysis,
    "Penalized_Discriminant_Analysis" = Penalized_Discriminant_Analysis,
    "Random_Forest" = Random_Forest,
    "Ranger" = Ranger,
    "RPart" = RPart,
    "Trees" = Trees,
    "Ensemble_ADA_Boost" = new_ensemble_adaboost,
    "Ensemble_Bagging" = new_ensemble_bagging,
    "Ensemble_C50" = new_ensemble_C50,
    "Ensemble_Gradient_Boosted" = new_ensemble_gb,
    "Ensemble_Partial_Least_Squares" = new_ensemble_pls,
    "Ensemble_Penalized_Discrmininat_Analysis" = new_ensemble_pda,
    "Ensemble_Ranger" = new_ensemble_ranger,
    "Ensemble_Regularized_Discrmininat_Analysis" = new_ensemble_rda,
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
    "ADA_Boost" = adaboost_table_total, "Bagged_Random_Forest" = bag_rf_table_total, "Bagging" = bagging_table_total, "BayesGLM" = bayesglm_table_total, "BayesRNN" = bayesrnn_table_total,
    "C50" = C50_table_total, "Cubist" = cubist_table_total, "Fixture Discrmininant Analysis" = fda_table_total, "Generalized Additive Methods" = gam_table_total,
    "Gradient Boosted" = gb_table_total, "Generalized Linear Models" = glm_table_total, "Linear" = linear_table_total, "Linear Discrmininant Analysis" = lda_table_total,
    "Mixture Discrminint Analysis" = mda_table_total, "Naive Bayes" = n_bayes_table_total, "Penalized Discrminant Analysis" = pda_table_total,
    "Quadratic Discrmininant Analysis" = qda_table_total, "Random Forest" = rf_table_total, "Ranger" = ranger_table_total,
    "RPart" = rpart_table_total, "Support Vector Machines" = svm_table_total, "Trees" = tree_table_total, "XGBoost" = xgb_table_total,
    "Ensembel ADABoost" = ensemble_adaboost_table_total, "Ensemble Bagging" = ensemble_bagging_table_total, "Ensemble C50" = ensemble_C50_table_total,
    "Ensemble Gradient Boosted" = ensemble_gb_table_total, "Ensemble Partial Least Squares" = ensemble_pls_table_total,
    "Ensemble Penalized Discrmininant Analysis" = ensemble_pda_table_total, "Ensemble Random Forest" = ensemble_rf_table_total,
    "Ensemble Ranger" = ensemble_ranger_table_total, "Ensemble Regularized Discrmininant Analysis" = ensemble_rda_table_total,
    "Ensemble RPart" = ensemble_rpart_table_total, "Ensemble Support Vector Machines" = ensemble_svm_table_total,
    "Ensemble Trees" = ensemble_tree_table_total, "Ensemble XGBoost" = ensemble_xgb_table_total
  )

  return(list(
    "Head_of_data" = head_df, "Summary_tables" = summary_tables, "accuracy_plot" = accuracy_plot, "total_plot" = total_plot, "overfitting_plot" = overfitting_plot,
    "accuracy_barchart" = accuracy_barchart, "Duration_barchart" = duration_barchart, "Overfitting_barchart" = overfitting_barchart, "ROC_curves" = ROC_curves,
    "Boxplots" = boxplots, "Barchart" = barchart, "Display_pairs" = display_pairs, "Correlation_table" = correlation_table,
    "Ensemble Correlation" = ensemble_correlation, "Ensemble_head" = head_ensemble, "New_data_results" = new_data_results,
    "Data_Summary" = data_summary, "Holdout_results" = holdout_results_final, "Data_dictionary" = str(df),
    "How_to_handle_strings" = how_to_handle_strings, "Train_amount" = train_amount, "Test_amount" = test_amount, "Validation_amount" = validation_amount
  ))
}

summary_tables <- list(
  "ADA_Boost" = adaboost_table_total, "Bagged_Random_Forest" = bag_rf_table_total, "Bagging" = bagging_table_total, "BayesGLM" = bayesglm_table_total, "BayesRNN" = bayesrnn_table_total,
  "C50" = C50_table_total, "Cubist" = cubist_table_total, "Fixture Discrmininant Analysis" = fda_table_total, "Generalized Additive Methods" = gam_table_total,
  "Gradient Boosted" = gb_table_total, "Generalized Linear Models" = glm_table_total, "Linear" = linear_table_total, "Linear Discrmininant Analysis" = lda_table_total,
  "Mixture Discrminint Analysis" = mda_table_total, "Naive Bayes" = n_bayes_table_total, "Penalized Discrminant Analysis" = pda_table_total,
  "Quadratic Discrmininant Analysis" = qda_table_total, "Random Forest" = rf_table_total, "Ranger" = ranger_table_total,
  "RPart" = rpart_table_total, "Support Vector Machines" = svm_table_total, "Trees" = tree_table_total, "XGBoost" = xgb_table_total,
  "Ensembel ADABoost" = ensemble_adaboost_table_total, "Ensemble Bagging" = ensemble_bagging_table_total, "Ensemble C50" = ensemble_C50_table_total,
  "Ensemble Gradient Boosted" = ensemble_gb_table_total, "Ensemble Partial Least Squares" = ensemble_pls_table_total,
  "Ensemble Penalized Discrmininant Analysis" = ensemble_pda_table_total, "Ensemble Random Forest" = ensemble_rf_table_total,
  "Ensemble Ranger" = ensemble_ranger_table_total, "Ensemble Regularized Discrmininant Analysis" = ensemble_rda_table_total,
  "Ensemble RPart" = ensemble_rpart_table_total, "Ensemble Support Vector Machines" = ensemble_svm_table_total,
  "Ensemble Trees" = ensemble_tree_table_total, "Ensemble XGBoost" = ensemble_xgb_table_total
)

return(list(
  "Head_of_data" = head_df, "Summary_tables" = summary_tables, "accuracy_plot" = accuracy_plot, "total_plot" = total_plot, "accuracy_barchart" = accuracy_barchart,
  "overfitting_plot" = overfitting_plot, "Duration_barchart" = duration_barchart, "Overfitting_barchart" = overfitting_barchart, "ROC_curves" = ROC_curves,
  "Boxplots" = boxplots, "Barchart" = barchart, "Display_pairs" = display_pairs, "Correlation_table" = correlation_table,
  "Ensemble Correlation" = ensemble_correlation, "Ensemble_head" = head_ensemble,
  "Data_Summary" = data_summary, "Holdout_results" = holdout_results_final,
  "How_to_handle_strings" = how_to_handle_strings, "Train_amount" = train_amount, "Test_amount" = test_amount, "Validation_amount" = validation_amount
)
)
}
