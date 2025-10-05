
# LogisticEnsembles

<!-- badges: start -->
<!-- badges: end -->

The goal of LogisticEnsembles is to perform a complete analysis of logistic data. The package automatically returns 24 models (13 individual models and 11 ensembles of models)

## Installation

You can install the development version of LogisticEnsembles like so:

``` r
devtools::install_github("InfiniteCuriosity/LogisticEnsembles")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LogisticEnsembles)
head(LogisticEnsembles::Lebron)
start_time <- Sys.time()
Logistic(data = Lebron,
         colnum = 6,
         numresamples = 25,
         remove_VIF_greater_than <- 5.00,
         save_all_trained_models = "N",
         save_all_plots = "N",
         set_seed = "N",
         how_to_handle_strings = 0,
         do_you_have_new_data = "N",
         remove_ensemble_correlations_greater_than = 0.80,
         use_parallel = "Y",
         train_amount = 0.50,
         test_amount = 0.25,
         validation_amount = 0.25)
end_time <- Sys.time()
duration <- end_time - start_time
duration
warnings()
```

LogisticEnsembles only requires the code seen above, and it automatically builds 24 logistic models from the data.

Each of the 24 models fit the data to the training set, make predictions and measure accuracy on the test and validation sets. It does this by converting the predictions using the `logistic` function in the stats package, as follows:
gam is used, but the process is identical for all 24 functions:
  
  1. Build the model on the training data
  gam_train_fit <- gam::gam(y ~ ., data = train)
  
  2. Use the model to make predictions on new data
  gam_train_pred <- stats::predict(gam_train_fit, train01, type = "response")
  
  3. Convert predicted values to logistic using the plogis function (commonly called the 'inverse logit' function. This transforms real numbers into probabilities between 0 and 1)
  gam_train_predictions <- as.numeric(plogis(gam_train_pred))

  4. Covert the predictions to binomial
  gam_train_predictions_binomial <- rbinom(n = length(gam_train_predictions), size = 1, prob = gam_train_predictions) # This converts the values to binomial

  5. Create a summary table (commonly called a confusion matrix)
  gam_train_table <- table(gam_train_predictions_binomial, y_train) # This creates a summary table (commonly called a confusion matrix)

The list of 24 models:

1. Cubist
2. Flexible Discriminant Analysis
3. Generalized Additive Models
4. Generalized Linear Models
5. Lasso
6. Linear
7. Linear Discriminant Analysis
8. Penalized Discriminant Analysis
9. Quadratic Discrmininant Analysis
10. Random Forest
11. Ridge
12. Support Vector Machines
13. Trees
14. Ensemble Bagging
15. Ensemble C50
16. Ensemble Gradient Boosted
17. Ensemble Lasso
18. Ensemble Partial Least Squares
19. Ensemble Penalized Discrmininant Analysis
20. Ensemble Ridge
21. Ensemble RPart
22. Ensemble Support Vector Machines
23. Ensemble Trees
24. Ensemble XGBoost

The 25 plots automatically created by the package are:
1. accuracy_plot
2. total_plot_fixed_scales
3. total_plot_free_scales
4. accuracy_barchart
5. overfitting_plot_fixed_scales
6. overfitting_plot_free_scales
7. Duration_barchart
8. Overfitting_barchart
9. ROC_curves
10. Boxplots
11. Barchart
12. True_positive_rate_fixed_scales
13. True_positive_rate_free_scales
14. True_negative_rate_fixed_scales
15. True_negative_rate_free_scales
16. False_positive_rate_fixed_scales
17. False_positive_rate_free_scales
18. False_negative_rate_fixed_scales
19. False_negative_rate_free_scales
20. F1_score_fixed_scales
21. F1_score_free_scales
22. Positive_predictive_value_fixed_scales
23. Positive_predictive_value_free_scales
24. Negative_predictive_value_fixed_scales
25. Negative_predictive_value_free_scales

Note that several of the plots are represented twice - once with fixed scales and once with free scales.

The 7 tables and reports automatically created:
1. Head_of_data
2. Summary_tables
3. Ensemble Correlation
4. Ensemble_head
5. Data_Summary
6. VIF_results
7. Summary report. This includes the Model, Accuracy, True Positive, True Negative, False Positive, False Negative, Positive Predictive Value, Negative Predictive Value, F1 score, Area under the curve, overfitting min, overfitting mean, overfitting max, and duration.

The package also returns all 24 summary confusion matrices, alphabetical by model. If the user uses resampling, it adds up the values, so any error is visible, even if an event only happens once. For example, from the results about the Lebron data:

$Summary_tables$Cubist
                                y_test
cubist_test_predictions_binomial    0    1
                               0 4801 2555
                               1 4730 7080

It is also possible to save all trained models, save all plots, set the seed, and much more within LogisticEnsembles.

Summary: LogisticEnsembles should give everything you need to write a summary report based on the data.
