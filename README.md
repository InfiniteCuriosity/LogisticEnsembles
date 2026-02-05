
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
Logistic(data = Diabetes,
         colnum = 9,
         numresamples = 10,
         remove_VIF_greater_than <- 5.00,
         save_all_trained_models = "N",
         save_all_plots = "N",
         set_seed = "N",
         how_to_handle_strings = 1,
         do_you_have_new_data = "N",
         remove_data_correlations_greater_than = 1.00,
         remove_ensemble_correlations_greater_than = 0.99,
         use_parallel = "Y",
         train_amount = 0.50,
         test_amount = 0.25,
         validation_amount = 0.25)
end_time <- Sys.time()
duration <- end_time - start_time
duration
warnings()
```

LogisticEnsembles only requires the code seen above, and it automatically builds 18 logistic models from the data.

Each of the 18 models fits the data to the training set, then make predictions and measure accuracy on the test and validation sets. It does this by converting the predictions using the `logistic` function in the stats package, as follows:
gam is used in this example, but the process is identical for all 18 logistic models:
  
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

The list of 18 logistic models:

1. BayesGLM
2. BayesRNN
3. C50
4. Cubist
5. Ensemble Bagging
6. Ensemble C50
7. Ensemble Support Vector Machines
8. Ensemble XGBoost
9. Flexible Discriminant Analysis
10. Generalized Additive Models
11. Generalized Linear Models
12. Linear
13. Linear Discriminant Analysis
14. Penalized Discriminant Analysis
15. Random Forest
15. Support Vector Machines
17. Trees
18. XGBoost

The 28 plots automatically created by the package are:
1. Correlation plot as circles and numbers
2. Correlation plot as circles and colors
3. Separators (the number of charts will depend on the number of features in the original data)
4. accuracy_plot
5. total_plot_fixed_scales
6. total_plot_free_scales
7. accuracy_barchart
8. overfitting_plot_fixed_scales
9. overfitting_plot_free_scales
10. Duration_barchart
11. Overfitting_barchart
12. ROC_curves
13. Boxplots
14 Barchart of target vs each feature
15. True_positive_rate_fixed_scales
16. True_positive_rate_free_scales
17. True_negative_rate_fixed_scales
18. True_negative_rate_free_scales
19. False_positive_rate_fixed_scales
20. False_positive_rate_free_scales
21. False_negative_rate_fixed_scales
22. False_negative_rate_free_scales
23. F1_score_fixed_scales
24. F1_score_free_scales
25. Positive_predictive_value_fixed_scales
26. Positive_predictive_value_free_scales
27. Negative_predictive_value_fixed_scales
28. Negative_predictive_value_free_scales

Note that several of the plots are represented twice - once with fixed scales and once with free scales.

The 7 tables and reports automatically created:
1. Head_of_data
2. Correlation of the original data
3. VIF results
4. Correlation of the ensemble
5. Head of the ensemble
6. Data_Summary
7. Summary report, named "Mean of Holdout results". This includes the Model, Accuracy, True Positive, True Negative, False Positive, False Negative, Positive Predictive Value, Negative Predictive Value, F1 score, Area under the curve, overfitting min, overfitting mean, overfitting max, and duration.

The package also returns all 24 summary confusion matrices, alphabetical by model. If the user uses resampling, it adds up the values, so any error is visible, even if an event only happens once. For example, from the results about the Lebron data:

$Summary_tables$Cubist
                                y_test
cubist_test_predictions_binomial    0    1
                               0 4801 2555
                               1 4730 7080

It is also possible to save all trained models, save all plots, set the seed, and much more within LogisticEnsembles.

Summary: LogisticEnsembles should give everything you need to write a summary report based on the data.
