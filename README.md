
# LogisticEnsembles

<!-- badges: start -->
<!-- badges: end -->

The goal of LogisticEnsembles is to perform a complete analysis of logistic data. The package automatically returns 36 models (23 individual and 13 ensembles of models)

## Installation

You can install the development version of LogisticEnsembles like so:

``` r
devtools::install_github("InfiniteCuriosity/LogisticEnsembles")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LogisticEnsembles)
Logistic(data = LogisticEnsembles::Lebron,
colnum = 6,
numresamples = 25,
remove_VIF_greater_than = 4.00,
remove_ensemble_correlations_greater_than = 0.98,
save_all_trained_models = "N",
save_all_plots = "N",
how_to_handle_strings = 0,
do_you_have_new_data = "N",
use_parallel = "Y",
train_amount = 0.60,
test_amount = 0.20,
validation_amount = 0.20)
```
Each of the 33 models returns a probability between 0 and 1. Each of the 33 models fit the data to the training set, make predictions and measure accuracy on the test and validation sets.

The list of 33 models:

1. Bagged Random Forest
2. Bagging
3. BayesGLM
4. C50
5. Cubist
6. Ensemble Bagging
7. Ensemble C50
8. Ensemble Gradient Boosted
9. Ensemble Partial Least Squares
10. Ensemble Penalized Discriminant Analysis
11. Ensemble Random Forest
12. Ensemble Ranger
13. Ensemble Regularized Discriminant Analysis
14. Ensemble RPart
15. Ensemble Support Vector Machines
16. Ensemble Trees
17. Ensemble XGBoost
18. Flexible Discriminant Analysis
19. Generalized Additive Models
20. Generalized Linear Models
21. Gradient Boosted
22. Linear Discriminant Analysis
23. Linear Model
24. Mixed Discriminant Analysis
25. Naive Bayes
26. Penalized Discriminant Analysis
27. Quadratic Discriminant Analysis
28. Random Forest
29. Ranger
30. RPart
31. Support Vector Machines
32. Trees
33. XGBoost

The 25 plots automatically created by the package are:
1. Correlation of the data as numbers and colors
2. Correlation of the data as colors and circles
3. 33 ROC curves (specificity vs sensitivity), including ROC value
4. Accuracy by model, fixed scales
5. Accuracy data including train and holdout results including train and holdout
6. Model accuracy barchart
7. Overfitting plot by model and resample
8. Duration barchart
9. Over or underfitting barchart
10. Boxplots of the numeric data
11. Barchart of target (0 or 1) vs target
12. True positive rate by model, fixed scales
13. True positive rate by model, free scales
14. True negative rate by model, fixed scales
15. True negative rate by model, free scales
16. False positive rate by model, fixed scales
17. False positive rate by model, free scales
18. False negative rate by model, fixed scales
19. False negative rate by model, free scales
20. F1 score by model, fixed scales
21. F1 score by model, free scales
22. Positive predictive value by model, fixed scales
23. Positive predictive value by model, free scales
24. Negative predictive value by model, fixed scales
25. Negative predictive value by model, free scales


The tables and reports automatically created:
1. Summary report. This includes the Model, Accuracy, True Positive, True Negative, False Positive, False Negative, Positive Predictive Value, Negative Predictive Value, F1 score, Area under the curve, overfitting min, overfitting mean, overfitting max, and duration.
2. Data summary
3. Head of the ensemble
4. Correlation of the ensemble
5. Variance Inflation factor
6. Correlation of the data
7. Head of the data frame

The package also returns all 33 summary confusion matrices, alphabetical by model. If the user uses resampling, it adds up the values, so any error is visibile. For example, for the Lebron data:

Summary_tables$`Random Forest`
                     y_test
rf_test_probabilities    0    1
                    0 7586    0
                    1    0 7779
