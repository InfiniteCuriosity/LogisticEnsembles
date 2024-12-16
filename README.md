
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
Logistic(data = SAHeart,
    colnum = 10,
    numresamples = 2,
    how_to_handle_strongs = 1,
    do_you_have_new_data = "N",
    save_all_trained_models = "N",
    remove_ensemble_correlations_greater_than = 1.00,
    use_parallel = "N",
    train_amount = 0.60,
    test_amount = 0.20,
    validation_amount = 0.20)
```
Each of the 36 models returns a probability between 0 and 1. Each of the 36 models fit the data to the training set, make predictions and measure accuracy on the test and validation sets.

The list of 36 logistic models:

1. ADA Boost
2. Bagged Random Forest
3. Bagging
4. BayesGLM
5. BayesRNN
6. C50
7. Cubist
8. Ensemble ADA Boost
9. Ensemble Bagging
10. Ensemble C50
11. Ensemble Gradient Boosted
12. Ensemble Partial Least Squares
13. Ensemble Penalized Discrminant Analysis
14. Ensemble Random Forest
15. Ensemble Ranger
16. Ensemble Regularized Discrminant Analysis
17. Ensemble RPart
18. Ensemble Support Vector Machines
19. Ensemble Trees
20. Ensemble XGBoost
21. Flexible Discriminant Analysis
22. Generalized Additive Models
23. Generalized Linear Models
24. Gradient Boosted
25. Linear Discrmininant Analysis
26. Linear Model
27. Mixed Discrmininant Analysis
28. Naive Bayes
28. Penalized Discrminant Analysis
30. Quadratic Discrmininant Analysis
31. Random Forest
32. Ranger
33. RPart
34. Support Vector Machines
35. Trees
36. XGBoost

The 13 plots automatically created by the package are:

1. Target vs each feature (multiple barcharts)
2. Boxplots of the numeric data
3. Over or underfitting barchart
4. Duration barchart
5. Overfitting by model and resample
6. Model accuracy barchart
7. Accuracy by model and resample
8. Accuracy by model
9. ROC curves
10. Pairwise scatterplots
11. Correlation of the data as circles by color and size
12. Correlation of the data by color and number

The tables and reports automtically created:
1. Summary report. This includes the Model, Accuracy, True Postive, True Negative, False Positive, False Negative, Positive Predictive Value, Negative Predictive Value, F1 score, Area under the curve, overfitting min, overfitting mean, overfitting max, and duration.
2. Data summary
3. Head of the ensemble
4. Correlation of the ensemble
5. Correlation of the data
6. Head of the data frame

The package also returns all 36 summary reports, alphabetical by model.
