## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
The only note states that "Adding so many packages to the search path is excessive and importing
  selectively is preferable."
The package has been tested on multiple different systems, and always ran without any errors or warnings. Removing any of the packages will either reduce the accuracy, or the results to the user (such as summary graphs).

* Please reduce the length of the title to less than 65 characters. Done
* Please do not start the description with the title, "This package", package name, or similar. Done
* Please add more details about the package functionality and implemented methods in your Description text. Done, more examples and detail provided.
* If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form. Done
* Please add small executable examples in your Rd-files to illustrate the use of the exported function but also enable automatic testing. The package does a comprehensive analysis of classification data,
therefore all the executable examples I've created take more than five seconds to run, therefore I have not put any executable examples in the Rd files.
* You write information messages to the console that cannot be easily suppressed. All print comments have been changed to comment.
* Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace. This took quite a bit of work, but it is done. Results are saved to tempdir()

* Update at 0.6.0, check() returns no errors or warnings.

* Update at 0.7.0, improved a number of the logistic formulas so they return more accurate results
* Included Lasso and Ridge from GLMNET
