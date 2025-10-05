#' SAHeart data
#'
#' This is the South African heart disease data originally published in Elements of Statistical Learning, see https://rdrr.io/cran/ElemStatLearn/man/SAheart.html
#'
#' @format SAHeart
#' \describe{
#'   \item{sbp}{Systolic blood pressure}
#'   \item{tobacco}{cumulative tobacco (kg)}
#'   \item{ldl}{low density lipoprotein cholesterol}
#'   \item{adiposity}{a numeric vector}
#'   \item{famhist}{family history of heart disease, a factor with levels Absent Present}
#'   \item{typea}{type-A behavior}
#'   \item{obesity}{a numeric vector}
#'   \item{alcohol}{current alcohol consumption}
#'   \item{age}{age at onset}
#'   \item{chd}{response, coronary heart disease}
#'
#' }
#' @source Rousseauw, J., du Plessis, J., Benade, A., Jordaan, P., Kotze, J. and Ferreira, J. (1983). Coronary risk factor screening in three rural communities, South African Medical Journal 64: 430–436.
"SAHeart"


#'  Lebron—A logistic data set, with the result indicating whether or not Lebron scored on each shot in the data set.
#'
#'  This dataset opens the door to the intricacies of the 2023 NBA season, offering a profound understanding of the art of scoring in professional basketball.
#'
#'  \describe{
#'  \item{top}{The vertical position on the court where the shot was taken}
#'  \item{left}{The horizontal position on the court where the shot was taken}
#'  \item{date}{The date when the shot was taken. (e.g., Oct 18, 2022)}
#'  \item{qtr}{The quarter in which the shot was attempted, typically represented as "1st Qtr," "2nd Qtr," etc.}
#'  \item{time_remaining}{The time remaining in the quarter when the shot was attempted, typically displayed as minutes and seconds (e.g., 09:26).}
#'  \item{result}{Indicates whether the shot was successful, with "TRUE" for a made shot and "FALSE" for a missed shot}
#'  \item{shot_type}{Describes the type of shot attempted, such as a "2" for a two-point shot or "3" for a three-point shot}
#'  \item{distance_ft}{The distance in feet from the hoop to where the shot was taken}
#'  \item{lead}{Indicates whether the team was leading when the shot was attempted, with "TRUE" for a lead and "FALSE" for no lead}
#'  \item{lebron_team_score}{The team's score (in points) when the shot was taken}
#'  \item{opponent_team_score}{The opposing team's score (in points) when the shot was taken}
#'  \item{opponent}{The abbreviation for the opposing team (e.g., GSW for Golden State Warriors)}
#'  \item{team}{The abbreviation for LeBron James's team (e.g., LAL for Los Angeles Lakers)}
#'  \item{season}{The season in which the shots were taken, indicated as the year (e.g., 2023)}
#'  \item{color}{Represents the color code associated with the shot, which may indicate shot outcomes or other characteristics (e.g., "red" or "green")}
#'  }
#'
#'  @source <https://www.kaggle.com/datasets/dhavalrupapara/nba-2023-player-shot-dataset>
"Lebron"


#' Diabetes—A logistic data set, determining whether a woman tested positive for diabetes. 100 percent accurate results are possible using the logistic function in the Ensembles package.
#'
#' @description
#' "This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective of the dataset
#' is to diagnostically predict whether or not a patient has diabetes, based on certain diagnostic measurements included in the dataset."
#'
#' \describe{
#' This data set is from www.kaggle.com. The original notes on the website state:
#' Context
#' "This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective of the dataset
#' is to diagnostically predict whether or not a patient has diabetes, based on certain diagnostic measurements included in the dataset.
#' Several constraints were placed on the selection of these instances from a larger database. In particular, all patients here are females
#' at least 21 years old of Pima Indian heritage."
#' Content
#' "The datasets consists of several medical predictor variables and one target variable, Outcome. Predictor variables includes the
#' number of pregnancies the patient has had, their BMI, insulin level, age, and so on.
#' Acknowledgements
#' Smith, J.W., Everhart, J.E., Dickson, W.C., Knowler, W.C., & Johannes, R.S. (1988). Using the ADAP learning algorithm to forecast the onset of diabetes mellitus.
#' In Proceedings of the Symposium on Computer Applications and Medical Care (pp. 261--265). IEEE Computer Society Press.
#'
#' \item{Pregnancies}{Number of time pregnant}
#' \item{Glucose}{Plasma glucose concentration a 2 hours in an oral glucose tolerance test}
#' \item{BloodPressure}{Diastolic blood pressure (mm Hg)}
#' \item{SkinThickness}{Triceps skin fold thickness (mm)}
#' \item{Insulin}{2-Hour serum insulin (mu U/ml)}
#' \item{BMI}{Body mass index (weight in kg/(height in m)^2)}
#' \item{DiabetesPedigreeFunction}{Diabetes pedigree function}
#' \item{Age}{Age (years)}
#' \item{Outcome}{Class variable (0 or 1) 268 of 768 are 1, the others are 0}
#' }
#'
#' @source <https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database/data>
"Diabetes"
