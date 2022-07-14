# UEFA-EURO-2020-prediction-competition

        Rules for participation

        We would like to find the best UEFA 2020 tournament predictor. We pose no restrictions on how the predictions were made (it can be your favorite statistical method, from reading tea leaves, from sacrificing a rooster at dawn, or any other method), but you need to follow these rules:

            Deadline for prediction submission is Thursday, June 10th, 2021 by midnight CEST.
            One entry per person.
            You should be willing to explain how you obtained the prediction.
            The prediction winner will be the participant who provided a prediction that will return the lowest Tournament Rank Prediction Score as proposed in Evaluating one-shot tournament predictions by Ekstrøm, Van Eetvelde, Ley and Brefeld. The socceR package on CRAN will be used for the computation.
            Your submission should be a 7 x 24 matrix where the columns represent the countries, and the rows represent the possible ranks obtained after the tournament is over. The entries in the matrix should be numbers between 0 and 1 and represent probabilities that a given country will end up at a given rank. Consequently, each column must sum to 1. The code below will give provide you with an empty matrix (filled with 0s) with the desired order of the countries and ranks. Essentially, you just need to fill out the numbers in the matrix and upload that to GitHub.

        teams <- data.frame(
                     country=c("Turkey", "Italy", "Wales", "Switzerland",
                               "Denmark", "Finland", "Belgium", "Russia",
                           "Netherlands", "Ukraine", "Austria", "North Macedonia",
                       "England", "Croatia", "Scotland", "Czech Republic",
                       "Spain", "Sweden", "Poland", "Slovakia",
                       "Hungary", "Portugal", "France", "Germany"),
                     group=rep(LETTERS[1:6], each=4))

        ranknames <- c("1st", "2nd", "3rd", "4th", "5th-8th", "9th-16th", "17th-24th")

        m <- matrix(0, nrow=7, ncol=24)
        colnames(m) <- teams$country
        rownames(m) <- ranknames
        m
                  Turkey Italy Wales Switzerland Denmark Finland Belgium Russia
        1st            0     0     0           0       0       0       0      0
        2nd            0     0     0           0       0       0       0      0
        3rd            0     0     0           0       0       0       0      0
        4th            0     0     0           0       0       0       0      0
        5th-8th        0     0     0           0       0       0       0      0
        9th-16th       0     0     0           0       0       0       0      0
        17th-24th      0     0     0           0       0       0       0      0
                  Netherlands Ukraine Austria North Macedonia England Croatia Scotland
        1st                 0       0       0               0       0       0        0
        2nd                 0       0       0               0       0       0        0
        3rd                 0       0       0               0       0       0        0
        4th                 0       0       0               0       0       0        0
        5th-8th             0       0       0               0       0       0        0
        9th-16th            0       0       0               0       0       0        0
        17th-24th           0       0       0               0       0       0        0
                  Czech Republic Spain Sweden Poland Slovakia Hungary Portugal France
        1st                    0     0      0      0        0       0        0      0
        2nd                    0     0      0      0        0       0        0      0
        3rd                    0     0      0      0        0       0        0      0
        4th                    0     0      0      0        0       0        0      0
        5th-8th                0     0      0      0        0       0        0      0
        9th-16th               0     0      0      0        0       0        0      0
        17th-24th              0     0      0      0        0       0        0      0
                  Germany
        1st             0
        2nd             0
        3rd             0
        4th             0
        5th-8th         0
        9th-16th        0
        17th-24th       0

        In case of unclear rules I’ll make a decision.

        The winner will be announced on sandsynligvis.dk and on R-bloggers no later than a week after the UEFA EURO 2020 tournament is over.

        Hope to many of you participating. Think of the fame! The fortune! The friends! Everything that goers with it.
        Validating input prediction

        You can use the following validate_prediction() function to validate your input before uploading it to the GitHub page. It should capture most of the common errors that could occur.

        validate_prediction <- function(m) {

           if (any(dim(m) != c(7, 24))) {
             stop("Dimensions are not correct. Should be a 7 x 24 matrix")
           }

           if (min(m)<0) {
             stop("Cannot have negative prediction probabilities")
           }

           if (max(m)>0) {
             stop("Cannot have prediction probabilities larger than 1")
           }

           if (any(rowSums(m) != c(1,1,1,1,4,8,8))) {
             print(rowSums(m))
             stop("Row sums shown above are not correct")
           }  

           if (any(colSums(m) != 1)) {
             print(colSums(m))
             stop("Column sums shown above are not correct")
           }
        }


