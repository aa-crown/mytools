#'@title Summary statistics
#'@description Calculate statistics by group level
#'@returns a tibble with n, mean, sd
#'@export
#'@import dplyr
#'@param data a data frame
#'@param x a numeric variable
#'@param ... one or more categorical variables
#'@examples
#'stats(mtcars, mpg, am)
#'stats(mtcars, mpg, am, vs)

stats <- function(data, x, ...){
  data %>%
    group_by(...) %>%
    summarize(n=n(),
              mean=mean({{x}}),
              sd=sd({{x}}))
}
