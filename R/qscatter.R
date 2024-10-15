#'@title Quick Scatter Plot
#'@description Scatter plot with lines of best fit, correlation coefficient, and p-value
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@returns a ggplot 2 graph
#'@export
#'@import ggplot2
#'@examples qscatter(mtcars, wt, mpg)


qscatter <- function(data, x, y){
  xname <- as.character(substitute(x))
  yname <- as.character(substitute(y))
  gtitle=paste("Relationship between", xname, "and", yname)
  result <- cor.test(data[[xname]], data[[yname]])
  correlation <- result$estimate
  p_val <- result$p.value
  gsubtitle <- paste("R: ",correlation, ", P value: ", format.pval(p_val, digits=3))
  ggplot(mtcars, aes(x={{x}}, y={{y}}))+
    geom_point()+
    geom_smooth(method="lm", se=FALSE, linetype="dashed", color="cornflowerblue")+
    theme_minimal()+
    labs(title=gtitle,
         subtitle=gsubtitle)
}
