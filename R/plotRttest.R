#' Plot Rttest
#'
#' @param funObject
#' @param i x vector from object index
#' @param j y vector from object index
#' @param k TRUE/FALSE independent value from object index
#' @param l TRUE/FALSE equal variance value from object index
#'
#' @return dot plot of x and y with test type as the title
#' @export
#'
#' @examples
#' \dontrun{plot(funObject)}
plot.Rttest = function(funObject, i=5, j=6, k=7, l=8){
  par(mar = c(1, 1, 1, 1))
  Y = funObject[[j]]
  X = funObject[[i]]
  plotT <- plot(Y~X,
                main = ifelse(funObject[[k]] == FALSE, "Paired t-test",
                              ifelse(funObject[[l]] == TRUE, "Independent, Equal Var t-test",
                                     "Independent, Unequal var t-test")), col = "blue",
                pch=19 )
  plotT

}
