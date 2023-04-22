#' Title t-test Installment 4
#'
#' @param x X Vector (numeric)
#' @param y Y vector (numeric)
#' @param alpha alpha/error value (numeric)
#' @param independentSamp Is independent? (default TRUE)
#' @param equalVar Has equal variance? (default TRUE)
#'
#' @return t-test and input results
#' @export
#'
#' @examples
#' \dontrun{tConstr(x=rnorm(30,5,2), y=rnorm(30,3,2), alpha=0.05)}

tConstr = function(x, y, alpha, independentSamp = TRUE, equalVar = TRUE){
  library(devtools)
  library(kableExtra)
  if(independentSamp==TRUE && equalVar == TRUE){
    #run the t-test with equal var
    ttest = t.test(x, y, var.equal = TRUE)
  }
  else if(independentSamp==TRUE && equalVar==FALSE){
    #run the t-test with unequal var
    ttest = t.test(x, y, var.equal = TRUE)
  }
  else{
    ttest = t.test(x, y, paired = TRUE)
  }
  #create the data frame of x and y
  #x and y won't always be the same length and may return an error
  #check the lengths of x and y and make corrections to length if needed with NA values
  if(length(x)==length(y)){
    df = data.frame(x = x, y = y)
  }

  else if(length(x)<length(y)){
    xtemp = rep(NA, length(y))
    for (i in 1:length(x)) {
      xtemp[i] = x[i]
    }
    df = data.frame(x = xtemp, y = y)
  }

  else{
    ytemp = rep(NA, length(x))
    for (i in 1:length(y)) {
      ytemp[i] = y[i]
    }
    df = data.frame(x = x, y = ytemp)
  }


  #return the function information in a list
  list(Data = df, Alpha = alpha, CI = ttest$conf.int, Pvalue = ttest$p.value,
       x = x, y = y, is = independentSamp, evar = equalVar)
}
