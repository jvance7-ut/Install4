tConstr = function(x, y, alpha, independentSamp = TRUE, equalVar = TRUE){

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

#generate x and y
#and set alpha
set.seed(21)
x <-rnorm(30,5,2)
set.seed(23)
y<- rnorm(30,3,2)
alpha <- 0.05



#set function to object
obj = tConstr(x=x, y=y, alpha=alpha)

#create print.Rttest setup
library(kableExtra)
print.Rttest = function(funObject){
  kFO = kable(funObject$Data)
  z = NextMethod(kFO)
  class(z) = c("Rttest", class(z))
  kFO
}
#create plot.Rttest setup
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

#change class to Rttest
attr(obj, "class") ="Rttest"

#call function object parts
class(obj)
print(obj)
plot(obj)

