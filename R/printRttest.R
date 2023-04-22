#' Print method for class Rttest
#'
#' @param funObject
#' @author Jennifer Vance
#'
#' @return a table with the data set
#' @importFrom kableExtra kabel
#' @export print.Rttest
#'
#' @export
#'
#' @examples
#' \dontrun{print(funObject)}
print.Rttest = function(funObject, ...){
  kFO = kable(funObject$Data)
  z = NextMethod(kFO)
  class(z) = c("Rttest", class(z))
  kFO
}
