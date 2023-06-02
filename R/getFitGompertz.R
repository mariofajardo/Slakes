#' Fit a Gompertz function
#'
#' This function evaluates the input coefficients from the function \code{\link{readCoefs}} and evaluates the function on a specific range of values (same than the android app)
#'
#'
#' @param coefs coefficients obtained by the function \code{\link{readCoefs}}
#' @param id 'Treatment' name of the sample to be fitted
#' @author Mario Fajardo
#' @export

getFitGompertz <- function(coefs=NULL,id=NULL){

  Time <- as.numeric(c("1","2","3","4","5","6","7","8","9","10",
                       "11","12","13","14","15","16","17","18","19",
                       "20","21","22","23","24","25","26","27","28","29","30",
                       "32","34","36","38","40","42","44","46","48","50",
                       "54","58","62","66","70","78","86","110","150",
                       "210","280","380","480","600"))

  coefs <- as.numeric(coefs[coefs$Treatment==id,2:4])


  GompertzFunction <- function(coefs,Time){
    (coefs[1]*(exp(-coefs[2]*(exp(-coefs[3]*log(Time))))))
  }

  result <- GompertzFunction(coefs,Time)
  names(result) <- Time

  return(result)

}
