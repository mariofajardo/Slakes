#' Fit a Gompertz function to 'Slakes' data
#'
#' This fits a Gompertz-like function on a specific range of values (same than the android app)
#' using Area versus Time data imported by the function \code{\link{readSlakes}}
#'
#' @import reshape
#' @param SlakesData data obtained by the function \code{\link{readSlakes}}
#' @author Mario Fajardo
#' @export
#'

FitGompertz <- function(SlakesData=NULL,path=NULL,id=NULL){

  SlakesData <- readSlakes(path)
  Timeserie <- SlakesData[,-c(1:3)]


p0 = c(a=0.01, b =5, c=0.5)

mo = ModelObject(
  name = 'Log-Gompertz',
  expr = expression(a*exp(-b*exp(-c*log(x)))))


CoefsByRep <- with(SlakesData,
                   data.frame(Treatment,
                              rep,
                              type='Predicted',
                              do.call(rbind,
                                      lapply(1:nrow(Timeserie),
                                             function(x){
                                               data_tmp <- melt(Timeserie[x,])
                                               colnames(data_tmp) <- c('x','y')
                                               data_tmp$x <- as.numeric(names(Timeserie))
                                               if(any(data_tmp$y<0))  data_tmp$y[data_tmp$y<0] <- 0
                                               fit = nlminb(p0, function(p, data){
                                                 r = data$y - mo$value(p,data)
                                                 return(r %*% r)
                                               }, gradient = mo$gradient,
                                               hessian = mo$hessian,
                                               data=data_tmp)$par
                                               return(fit)
                                             }))))
return(CoefsByRep)
}
