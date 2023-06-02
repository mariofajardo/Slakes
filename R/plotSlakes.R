#' Plot 'slakes' data
#'
#' This function creates a \code{\link{ggplot}} object and plots data exported from the android app "slakes"
#'
#'
#' @param dataSlakes data obtained by using the function \code{\link{readSlakes}}
#' @author Mario Fajardo
#' @import ggplot2
#' @import reshape
#' @return A \code{\link{ggplot}} object
#' @export


plotSlakes <- function(dataSlakes=NULL,
                       logScale=TRUE,
                       facet='Treatment',
                       Id='ALL'){

  if(any('rep'%in%colnames(dataSlakes))){
  dataSlakes <- melt(dataSlakes,id.vars = c('Treatment','rep','type'),variable_name='Time')
  }else{
    dataSlakes <- melt(dataSlakes,id.vars = c('Treatment','type'),variable_name='Time')
  }
  dataSlakes[,'Time'] <- as.numeric(as.character(dataSlakes[,'Time']))
  dataSlakes$value <- as.numeric(as.character(dataSlakes$value))

if(Id!='ALL'){
  finalPlot <- ggplot(dataSlakes[!grepl('Predicted',dataSlakes$type)&dataSlakes$Treatment%in%Id,],aes_string('Time','value',colour=facet))+
    geom_point()+
    geom_line(data = dataSlakes[grepl('Predicted',dataSlakes$type)&dataSlakes$Treatment%in%Id,],aes_string('Time','value',colour=facet))
}else{
  finalPlot <- ggplot(dataSlakes[!grepl('Predicted',dataSlakes$type),],aes_string('Time','value',colour=facet))+
    geom_point()+
    geom_line(data = dataSlakes[grepl('Predicted',dataSlakes$type),],aes_string('Time','value',colour=facet))
}


  if(logScale) {
    return(finalPlot+scale_x_log10())
  }else{
    return(finalPlot)}

}
