#' Read 'slakes' data
#'
#' This function imports data exported from the android app "slakes"
#'
#'
#' @param path path to the folder "slakes"
#' @author Mario Fajardo
#' @export

readSlakes <- function(path=NULL){

  prev_dir <- getwd()
  setwd(path)

  samples <- lapply(dir(pattern = 'csv'),function(x){


    treatment <- gsub('.csv','',x)
    treatment <- gsub('data_','',treatment)

    tmp <- read.csv(x,skip = 2,header = T,check.names = F)
    tmpSample <- data.frame(Treatment=treatment,
                            rep=paste0('rep',seq(1:nrow(tmp))),
                            type='Observed',
                            tmp,stringsAsFactors = F)
  })

  setwd(prev_dir)

  FINAL <- do.call(rbind,samples)
  colnames(FINAL)[-c(1:2)] <-gsub('X','',colnames(FINAL)[-c(1:2)])
  return(FINAL)

}
