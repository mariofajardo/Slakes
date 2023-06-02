#' Read 'slakes' coefficients data
#'
#' This function imports soil slaking fitted coefficients from the android app "slakes"
#'
#'
#' @param path path to the folder "slakes"
#' @author Mario Fajardo
#' @export

readCoefs <- function(path=NULL){

  prev_dir <- getwd()
  setwd(path)

  samples <- lapply(dir(pattern = 'csv'),function(x){


    treatment <- gsub('.csv','',x)
    treatment <- gsub('data_','',treatment)

    tmp <- read.csv(x,nrows = 1,header = F)

    coefs <- data.frame(Treatment=treatment,CoefA=tmp$V2,CoefB=tmp$V4,CoefC=tmp$V6)
  })

  setwd(prev_dir)

  FINAL <- do.call(rbind,samples)
  return(FINAL)
}
