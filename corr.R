corr <- function(directory, threshold = 0) {
  comp_obs <- complete(directory)
  curr_dir <- getwd()
  setwd(directory)
  comp_obs_threshold <- comp_obs[comp_obs$nobs>threshold,]
  if (dim(comp_obs_threshold)[1]==0){
    setwd(curr_dir)
    return(numeric(0))
  } 
  if (dim(comp_obs_threshold)[1]!=0){
    id <- comp_obs_threshold$id
    file_names <- paste(formatC(id,width=3,flag=0),".csv",sep="")
    file_contents <- list()
    corr_values <- numeric()
    for (i in 1:length(id)){
      file_contents[[i]]<-read.csv(file_names[i])
      sulfate <- file_contents[[i]][complete.cases(file_contents[[i]]),2]
      nitrate <- file_contents[[i]][complete.cases(file_contents[[i]]),3]
      corr_values <- c(corr_values,cor(sulfate,nitrate))
    }
    setwd(curr_dir)
    corr_values
  }
}