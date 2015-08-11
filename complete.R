complete <- function(directory, id = 1:332){
  curr_dir <- getwd()
  setwd(directory)
  file_names <- paste(formatC(id,width=3,flag=0),".csv",sep="")
  file_contents <- list()
  answer<-matrix(nrow=length(id),ncol=2)
  names(answer)<-c("id","nobs")
  for (i in 1:length(id)){
    file_contents[[i]]<-read.csv(file_names[i])
    answer[i,1] <- id[i]
    answer[i,2] <- sum(complete.cases(file_contents[[i]]))
  }
  setwd(curr_dir)
  final_answer<-data.frame(answer)
  names(final_answer)<-c("id","nobs")
  final_answer
}