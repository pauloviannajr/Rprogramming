pollutantmean <- function(directory, pollutant, id = 1:332){
    curr_dir <- getwd()
    setwd(directory)
    file_names <- paste(formatC(id,width=3,flag=0),".csv",sep="")
    file_contents <- list()
    pollutant_values <- NA
    if (pollutant == "sulfate") column=2 else if (pollutant == "nitrate") column=3
    for (i in 1:length(id)){
        file_contents[[i]]<-read.csv(file_names[i])
        pollutant_values <- c(pollutant_values,file_contents[[i]][,column])
    }
    final_mean <- mean(pollutant_values,na.rm=T)
    setwd(curr_dir)
    round(final_mean,3)
}
  