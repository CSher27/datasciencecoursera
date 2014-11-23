pollutantmean <- function(dir, poll, id = 1:332) {
        ## dir = character vector, length 1, directory location of CSV files 

        ## poll = character vector, length 1, pollutant - either "sulfate" or "nitrate"

        ## id = integer vector, length 1, ID numbers of monitors to read

        ## Returns the mean of the selected pollutant reading from monitors 
        ## in the 'id' vector (ignoring NA values)

## generate vector of file names for specified id
fnames<-c(paste(dir,'/',sprintf("%03d",id),'.csv',sep=''))

##  choose set columns to be read based on which measure needed
mycols<-if(poll=='sulfate') {
          c('NULL',sulfate="numeric",'NULL','NULL')
} else {
          c('NULL','NULL',nitrate="numeric",'NULL')
}

##  read in data from files
mydata<-do.call("rbind",lapply(fnames,read.csv,colClasses=mycols,comment.char=""))

##clean the data of NA values
cleandata<-mydata[!is.na(mydata)]

## calculate the mean and round to 3 decimals
round(mean(cleandata),digits=3)

}